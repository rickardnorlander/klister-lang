use std::collections::HashMap;
use std::env;
use std::ffi::OsString;

use gc::Gc;

use crate::ast::*;
use crate::ccall::Libraries;
use crate::except::KlisterRTE;
use crate::value::bin_op;
use crate::value::KlisterArray;
use crate::value::KlisterBytes;
use crate::value::KlisterCFunction;
use crate::value::KlisterDict;
use crate::value::KlisterFunction;
use crate::value::KlisterResult;
use crate::value::KlisterShellRes;
use crate::value::KlisterStr;
use crate::value::KlisterNothing;
use crate::value::ValWrap;
use crate::value::valwrap;

#[derive(gc::Trace, gc::Finalize)]
pub struct Context {
    #[unsafe_ignore_trace]
    pub libs: Libraries,
    global_scope: HashMap<String, ValWrap>,
    function_scopes: Vec<HashMap<String, ValWrap>>,
}

impl Context {
    fn new() -> Context {
        return Context{libs: Libraries::new(), global_scope: HashMap::new(), function_scopes: Vec::new()}
    }

    fn get_var(&self, s: &str) -> Option<&ValWrap> {
        if let Some(f_scope) = self.function_scopes.last() {
            if let Some(v) = f_scope.get(s) {
                return Some(v);
            }
        }
        return self.global_scope.get(s);
    }

    pub fn put_var(&mut self, s: &str, v: ValWrap) {
        let scope: &mut _ = self.function_scopes.last_mut().unwrap_or(&mut self.global_scope);
        scope.insert(s.to_string(), v);
    }

    pub fn enter_function(&mut self) {
        self.function_scopes.push(HashMap::new());
    }

    pub fn exit_function(&mut self) {
        self.function_scopes.pop();
    }
}

fn handle_import(context: &mut Context, libname: &str, fname: &str, rettypename: &str, argnames: &Vec<String>) -> Result<(), KlisterRTE> {
    context.libs.load_fn(libname, fname, rettypename, &argnames.iter().map(String::as_str).collect::<Vec<_>>())?;
    context.put_var(fname, KlisterCFunction::wrapped(fname));
    Ok(())
}

fn handle_shell_arg(context: &mut Context, glob_parts: &Vec<GlobPart>, just_one: bool) -> Result<Vec<Vec<u8>>, KlisterRTE> {
    let mut out_arg = Vec::<u8>::new();
    for part in glob_parts {
        match part {
            GlobPart::Str(ref s) => {
                out_arg.extend(s.as_bytes());
            }
            GlobPart::Interpolation(ref expr) => {
                let val = handle_expression(context, expr)?.interpolate()?;
                out_arg.extend(&val);
            }
            GlobPart::Asterisk => {
                return Err(KlisterRTE::new("Asterisks are not supported yet", false));
            }
            GlobPart::ArrayInterpolation(ref expr) => {
                if just_one {
                    return Err(KlisterRTE::new("Array interpolation not allowed in this context", false));
                }
                if glob_parts.len() != 1 {
                    return Err(KlisterRTE::new("Array interpolation cannot be affixed", false));
                }
                let val = handle_expression(context, expr)?;
                let xx: &dyn KlisterValueV2 = (*val).as_ref();
                let arr = xx.as_any().downcast_ref::<KlisterArray>().ok_or_else(|| KlisterRTE::new("Expression was not array", false))?;
                let mut out_vec = Vec::new();
                for qq in &arr.val {
                    out_vec.push(qq.interpolate()?);
                }
                return Ok(out_vec);
            }
        }
    }
    let out_vec = vec![out_arg];
    return Ok(out_vec);
}

fn handle_just_one(context: &mut Context, glob_parts: &Vec<GlobPart>) -> Result<OsString, KlisterRTE> {
    let ret = handle_shell_arg(context, glob_parts, true)?;
    if ret.len() != 1 {
        return Err(KlisterRTE::new("Invalid expansion", false)); 
    }
    return Ok(verified_osstring_from_vec(ret.into_iter().next().unwrap())?)
}

fn handle_just_two(context: &mut Context, glob_parts: &Vec<GlobPart>) -> Result<Vec<u8>, KlisterRTE> {
    let ret = handle_shell_arg(context, glob_parts, true)?;
    if ret.len() != 1 {
        return Err(KlisterRTE::new("Invalid expansion", false)); 
    }
    return Ok(ret.into_iter().next().unwrap())
}

use std::os::unix::ffi::OsStringExt;

fn verified_osstring_from_vec(v: Vec<u8>) -> Result<OsString, KlisterRTE> {
    for b in &v {
        if *b == 0 {
            return Err(KlisterRTE::new("Embedded nul", true)); 
        }
    }
    return Ok(OsString::from_vec(v));
}

fn handle_shell_args(context: &mut Context, argon: &Vec<Vec<GlobPart>>) -> Result<Vec<OsString>, KlisterRTE> {
    let mut ret = Vec::<OsString>::new();
    for argona in argon {
        for x in handle_shell_arg(context, argona, false)? {
            ret.push(verified_osstring_from_vec(x)?);
        }
    }
    return Ok(ret);
}

fn handle_shell_pipeline(context: &mut Context, sp: &ShellPipelineS) -> Result<KlisterShellRes, KlisterRTE> {
    let cmds = &sp.commands;
    if cmds.len() == 0 {
        return Err(KlisterRTE::new("Empty pipeline", false));
    };

    let mut output_opt: Option<Vec<u8>> = None;

    let mut i = 0;
    let cmdlen = cmds.len();

    for cmd in cmds.into_iter() {
        let args = handle_shell_args(context, &cmd.args)?;
        let command = handle_just_one(context, &cmd.command)?;

        let mut ductcmd = duct::cmd(command, args);

        let go_through = i == cmdlen-1 && sp.is_write;

        match output_opt {
            Some(bytes) => {
                if !matches!(cmd.stdin, Stdinput::Default) {
                    return Err(KlisterRTE::new("Invalid stdin redirect", false));
                }
                ductcmd = ductcmd.stdin_bytes(bytes);
            }
            None => {
                match cmd.stdin {
                    Stdinput::Default => {}
                    Stdinput::File(ref f) => {ductcmd = ductcmd.stdin_path(handle_just_one(context, f)?);}
                    Stdinput::Heredoc(ref f) => {ductcmd = ductcmd.stdin_bytes(handle_just_two(context, f)?);}
                }
            }
        };
        match &cmd.outerr {
            OutErr::NoMerge(o_opt, e_opt) => {
                if let Some(ref o) = o_opt {
                    if go_through {return Err(KlisterRTE::new("Invalid stdout redirect", false));}
                    ductcmd = ductcmd.stdout_path(handle_just_one(context, o)?);
                }
                if let Some(ref e) = e_opt {
                    ductcmd = ductcmd.stderr_path(handle_just_one(context, e)?);
                }
            }
            OutErr::MergedToStderr => {
                if go_through {return Err(KlisterRTE::new("Invalid stdout redirect", false));}
                ductcmd = ductcmd.stdout_to_stderr()
            }
            OutErr::MergedToStdout => {ductcmd = ductcmd.stderr_to_stdout()}
            OutErr::MergedToFile(ref o) => {
                if go_through {return Err(KlisterRTE::new("Invalid stdout redirect", false));}
                ductcmd = ductcmd.stderr_to_stdout().stdout_path(handle_just_one(context, o)?);
            }
        };
        if !go_through {
            ductcmd = ductcmd.stdout_capture();
        }
        let child_res = ductcmd.unchecked().run();
        let Ok(child_res) = child_res else {
            return Ok(KlisterShellRes::SResErr(KlisterRTE::new("Failed to run command", true), Vec::new(), None));
        };

        if !child_res.status.success() {
            return Ok(KlisterShellRes::SResErr(KlisterRTE::new("Shell command failed", true), child_res.stdout, child_res.status.code()));
        }

        // todo: Unnecessary clones here, clean this up.
        i+=1;
        output_opt = Some(child_res.stdout.clone())
    }
    let output = output_opt.expect("Internal interpreter error: Output was none");
    Ok(KlisterShellRes::SResOk(output))
}

fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> Result<ValWrap, KlisterRTE> {
    match expression {
        KlisterExpression::Array(arr) => {
            let argument_values: Vec<ValWrap> = arr.iter().map(|x| handle_expression(context, x)).collect::<Result<Vec<ValWrap>, KlisterRTE>>()?;
            return Ok(valwrap(KlisterArray{val: argument_values}))
        }
        KlisterExpression::Call(fn_expr, arguments) => {
            let argument_values: Vec<ValWrap> = arguments.iter().map(|x| handle_expression(context, x)).collect::<Result<Vec<ValWrap>, KlisterRTE>>()?;
            let v = handle_expression(context, fn_expr)?;
            return v.call(context, argument_values)
        }
        KlisterExpression::Index(arr, index) => {
            let lv = handle_expression(context, arr)?;
            let rv = handle_expression(context, index)?;
            return lv.subscript(context, &rv);
        }
        KlisterExpression::Dot(obj_expr, subscript) => {
            let obj = handle_expression(context, obj_expr)?;
            return obj.dot(&obj, subscript);
        }
        KlisterExpression::Literal(v) => {Ok(Gc::new(v.clone()))}
        KlisterExpression::Variable(v) => {
            match context.get_var(v) {
                Some(val) => Ok(val.clone()),
                None => {return Err(KlisterRTE::new(&format!("Variable not defined {}", v), false));}
            }
        }
        KlisterExpression::BinOp(op, left, right) => {
            let lv = handle_expression(context, left)?;
            let rv = handle_expression(context, right)?;
            bin_op(op.clone(), lv, rv)
        }
        KlisterExpression::Not(expr) => {
            let v = handle_expression(context, expr)?;
            v.un_op("!")
        }
        KlisterExpression::CatchExpr(expr) => {
            let v_res = handle_expression(context, expr);
            match v_res {
                Ok(v) => {
                    Ok(KlisterResult::ok_wrapped(v))
                }
                Err(e) => {
                    if e.catchable {
                        Ok(valwrap(KlisterResult::ResErr(Box::new(e))))
                    } else {
                        Err(e)
                    }
                }
            }
        }
        KlisterExpression::CatchBlock(block) => {
            let v_res = handle_statement(context, block);
            match v_res {
                StatementE::AllGood => {
                    Ok(KlisterResult::ok_wrapped(valwrap(KlisterNothing{})))
                }
                StatementE::Return(r) => {
                    todo!("Can't return from catch blocks")
                }
                StatementE::Err(e) => {
                    if e.catchable {
                        Ok(valwrap(KlisterResult::ResErr(Box::new(e))))
                    } else {
                        Err(e)
                    }
                }
            }
        }
        KlisterExpression::ShellPipeline(sp) => {
            let result = handle_shell_pipeline(context, sp)?;
            if sp.is_catch {
                return Ok(valwrap(result));
            }
            return match result {
                // Todo: unncessary clones here, remove.
                KlisterShellRes::SResOk(ref v) => {
                    Ok(valwrap(KlisterBytes{val: v.clone()}))
                }
                KlisterShellRes::SResErr(ref e, _, _) => {
                    Err(e.clone())
                }
            }
        }
    }
}

pub enum StatementE {
    AllGood,
    Return(ValWrap),
    Err(KlisterRTE),
}

fn rtose<T>(a: Result<T, KlisterRTE>) -> StatementE {
    match a {
        Ok(_) => StatementE::AllGood,
        Err(e) => StatementE::Err(e),
    }
}


macro_rules! ask {
    ($myexpr:expr) => {
        match $myexpr {
            Ok(v) => v,
            Err(e) => {return StatementE::Err(e)},
        }
    };
}

use crate::value::KlisterInteger;
use crate::value::KlisterValueV2;

pub fn handle_statement(context: &mut Context, statement: &KlisterStatement) -> StatementE {
    match statement {
        KlisterStatement::Function(name, arg_names, block) => {
            context.put_var(name, valwrap(KlisterFunction{body: block.clone(), arg_names: arg_names.clone()}));
            StatementE::AllGood
        }
        KlisterStatement::Import(libname, fname, rettypename, argnames) => {
            rtose(handle_import(context, libname, fname, rettypename, argnames))
        }
        KlisterStatement::Expression(expression) => {
            rtose(handle_expression(context, &expression))
        }
        KlisterStatement::Assign(name, expression) => {
            let val = ask!(handle_expression(context, &expression));
            context.put_var(name, val);
            StatementE::AllGood
        }
        KlisterStatement::Return(expression) => {
            let val = ask!(handle_expression(context, &expression));
            StatementE::Return(val)
        }
        KlisterStatement::Block(statements) => {
            for part in statements {
                match handle_statement(context, part) {
                    StatementE::AllGood => {},
                    StatementE::Err(e) => {return StatementE::Err(e)},
                    StatementE::Return(r) => {return StatementE::Return(r)},
                }
            }
            StatementE::AllGood
        }
        KlisterStatement::While(condition, block) => {
            loop {
                let cond_val = ask!(ask!(handle_expression(context, &condition)).bool_val());
                if !cond_val {
                    break;
                }
                match handle_statement(context, block) {
                    StatementE::AllGood => {},
                    StatementE::Err(e) => {return StatementE::Err(e)},
                    StatementE::Return(r) => {return StatementE::Return(r)},
                }
            }
            StatementE::AllGood
        }
        KlisterStatement::If(condition, ifblock, elseblock_opt) => {
            let cond_val = ask!(ask!(handle_expression(context, &condition)).bool_val());
            if cond_val {
                return handle_statement(context, ifblock);
            } else if let Some(elseblock) = elseblock_opt {
                return handle_statement(context, elseblock);
            }
            StatementE::AllGood
        }
        KlisterStatement::ForEach(varname, expr, block) => {
            let arraylike = ask!(handle_expression(context, &expr));
            let len = ask!(arraylike.dot(&arraylike, "len"));
            let xx: &dyn KlisterValueV2 = (*len).as_ref();
            let len: usize = xx.as_any().downcast_ref::<KlisterInteger>().unwrap().val.clone().try_into().unwrap();
            for i in 0..len {
                let v = ask!(arraylike.subscript(context, &valwrap(KlisterInteger{val: i.into()})));

                context.put_var(varname, v);

                match handle_statement(context, block) {
                    StatementE::AllGood => {},
                    StatementE::Err(e) => {return StatementE::Err(e)},
                    StatementE::Return(r) => {return StatementE::Return(r)},
                }
            }
            StatementE::AllGood
        }
    }
}

pub fn interpret_ast(ast: KlisterStatement, arguments: Vec<String>) -> Option<KlisterRTE> {
    let mut context = Context::new();

    let wrapped_argv = valwrap(KlisterArray{val: arguments.into_iter().map(|x| valwrap(KlisterStr{val: x})).collect::<Vec<ValWrap>>()});

    context.put_var("argv", wrapped_argv);

    let wrapped_env_vars = valwrap(KlisterDict{val: env::vars().map(|(x,y)| (x, valwrap(KlisterStr{val: y}))).collect()});
    context.put_var("env", wrapped_env_vars);

    match handle_statement(&mut context, &ast) {
        StatementE::AllGood => None,
        StatementE::Err(e) => {return Some(e)},
        StatementE::Return(_r) => {return Some(KlisterRTE::new("Global scope cannot return", false))},
    } 
}
