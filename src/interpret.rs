use std::collections::HashMap;
use std::env;
use std::ffi::OsString;

use gc::Gc;
use gc::GcCell;

use crate::ast::*;
use crate::ccall::Libraries;
use crate::except::KlisterRTE;
use crate::value::bin_op;
use crate::value::KlisterArray;
use crate::value::KlisterBool;
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


#[must_use] 
pub enum ExpE<T> {
    Ok(T),
    Err(KlisterRTE),
    Return(ValWrap),
}

type ExpV = ExpE<ValWrap>;

macro_rules! ask {
    ($myexpr:expr) => {
        match $myexpr {
            Ok(v) => v,
            Err(e) => {return ExpE::Err(e)},
        }
    };
}

macro_rules! ask2 {
    ($myexpr:expr) => {
        match $myexpr {
            ExpE::Ok(v) => v,
            ExpE::Err(e) => {return ExpE::Err(e)},
            ExpE::Return(r) => {return ExpE::Return(r)},
        }
    };
}

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

fn handle_shell_arg(context: &mut Context, glob_parts: &Vec<GlobPart>, just_one: bool) -> ExpE<Vec<Vec<u8>>> {
    let mut out_arg = Vec::<u8>::new();
    for part in glob_parts {
        match part {
            GlobPart::Str(ref s) => {
                out_arg.extend(s.as_bytes());
            }
            GlobPart::Interpolation(ref expr) => {
                let val = ask!(ask2!(handle_expression(context, expr)).borrow().interpolate());
                out_arg.extend(&val);
            }
            GlobPart::Asterisk => {
                return ExpE::Err(KlisterRTE::new("Asterisks are not supported yet", false));
            }
            GlobPart::ArrayInterpolation(ref expr) => {
                if just_one {
                    return ExpE::Err(KlisterRTE::new("Array interpolation not allowed in this context", false));
                }
                if glob_parts.len() != 1 {
                    return ExpE::Err(KlisterRTE::new("Array interpolation cannot be affixed", false));
                }
                let val = ask2!(handle_expression(context, expr));
                let borrower = val.borrow();
                let xx: &dyn KlisterValueV2 = borrower.as_ref();
                let arr = ask!(xx.as_any().downcast_ref::<KlisterArray>().ok_or_else(|| KlisterRTE::new("Expression was not array", false)));
                let mut out_vec = Vec::new();
                for qq in &arr.val {
                    out_vec.push(ask!(qq.borrow().interpolate()));
                }
                return ExpE::Ok(out_vec);
            }
        }
    }
    let out_vec = vec![out_arg];
    return ExpE::Ok(out_vec);
}

fn handle_just_one(context: &mut Context, glob_parts: &Vec<GlobPart>) -> ExpE<OsString> {
    let ret = ask2!(handle_shell_arg(context, glob_parts, true));
    if ret.len() != 1 {
        return ExpE::Err(KlisterRTE::new("Invalid expansion", false)); 
    }
    return ExpE::Ok(ask2!(verified_osstring_from_vec(ret.into_iter().next().unwrap())))
}

fn handle_just_two(context: &mut Context, glob_parts: &Vec<GlobPart>) -> ExpE<Vec<u8>> {
    let ret = ask2!(handle_shell_arg(context, glob_parts, true));
    if ret.len() != 1 {
        return ExpE::Err(KlisterRTE::new("Invalid expansion", false)); 
    }
    return ExpE::Ok(ret.into_iter().next().unwrap())
}

use std::os::unix::ffi::OsStringExt;

fn verified_osstring_from_vec(v: Vec<u8>) -> ExpE<OsString> {
    for b in &v {
        if *b == 0 {
            return ExpE::Err(KlisterRTE::new("Embedded nul", true)); 
        }
    }
    return ExpE::Ok(OsString::from_vec(v));
}

fn handle_shell_args(context: &mut Context, argon: &Vec<Vec<GlobPart>>) -> ExpE<Vec<OsString>> {
    let mut ret = Vec::<OsString>::new();
    for argona in argon {
        for x in ask2!(handle_shell_arg(context, argona, false)) {
            ret.push(ask2!(verified_osstring_from_vec(x)));
        }
    }
    return ExpE::Ok(ret);
}

fn handle_shell_pipeline(context: &mut Context, sp: &ShellPipelineS) -> ExpE<KlisterShellRes> {
    let cmds = &sp.commands;
    if cmds.len() == 0 {
        return ExpE::Err(KlisterRTE::new("Empty pipeline", false));
    };

    let mut output_opt: Option<Vec<u8>> = None;

    let mut i = 0;
    let cmdlen = cmds.len();

    for cmd in cmds.into_iter() {
        let args = ask2!(handle_shell_args(context, &cmd.args));
        let command = ask2!(handle_just_one(context, &cmd.command));

        let mut ductcmd = duct::cmd(command, args);

        let go_through = i == cmdlen-1 && sp.is_write;

        match output_opt {
            Some(bytes) => {
                if !matches!(cmd.stdin, Stdinput::Default) {
                    return ExpE::Err(KlisterRTE::new("Invalid stdin redirect", false));
                }
                ductcmd = ductcmd.stdin_bytes(bytes);
            }
            None => {
                match cmd.stdin {
                    Stdinput::Default => {}
                    Stdinput::File(ref f) => {ductcmd = ductcmd.stdin_path(ask2!(handle_just_one(context, f)));}
                    Stdinput::Heredoc(ref f) => {ductcmd = ductcmd.stdin_bytes(ask2!(handle_just_two(context, f)));}
                }
            }
        };
        match &cmd.outerr {
            OutErr::NoMerge(o_opt, e_opt) => {
                if let Some(ref o) = o_opt {
                    if go_through {return ExpE::Err(KlisterRTE::new("Invalid stdout redirect", false));}
                    ductcmd = ductcmd.stdout_path(ask2!(handle_just_one(context, o)));
                }
                if let Some(ref e) = e_opt {
                    ductcmd = ductcmd.stderr_path(ask2!(handle_just_one(context, e)));
                }
            }
            OutErr::MergedToStderr => {
                if go_through {return ExpE::Err(KlisterRTE::new("Invalid stdout redirect", false));}
                ductcmd = ductcmd.stdout_to_stderr()
            }
            OutErr::MergedToStdout => {ductcmd = ductcmd.stderr_to_stdout()}
            OutErr::MergedToFile(ref o) => {
                if go_through {return ExpE::Err(KlisterRTE::new("Invalid stdout redirect", false));}
                ductcmd = ductcmd.stderr_to_stdout().stdout_path(ask2!(handle_just_one(context, o)));
            }
        };
        if !go_through {
            ductcmd = ductcmd.stdout_capture();
        }
        let child_res = ductcmd.unchecked().run();
        let Ok(child_res) = child_res else {
            return ExpE::Ok(KlisterShellRes::SResErr(KlisterRTE::new("Failed to run command", true), Vec::new(), None));
        };

        if !child_res.status.success() {
            return ExpE::Ok(KlisterShellRes::SResErr(KlisterRTE::new("Shell command failed", true), child_res.stdout, child_res.status.code()));
        }

        // todo: Unnecessary clones here, clean this up.
        i+=1;
        output_opt = Some(child_res.stdout.clone())
    }
    let output = output_opt.expect("Internal interpreter error: Output was none");
    ExpE::Ok(KlisterShellRes::SResOk(output))
}


fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> ExpV {
    match expression {
        KlisterExpression::Array(arr) => {
            let mut argument_values = Vec::new();
            for x in arr.iter() {
                argument_values.push(ask2!(handle_expression(context, x)));
            }
            return ExpE::Ok(valwrap(KlisterArray{val: argument_values}))
        }
        KlisterExpression::Call(fn_expr, arguments) => {
            let mut argument_values = Vec::new();
            for x in arguments.iter() {
                argument_values.push(ask2!(handle_expression(context, x)));
            }
            let v = ask2!(handle_expression(context, fn_expr));
            let res = {
                let borrower = v.borrow();
                borrower.call(context, argument_values)
            };
            return ExpE::Ok(ask!(res))
        }
        KlisterExpression::Index(arr, index) => {
            let lv = ask2!(handle_expression(context, arr));
            let rv = ask2!(handle_expression(context, index));
            return ExpE::Ok(ask!(lv.borrow().subscript(context, &rv)));
        }
        KlisterExpression::Dot(obj_expr, subscript) => {
            let obj = ask2!(handle_expression(context, obj_expr));
            return ExpE::Ok(ask!(obj.borrow().dot(&obj, subscript)));
        }
        KlisterExpression::Literal(v) => {ExpE::Ok(Gc::new(GcCell::new(v.clone())))}
        KlisterExpression::Variable(v) => {
            match context.get_var(v) {
                Some(val) => ExpE::Ok(val.clone()),
                None => {return ExpE::Err(KlisterRTE::new(&format!("Variable not defined {}", v), false));}
            }
        }
        KlisterExpression::BinOp(op, left, right) => {
            let lv = ask2!(handle_expression(context, left));
            if matches!(op, Operation::Or) && ask!(lv.borrow().bool_val()) == true {
                return ExpE::Ok(valwrap(KlisterBool{val: true}))
            }
            if matches!(op, Operation::And) && ask!(lv.borrow().bool_val()) == false {
                return ExpE::Ok(valwrap(KlisterBool{val: false}))
            }
            let rv = ask2!(handle_expression(context, right));
            ExpE::Ok(ask!(bin_op(op.clone(), lv, rv)))
        }
        KlisterExpression::Not(expr) => {
            let v = ask2!(handle_expression(context, expr));
            let res = {
                let borrower = v.borrow();
                borrower.un_op("!")
            };
            ExpE::Ok(ask!(res))
        }
        KlisterExpression::CatchExpr(expr) => {
            let v_res = handle_expression(context, expr);
            match v_res {
                ExpE::Ok(v) => {
                    ExpE::Ok(KlisterResult::ok_wrapped(v))
                }
                ExpE::Err(e) => {
                    if e.catchable {
                        ExpE::Ok(valwrap(KlisterResult::ResErr(Box::new(e))))
                    } else {
                        ExpE::Err(e)
                    }
                }
                ExpE::Return(r) => {
                    return ExpE::Return(r);
                }
            }
        }
        KlisterExpression::CatchBlock(block) => {
            let v_res = handle_statement(context, block);
            match v_res {
                ExpE::Ok(_) => {
                    ExpE::Ok(KlisterResult::ok_wrapped(valwrap(KlisterNothing{})))
                }
                ExpE::Return(r) => {
                    return ExpE::Return(r);
                }
                ExpE::Err(e) => {
                    if e.catchable {
                        ExpE::Ok(valwrap(KlisterResult::ResErr(Box::new(e))))
                    } else {
                        ExpE::Err(e)
                    }
                }
            }
        }
        KlisterExpression::ShellPipeline(sp) => {
            let result = ask2!(handle_shell_pipeline(context, sp));
            if sp.is_catch {
                return ExpE::Ok(valwrap(result));
            }
            return match result {
                // Todo: unncessary clones here, remove.
                KlisterShellRes::SResOk(ref v) => {
                    ExpE::Ok(valwrap(KlisterBytes{val: v.clone()}))
                }
                KlisterShellRes::SResErr(ref e, _, _) => {
                    ExpE::Err(e.clone())
                }
            }
        }
    }
}

use crate::value::KlisterInteger;
use crate::value::KlisterValueV2;

pub fn handle_statement(context: &mut Context, statement: &KlisterStatement) -> ExpE<()> {
    match statement {
        KlisterStatement::Function(name, arg_names, block) => {
            context.put_var(name, valwrap(KlisterFunction{body: block.clone(), arg_names: arg_names.clone()}));
            ExpE::Ok(())
        }
        KlisterStatement::Import(libname, fname, rettypename, argnames) => {
            ask!(handle_import(context, libname, fname, rettypename, argnames));
            ExpE::Ok(())
        }
        KlisterStatement::Expression(expression) => {
            ask2!(handle_expression(context, &expression));
            ExpE::Ok(())
        }
        KlisterStatement::Assign(lhs, rhs) => {
            match lhs {
                KlisterExpression::Variable(name) => {
                    let val = ask2!(handle_expression(context, &rhs));
                    context.put_var(name, val);
                    ExpE::Ok(())
                }
                KlisterExpression::Index(ref arr, ref ind) => {
                    let arr_v = ask2!(handle_expression(context, arr));
                    let ind_v = ask2!(handle_expression(context, ind));
                    let val = ask2!(handle_expression(context, &rhs));
                    ask!(arr_v.borrow_mut().subscript_assign(context, ind_v, val));
                    ExpE::Ok(())
                }
                _ => {return ExpE::Err(KlisterRTE::new("Assignment needs lvalue", false))}
            }
        }
        KlisterStatement::Return(expression) => {
            let val = ask2!(handle_expression(context, &expression));
            ExpE::Return(val)
        }
        KlisterStatement::Block(statements) => {
            for part in statements {
                ask2!(handle_statement(context, part));
            }
            ExpE::Ok(())
        }
        KlisterStatement::While(condition, block) => {
            loop {
                let cond_val = ask!(ask2!(handle_expression(context, &condition)).borrow().bool_val());
                if !cond_val {
                    break;
                }
                ask2!(handle_statement(context, block));
            }
            ExpE::Ok(())
        }
        KlisterStatement::If(condition, ifblock, elseblock_opt) => {
            let cond_val = ask!(ask2!(handle_expression(context, &condition)).borrow().bool_val());
            if cond_val {
                return handle_statement(context, ifblock);
            } else if let Some(elseblock) = elseblock_opt {
                return handle_statement(context, elseblock);
            }
            ExpE::Ok(())
        }
        KlisterStatement::ForEach(varname, expr, block) => {
            let arraylike = ask2!(handle_expression(context, &expr));
            let len = ask!(arraylike.borrow().dot(&arraylike, "len"));
            let len: usize = {
                let borrower = len.borrow();
                let xx: &dyn KlisterValueV2 = borrower.as_ref();
                xx.as_any().downcast_ref::<KlisterInteger>().unwrap().val.clone().try_into().unwrap()
            };
            for i in 0..len {
                let v = ask!(arraylike.borrow().subscript(context, &valwrap(KlisterInteger{val: i.into()})));

                context.put_var(varname, v);

                ask2!(handle_statement(context, block));
            }
            ExpE::Ok(())
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
        ExpE::Ok(_) => None,
        ExpE::Err(e) => {return Some(e)},
        ExpE::Return(_r) => {return Some(KlisterRTE::new("Global scope cannot return", false))},
    } 
}
