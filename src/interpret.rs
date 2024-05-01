use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::process::Command;

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

fn handle_shell_arg(context: &mut Context, argona: &Argon) -> Result<String, KlisterRTE> {
    let Argon::ArgonGlob(glob_parts) = argona else {
        return Err(KlisterRTE::new("Array refs are not supported yet", false));
    };
    let mut out_arg = String::new();
    for part in glob_parts {
        match part {
            GlobPart::GlobPartS(ref s) => {
                out_arg.push_str(s);
            }
            GlobPart::GlobPartInterpolation(ref expr) => {
                let val = handle_expression(context, expr)?.str_val()?;
                out_arg.push_str(&val);
            }
            GlobPart::GlobPartAsterisk => {
                return Err(KlisterRTE::new("Asterisks are not supported yet", false));
            }
        }
    }
    return Ok(out_arg);
}

fn handle_shell_args(context: &mut Context, argon: &Vec<Argon>) -> Result<Vec<String>, KlisterRTE> {
    let mut ret = Vec::<String>::new();
    for argona in argon {
        ret.push(handle_shell_arg(context, argona)?);
    }
    return Ok(ret);
}

fn handle_shell_pipeline(context: &mut Context, sp: &ShellPipelineS) -> Result<KlisterShellRes, KlisterRTE> {
    let cmds = &sp.commands;
    if cmds.len() == 0 {
        return Err(KlisterRTE::new("Empty pipeline", false));
    };

    let mut output_opt: Option<Vec<u8>> = None;

    for cmd in cmds.into_iter() {
        let stdinxxx = match output_opt.is_some() {
            true => std::process::Stdio::piped(),
            false => std::process::Stdio::inherit(),
        };
        let args = handle_shell_args(context, &cmd.args)?;
        let command = handle_shell_arg(context, &cmd.command)?;
        let child_res = Command::new(command).args(args).stdin(stdinxxx).stderr(std::process::Stdio::inherit()).stdout(std::process::Stdio::piped()).spawn();
        let Ok(mut child) = child_res else {
            return Ok(KlisterShellRes::SResErr(KlisterRTE::new("Failed to spawn child", true), Vec::new(), None));
        };
        let mut write_ok = true;

        let scope_result = std::thread::scope(|my_thread_scope| {
            if let Some(output) = output_opt {
                let Some(mut stdin) = child.stdin.take() else {
                    return KlisterShellRes::SResErr(KlisterRTE::new("Failed to open stdin", true), Vec::new(), None);
                };
                {
                    let write_ok = &mut write_ok;
                    my_thread_scope.spawn(move || {
                        if stdin.write_all(&output).is_err() {
                            *write_ok = false;
                        }
                    });
                }
            }
    
            let Ok(output) = child.wait_with_output() else {
                return KlisterShellRes::SResErr(KlisterRTE::new("Failed to read stdout", true), Vec::new(), None);
            };
            if !output.status.success() {
                return KlisterShellRes::SResErr(KlisterRTE::new("Shell command failed", true), output.stdout, output.status.code());
            }
            return KlisterShellRes::SResOk(output.stdout);
        });

        if !write_ok {
            return Ok(KlisterShellRes::SResErr(KlisterRTE::new("Failed to write to stdin", true), Vec::new(), None));
        }

        let KlisterShellRes::SResOk(ref v) = scope_result else {
            return Ok(scope_result);
        };
        // todo: Unnecessary clones here, clean this up.
        output_opt = Some(v.clone())
    }
    let output = output_opt.expect("Internal interpreter error: Output was none");
    Ok(KlisterShellRes::SResOk(output))
}

fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> Result<ValWrap, KlisterRTE> {
    match expression {
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
