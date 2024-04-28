use std::collections::HashMap;
use std::io::Write;
use std::process::Command;

use gc::Gc;
use num_bigint::BigInt;

use crate::ast::*;
use crate::ccall::ffi_call;
use crate::ccall::Libraries;
use crate::except::KlisterRTE;
use crate::interpret::KlisterValue::Res;


#[derive(gc::Trace, gc::Finalize)]
pub struct Context {
    #[unsafe_ignore_trace]
    libs: Libraries,
    variables: HashMap<String, Gc<KlisterValue>>,
}

fn handle_import(context: &mut Context, libname: &str, fname: &str, rettypename: &str, argnames: &Vec<String>) -> Result<(), KlisterRTE> {
    context.libs.load_fn(libname, fname, rettypename, &argnames.iter().map(String::as_str).collect::<Vec<_>>())?;
    context.variables.insert(fname.to_string(), Gc::new(KlisterValue::CFunction(fname.to_string())));
    Ok(())
}

fn handle_ffi_call(context: &mut Context, fn_name: &str, arguments: &Vec<KlisterExpression>) -> Result<KlisterValue, KlisterRTE> {
    let argument_values: Vec<KlisterValue> = arguments.iter().map(|x| handle_expression(context, x).map(|y| (*y).clone())).collect::<Result<Vec<KlisterValue>, KlisterRTE>>()?;
    return ffi_call(&mut context.libs, fn_name, argument_values);
}

fn unpack_cs(kv: Gc<KlisterValue>) -> Result<String, KlisterRTE> {
    match *kv {
        KlisterValue::CS(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::new("Type error", false)),
    }
}

fn unpack_int(kv: Gc<KlisterValue>) -> Result<BigInt, KlisterRTE> {
    match *kv {
        KlisterValue::BInt(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::new("Type error", false)),
    }
}

fn unpack_bool(kv: Gc<KlisterValue>) -> Result<bool, KlisterRTE> {
    match *kv {
        KlisterValue::Bool(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::new("Type error", false)),
    }
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
                let val = unpack_cs(handle_expression(context, expr)?)?;
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

fn handle_shell_pipeline(context: &mut Context, sp: &ShellPipelineS) -> Result<ShellResE, KlisterRTE> {
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
            return Ok(ShellResE::SResErr(KlisterRTE::new("Failed to spawn child", true), Vec::new(), None));
        };
        let mut write_ok = true;

        let scope_result = std::thread::scope(|my_thread_scope| {
            if let Some(output) = output_opt {
                let Some(mut stdin) = child.stdin.take() else {
                    return ShellResE::SResErr(KlisterRTE::new("Failed to open stdin", true), Vec::new(), None);
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
                return ShellResE::SResErr(KlisterRTE::new("Failed to read stdout", true), Vec::new(), None);
            };
            if !output.status.success() {
                return ShellResE::SResErr(KlisterRTE::new("Shell command failed", true), output.stdout, output.status.code());
            }
            return ShellResE::SResOk(output.stdout);
        });

        if !write_ok {
            return Ok(ShellResE::SResErr(KlisterRTE::new("Failed to write to stdin", true), Vec::new(), None));
        }

        let ShellResE::SResOk(v) = scope_result else {
            return Ok(scope_result);
        };
        output_opt = Some(v)
    }
    let output = output_opt.expect("Internal interpreter error: Output was none");
    Ok(ShellResE::SResOk(output))
}

fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> Result<Gc<KlisterValue>, KlisterRTE> {
    match expression {
        KlisterExpression::Call(fn_expr, arguments) => {
            let v = handle_expression(context, fn_expr)?;
            if let KlisterValue::CFunction(ref fn_name) = *v {
                handle_ffi_call(context, fn_name, arguments).map(|x| Gc::new(x))
            } else if let KlisterValue::KlisterFunction(ref body) = *v {
                return handle_statement(context, body).map(|_| Gc::new(KlisterValue::Nothing));
            } else if let KlisterValue::MemberFunction(ref obj, ref r_name) = *v {
                match **obj {
                    KlisterValue::BInt(ref b) => {
                        match r_name.as_str() {
                            "to_string" => {
                                if arguments.len() != 0 {
                                    return Err(KlisterRTE::new("Wrong number of arguments", false));
                                }
                                return Ok(Gc::new(KlisterValue::CS(b.to_string())));
                            }
                            _ => {
                                return Err(KlisterRTE::new("Invalid member function", false));
                            }
                        }
                    }
                    _ => {
                        return Err(KlisterRTE::new("Invalid member function", false));
                    }
                }
            } else {
                return Err(KlisterRTE::new("Object is not callable", false));
            }
        }
        KlisterExpression::Index(arr, index) => {
            let lv = unpack_cs(handle_expression(context, arr)?)?;
            let rv = unpack_int(handle_expression(context, index)?)?;
            let s = lv.as_str();
            let Ok(ind) = rv.try_into() else {
                return Err(KlisterRTE::new("Index is not valid usize", false));
            };
            let Some(nthchar) = s.chars().nth(ind) else {
                return Err(KlisterRTE::new("Index out of bounds", false));
            };

            let cu32 = nthchar as u32;
            let bi:BigInt = cu32.into();
            Ok(Gc::new(KlisterValue::BInt(bi)))
        }
        KlisterExpression::Dot(obj_expr, subscript) => {
            let obj = handle_expression(context, obj_expr)?;
            match *obj {
                Res(ref r) => {
                    match subscript.as_str() {
                        "is_ok" => {
                            return Ok(Gc::new(KlisterValue::Bool(matches!(r, KlisterResult::ResOk(_)))))
                        }
                        "ok_variant" => {
                            let KlisterResult::ResOk(ok) = r else {
                                // todo: reconsider whether this should be catchable
                                return Err(KlisterRTE::new("Accessed inactive variant", true));
                            };
                            return Ok(ok.clone());
                        }
                        "err_variant" => {
                            let KlisterResult::ResErr(err) = r else {
                                return Err(KlisterRTE::new("Accessed inactive variant", false));
                            };
                            return Ok(Gc::new(KlisterValue::Exception((*err).clone())));
                        }
                        _ => {return Err(KlisterRTE::new("Member doesn't exist", false));}
                    }
                }
                KlisterValue::BInt(_) => {
                    match subscript.as_str() {
                        "to_string" => {
                            return Ok(Gc::new(KlisterValue::MemberFunction(obj, subscript.clone())));
                        }
                        _ => {return Err(KlisterRTE::new("Member doesn't exist", false));}
                    }
                }
                KlisterValue::ShellRes(ref r) => {
                    match subscript.as_str() {
                        "is_ok" => {
                            return Ok(Gc::new(KlisterValue::Bool(matches!(r, ShellResE::SResOk(_)))))
                        }
                        _ => {return Err(KlisterRTE::new("Member doesn't exist", false));}
                    }
                }
                KlisterValue::Bytes(ref bytes) => {
                    match subscript.as_str() {
                        "len" => {
                            return Ok(Gc::new(KlisterValue::BInt(bytes.len().into())))
                        }
                        _ => {return Err(KlisterRTE::new("Member doesn't exist", false));}
                    }
                }
                _ => {return Err(KlisterRTE::new("Member doesn't exist", false));}
            };
        }
        KlisterExpression::Literal(v) => {Ok(Gc::new(v.clone()))}
        KlisterExpression::Variable(v) => {
            match context.variables.get(v) {
                Some(val) => Ok(val.clone()),
                None => {return Err(KlisterRTE::new(&format!("Variable not defined {}", v), false));}
            }
        }
        KlisterExpression::Add(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::BInt(lv+rv)))
        }
        KlisterExpression::Sub(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::BInt(lv-rv)))
        }
        KlisterExpression::Mul(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::BInt(lv*rv)))
        }
        KlisterExpression::Div(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::BInt(lv/rv)))
        }
        KlisterExpression::Lt(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv<rv)))
        }
        KlisterExpression::Gt(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv>rv)))
        }
        KlisterExpression::Lte(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv<=rv)))
        }
        KlisterExpression::Gte(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv>=rv)))
        }
        KlisterExpression::Eq(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv==rv)))
        }
        KlisterExpression::Ne(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv!=rv)))
        }
        KlisterExpression::Or(left, right) => {
            let lv = unpack_bool(handle_expression(context, left)?)?;
            let rv = unpack_bool(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv||rv)))
        }
        KlisterExpression::And(left, right) => {
            let lv = unpack_bool(handle_expression(context, left)?)?;
            let rv = unpack_bool(handle_expression(context, right)?)?;
            Ok(Gc::new(KlisterValue::Bool(lv&&rv)))
        }
        KlisterExpression::Not(expr) => {
            let v = unpack_bool(handle_expression(context, expr)?)?;
            Ok(Gc::new(KlisterValue::Bool(!v)))
        }
        KlisterExpression::CatchExpr(expr) => {
            let v_res = handle_expression(context, expr);
            match v_res {
                Ok(v) => {
                    Ok(Gc::new(KlisterValue::Res(KlisterResult::ResOk(v))))
                }
                Err(e) => {
                    if e.catchable {
                        Ok(Gc::new(KlisterValue::Res(KlisterResult::ResErr(Box::new(e)))))
                    } else {
                        Err(e)
                    }
                }
            }
        }
        KlisterExpression::ShellPipeline(sp) => {
            let result = handle_shell_pipeline(context, sp)?;
            if sp.is_catch {
                return Ok(Gc::new(KlisterValue::ShellRes(result)));
            }
            return match result {
                ShellResE::SResOk(v) => {
                    Ok(Gc::new(KlisterValue::Bytes(v)))
                }
                ShellResE::SResErr(e, _, _) => {
                    Err(e)
                }
            }
        }
    }
}

fn handle_statement(context: &mut Context, statement: &KlisterStatement) -> Result<(), KlisterRTE> {
    match statement {
        KlisterStatement::Function(name, block) => {
            context.variables.insert(name.to_string(), Gc::new(KlisterValue::KlisterFunction(block.clone())));
            Ok(())
        }
        KlisterStatement::Import(libname, fname, rettypename, argnames) => {
            handle_import(context, libname, fname, rettypename, argnames)?;
            Ok(())
        }
        KlisterStatement::Expression(expression) => {
            handle_expression(context, &expression)?;
            Ok(())
        }
        KlisterStatement::Assign(name, expression) => {
            let val = handle_expression(context, &expression)?;
            context.variables.insert(name.to_string(), val);
            Ok(())
        }

        KlisterStatement::Block(statements) => {
            for part in statements {
                handle_statement(context, part)?;
            }
            Ok(())
        }
        KlisterStatement::While(condition, block) => {
            loop {
                let cond_val = unpack_bool(handle_expression(context, &condition)?)?;
                if !cond_val {
                    break;
                }
                handle_statement(context, block)?;
            }
            Ok(())
        }
        KlisterStatement::If(condition, ifblock, elseblock_opt) => {
            let cond_val = unpack_bool(handle_expression(context, &condition)?)?;
            if cond_val {
                return handle_statement(context, ifblock);
            } else if let Some(elseblock) = elseblock_opt {
                return handle_statement(context, elseblock);
            }
            Ok(())
        }
    }
}

pub fn interpret_ast(ast: KlisterStatement) -> Option<KlisterRTE> {
    let mut context: Context = Context{libs: Libraries::new(), variables: HashMap::new()};

    return handle_statement(&mut context, &ast).err();
}
