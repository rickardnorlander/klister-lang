use std::collections::HashMap;
use std::io::Write;
use std::process::Command;

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
    variables: HashMap<String, KlisterValue>,
}

fn handle_import(context: &mut Context, libname: &str, fname: &str, rettypename: &str, argnames: &Vec<String>) -> Result<(), KlisterRTE> {
    context.libs.load_fn(libname, fname, rettypename, &argnames.iter().map(String::as_str).collect::<Vec<_>>());
    context.variables.insert(fname.to_string(), KlisterValue::CFunction(fname.to_string()));
    Ok(())
}

fn handle_ffi_call(context: &mut Context, fn_name: &str, arguments: &Vec<KlisterExpression>) -> Result<KlisterValue, KlisterRTE> {
    let argument_values: Vec<KlisterValue> = arguments.iter().map(|x| handle_expression(context, x)).collect::<Result<Vec<KlisterValue>, KlisterRTE>>()?;
    return ffi_call(&mut context.libs, fn_name, argument_values);
}

fn unpack_cs(kv: KlisterValue) -> Result<String, KlisterRTE> {
    match kv {
        KlisterValue::CS(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}

fn unpack_int(kv: KlisterValue) -> Result<BigInt, KlisterRTE> {
    match kv {
        KlisterValue::BInt(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}

fn unpack_bool(kv: KlisterValue) -> Result<bool, KlisterRTE> {
    match kv {
        KlisterValue::Bool(ref lv) => Ok(lv.clone()),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}


fn handle_shell_pipeline(sp: &ShellPipelineS) -> ShellResE {
    let cmds = &sp.commands;
    if cmds.len() == 0 {
        return ShellResE::SResErr(KlisterRTE::from_str("Empty pipeline"), Vec::new(), None);
    };

    let mut output_opt: Option<Vec<u8>> = None;

    for cmd in cmds.into_iter() {
        let stdinxxx = match output_opt.is_some() {
            true => std::process::Stdio::piped(),
            false => std::process::Stdio::inherit(),
        };
        let child_res = Command::new(cmd.command.clone()).args(cmd.args.clone()).stdin(stdinxxx).stderr(std::process::Stdio::inherit()).stdout(std::process::Stdio::piped()).spawn();
        let mut child = child_res.unwrap();
        let mut write_ok = true;

        let scope_result = std::thread::scope(|my_thread_scope| {
            if let Some(output) = output_opt {
                let Some(mut stdin) = child.stdin.take() else {
                    return ShellResE::SResErr(KlisterRTE::from_str("Failed to open stdin"), Vec::new(), None);
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
                return ShellResE::SResErr(KlisterRTE::from_str("Failed to read stdout"), Vec::new(), None);
            };
            if !output.status.success() {
                return ShellResE::SResErr(KlisterRTE::from_str("Shell command failed"), output.stdout, output.status.code());
            }
            return ShellResE::SResOk(output.stdout);
        });

        if !write_ok {
            return ShellResE::SResErr(KlisterRTE::from_str("Failed to write to stdin"), Vec::new(), None);
        }

        let ShellResE::SResOk(ref v) = scope_result else {
            return scope_result;
        };
        output_opt = Some(v.clone())
    }
    let output = output_opt.unwrap();
    ShellResE::SResOk(output)
}

fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> Result<KlisterValue, KlisterRTE> {
    match expression {
        KlisterExpression::Call(fn_expr, arguments) => {
            let v = handle_expression(context, fn_expr)?;
            if let KlisterValue::CFunction(ref fn_name) = v {
                handle_ffi_call(context, fn_name, arguments)
            } else if let KlisterValue::MemberFunction(ref obj, ref r_name) = v {
                match **obj {
                    KlisterValue::BInt(ref b) => {
                        match r_name.as_str() {
                            "to_string" => {
                                if arguments.len() != 0 {
                                    return Err(KlisterRTE::from_str("Wrong number of arguments"));
                                }
                                return Ok(KlisterValue::CS(b.to_string()));
                            }
                            _ => todo!()
                        }
                    }
                    _ => {
                        todo!();
                    }
                }
            } else {
                return Err(KlisterRTE::from_str("Object is not callable"));
            }
        }
        KlisterExpression::Index(arr, index) => {
            let lv = unpack_cs(handle_expression(context, arr)?)?;
            let rv = unpack_int(handle_expression(context, index)?)?;
            let s = lv.as_str();
            let ind = rv.try_into().unwrap();
            let cu32 = s.chars().nth(ind).unwrap() as u32;
            let bi:BigInt = cu32.try_into().unwrap();
            Ok(KlisterValue::BInt(bi))
        }
        KlisterExpression::Dot(obj_expr, subscript) => {
            let obj = handle_expression(context, obj_expr)?;
            match obj {
                Res(ref r) => {
                    match subscript.as_str() {
                        "is_ok" => {
                            return Ok(KlisterValue::Bool(matches!(r, KlisterResult::ResOk(_))))
                        }
                        "ok_variant" => {
                            let KlisterResult::ResOk(ok) = r else {
                                return Err(KlisterRTE::from_str("Accessed inactive variant"));
                            };
                            return Ok((**ok).clone());
                        }
                        "err_variant" => {
                            let KlisterResult::ResErr(err) = r else {
                                return Err(KlisterRTE::from_str("Accessed inactive variant"));
                            };
                            return Ok(KlisterValue::Exception((*err).clone()));
                        }
                        _ => todo!()
                    }
                }
                KlisterValue::BInt(_) => {
                    match subscript.as_str() {
                        "to_string" => {
                            return Ok(KlisterValue::MemberFunction(gc::Gc::new(obj), subscript.clone()));
                        }
                        _ => todo!()
                    }
                }
                KlisterValue::ShellRes(ref r) => {
                    match subscript.as_str() {
                        "is_ok" => {
                            return Ok(KlisterValue::Bool(matches!(r, ShellResE::SResOk(_))))
                        }
                        _ => todo!()
                    }
                }
                KlisterValue::Bytes(ref bytes) => {
                    match subscript.as_str() {
                        "len" => {
                            return Ok(KlisterValue::BInt(bytes.len().into()))
                        }
                        _ => todo!()
                    }
                }
                _ => todo!()
            };
        }
        KlisterExpression::Literal(v) => {Ok(v.clone())}
        KlisterExpression::Variable(v) => {
            match context.variables.get(v) {
                Some(val) => Ok(val.clone()),
                None => panic!("Variable not defined: {}", v),
            }
        }
        KlisterExpression::Add(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::BInt(lv+rv))
        }
        KlisterExpression::Sub(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::BInt(lv-rv))
        }
        KlisterExpression::Mul(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::BInt(lv*rv))
        }
        KlisterExpression::Div(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::BInt(lv/rv))
        }
        KlisterExpression::Lt(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv<rv))
        }
        KlisterExpression::Gt(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv>rv))
        }
        KlisterExpression::Lte(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv<=rv))
        }
        KlisterExpression::Gte(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv>=rv))
        }
        KlisterExpression::Eq(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv==rv))
        }
        KlisterExpression::Ne(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv!=rv))
        }
        KlisterExpression::Or(left, right) => {
            let lv = unpack_bool(handle_expression(context, left)?)?;
            let rv = unpack_bool(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv||rv))
        }
        KlisterExpression::And(left, right) => {
            let lv = unpack_bool(handle_expression(context, left)?)?;
            let rv = unpack_bool(handle_expression(context, right)?)?;
            Ok(KlisterValue::Bool(lv&&rv))
        }
        KlisterExpression::Not(expr) => {
            let v = unpack_bool(handle_expression(context, expr)?)?;
            Ok(KlisterValue::Bool(!v))
        }
        KlisterExpression::CatchExpr(expr) => {
            let v_opt = handle_expression(context, expr);
            match v_opt {
                Ok(v) => {
                    Ok(KlisterValue::Res(KlisterResult::ResOk(Box::new(v))))
                }
                Err(e) => {
                    Ok(KlisterValue::Res(KlisterResult::ResErr(Box::new(e))))
                }
            }
        }
        KlisterExpression::ShellPipeline(sp) => {
            let result = handle_shell_pipeline(sp);
            if sp.is_catch {
                return Ok(KlisterValue::ShellRes(result));
            }
            return match result {
                ShellResE::SResOk(ref v) => {
                    Ok(KlisterValue::Bytes(v.clone()))
                }
                ShellResE::SResErr(ref e, _, _) => {
                    Err(e.clone())
                }
            }
        }
    }
}

fn handle_statement(context: &mut Context, statement: &KlisterStatement) -> Result<(), KlisterRTE> {
    match statement {
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

pub fn interpret_ast(ast: KlisterStatement) {
    let mut context: Context = Context{libs: Libraries::new(), variables: HashMap::new()};

    handle_statement(&mut context, &ast).unwrap();
}
