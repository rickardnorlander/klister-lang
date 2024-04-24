#![allow(dead_code)]
//#![allow(unused_variables)]
#![allow(unused_imports)]
//#![allow(unused_mut)]
use std::mem;
use std::collections::HashMap;
use crate::ccall::FunctionData;
use crate::ccall::Libraries;
mod ccall;
mod ast;
mod types;
mod except;

use crate::except::KlisterRTE;
use crate::ast::*;
use std::process::Command;

use libffi::middle::*;
use libffi::low;

use std::ffi::c_int;
use std::ffi::c_char;
//use std::ffi::CStr;
use std::ffi::CString;
use std::ffi::c_long;
use std::ffi::c_void;
use crate::types::TypeTag;

mod parse;

use crate::parse::parse_ast;

struct Context {
    libs: Libraries,
    variables: HashMap<String, KlisterValue>,
}

fn handle_import(context: &mut Context, libname: &str, fname: &str, rettypename: &str, argnames: &Vec<String>) -> Result<(), KlisterRTE> {
    context.libs.load_fn(libname, fname, rettypename, &argnames.iter().map(String::as_str).collect::<Vec<_>>());
    Ok(())
}

use std::ffi::CStr;

fn handle_call(context: &mut Context, fn_name: &str, arguments: &Vec<KlisterExpression>) -> Result<KlisterValue, KlisterRTE> {
    let argument_values: Vec<KlisterValue> = arguments.iter().map(|x| handle_expression(context, x)).collect::<Result<Vec<KlisterValue>, KlisterRTE>>()?;

    let mut args = Vec::<Arg>::new();
    let mut arg_storage = Vec::<Vec<u8>>::new();
    let mut ptr_storage = Vec::<Box<*mut c_void>>::new();

    for v in &argument_values {
        match v {
            KlisterValue::CS(x) => {
                let mut st = Vec::with_capacity(x.len() + 1);
                st.extend(x.as_bytes());
                st.push(0);
                CStr::from_bytes_with_nul(&st).expect("Validation as c string failed");
                arg_storage.push(st);
                ptr_storage.push(Box::new(arg_storage.last().unwrap().as_ptr() as *mut c_void));
                args.push(arg(ptr_storage.last().unwrap().as_ref()));
            }
            KlisterValue::Int(ref x) => {
                args.push(arg(x));
            }
            KlisterValue::Bytes(x) => {
                let xclone = x.clone();
                CStr::from_bytes_with_nul(&xclone).expect("Validation as c string failed");
                arg_storage.push(xclone);
                ptr_storage.push(Box::new(arg_storage.last().unwrap().as_ptr() as *mut c_void));
                args.push(arg(ptr_storage.last().unwrap().as_ref()));
            }
            KlisterValue::Bool(_) => {
                todo!();
            }
            KlisterValue::Exception => {
                return Err(KlisterRTE::from_str("Can't pass exception to c-api"));
            }
            KlisterValue::Res(_) => {
                return Err(KlisterRTE::from_str("Can't pass res to c-api"));
            }
            KlisterValue::Nothing => {
                return Err(KlisterRTE::from_str("Can't pass 'nothing' to c-api"));
            }
        }
    }


    let fn_data = context.libs.functions.get(&fn_name as &str).unwrap();
    let FunctionData{ptr: fn_ptr, cif, retstr: rettypename, args:_} = fn_data;
    let codeptr = low::CodePtr::from_ptr(*fn_ptr);

    let tt = TypeTag::from_string(&rettypename);
    let mut result = vec![0u8; tt.size()];

    unsafe {
        libffi::raw::ffi_call(cif.as_raw_ptr(), Some(*codeptr.as_fun()), result.as_mut_ptr() as *mut c_void, (&args).as_ptr() as *mut *mut c_void);
    }

    let ret = match rettypename.as_str() {
        "int" => Ok(KlisterValue::Int(i32::from_ne_bytes(result.try_into().unwrap()))), // TODO: should check int is 32bits.
            _ => Err(KlisterRTE::new())
    };

    // There are pointers into these structures.
    // So use explicit drops to make sure the values live until this point.
    drop(arg_storage);
    drop(ptr_storage);
    drop(argument_values);
    return ret;
}

fn unpack_cs(kv: KlisterValue) -> Result<String, KlisterRTE> {
    match kv {
        KlisterValue::CS(lv) => Ok(lv),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}

fn unpack_int(kv: KlisterValue) -> Result<c_int, KlisterRTE> {
    match kv {
        KlisterValue::Int(lv) => Ok(lv),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}

fn unpack_bool(kv: KlisterValue) -> Result<bool, KlisterRTE> {
    match kv {
        KlisterValue::Bool(lv) => Ok(lv),
        _ => Err(KlisterRTE::from_str("Type error")),
    }
}

use std::io::Write;

fn handle_expression(context: &mut Context, expression: &KlisterExpression) -> Result<KlisterValue, KlisterRTE> {
    match expression {
        KlisterExpression::Call(fn_name, argubayas) => {
            handle_call(context, fn_name, argubayas)
        }
        KlisterExpression::Index(arr, index) => {
            let lv = unpack_cs(handle_expression(context, arr)?)?;
            let rv = unpack_int(handle_expression(context, index)?)?;
            let s = lv.as_str();
            let ind = rv.try_into().unwrap();
            let cu32 = s.chars().nth(ind).unwrap() as u32;
            let ci32:i32 = cu32.try_into().unwrap();
            Ok(KlisterValue::Int(ci32))
        }
        KlisterExpression::Literal(v) => {Ok(v.clone())}
        KlisterExpression::Variable(v) => {Ok(context.variables.get(v).unwrap().clone())}
        KlisterExpression::Add(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Int(lv+rv))
        }
        KlisterExpression::Sub(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Int(lv-rv))
        }
        KlisterExpression::Mul(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Int(lv*rv))
        }
        KlisterExpression::Div(left, right) => {
            let lv = unpack_int(handle_expression(context, left)?)?;
            let rv = unpack_int(handle_expression(context, right)?)?;
            Ok(KlisterValue::Int(lv/rv))
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
        KlisterExpression::Catch(expr) => {
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
        KlisterExpression::ShellPipeline(cmds) => {
            if cmds.len() == 0 {
                return Err(KlisterRTE::from_str("Empty pipeline"));
            };

            let mut output_opt: Option<Vec<u8>> = None;

            for cmd in cmds.into_iter() {
                let stdinxxx = match output_opt.is_some() {
                    true => std::process::Stdio::piped(),
                    false => std::process::Stdio::inherit(),
                };
                let child_res = Command::new(cmd.command.clone()).args(cmd.args.clone()).stdin(stdinxxx).stderr(std::process::Stdio::inherit()).stdout(std::process::Stdio::piped()).spawn();
                let mut child = child_res.unwrap();

                let thread_handle_opt = if let Some(output) = output_opt {
                    let mut stdin = child.stdin.take().expect("Failed to open stdin");
                    Some(std::thread::spawn(move || {
                        stdin.write_all(&output).expect("Failed to write to stdin");
                    }))
                } else {
                    None
                };


                let output = child.wait_with_output().expect("Failed to read stdout");
                if !output.status.success() {
                    return Err(KlisterRTE::from_str("Shell command failed"))
                }
                output_opt = Some(output.stdout);
                if let Some(thread_handle) = thread_handle_opt {
                    thread_handle.join().unwrap();
                }
            }
            let output = output_opt.unwrap();
            println!("{:?}", output);
            Ok(KlisterValue::Bytes(output))
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
        KlisterStatement::Shell(cmd_s, args) => {
            let output_res = Command::new(cmd_s).args(args).stdin(std::process::Stdio::inherit()).stdout(std::process::Stdio::inherit()).stderr(std::process::Stdio::inherit()).output();
            let Ok(output) = output_res else {return Err(KlisterRTE::from_str("Failed to run process for unknown reasons"))};
            if !output.status.success() {
                return Err(KlisterRTE::from_str("Shell command failed"))
            }
            Ok(())
        }
    }
}



use crate::ccall::dlsym;
fn main() {
    let ast_res = parse_ast();
    let Ok(ast) = ast_res else {
        if let Err(error) = ast_res {
            println!("{:?}", error);
        }
        panic!();
    };

    let mut context: Context = Context{libs: Libraries::new(), variables: HashMap::new()};

    let res = handle_statement(&mut context, &ast);
    println!("{:?}", res);

    println!("{:?}", context.variables.get("asdf"));
}
