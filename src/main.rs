mod ast;
mod ccall;
mod except;
mod interpret;
mod parse;
mod types;
mod value;

use std::env;
use std::fs;
use std::process::ExitCode;

use crate::parse::parse_ast;
use crate::interpret::interpret_ast;

fn main() -> ExitCode {
    // todo: Use a more principled arg parsing
    // In particular, handle non-utf8
    let mut args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        println!("No script provided");
        return ExitCode::FAILURE;
    }

    let command = args[1].clone();

    let arguments = args.split_off(2);

    let Ok(contents) = fs::read_to_string(&command) else {
        eprintln!("File not found");
        return ExitCode::FAILURE;
    };

    let ast_res = parse_ast(&contents);
    let Ok(ast) = ast_res else {
        if let Err(error) = ast_res {
            println!("Ast parse error {}", error.prettyprint(&contents));
        }
        return ExitCode::FAILURE;
    };

    let err_opt = interpret_ast(ast, arguments);

    if let Some(err) = err_opt {
        eprintln!("Script failed {}", err.s);
        return ExitCode::FAILURE;
    }

    return ExitCode::SUCCESS;
}
