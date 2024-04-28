mod ast;
mod ccall;
mod except;
mod interpret;
mod parse;
mod types;

use std::fs;
use std::process::ExitCode;

use argparse::ArgumentParser;
use argparse::Store;

use crate::parse::parse_ast;
use crate::interpret::interpret_ast;

fn main() -> ExitCode {
    let mut command = String::new();

    {
        let mut parser = ArgumentParser::new();
        parser.refer(&mut command).add_argument("script", Store, "Script to run").required();
        parser.parse_args_or_exit();
    }

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

    let err_opt = interpret_ast(ast);

    if let Some(err) = err_opt {
        eprintln!("Script failed {}", err.s);
        return ExitCode::FAILURE;
    }

    return ExitCode::SUCCESS;
}
