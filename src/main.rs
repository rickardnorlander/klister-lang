mod ast;
mod ccall;
mod except;
mod interpret;
mod parse;
mod types;

use std::fs;

use argparse::ArgumentParser;
use argparse::Store;

use crate::parse::parse_ast;
use crate::interpret::interpret_ast;

fn main() {
    let mut command = String::new();

    {
        let mut parser = ArgumentParser::new();
        parser.refer(&mut command).add_argument("script", Store, "Script to run").required();
        parser.parse_args_or_exit();
    }

    let contents = fs::read_to_string(&command).expect("Couldn't read script");

    let ast_res = parse_ast(&contents);
    let Ok(ast) = ast_res else {
        if let Err(error) = ast_res {
            println!("{:?}", error);
        }
        panic!();
    };

    interpret_ast(ast);
}
