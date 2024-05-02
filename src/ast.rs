#![allow(dead_code)]

use crate::value::KlisterValueV2;

#[derive(Clone)]
#[derive(Debug)]
pub enum GlobPart {
    Str(String),
    Asterisk,
    ArrayInterpolation(Box<KlisterExpression>),
    Interpolation(Box<KlisterExpression>),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Argon {
    ArgonGlob(Vec<GlobPart>),
}

#[derive(Clone)]
#[derive(Debug)]
pub struct ShellCommand{pub command: Argon, pub args: Vec<Argon>}
//pub struct ShellCommand{pub command: Argon, pub args: Vec<Argon>, pub stdin: Option<String>, pub stdout: Option<String>, pub stderr: Option<String>, pub stderr_merge_into_stdout: bool, pub stdout_merge_into_stderr: bool}

#[derive(Clone)]
#[derive(Debug)]
pub struct ShellPipelineS {
    pub commands: Vec<ShellCommand>,
    pub is_catch: bool,
}

impl ShellPipelineS {
    pub fn new(commands: Vec<ShellCommand>, is_catch: bool) -> ShellPipelineS {
        ShellPipelineS{commands, is_catch}
    }
}

#[derive(Copy)]
#[derive(Clone)]
#[derive(Debug)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
    Lte,
    Gte,
    Ne,
    Or,
    And,
}

#[derive(Clone)]
#[derive(Debug)]
pub enum KlisterExpression {
    Call(Box<KlisterExpression>, Vec<KlisterExpression>),
    Index(Box<KlisterExpression>, Box<KlisterExpression>),
    Dot(Box<KlisterExpression>, String),
    BinOp(Operation, Box<KlisterExpression>, Box<KlisterExpression>),
    Not(Box<KlisterExpression>),
    CatchExpr(Box<KlisterExpression>),
    Variable(String),
    // Use box instead of gc as a safeguard, to unsure the ast doesnt get accidentally mutated
    // It's probably actually be fine to use gc though, think about it more at some point.
    Literal(Box<dyn KlisterValueV2>),
    ShellPipeline(ShellPipelineS),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum KlisterStatement {
    Function(String, Vec<String>, Box<KlisterStatement>),
    Import(String, String, String, Vec<String>),
    Assign(String, KlisterExpression),
    Expression(KlisterExpression),
    Return(KlisterExpression),
    Block(Vec<KlisterStatement>),
    While(KlisterExpression, Box<KlisterStatement>),
    If(KlisterExpression, Box<KlisterStatement>, Option<Box<KlisterStatement>>),
    ForEach(String, KlisterExpression, Box<KlisterStatement>),
}
