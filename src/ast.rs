use std::ffi::c_int;

use crate::KlisterRTE;

#[derive(Debug)]
#[derive(Clone)]
pub enum KlisterResult {
    ResOk(Box<KlisterValue>),
    ResErr(Box<KlisterRTE>),
}

#[derive(Debug)]
#[derive(Clone)]
pub enum KlisterValue {
    CS(String),
    Int(c_int),
    Bool(bool),
    Bytes(Vec<u8>),
    Exception,
    Res(KlisterResult),
    Nothing,
}

#[derive(Debug)]
pub struct ShellCommand{pub command: String, pub args: Vec<String>}

#[derive(Debug)]
pub enum KlisterExpression {
    Call(String, Vec<KlisterExpression>),
    Index(Box<KlisterExpression>, Box<KlisterExpression>),
    Add(Box<KlisterExpression>, Box<KlisterExpression>),
    Sub(Box<KlisterExpression>, Box<KlisterExpression>),
    Mul(Box<KlisterExpression>, Box<KlisterExpression>),
    Div(Box<KlisterExpression>, Box<KlisterExpression>),
    Lt(Box<KlisterExpression>, Box<KlisterExpression>),
    Gt(Box<KlisterExpression>, Box<KlisterExpression>),
    Eq(Box<KlisterExpression>, Box<KlisterExpression>),
    Lte(Box<KlisterExpression>, Box<KlisterExpression>),
    Gte(Box<KlisterExpression>, Box<KlisterExpression>),
    Ne(Box<KlisterExpression>, Box<KlisterExpression>),
    Catch(Box<KlisterExpression>),
    Variable(String),
    Literal(KlisterValue),
    ShellPipeline(Vec<ShellCommand>),
}

pub enum KlisterStatement {
    Import(String, String, String, Vec<String>),
    Assign(String, KlisterExpression),
    Expression(KlisterExpression),
    Block(Vec<KlisterStatement>),
    While(KlisterExpression, Box<KlisterStatement>),
    Shell(String, Vec<String>,),
    If(KlisterExpression, Box<KlisterStatement>, Option<Box<KlisterStatement>>),
}

pub fn im(a: &str, b: &str, c: &str, d: &[&str]) -> KlisterStatement {
    return KlisterStatement::Import(a.to_string(), b.to_string(), c.to_string(), d.iter().map(|x|x.to_string()).collect());
}

