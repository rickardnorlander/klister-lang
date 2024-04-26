#![allow(dead_code)]

use num_bigint::BigInt;

use crate::except::KlisterRTE;

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
    BInt(BigInt),
    Bool(bool),
    Bytes(Vec<u8>),
    Exception(Box<KlisterRTE>),
    Res(KlisterResult),
    ShellRes(ShellResE),
    Nothing,
    CFunction(String),
    // Just a wrapper around a value for now
    // todo: Do things properly but lots of work possibly.
    FakeFunction(Box<KlisterValue>),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum ShellResE {
    SResOk(Vec<u8>),
    SResErr(KlisterRTE, Vec<u8>, Option<i32>),
}


#[derive(Debug)]
pub struct ShellCommand{pub command: String, pub args: Vec<String>}

#[derive(Debug)]
pub struct ShellPipelineS {
    pub commands: Vec<ShellCommand>,
    pub is_catch: bool,
}

impl ShellPipelineS {
    pub fn new(cmds: Vec<ShellCommand>) -> ShellPipelineS {
        ShellPipelineS{commands: cmds, is_catch: false}
    }

    pub fn catching(cmds: Vec<ShellCommand>) -> ShellPipelineS {
        ShellPipelineS{commands: cmds, is_catch: true}
    }
}

#[derive(Debug)]
pub enum KlisterExpression {
    Call(Box<KlisterExpression>, Vec<KlisterExpression>),
    Index(Box<KlisterExpression>, Box<KlisterExpression>),
    Dot(Box<KlisterExpression>, String),
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
    Or(Box<KlisterExpression>, Box<KlisterExpression>),
    And(Box<KlisterExpression>, Box<KlisterExpression>),
    Not(Box<KlisterExpression>),
    CatchExpr(Box<KlisterExpression>),
    Variable(String),
    Literal(KlisterValue),
    ShellPipeline(ShellPipelineS),
}

pub enum KlisterStatement {
    Import(String, String, String, Vec<String>),
    Assign(String, KlisterExpression),
    Expression(KlisterExpression),
    Block(Vec<KlisterStatement>),
    While(KlisterExpression, Box<KlisterStatement>),
    If(KlisterExpression, Box<KlisterStatement>, Option<Box<KlisterStatement>>),
}
