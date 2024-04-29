#![allow(dead_code)]

use crate::value::KlisterValueV2;

#[derive(Clone)]
#[derive(Debug)]
pub enum GlobPart {
    GlobPartS(String),
    GlobPartAsterisk,
    GlobPartInterpolation(Box<KlisterExpression>),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Argon {
    ArgonGlob(Vec<GlobPart>),
    ArgonArrayRef(String),
}

#[derive(Clone)]
#[derive(Debug)]
pub struct ShellCommand{pub command: Argon, pub args: Vec<Argon>}

#[derive(Clone)]
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

#[derive(Clone)]
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
    // Use box instead of gc as a safeguard, to unsure the ast doesnt get accidentally mutated
    // It's probably actually be fine to use gc though, think about it more at some point.
    Literal(Box<dyn KlisterValueV2>),
    ShellPipeline(ShellPipelineS),
}

#[derive(Clone)]
#[derive(Debug)]
pub enum KlisterStatement {
    Function(String, Box<KlisterStatement>),
    Import(String, String, String, Vec<String>),
    Assign(String, KlisterExpression),
    Expression(KlisterExpression),
    Block(Vec<KlisterStatement>),
    While(KlisterExpression, Box<KlisterStatement>),
    If(KlisterExpression, Box<KlisterStatement>, Option<Box<KlisterStatement>>),
}
