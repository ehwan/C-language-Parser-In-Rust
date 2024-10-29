use crate::ast;

use super::{CVType, VariableInfo};

#[derive(Debug, Clone)]
pub enum Expression {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    String(String),

    Variable(VariableInfo),

    Conditional(ExprConditional),
    Cast(ExprCast),

    Paren(ExprParen),
    Bracket(ExprBracket),
    Arrow(ExprArrow),
    Unary(ExprUnary),
    Binary(ExprBinary),
    InitializerList(ExprInitializerList),
}
impl Expression {}

#[derive(Debug, Clone)]
pub struct ExprConditional {
    pub cond: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExprCast {
    pub expr: Box<Expression>,
    pub type_: CVType,
}

#[derive(Debug, Clone)]
pub struct ExprParen {
    pub src: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExprBracket {
    pub src: Box<Expression>,
    pub index: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct ExprArrow {
    pub src: Box<Expression>,
    pub member: String,
}

pub type ExprUnaryOp = ast::ExprUnaryOperator;

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprUnaryOp,
    pub expr: Box<Expression>,
}

pub type ExprBinaryOp = ast::ExprBinaryOperator;

#[derive(Debug, Clone)]
pub struct ExprBinary {
    pub op: ast::ExprBinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExprInitializerList {
    pub exprs: Vec<Expression>,
}
