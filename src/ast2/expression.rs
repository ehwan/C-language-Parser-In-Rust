use crate::ast;

use super::PrimitiveType;

#[derive(Debug, Clone)]
pub enum Expression {
    SizeofExpr(Box<Expression>),

    Conditional(ExprConditional),

    Unary(ExprUnary),
    Binary(ExprBinary),
    InitializerList(ExprInitializerList),
}
impl Expression {
    pub fn get_primitive_type(&self) -> Option<PrimitiveType> {
        match self {
            Expression::SizeofExpr(_) => Some(PrimitiveType::UnsignedLong),
            Expression::Conditional(_) => None,
            Expression::Unary(_) => None,
            Expression::Binary(_) => None,
            Expression::InitializerList(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprConditional {
    pub cond: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
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
