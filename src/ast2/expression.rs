use crate::ast;

use super::CompileError;
use super::PrimitiveType;
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
    Member(ExprMember),
    Arrow(ExprMember),

    Paren(ExprParen),
    Bracket(ExprBracket),
    Unary(ExprUnary),
    Binary(ExprBinary),
    InitializerList(ExprInitializerList),
}
impl Expression {
    /// is this expression returns an address upon generating instructions?
    pub fn is_address(&self) -> bool {
        match self {
            Expression::I8(_)
            | Expression::I16(_)
            | Expression::I32(_)
            | Expression::I64(_)
            | Expression::U8(_)
            | Expression::U16(_)
            | Expression::U32(_)
            | Expression::U64(_)
            | Expression::F32(_)
            | Expression::F64(_) => false,
            Expression::String(_) => unimplemented!("is_address - String literal"),
            Expression::Variable(_) => true,
            Expression::Conditional(_) => false,
            Expression::Cast(_) => false,
            Expression::Member(_) => true,
            Expression::Arrow(_) => true,
            Expression::Paren(_) => false,
            Expression::Bracket(_) => true,
            Expression::Unary(expr) => match expr.op {
                _ => false,
            },
            Expression::Binary(_) => false,
            Expression::InitializerList(_) => false,
        }
    }

    pub fn cv_type(&self) -> Result<CVType, CompileError> {
        Ok(match self {
            Expression::Variable(var) => var.cv_type.clone(),
            Expression::I8(_) => CVType::from_primitive(PrimitiveType::Int8),
            Expression::I16(_) => CVType::from_primitive(PrimitiveType::Int16),
            Expression::I32(_) => CVType::from_primitive(PrimitiveType::Int32),
            Expression::I64(_) => CVType::from_primitive(PrimitiveType::Int64),
            Expression::U8(_) => CVType::from_primitive(PrimitiveType::UInt8),
            Expression::U16(_) => CVType::from_primitive(PrimitiveType::UInt16),
            Expression::U32(_) => CVType::from_primitive(PrimitiveType::UInt32),
            Expression::U64(_) => CVType::from_primitive(PrimitiveType::UInt64),
            Expression::F32(_) => CVType::from_primitive(PrimitiveType::Float32),
            Expression::F64(_) => CVType::from_primitive(PrimitiveType::Float64),
            Expression::String(_) => {
                CVType::from_primitive(PrimitiveType::Pointer(Box::new(CVType {
                    type_: PrimitiveType::Int8,
                    const_: true,
                    volatile: false,
                })))
            }
            Expression::Cast(expr) => expr.type_.clone(),
            Expression::Bracket(expr) => match expr.src.cv_type()?.type_ {
                PrimitiveType::Pointer(t) => *t,
                PrimitiveType::Array(t) => *t.cv_type,
                _ => return Err(CompileError::BracketOnNonArrayOrPointer),
            },
            Expression::Conditional(expr) => {
                let cond_type = expr.cond.cv_type()?.type_;
                if cond_type.is_bool_castable() {
                    let then_type = expr.then_expr.cv_type()?.type_;
                    let else_type = expr.else_expr.cv_type()?.type_;
                    match then_type.common_type(&else_type) {
                        Some(t) => CVType::from_primitive(t),
                        None => return Err(CompileError::ConditionalTypeMismatch),
                    }
                } else {
                    return Err(CompileError::ConditionalNotBool);
                }
            }
            Expression::InitializerList(expr) => unimplemented!("expression_type InitializerList"),
            Expression::Paren(expr) => match expr.src.cv_type()?.type_ {
                // @TODO arg check
                PrimitiveType::Function(func) => *func.return_type,
                _ => return Err(CompileError::CallNonFunction),
            },
            Expression::Binary(expr) => unimplemented!("expression_type Binary"),
            Expression::Unary(expr) => unimplemented!("expression_type Unary"),
            Expression::Member(expr) => expr.member_type.clone(),
            Expression::Arrow(expr) => expr.member_type.clone(),
        })
    }
}

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

#[derive(Debug, Clone)]
pub struct ExprMember {
    pub src: Box<Expression>,
    pub member_offset: usize,
    pub member_type: CVType,
}
