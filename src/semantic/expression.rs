use crate::ast;
use crate::semantic::ArrayType;

use super::CompileError;
use super::Float;
use super::Integer;
use super::PrimitiveType;
use super::{CVType, VariableInfo};

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i64, Integer),
    Float(f64, Float),
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

    Default(PrimitiveType),
}
impl Expression {
    pub fn is_reference(&self) -> bool {
        match self {
            Expression::Integer(_, _) | Expression::Float(_, _) => false,
            Expression::String(_) => false,
            Expression::Variable(var) => match &var.cv_type.type_ {
                PrimitiveType::Array(_) => false,
                PrimitiveType::Function(_) => false,
                _ => true,
            },
            Expression::Conditional(_) => false,
            Expression::Cast(_) => false,
            Expression::Member(src) => src.src.is_reference(),
            Expression::Arrow(_) => true,
            Expression::Paren(_) => false,
            Expression::Bracket(_) => true,
            Expression::Unary(expr) => match expr.op {
                ExprUnaryOp::Plus => false,
                ExprUnaryOp::Minus => false,
                ExprUnaryOp::LogicalNot => false,
                ExprUnaryOp::BitwiseNot => false,
                ExprUnaryOp::Dereference => true,
                ExprUnaryOp::AddressOf => false,
                ExprUnaryOp::IncrementPre => true,
                ExprUnaryOp::DecrementPre => true,
                ExprUnaryOp::DecrementPost => false,
                ExprUnaryOp::IncrementPost => false,
            },
            Expression::Binary(expr) => match expr.op {
                ExprBinaryOp::Add
                | ExprBinaryOp::Sub
                | ExprBinaryOp::Mul
                | ExprBinaryOp::Div
                | ExprBinaryOp::Mod => false,

                ExprBinaryOp::BitwiseAnd | ExprBinaryOp::BitwiseOr | ExprBinaryOp::BitwiseXor => {
                    false
                }

                ExprBinaryOp::ShiftLeft | ExprBinaryOp::ShiftRight => false,

                ExprBinaryOp::Equal
                | ExprBinaryOp::NotEqual
                | ExprBinaryOp::LessThan
                | ExprBinaryOp::LessThanOrEqual
                | ExprBinaryOp::GreaterThan
                | ExprBinaryOp::GreaterThanOrEqual
                | ExprBinaryOp::LogicalAnd
                | ExprBinaryOp::LogicalOr => false,

                ExprBinaryOp::AddAssign
                | ExprBinaryOp::SubAssign
                | ExprBinaryOp::MulAssign
                | ExprBinaryOp::DivAssign
                | ExprBinaryOp::ModAssign
                | ExprBinaryOp::BitwiseAndAssign
                | ExprBinaryOp::BitwiseOrAssign
                | ExprBinaryOp::BitwiseXorAssign
                | ExprBinaryOp::ShiftLeftAssign
                | ExprBinaryOp::ShiftRightAssign
                | ExprBinaryOp::Assign(_) => true,

                ExprBinaryOp::Comma => expr.rhs.is_reference(),
            },
            Expression::InitializerList(_) => false,
            Expression::Default(_) => false,
        }
    }

    pub fn cv_type(&self) -> Result<CVType, CompileError> {
        Ok(match self {
            Expression::Variable(var) => var.cv_type.clone(),
            Expression::Integer(_, i) => CVType::from_primitive(PrimitiveType::Integer(*i)),
            Expression::Float(_, f) => CVType::from_primitive(PrimitiveType::Float(*f)),
            Expression::String(str) => {
                let str = str.as_bytes();
                let ch_type = CVType {
                    type_: PrimitiveType::Integer(Integer::Int8),
                    const_: true,
                    volatile: false,
                };
                let array_type = PrimitiveType::Array(ArrayType {
                    cv_type: Box::new(ch_type),
                    size: str.len() + 1,
                });
                CVType::from_primitive(array_type)
            }
            Expression::Cast(expr) => CVType::from_primitive(expr.type_.clone()),
            Expression::Bracket(expr) => match expr.src.cv_type()?.type_ {
                PrimitiveType::Pointer(t) => *t,
                PrimitiveType::Array(t) => *t.cv_type,
                _ => return Err(CompileError::BracketOnNonArrayOrPointer),
            },
            Expression::Conditional(expr) => CVType::from_primitive(
                expr.else_expr
                    .cv_type()?
                    .type_
                    .common_type(&expr.then_expr.cv_type()?.type_)
                    .unwrap(),
            ),
            Expression::Member(expr) => expr.member_type.clone(),
            Expression::Arrow(expr) => expr.member_type.clone(),
            Expression::InitializerList(expr) => unimplemented!("expression_type InitializerList"),
            Expression::Paren(expr) => match expr.src.cv_type()?.type_ {
                PrimitiveType::Function(func) => *func.return_type,
                _ => unreachable!("Paren expression type"),
            },
            Expression::Unary(expr) => match expr.op {
                ExprUnaryOp::Plus => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::Minus => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::LogicalNot => {
                    CVType::from_primitive(PrimitiveType::Integer(Integer::Int8))
                }
                ExprUnaryOp::BitwiseNot => match expr.expr.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => {
                        CVType::from_primitive(PrimitiveType::Integer(i.to_unsigned()))
                    }
                    _ => unreachable!("BitwiseNot expression type"),
                },
                ExprUnaryOp::Dereference => match expr.expr.cv_type()?.type_ {
                    PrimitiveType::Pointer(t) => *t,
                    PrimitiveType::Array(t) => *t.cv_type,
                    _ => unreachable!("Dereference expression type"),
                },
                ExprUnaryOp::AddressOf => {
                    CVType::from_primitive(PrimitiveType::Pointer(Box::new(expr.expr.cv_type()?)))
                }
                ExprUnaryOp::IncrementPre => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::DecrementPre => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::IncrementPost => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::DecrementPost => CVType::from_primitive(expr.expr.cv_type()?.type_),
            },
            Expression::Binary(expr) => {
                let lhs_type = expr.lhs.cv_type()?;
                let rhs_type = expr.rhs.cv_type()?;
                #[allow(unused)]
                match expr.op {
                    ExprBinaryOp::Add => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }
                            (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(b)
                            }

                            (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                                PrimitiveType::Float(a)
                            }
                            (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(a.common_type(&b))
                            }

                            (PrimitiveType::Pointer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Pointer(a)
                            }
                            (PrimitiveType::Array(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Pointer(a.cv_type)
                            }

                            _ => unreachable!("Add expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::Sub => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }
                            (PrimitiveType::Integer(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(b)
                            }

                            (PrimitiveType::Float(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Float(a)
                            }
                            (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(a.common_type(&b))
                            }

                            (PrimitiveType::Pointer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Pointer(a)
                            }
                            (PrimitiveType::Array(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Pointer(a.cv_type)
                            }

                            (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                                PrimitiveType::Integer(Integer::Int64)
                            }

                            _ => unreachable!("Sub expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::Mul | ExprBinaryOp::Div => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }
                            (PrimitiveType::Integer(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(b)
                            }

                            (PrimitiveType::Float(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Float(a)
                            }
                            (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(a.common_type(&b))
                            }

                            _ => unreachable!("Mul expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::Mod => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("Mod expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::BitwiseAnd
                    | ExprBinaryOp::BitwiseOr
                    | ExprBinaryOp::BitwiseXor => match (lhs_type.type_, rhs_type.type_) {
                        (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                            CVType::from_primitive(PrimitiveType::Integer(a.common_type(&b)))
                        }

                        _ => unreachable!("BitOp expression type"),
                    },
                    ExprBinaryOp::ShiftLeft | ExprBinaryOp::ShiftRight => {
                        CVType::from_primitive(lhs_type.type_)
                    }
                    ExprBinaryOp::Equal
                    | ExprBinaryOp::NotEqual
                    | ExprBinaryOp::LessThan
                    | ExprBinaryOp::LessThanOrEqual
                    | ExprBinaryOp::GreaterThan
                    | ExprBinaryOp::GreaterThanOrEqual
                    | ExprBinaryOp::LogicalAnd
                    | ExprBinaryOp::LogicalOr => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int8))
                    }

                    ExprBinaryOp::AddAssign
                    | ExprBinaryOp::SubAssign
                    | ExprBinaryOp::MulAssign
                    | ExprBinaryOp::DivAssign
                    | ExprBinaryOp::ModAssign
                    | ExprBinaryOp::BitwiseAndAssign
                    | ExprBinaryOp::BitwiseOrAssign
                    | ExprBinaryOp::BitwiseXorAssign
                    | ExprBinaryOp::ShiftLeftAssign
                    | ExprBinaryOp::ShiftRightAssign
                    | ExprBinaryOp::Assign(_) => lhs_type,

                    ExprBinaryOp::Comma => rhs_type,
                }
            }
            Expression::Default(t) => CVType::from_primitive(t.clone()),
        })
    }
    pub fn primitive_type(&self) -> Result<PrimitiveType, CompileError> {
        Ok(self.cv_type()?.type_)
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
    pub type_: PrimitiveType,
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
    pub member_index: usize,
    pub member_type: CVType,
}
