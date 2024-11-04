use crate::ast;

use super::CompileError;
use super::Float;
use super::Integer;
use super::PrimitiveType;
use super::{CVType, VariableInfo};

#[derive(Debug, Clone)]
pub enum Expression {
    Signed(i64, Integer),
    Unsigned(u64, Integer),
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
}
impl Expression {
    /// is this expression returns an address upon generating instructions?
    pub fn is_address(&self) -> bool {
        match self {
            Expression::Signed(_, _) | Expression::Unsigned(_, _) | Expression::Float(_, _) => {
                false
            }
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
            Expression::Signed(_, i) => CVType::from_primitive(PrimitiveType::Integer(*i)),
            Expression::Unsigned(_, i) => CVType::from_primitive(PrimitiveType::Integer(*i)),
            Expression::Float(_, f) => CVType::from_primitive(PrimitiveType::Float(*f)),
            Expression::String(_) => {
                CVType::from_primitive(PrimitiveType::Pointer(Box::new(CVType {
                    type_: PrimitiveType::Integer(Integer::Int8),
                    const_: true,
                    volatile: false,
                })))
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
                ExprUnaryOp::AddressOf => {
                    CVType::from_primitive(PrimitiveType::Pointer(Box::new(expr.expr.cv_type()?)))
                }
                ExprUnaryOp::BitwiseNot => match expr.expr.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => {
                        CVType::from_primitive(PrimitiveType::Integer(i.to_unsigned()))
                    }
                    _ => unreachable!("BitwiseNot expression type"),
                },
                ExprUnaryOp::DecrementPost => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::DecrementPre => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::IncrementPost => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::IncrementPre => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::LogicalNot => {
                    CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                }
                ExprUnaryOp::Minus => CVType::from_primitive(expr.expr.cv_type()?.type_),
                ExprUnaryOp::Dereference => match expr.expr.cv_type()?.type_ {
                    PrimitiveType::Pointer(t) => *t,
                    PrimitiveType::Array(t) => *t.cv_type,
                    _ => unreachable!("Dereference expression type"),
                },
                ExprUnaryOp::Plus => CVType::from_primitive(expr.expr.cv_type()?.type_),
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
                            (PrimitiveType::Integer(_), PrimitiveType::Pointer(b)) => {
                                PrimitiveType::Pointer(b)
                            }
                            (PrimitiveType::Integer(_), PrimitiveType::Array(b)) => {
                                PrimitiveType::Pointer(b.cv_type)
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
                    ExprBinaryOp::AddAssign => lhs_type,
                    ExprBinaryOp::Sub => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }
                            (PrimitiveType::Integer(a), PrimitiveType::Float(b)) => {
                                PrimitiveType::Float(b)
                            }
                            (PrimitiveType::Integer(a), PrimitiveType::Pointer(b)) => {
                                PrimitiveType::Pointer(b)
                            }
                            (PrimitiveType::Integer(a), PrimitiveType::Array(b)) => {
                                PrimitiveType::Pointer(b.cv_type)
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

                            _ => unreachable!("Sub expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::SubAssign => lhs_type,
                    ExprBinaryOp::Mul => {
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
                    ExprBinaryOp::MulAssign => lhs_type,
                    ExprBinaryOp::Div => {
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

                            _ => unreachable!("Div expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::DivAssign => lhs_type,
                    ExprBinaryOp::Mod => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("Mod expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::ModAssign => lhs_type,
                    ExprBinaryOp::BitwiseAnd => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("BitAnd expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::BitwiseAndAssign => lhs_type,
                    ExprBinaryOp::BitwiseOr => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("BitOr expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::BitwiseOrAssign => lhs_type,
                    ExprBinaryOp::BitwiseXor => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("BitAnd expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::BitwiseXorAssign => lhs_type,
                    ExprBinaryOp::ShiftLeft => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("BitAnd expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::ShiftLeftAssign => lhs_type,
                    ExprBinaryOp::ShiftRight => {
                        let type_ = match (lhs_type.type_, rhs_type.type_) {
                            (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                                PrimitiveType::Integer(a.common_type(&b))
                            }

                            _ => unreachable!("BitAnd expression type"),
                        };
                        CVType::from_primitive(type_)
                    }
                    ExprBinaryOp::ShiftRightAssign => lhs_type,
                    ExprBinaryOp::Equal => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::NotEqual => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::LessThan => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::LessThanOrEqual => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::GreaterThan => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::GreaterThanOrEqual => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::LogicalAnd => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::LogicalOr => {
                        CVType::from_primitive(PrimitiveType::Integer(Integer::Int32))
                    }
                    ExprBinaryOp::Comma => rhs_type,
                    ExprBinaryOp::Assign => lhs_type,
                }
            }
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
    pub member_offset: usize,
    pub member_type: CVType,
}
