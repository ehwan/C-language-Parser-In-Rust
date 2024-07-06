use std::fmt::Debug;

use super::context::PreprocessorContext;

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    ShiftLeft,
    ShiftRight,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
}

pub trait PreprocessorExpression: Debug {
    fn eval(&self, ctx: &mut PreprocessorContext) -> i64;
}

#[derive(Debug)]
pub struct Constant {
    pub value: i64,
}
impl PreprocessorExpression for Constant {
    fn eval(&self, _ctx: &mut PreprocessorContext) -> i64 {
        self.value
    }
}

#[derive(Debug)]
pub struct Defined {
    pub name: String,
}
impl PreprocessorExpression for Defined {
    fn eval(&self, ctx: &mut PreprocessorContext) -> i64 {
        if ctx.define_map.contains_key(&self.name) {
            1
        } else {
            0
        }
    }
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub op: UnaryOperator,
    pub src: Box<dyn PreprocessorExpression>,
}
impl PreprocessorExpression for UnaryExpression {
    fn eval(&self, ctx: &mut PreprocessorContext) -> i64 {
        match self.op {
            UnaryOperator::Plus => self.src.eval(ctx),
            UnaryOperator::Minus => -self.src.eval(ctx),
            UnaryOperator::LogicalNot => {
                if self.src.eval(ctx) == 0 {
                    1
                } else {
                    0
                }
            }
            UnaryOperator::BitwiseNot => !self.src.eval(ctx),
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn PreprocessorExpression>,
    pub rhs: Box<dyn PreprocessorExpression>,
}
impl PreprocessorExpression for BinaryExpression {
    fn eval(&self, ctx: &mut PreprocessorContext) -> i64 {
        let lhs = self.lhs.eval(ctx);
        let rhs = self.rhs.eval(ctx);
        match self.op {
            BinaryOperator::Add => lhs + rhs,
            BinaryOperator::Sub => lhs - rhs,
            BinaryOperator::Mul => lhs * rhs,
            BinaryOperator::Div => lhs / rhs,
            BinaryOperator::Mod => lhs % rhs,
            BinaryOperator::BitwiseAnd => lhs & rhs,
            BinaryOperator::BitwiseOr => lhs | rhs,
            BinaryOperator::BitwiseXor => lhs ^ rhs,
            BinaryOperator::LogicalAnd => {
                if lhs != 0 && rhs != 0 {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::LogicalOr => {
                if lhs != 0 || rhs != 0 {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::ShiftLeft => lhs << rhs,
            BinaryOperator::ShiftRight => lhs >> rhs,
            BinaryOperator::LessThan => {
                if lhs < rhs {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::GreaterThan => {
                if lhs > rhs {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::LessThanOrEqual => {
                if lhs <= rhs {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::GreaterThanOrEqual => {
                if lhs >= rhs {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::Equal => {
                if lhs == rhs {
                    1
                } else {
                    0
                }
            }
            BinaryOperator::NotEqual => {
                if lhs != rhs {
                    1
                } else {
                    0
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ConditionalExpression {
    pub cond: Box<dyn PreprocessorExpression>,
    pub then_expr: Box<dyn PreprocessorExpression>,
    pub else_expr: Box<dyn PreprocessorExpression>,
}
impl PreprocessorExpression for ConditionalExpression {
    fn eval(&self, ctx: &mut PreprocessorContext) -> i64 {
        let cond = self.cond.eval(ctx);
        if cond != 0 {
            self.then_expr.eval(ctx)
        } else {
            self.else_expr.eval(ctx)
        }
    }
}
