use super::declarator;

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(ExprIdentifier),

    ConstantCharacter(ExprConstantCharacter),
    ConstantInteger(ExprConstantInteger),
    ConstantUnsignedInteger(ExprConstantUnsignedInteger),
    ConstantLong(ExprConstantLong),
    ConstantUnsignedLong(ExprConstantUnsignedLong),
    ConstantFloat(ExprConstantFloat),
    ConstantDouble(ExprConstantDouble),
    String(ExprString),

    Member(ExprMember),
    Arrow(ExprArrow),
    Bracket(ExprBracket),
    Paren(ExprParen),
    Cast(ExprCast),
    SizeofType(ExprSizeOfType),
    SizeofExpr(ExprSizeOfExpr),
    Conditional(ExprConditional),

    Unary(ExprUnary),
    Binary(ExprBinary),

    InitializerList(ExprInitializerList),
}

#[derive(Debug, Clone)]
pub struct ExprIdentifier {
    pub name: String,
}

/// 'c' character
#[derive(Debug, Clone)]
pub struct ExprConstantCharacter {
    pub value: i8,
}
/// constant integer
#[derive(Debug, Clone)]
pub struct ExprConstantInteger {
    pub value: i32,
}
/// constant unsigned integer
#[derive(Debug, Clone)]
pub struct ExprConstantUnsignedInteger {
    pub value: u32,
}
/// constant long
#[derive(Debug, Clone)]
pub struct ExprConstantLong {
    pub value: i64,
}
/// constant unsigned long
#[derive(Debug, Clone)]
pub struct ExprConstantUnsignedLong {
    pub value: u64,
}

/// constant float
#[derive(Debug, Clone)]
pub struct ExprConstantFloat {
    pub value: f32,
}

/// constant double
#[derive(Debug, Clone)]
pub struct ExprConstantDouble {
    pub value: f64,
}
/// "string literal"
#[derive(Debug, Clone)]
pub struct ExprString {
    pub value: String,
}

/// src.member
#[derive(Debug, Clone)]
pub struct ExprMember {
    pub src: Box<Expression>,
    pub member: String,
}
// src->member
#[derive(Debug, Clone)]
pub struct ExprArrow {
    pub src: Box<Expression>,
    pub member: String,
}
/// src[index]
#[derive(Debug, Clone)]
pub struct ExprBracket {
    pub src: Box<Expression>,
    pub index: Box<Expression>,
}

/// src( args... )
#[derive(Debug, Clone)]
pub struct ExprParen {
    pub src: Box<Expression>,
    pub args: Vec<Expression>,
}
/// (typeinfo)src
#[derive(Debug, Clone)]
pub struct ExprCast {
    pub src: Box<Expression>,
    pub typename: declarator::Typename,
}
/// sizeof(type)
#[derive(Debug, Clone)]
pub struct ExprSizeOfType {
    pub typename: declarator::Typename,
}
/// sizeof(expr)
#[derive(Debug, Clone)]
pub struct ExprSizeOfExpr {
    pub expr: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct ExprConditional {
    pub cond: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprUnaryOperator,
    pub src: Box<Expression>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprUnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    Dereference,
    AddressOf,
    IncrementPre,
    DecrementPre,
    IncrementPost,
    DecrementPost,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprBinaryOperator {
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
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    Comma,
}

#[derive(Debug, Clone)]
pub struct ExprBinary {
    pub op: ExprBinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ExprInitializerList {
    pub initializers: Vec<Expression>,
}
