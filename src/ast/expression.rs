use super::ast::AST;
use super::typename::TypenameTrait;
use crate::program::Program;

use std::any::Any;

pub enum ExpressionType {
    Identifier,
    ConstantInteger,
    ConstantCharacter,
    ConstantLong,
    ConstantFloat,
    ConstantDouble,
    StringLiteral,
    PostBracket,
    PostParen,
    PostMember,
    PostIncreasement,
    PostDecreasement,
    Cast,
    SizeofType,
    SizeofExpr,
    UnaryOperation,
    BinaryOperation,
    Arrow,
    Conditional,
    Comma,
    Initializer,
    InitializerList,
}
pub trait ExpressionTrait: AST {
    fn get_type(&self) -> ExpressionType;
    fn is_writeable(&self) -> bool;
}

#[derive(Debug, Clone)]
pub struct PrimaryIdentifierAST {
    pub name: String,
}
impl AST for PrimaryIdentifierAST {
    fn emit(&self, program: &mut Program) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PrimaryIdentifierAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Identifier
    }
    fn is_writeable(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub struct ConstantIntegerAST {
    pub value: u32,
}
impl AST for ConstantIntegerAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConstantIntegerAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::ConstantInteger
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantCharacterAST {
    pub value: u8,
}
impl AST for ConstantCharacterAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConstantCharacterAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::ConstantCharacter
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantLongAST {
    pub value: u64,
}
impl AST for ConstantLongAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConstantLongAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::ConstantLong
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantFloatAST {
    pub value: f32,
}
impl AST for ConstantFloatAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConstantFloatAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::ConstantFloat
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantDoubleAST {
    pub value: f64,
}
impl AST for ConstantDoubleAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConstantDoubleAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::ConstantDouble
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteralAST {
    pub value: String,
}
impl AST for StringLiteralAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for StringLiteralAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::StringLiteral
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PostBracketAST {
    pub src: Box<dyn ExpressionTrait>,
    pub index: Box<dyn ExpressionTrait>,
}
impl AST for PostBracketAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostBracketAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::PostBracket
    }
    fn is_writeable(&self) -> bool {
        self.src.is_writeable()
    }
}

#[derive(Debug)]
pub struct PostParen {
    pub src: Box<dyn ExpressionTrait>,
    pub args: Vec<Box<dyn ExpressionTrait>>,
}
impl AST for PostParen {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostParen {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::PostParen
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}

#[derive(Debug)]
pub struct PostMemberAST {
    pub src: Box<dyn ExpressionTrait>,
    pub member: String,
}
impl AST for PostMemberAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostMemberAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::PostMember
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}

#[derive(Debug)]
pub struct PostIncrementAST {
    pub src: Box<dyn ExpressionTrait>,
}
impl AST for PostIncrementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostIncrementAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::PostIncreasement
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PostDecrementAST {
    pub src: Box<dyn ExpressionTrait>,
}
impl AST for PostDecrementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostDecrementAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::PostDecreasement
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct CastExpressionAST {
    pub src: Box<dyn ExpressionTrait>,
    pub typename: Box<dyn TypenameTrait>,
}
impl AST for CastExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for CastExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Cast
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct SizeofTypeAST {
    pub typename: Box<dyn TypenameTrait>,
}
impl AST for SizeofTypeAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for SizeofTypeAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::SizeofType
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct SizeofExprAST {
    pub expr: Box<dyn ExpressionTrait>,
}
impl AST for SizeofExprAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for SizeofExprAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::SizeofExpr
    }
    fn is_writeable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    Dereference,
    AddressOf,
    Increment,
    Decrement,
}
#[derive(Debug)]
pub struct UnaryExpressionAST {
    pub op: UnaryOperator,
    pub src: Box<dyn ExpressionTrait>,
}
impl AST for UnaryExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for UnaryExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::UnaryOperation
    }
    fn is_writeable(&self) -> bool {
        self.op == UnaryOperator::Dereference
    }
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
}
#[derive(Debug)]
pub struct BinaryExpressionAST {
    pub op: BinaryOperator,
    pub lhs: Box<dyn ExpressionTrait>,
    pub rhs: Box<dyn ExpressionTrait>,
}
impl AST for BinaryExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for BinaryExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::BinaryOperation
    }
    fn is_writeable(&self) -> bool {
        self.op == BinaryOperator::Assign && self.lhs.is_writeable()
    }
}

#[derive(Debug)]
pub struct PostArrowAST {
    pub src: Box<dyn ExpressionTrait>,
    pub member: String,
}
impl AST for PostArrowAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for PostArrowAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Arrow
    }
    fn is_writeable(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct ConditionalExpressionAST {
    pub cond: Box<dyn ExpressionTrait>,
    pub then_expr: Box<dyn ExpressionTrait>,
    pub else_expr: Box<dyn ExpressionTrait>,
}
impl AST for ConditionalExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for ConditionalExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Conditional
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}

#[derive(Debug)]
pub struct CommaExpressionAST {
    pub lhs: Box<dyn ExpressionTrait>,
    pub rhs: Box<dyn ExpressionTrait>,
}
impl AST for CommaExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for CommaExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Comma
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}

#[derive(Debug)]
pub struct InitializerExpressionAST {
    pub initializer: Box<dyn ExpressionTrait>,
}
impl AST for InitializerExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for InitializerExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Initializer
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}

#[derive(Debug)]
pub struct InitializerListExpressionAST {
    pub initializers: Vec<Box<dyn ExpressionTrait>>,
}
impl AST for InitializerListExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl ExpressionTrait for InitializerListExpressionAST {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::InitializerList
    }
    fn is_writeable(&self) -> bool {
        todo!();
    }
}
