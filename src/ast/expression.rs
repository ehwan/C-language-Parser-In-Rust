use super::ast::ASTType;
use super::ast::AST;
use super::typename::TypeInfo;
use crate::program::Program;

use std::any::Any;

#[derive(Debug, Clone)]
pub struct PrimaryIdentifierAST {
    pub name: String,
}
impl AST for PrimaryIdentifierAST {
    fn emit(&self, program: &mut Program) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionIdentifier
    }

    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        let var = program.get_variable(&self.name);
        if let Some(var) = var {
            var.0.clone()
        } else {
            panic!("Variable {} not found", self.name);
        }
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConstantInteger
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt32
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConstantCharacter
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt8
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConstantLong
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConstantFloat
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::Float32
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConstantDouble
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::Float64
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
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionStringLiteral
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        // len+1 for null-terminator
        TypeInfo::Array(Box::new(TypeInfo::UInt8), Some(self.value.len() + 1))
    }
}

#[derive(Debug)]
pub struct PostBracketAST {
    pub src: Box<dyn AST>,
    pub index: Box<dyn AST>,
}
impl AST for PostBracketAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionPostBracket
    }
}

#[derive(Debug)]
pub struct PostParen {
    pub src: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>,
}
impl AST for PostParen {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionPostParen
    }
}

#[derive(Debug)]
pub struct PostMemberAST {
    pub src: Box<dyn AST>,
    pub member: String,
}
impl AST for PostMemberAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionPostMember
    }
}

#[derive(Debug)]
pub struct PostIncrementAST {
    pub src: Box<dyn AST>,
}
impl AST for PostIncrementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionPostIncreasement
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        self.src.get_typeinfo_from_expression(program)
    }
}

#[derive(Debug)]
pub struct PostDecrementAST {
    pub src: Box<dyn AST>,
}
impl AST for PostDecrementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionPostDecreasement
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        self.src.get_typeinfo_from_expression(program)
    }
}

#[derive(Debug)]
pub struct CastExpressionAST {
    pub src: Box<dyn AST>,
    pub typeinfo: TypeInfo,
}
impl AST for CastExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionCast
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        self.typeinfo.clone()
    }
}

#[derive(Debug)]
pub struct SizeofTypeAST {
    pub typeinfo: TypeInfo,
}
impl AST for SizeofTypeAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionSizeofType
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.typeinfo.sizeof() as i64)
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
    }
}

#[derive(Debug)]
pub struct SizeofExprAST {
    pub expr: Box<dyn AST>,
}
impl AST for SizeofExprAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionSizeofExpr
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
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
    pub src: Box<dyn AST>,
}
impl AST for UnaryExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionUnaryOperation
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        let srctype = self.src.get_typeinfo_from_expression(program);
        match self.op {
            UnaryOperator::Plus | UnaryOperator::Minus => srctype,
            UnaryOperator::LogicalNot | UnaryOperator::BitwiseNot => srctype,
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(t) = self.src.get_typeinfo_from_expression(program) {
                    *t
                } else {
                    panic!("Dereference on non-pointer type");
                }
            }
            UnaryOperator::AddressOf => TypeInfo::Pointer(Box::new(srctype)),
            UnaryOperator::Increment | UnaryOperator::Decrement => srctype,
        }
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
    pub lhs: Box<dyn AST>,
    pub rhs: Box<dyn AST>,
}
impl AST for BinaryExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionBinaryOperation
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        match self.op {
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::BitwiseAnd
            | BinaryOperator::BitwiseOr
            | BinaryOperator::BitwiseXor
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
            | BinaryOperator::LogicalAnd
            | BinaryOperator::LogicalOr => todo!("Binary operator {:?}", self.op),
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThanOrEqual
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual => TypeInfo::UInt32,
            BinaryOperator::Assign
            | BinaryOperator::AddAssign
            | BinaryOperator::SubAssign
            | BinaryOperator::MulAssign
            | BinaryOperator::DivAssign
            | BinaryOperator::ModAssign
            | BinaryOperator::BitwiseAndAssign
            | BinaryOperator::BitwiseOrAssign
            | BinaryOperator::BitwiseXorAssign
            | BinaryOperator::ShiftLeftAssign
            | BinaryOperator::ShiftRightAssign => self.lhs.get_typeinfo_from_expression(program),
        }
    }
}

#[derive(Debug)]
pub struct PostArrowAST {
    pub src: Box<dyn AST>,
    pub member: String,
}
impl AST for PostArrowAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionArrow
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        let src_type = self.src.get_typeinfo_from_expression(program);
        if let TypeInfo::Pointer(t) = src_type {
            if let TypeInfo::Struct(s) = *t {
                let fields = if let Some(fields) = s.fields {
                    fields
                } else {
                    if let Some(name) = s.name {
                        let s = program
                            .get_type_info(&name)
                            .expect(format!("struct not found: {}", name).as_str());
                        if let TypeInfo::Struct(s) = s {
                            s.fields.unwrap()
                        } else {
                            panic!("{} is not a struct", name);
                        }
                    } else {
                        panic!("invalid struct type");
                    }
                };
                fields
                    .get(&self.member)
                    .expect(format!("field not found: {}", self.member).as_str())
                    .clone()
            } else {
                panic!("Arrow on non-struct type");
            }
        } else {
            panic!("Arrow on non-pointer type");
        }
    }
}

#[derive(Debug)]
pub struct ConditionalExpressionAST {
    pub cond: Box<dyn AST>,
    pub then_expr: Box<dyn AST>,
    pub else_expr: Box<dyn AST>,
}
impl AST for ConditionalExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionConditional
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        self.then_expr.get_typeinfo_from_expression(program)
    }
}

#[derive(Debug)]
pub struct CommaExpressionAST {
    pub lhs: Box<dyn AST>,
    pub rhs: Box<dyn AST>,
}
impl AST for CommaExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionComma
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        self.rhs.get_typeinfo_from_expression(program)
    }
}

#[derive(Debug)]
pub struct InitializerExpressionAST {
    pub initializer: Box<dyn AST>,
}
impl AST for InitializerExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionInitializer
    }
}

#[derive(Debug)]
pub struct InitializerListExpressionAST {
    pub initializers: Vec<Box<dyn AST>>,
}
impl AST for InitializerListExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::ExpressionInitializerList
    }
}
