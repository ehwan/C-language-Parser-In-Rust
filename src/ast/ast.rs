use crate::program::Program;
use std::any::Any;

use super::typename::TypeInfo;

pub enum ASTType {
    Unknown,

    // Statements
    StatementNullExpression, // ;
    StatementExpression,     // Expression;
    StatementLabeled,
    StatementCase,
    StatementDefault,
    StatementCompound,
    StatementIf,
    StatementSwitch,
    StatementWhile,
    StatementDoWhile,
    StatementFor,
    StatementGoto,
    StatementContinue,
    StatementBreak,
    StatementReturn,
    StatementDeclarationVars,
    StatementDeclaration,
    StatementFunctionDefinition,
    StatementTranslationUnit,

    /// Declarator
    DeclaratorIdentifier,
    DeclaratorDirectArrayFixed,
    DeclaratorDirectArrayUnbounded,
    DeclaratorDirectFunction,

    DeclaratorPointer,
    DeclaratorInit,

    DeclaratorAbstractArrayFixed,
    DeclaratorAbstractArrayUnbounded,
    DeclaratorAbstractFunction,
    DeclaratorAbstractPointer,

    /// Expression
    ExpressionIdentifier,
    ExpressionConstantInteger,
    ExpressionConstantCharacter,
    ExpressionConstantLong,
    ExpressionConstantFloat,
    ExpressionConstantDouble,
    ExpressionStringLiteral,
    ExpressionPostBracket,
    ExpressionPostParen,
    ExpressionPostMember,
    ExpressionPostIncreasement,
    ExpressionPostDecreasement,
    ExpressionCast,
    ExpressionSizeofType,
    ExpressionSizeofExpr,
    ExpressionUnaryOperation,
    ExpressionBinaryOperation,
    ExpressionArrow,
    ExpressionConditional,
    ExpressionComma,
    ExpressionInitializer,
    ExpressionInitializerList,
}

pub trait AST: core::fmt::Debug + Any {
    fn emit(&self, program: &mut Program);
    fn as_any(&self) -> &dyn Any;

    fn get_type(&self) -> ASTType {
        ASTType::Unknown
    }

    /// ==================== for expression ====================
    fn is_writeable_expression(&self) -> bool {
        panic!("is_writeable_expression not implemented for {:?}", self);
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Err(format!("get_constant_i64 not implemented for {:?}", self))
    }
    fn get_typeinfo_from_expression(&self, program: &Program) -> TypeInfo {
        panic!(
            "get_typeinfo_from_expression not implemented for {:?}",
            self
        );
    }

    // ==================== for declarator ====================
    // get decorated (with pointer, array, function) type info from declarator
    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        panic!(
            "get_abstract_typeinfo_with_declarator not implemented for {:?}",
            self
        );
    }
    // get (variable_name, real_type) from direct declarator and type info
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        panic!(
            "get_direct_typeinfo_from_declarator not implemented for {:?}",
            self
        );
    }
}
