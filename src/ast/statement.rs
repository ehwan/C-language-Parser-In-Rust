use super::{
    ast::AST, declarator::DeclaratorTrait, expression::ExpressionTrait, typename::TypeSpecifier,
};
use crate::program::Program;

use std::any::Any;

pub enum StatementType {
    NullExpression, // ;
    Expression,     // Expression;
    Labeled,
    Case,
    Default,
    Compound,
    If,
    Switch,
    While,
    DoWhile,
    For,
    Goto,
    Continue,
    Break,
    Return,
    DeclarationVars,
    Declaration,
    FunctionDefinition,
    TranslationUnit,
    ParameterDeclaration,
    StructMemberDeclaration,
}

pub trait StatementTrait: AST {
    fn get_type(&self) -> StatementType;
}

#[derive(Debug)]
pub struct NullStatementAST;
impl AST for NullStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for NullStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::NullExpression
    }
}

#[derive(Debug)]
pub struct ExpressionStatementAST {
    pub expr: Box<dyn ExpressionTrait>,
}
impl AST for ExpressionStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for ExpressionStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Expression
    }
}

#[derive(Debug)]
pub struct LabeledStatementAST {
    pub label: String,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for LabeledStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for LabeledStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Labeled
    }
}

#[derive(Debug)]
pub struct CaseStatementAST {
    pub value: Box<dyn ExpressionTrait>,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for CaseStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for CaseStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Case
    }
}

#[derive(Debug)]
pub struct DefaultStatementAST {
    pub statement: Box<dyn StatementTrait>,
}
impl AST for DefaultStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for DefaultStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Default
    }
}

#[derive(Debug)]
pub struct CompoundStatementAST {
    pub statements: Vec<Box<dyn StatementTrait>>,
}
impl AST for CompoundStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for CompoundStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Compound
    }
}

#[derive(Debug)]
pub struct IfStatementAST {
    pub cond: Box<dyn ExpressionTrait>,
    pub then_statement: Box<dyn StatementTrait>,
    pub else_statement: Option<Box<dyn StatementTrait>>,
}
impl AST for IfStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for IfStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::If
    }
}

#[derive(Debug)]
pub struct SwitchStatementAST {
    pub cond: Box<dyn ExpressionTrait>,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for SwitchStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for SwitchStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Switch
    }
}

#[derive(Debug)]
pub struct WhileStatementAST {
    pub cond: Box<dyn ExpressionTrait>,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for WhileStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for WhileStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::While
    }
}

#[derive(Debug)]
pub struct DoWhileStatementAST {
    pub cond: Box<dyn ExpressionTrait>,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for DoWhileStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for DoWhileStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::DoWhile
    }
}

#[derive(Debug)]
pub struct ForStatementAST {
    pub init: Box<dyn StatementTrait>,
    pub cond: Box<dyn StatementTrait>,
    pub next: Option<Box<dyn ExpressionTrait>>,
    pub statement: Box<dyn StatementTrait>,
}
impl AST for ForStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for ForStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::For
    }
}

#[derive(Debug)]
pub struct GotoStatementAST {
    pub label: String,
}
impl AST for GotoStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for GotoStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Goto
    }
}

#[derive(Debug)]
pub struct ContinueStatementAST;
impl AST for ContinueStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for ContinueStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Continue
    }
}

#[derive(Debug)]
pub struct BreakStatementAST;
impl AST for BreakStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for BreakStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Break
    }
}

#[derive(Debug)]
pub struct ReturnStatementAST {
    pub expr: Option<Box<dyn ExpressionTrait>>,
}
impl AST for ReturnStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for ReturnStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Return
    }
}

#[derive(Debug)]
pub struct DeclarationVarsStatementAST {
    pub specifier: TypeSpecifier,
    pub declarators: Vec<Box<dyn DeclaratorTrait>>,
}
impl AST for DeclarationVarsStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for DeclarationVarsStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::DeclarationVars
    }
}

#[derive(Debug)]
pub struct DeclarationStatementAST {
    pub specifier: TypeSpecifier,
}
impl AST for DeclarationStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for DeclarationStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::Declaration
    }
}

#[derive(Debug)]
pub struct FunctionDefinitionStatementAST {
    pub specifier: TypeSpecifier,
    pub declarator: Box<dyn DeclaratorTrait>,
    pub body: Box<dyn StatementTrait>,
}
impl AST for FunctionDefinitionStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for FunctionDefinitionStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::FunctionDefinition
    }
}

#[derive(Debug)]
pub struct TranslationUnitAST {
    pub statements: Vec<Box<dyn StatementTrait>>,
}
impl AST for TranslationUnitAST {
    fn emit(&self, program: &mut Program) {
        for statement in &self.statements {
            statement.emit(program);
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for TranslationUnitAST {
    fn get_type(&self) -> StatementType {
        StatementType::TranslationUnit
    }
}

#[derive(Debug)]
pub struct ParameterDeclarationStatementAST {
    pub specifier: TypeSpecifier,
    pub declarator: Option<Box<dyn DeclaratorTrait>>,
}
impl AST for ParameterDeclarationStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for ParameterDeclarationStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::ParameterDeclaration
    }
}

#[derive(Debug)]
pub struct StructMemberDeclarationStatementAST {
    pub specifier: TypeSpecifier,
    pub declarators: Vec<Box<dyn DeclaratorTrait>>,
}
impl AST for StructMemberDeclarationStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl StatementTrait for StructMemberDeclarationStatementAST {
    fn get_type(&self) -> StatementType {
        StatementType::StructMemberDeclaration
    }
}
