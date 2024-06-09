use super::{
    ast::{ASTType, AST},
    typename::TypeInfo,
};
use crate::program::Program;

use std::any::Any;

#[derive(Debug)]
pub struct NullStatementAST;
impl AST for NullStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementNullExpression
    }
}

#[derive(Debug)]
pub struct ExpressionStatementAST {
    pub expression: Box<dyn AST>,
}
impl AST for ExpressionStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementExpression
    }
}

#[derive(Debug)]
pub struct LabeledStatementAST {
    pub label: String,
    pub statement: Box<dyn AST>,
}
impl AST for LabeledStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementLabeled
    }
}

#[derive(Debug)]
pub struct CaseStatementAST {
    pub value: Box<dyn AST>,
    pub statement: Box<dyn AST>,
}
impl AST for CaseStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementCase
    }
}

#[derive(Debug)]
pub struct DefaultStatementAST {
    pub statement: Box<dyn AST>,
}
impl AST for DefaultStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementDefault
    }
}

#[derive(Debug)]
pub struct CompoundStatementAST {
    pub statements: Vec<Box<dyn AST>>,
}
impl AST for CompoundStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementCompound
    }
}

#[derive(Debug)]
pub struct IfStatementAST {
    pub cond: Box<dyn AST>,
    pub then_statement: Box<dyn AST>,
    pub else_statement: Option<Box<dyn AST>>,
}
impl AST for IfStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementIf
    }
}

#[derive(Debug)]
pub struct SwitchStatementAST {
    pub cond: Box<dyn AST>,
    pub statement: Box<dyn AST>,
}
impl AST for SwitchStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementSwitch
    }
}

#[derive(Debug)]
pub struct WhileStatementAST {
    pub cond: Box<dyn AST>,
    pub statement: Box<dyn AST>,
}
impl AST for WhileStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementWhile
    }
}

#[derive(Debug)]
pub struct DoWhileStatementAST {
    pub cond: Box<dyn AST>,
    pub statement: Box<dyn AST>,
}
impl AST for DoWhileStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementDoWhile
    }
}

#[derive(Debug)]
pub struct ForStatementAST {
    pub init: Box<dyn AST>,
    pub cond: Box<dyn AST>,
    pub next: Option<Box<dyn AST>>,
    pub statement: Box<dyn AST>,
}
impl AST for ForStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementFor
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
    fn get_type(&self) -> ASTType {
        ASTType::StatementGoto
    }
}

#[derive(Debug)]
pub struct ContinueStatementAST;
impl AST for ContinueStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementContinue
    }
}

#[derive(Debug)]
pub struct BreakStatementAST;
impl AST for BreakStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementBreak
    }
}

#[derive(Debug)]
pub struct ReturnStatementAST {
    pub expr: Option<Box<dyn AST>>,
}
impl AST for ReturnStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementReturn
    }
}

#[derive(Debug)]
pub struct DeclarationVarsStatementAST {
    pub typeinfo: TypeInfo,
    pub declarators: Vec<Box<dyn AST>>,
}
impl AST for DeclarationVarsStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementDeclarationVars
    }
}

#[derive(Debug)]
pub struct DeclarationStatementAST {
    pub typeinfo: TypeInfo,
}
impl AST for DeclarationStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementDeclaration
    }
}

#[derive(Debug)]
pub struct FunctionDefinitionStatementAST {
    pub return_type: TypeInfo,
    pub declarator: Box<dyn AST>,
    pub body: Box<dyn AST>,
}
impl AST for FunctionDefinitionStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::StatementFunctionDefinition
    }
}

#[derive(Debug)]
pub struct TranslationUnitAST {
    pub statements: Vec<Box<dyn AST>>,
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
    fn get_type(&self) -> ASTType {
        ASTType::StatementTranslationUnit
    }
}
