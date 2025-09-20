use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::scope::FunctionDefinition;

use super::{Expression, LabelInfo, VariableInfo};

#[derive(Debug, Clone)]
pub enum Statement {
    None,
    Expression(StmtExpression),
    Labeled(StmtLabeled),
    Goto(StmtGoto),
    Compound(StmtCompound),
    If(StmtIf),
    Switch(StmtSwitch),
    /// for internal use; this variant will not be visible to end-users
    _Case(StmtCase),
    /// for internal use; this variant will not be visible to end-users
    _Default(StmtDefault),
    Continue,
    Break,
    Return(StmtReturn),
    For(StmtFor),
    While(StmtWhile),
    DoWhile(StmtDoWhile),

    VariableDeclaration(StmtVariableDeclaration),
}

#[derive(Debug, Clone)]
pub struct StmtExpression {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct StmtLabeled {
    pub label: Rc<RefCell<LabelInfo>>,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct StmtGoto {
    pub label: Rc<RefCell<LabelInfo>>,
}

#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct StmtIf {
    pub condition: Expression,
    pub then: Box<Statement>,
    pub else_: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct StmtSwitch {
    pub value: Expression,
    pub cases: Vec<StmtSwitchCase>,
    pub default: Option<usize>,
}
#[derive(Debug, Clone)]
pub struct StmtSwitchCase {
    /// `None` for `default`
    pub value: Option<Expression>,
    pub statements: Vec<Statement>,
}
#[derive(Debug, Clone)]
pub struct StmtCase {
    pub value: Expression,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct StmtDefault {
    pub statement: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub expression: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct StmtFor {
    pub init: Box<Statement>,
    pub condition: Expression,
    pub next: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct StmtDoWhile {
    pub body: Box<Statement>,
    pub condition: Expression,
}

#[derive(Debug, Clone)]
pub struct StmtVariableDeclaration {
    /// (variable, initialization statements) pairs
    pub pairs: Vec<(VariableInfo, Vec<Statement>)>,
}

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub statements: Vec<Statement>,
    pub variables: HashMap<String, VariableInfo>,
    pub functions: HashMap<String, FunctionDefinition>,
}
