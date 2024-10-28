use super::declarator;
use super::expression::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Null(StmtNull),
    Expression(StmtExpression),
    Labeled(StmtLabeled),
    Compound(StmtCompound),
    If(StmtIf),
    Switch(StmtSwitch),
    Case(StmtCase),
    Default(StmtDefault),
    Continue(StmtContinue),
    Break(StmtBreak),
    While(StmtWhile),
    DoWhile(StmtDoWhile),
    For(StmtFor),
    Goto(StmtGoto),
    Return(StmtReturn),
    Declaration(StmtDeclaration),
    FunctionDefinition(StmtFunctionDefinition),
}

/// Statements that do nothing
#[derive(Debug, Clone)]
pub struct StmtNull;
impl StmtNull {}

/// for any expression ends with semicolon ';'
#[derive(Debug, Clone)]
pub struct StmtExpression {
    pub expression: Expression,
}
impl StmtExpression {}

/// label:
///    statement
#[derive(Debug, Clone)]
pub struct StmtLabeled {
    pub label: String,
    pub statement: Box<Statement>,
}
impl StmtLabeled {}

/// { statements ... }
#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub statements: Vec<Statement>,
}
impl StmtCompound {}

/// if ( condition_expression ) then_statement else else_statement
/// no else if statement
#[derive(Debug, Clone)]
pub struct StmtIf {
    pub cond: Expression,
    pub then_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}
impl StmtIf {}

/// switch ( target_expression ) body_statement
#[derive(Debug, Clone)]
pub struct StmtSwitch {
    pub target: Expression,
    pub statement: Box<Statement>,
}
impl StmtSwitch {}
/// case value: statement
#[derive(Debug, Clone)]
pub struct StmtCase {
    pub value: Expression,
    pub statement: Box<Statement>,
}
impl StmtCase {}

/// default: statement
#[derive(Debug, Clone)]
pub struct StmtDefault {
    pub statement: Box<Statement>,
}
impl StmtDefault {}

/// continue;
#[derive(Debug, Clone)]
pub struct StmtContinue;
impl StmtContinue {}

/// break;
#[derive(Debug, Clone)]
pub struct StmtBreak;
impl StmtBreak {}

/// while ( condition_expression ) statement
#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub cond: Expression,
    pub statement: Box<Statement>,
}
impl StmtWhile {}

/// do statement while ( condition_expression );

#[derive(Debug, Clone)]
pub struct StmtDoWhile {
    pub cond: Expression,
    pub statement: Box<Statement>,
}

/// for ( init; cond; next ) statement
/// since init is expression, must declare variable before entering for loop
#[derive(Debug, Clone)]
pub struct StmtFor {
    pub init: Expression,
    pub cond: Expression,
    pub next: Option<Expression>,
    pub statement: Box<Statement>,
}

/// goto label;
#[derive(Debug, Clone)]
pub struct StmtGoto {
    pub label: String,
}

/// return; or return expression;
#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub expr: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct StmtDeclaration {
    pub specs: Vec<declarator::DeclarationSpecifier>,
    pub inits: Option<Vec<declarator::DeclInit>>,
}

#[derive(Debug, Clone)]
pub struct StmtFunctionDefinition {
    pub specs: Option<Vec<declarator::DeclarationSpecifier>>,
    pub decl: declarator::Declarator,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub statements: Vec<Statement>,
}
