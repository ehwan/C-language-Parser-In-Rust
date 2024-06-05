use crate::program::program::Program;
use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec::Vec;

use crate::program::*;

#[derive(Debug, Clone, Copy)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
}
pub trait AST: core::fmt::Debug + Any {
    fn emit(&self, program: &mut Program);
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone)]
pub struct NullAST;

impl AST for NullAST {
    fn emit(&self, program: &mut Program) {
        // do nothing
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

use rusty_parser as rp;

// dyn Parser type
pub type DynASTParser = Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>;

#[derive(Debug, Clone)]
pub struct PrimaryIdentifierAST {
    pub name: String,
}
impl AST for PrimaryIdentifierAST {
    fn emit(&self, program: &mut Program) {
        if let Some(var_ptr) = program.get_variable(&self.name) {
            program.stack.push(variable::Variable {
                data: Box::new(var_ptr),
            });
        } else {
            panic!("Variable {} not found", self.name);
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ConstantIntegerAST {
    pub value: u64,
}
impl AST for ConstantIntegerAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ConstantFloatAST {
    pub value: f64,
}
impl AST for ConstantFloatAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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

// expr[ index ]
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
}

// e1, e2, e3, ..., en
#[derive(Debug)]
pub struct ArgumentExpressionListAST {
    pub args: Vec<Box<dyn AST>>,
}
impl AST for ArgumentExpressionListAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

// expr( args... )
#[derive(Debug)]
pub struct PostParen {
    pub src: Box<dyn AST>,
    pub args: Box<dyn AST>,
}
impl AST for PostParen {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

// expr.member
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
}

#[derive(Debug)]
pub struct PostPointerAST {
    pub src: Box<dyn AST>,
    pub member: String,
}
impl AST for PostPointerAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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
}

#[derive(Debug)]
pub struct CastExpressionAST {
    pub src: Box<dyn AST>,
    pub typename: TypeSpecifier,
}
impl AST for CastExpressionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct TypenameAST {
    pub specifiers: Vec<Box<dyn AST>>,
    pub declarator: Box<dyn AST>,
}
impl AST for TypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct SizeofTypeAST {
    pub typename: TypeSpecifier,
}
impl AST for SizeofTypeAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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
}

#[derive(Debug, Clone, Copy)]
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
}

#[derive(Debug, Clone, Copy)]
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
}

#[derive(Debug)]
pub struct InitializerListAST {
    pub initializers: Vec<Box<dyn AST>>,
}
impl AST for InitializerListAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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
}

#[derive(Debug)]
pub struct ForStatementAST {
    pub init: Box<dyn AST>,
    pub cond: Box<dyn AST>,
    pub next: Box<dyn AST>,
    pub statement: Box<dyn AST>,
}
impl AST for ForStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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

#[derive(Debug)]
pub struct ContinueStatementAST;
impl AST for ContinueStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
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

#[derive(Debug)]
pub struct ReturnStatementAST {
    pub expr: Option<Box<dyn AST>>,
}
impl AST for ReturnStatementAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct TranslationUnitAST {
    pub declarations: Vec<Box<dyn AST>>,
}
impl AST for TranslationUnitAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclarationListAST {
    pub declarations: Vec<Box<dyn AST>>,
}
impl AST for DeclarationListAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclaratorIdentifierAST {
    pub name: String,
}
impl AST for DeclaratorIdentifierAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclaratorArrayAST {
    pub src: Box<dyn AST>,
    pub size: Option<Box<dyn AST>>,
}
impl AST for DeclaratorArrayAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclaratorFunctionAST {
    pub decl: Box<dyn AST>,
    pub args: Vec<Box<dyn AST>>,
}
impl AST for DeclaratorFunctionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ParameterDeclarationAST {
    pub specifiers: TypeSpecifier,
    pub declarator: Option<Box<dyn AST>>,
}
impl AST for ParameterDeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct InitDeclaratorAST {
    pub declarator: Box<dyn AST>,
    pub initializer: Box<dyn AST>,
}
impl AST for InitDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclarationAST {
    pub specifier: TypeSpecifier,
    pub init_declarators: Vec<Box<dyn AST>>,
}
impl AST for DeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct FunctionDefinitionAST {
    pub return_type: TypeSpecifier,
    pub funcdecl: Box<dyn AST>,
    pub body: Box<dyn AST>,
}
impl AST for FunctionDefinitionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
