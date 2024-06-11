use super::{expression::Expression, typename::TypeInfo};
use crate::program::instruction::binary::Assign;
use crate::program::instruction::{
    self, DeclareEnum, DeclareStructure, DeclareUnion, GetLabelAddress, Jump, JumpNotZero,
    JumpZero, NewScope, PopScope, Return,
};
use crate::program::instruction::{GetVariable, Instruction};
use crate::program::program::FunctionData;
use crate::program::program::Program;

use std::any::Any;

pub trait Statement: core::fmt::Debug + Any {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>);
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct NullStatement;
impl Statement for NullStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<dyn Expression>,
}
impl Statement for ExpressionStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.expression.emit(program, instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct LabeledStatement {
    pub label: String,
    pub statement: Box<dyn Statement>,
}
impl Statement for LabeledStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        program.set_label(self.label.clone(), instructions);
        self.statement.emit(program, instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct CompoundStatement {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Statement for CompoundStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(NewScope {}));
        for statement in &self.statements {
            statement.emit(program, instructions);
        }
        instructions.push(Box::new(PopScope {}));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Box<dyn Expression>,
    pub then_statement: Box<dyn Statement>,
    pub else_statement: Option<Box<dyn Statement>>,
}
impl Statement for IfStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.cond.emit(program, instructions);
        let else_label = program.get_unique_label();
        let end_label = program.get_unique_label();
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: else_label.clone(),
        }));
        instructions.push(Box::new(JumpZero::<1, 0> {}));
        self.then_statement.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: end_label.clone(),
        }));
        instructions.push(Box::new(Jump::<1> {}));
        program.set_label(else_label, instructions);
        if let Some(else_statement) = &self.else_statement {
            else_statement.emit(program, instructions);
        }
        program.set_label(end_label.clone(), instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct SwitchStatement {
    pub target: Box<dyn Expression>,
    pub statement: Box<dyn Statement>,
}
impl Statement for SwitchStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("SwitchStatementAST::emit not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct CaseStatement {
    pub value: Box<dyn Expression>,
    pub statement: Box<dyn Statement>,
}
impl Statement for CaseStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("CaseStatementAST::emit not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct DefaultStatement {
    pub statement: Box<dyn Statement>,
}
impl Statement for DefaultStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("DefaultStatementAST::emit not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ContinueStatement;
impl Statement for ContinueStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        if program.label_stack.len() < 2 {
            panic!("Continue: label_stack is empty");
        }
        let continue_label = &program.label_stack[program.label_stack.len() - 2];
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: continue_label.clone(),
        }));
        instructions.push(Box::new(Jump::<1> {}));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct BreakStatement;
impl Statement for BreakStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let end_label = program
            .label_stack
            .last()
            .expect("Break: label_stack is empty")
            .clone();
        instructions.push(Box::new(GetLabelAddress::<1> { label: end_label }));
        instructions.push(Box::new(Jump::<1> {}));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct WhileStatement {
    pub cond: Box<dyn Expression>,
    pub statement: Box<dyn Statement>,
}
impl Statement for WhileStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let start_label = program.get_unique_label();
        let end_label = program.get_unique_label();

        program.label_stack.push(start_label.clone());
        program.label_stack.push(end_label.clone());

        program.set_label(start_label.clone(), instructions);
        self.cond.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: end_label.clone(),
        }));
        instructions.push(Box::new(JumpZero::<1, 0> {}));
        self.statement.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: start_label.clone(),
        }));
        instructions.push(Box::new(Jump::<1> {}));
        program.set_label(end_label, instructions);

        program.label_stack.pop().expect("label_stack is empty");
        program.label_stack.pop().expect("label_stack is empty");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DoWhileStatement {
    pub cond: Box<dyn Expression>,
    pub statement: Box<dyn Statement>,
}
impl Statement for DoWhileStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let start_label = program.get_unique_label();
        let continue_label = program.get_unique_label();
        let end_label = program.get_unique_label();

        // start_label:
        //    do { body ... }
        // continue_label:
        //    while (cond);
        // end_label:

        program.label_stack.push(continue_label.clone());
        program.label_stack.push(end_label.clone());

        program.set_label(start_label.clone(), instructions);
        self.statement.emit(program, instructions);

        program.set_label(continue_label.clone(), instructions);
        self.cond.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: start_label.clone(),
        }));
        instructions.push(Box::new(JumpNotZero::<1, 0> {}));
        program.set_label(end_label.clone(), instructions);

        program
            .label_stack
            .pop()
            .expect("DoWhile: label_stack is empty");
        program
            .label_stack
            .pop()
            .expect("DoWhile: label_stack is empty");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Box<dyn Expression>,
    pub cond: Box<dyn Expression>,
    pub next: Option<Box<dyn Expression>>,
    pub statement: Box<dyn Statement>,
}
impl Statement for ForStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let cond_label = program.get_unique_label();
        let end_label = program.get_unique_label();
        let continue_label = program.get_unique_label();

        program.label_stack.push(continue_label.clone());
        program.label_stack.push(end_label.clone());

        self.init.emit(program, instructions);
        program.set_label(cond_label.clone(), instructions);
        self.cond.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: end_label.clone(),
        }));
        instructions.push(Box::new(JumpZero::<1, 0> {}));

        self.statement.emit(program, instructions);
        program.set_label(continue_label.clone(), instructions);
        if let Some(next) = &self.next {
            next.emit(program, instructions);
        }
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: cond_label.clone(),
        }));
        instructions.push(Box::new(Jump::<1> {}));
        program.set_label(end_label, instructions);

        program.label_stack.pop().expect("label_stack is empty");
        program.label_stack.pop().expect("label_stack is empty");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct GotoStatement {
    pub label: String,
}
impl Statement for GotoStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("GotoStatementAST::emit not implemented");
        // comment out due to scope level not matching
        // instructions.push(Box::new(GetLabelAddress::<1> {
        //     label: self.label.clone(),
        // }));
        // instructions.push(Box::new(Jump::<1> {}));
        /*
        program
            .instructions
            .push(Box::new(crate::program::instruction::conditional::Jump {
                label: self.label.clone(),
            }));
            */
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expr: Option<Box<dyn Expression>>,
}
impl Statement for ReturnStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        if let Some(expr) = &self.expr {
            expr.emit(program, instructions);
        }
        instructions.push(Box::new(Return {}));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub vars: Vec<(Option<String>, TypeInfo, Option<Box<dyn Expression>>)>, // name, type, initializer
}
impl Statement for DeclarationStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        for declaration in &self.vars {
            if declaration.0.is_none() && declaration.2.is_none() {
                // Struct & Enum & Union declaration
                match declaration.1 {
                    TypeInfo::Struct(ref t) => {
                        if t.name.is_none() {
                            println!("Anonymous struct in declaration statement; ignored it");
                        } else {
                            instructions.push(Box::new(DeclareStructure { info: t.clone() }));
                        }
                    }
                    TypeInfo::Union(ref t) => {
                        if t.name.is_none() {
                            println!("Anonymous union in declaration statement; ignored it");
                        } else {
                            instructions.push(Box::new(DeclareUnion { info: t.clone() }));
                        }
                    }
                    TypeInfo::Enum(ref t) => {
                        if t.name.is_none() {
                            println!("Anonymous enum in declaration statement; ignored it");
                        } else {
                            instructions.push(Box::new(DeclareEnum { info: t.clone() }));
                        }
                    }
                    _ => panic!("Invalid type for anonymous declaration"),
                }
            } else if declaration.0.is_none() {
                panic!("Anonymous variable is not allowed in declaration statement");
            } else {
                // variable declaration
                if let Some(initial_value) = &declaration.2 {
                    // variable with initial value
                    initial_value.emit(program, instructions);

                    instructions.push(Box::new(crate::program::instruction::NewVariable {
                        name: declaration.0.clone().unwrap(),
                        info: declaration.1.clone(),
                    }));
                    instructions.push(Box::new(GetVariable::<1> {
                        name: declaration.0.clone().unwrap(),
                    }));

                    instructions.push(Box::new(Assign::<1, 0> {}));
                } else {
                    // variable with default value
                    instructions.push(Box::new(crate::program::instruction::NewVariable {
                        name: declaration.0.clone().unwrap(),
                        info: declaration.1.clone(),
                    }));
                }
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct FunctionDefinitionStatement {
    pub return_type: TypeInfo,
    pub name: String,
    pub params: Vec<(Option<String>, TypeInfo)>,
    pub body: Box<dyn Statement>,
}
impl Statement for FunctionDefinitionStatement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let function_data = FunctionData {
            return_type: self.return_type.clone(),
            params: self.params.clone(),
        };
        let old = program.functions.insert(self.name.clone(), function_data);
        if let Some(_) = old {
            panic!("redeclaration of function {}", &self.name);
        } else {
            println!("Function Definition: {}", &self.name);
        }

        program.set_label(self.name.clone(), instructions);
        // param declarations
        // should be done at function calling

        // body
        self.body.emit(program, instructions);

        // force add return statement
        let return_statement = ReturnStatement { expr: None };
        return_statement.emit(program, instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct TranslationUnit {
    pub statements: Vec<Box<dyn Statement>>,
}
impl Statement for TranslationUnit {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        for statement in &self.statements {
            statement.emit(program, instructions);
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
