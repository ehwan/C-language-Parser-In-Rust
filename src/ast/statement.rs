use super::expression::InitializerListExpression;
use super::{expression::Expression, typename::TypeInfo};
use crate::virtualmachine::instruction::binary::*;
use crate::virtualmachine::instruction::generation::FunctionInfo;
use crate::virtualmachine::instruction::generation::InstructionGenerator;
use crate::virtualmachine::instruction::operand::Operand;
use crate::virtualmachine::instruction::*;
use crate::virtualmachine::program::STACK_POINTER_BASE_REGISTER;
use crate::virtualmachine::program::STACK_POINTER_REGISTER;
use crate::virtualmachine::scope::FunctionScope;
use crate::virtualmachine::variable::VariableData;

use std::any::Any;

pub trait Statement: core::fmt::Debug + Any {
    fn emit(&self, instructions: &mut InstructionGenerator);
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug)]
pub struct NullStatement;
impl Statement for NullStatement {
    fn emit(&self, _instructions: &mut InstructionGenerator) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<dyn Expression>,
}
impl Statement for ExpressionStatement {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.expression.emit(instructions);
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.set_label(&self.label);
        self.statement.emit(instructions);
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push_scope();

        for statement in &self.statements {
            statement.emit(instructions);
        }

        instructions.pop_scope();
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let else_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        self.cond.emit(instructions);
        instructions.push(JumpZero {
            label: else_label.clone(),
            operand_cond: Operand::Register(0),
        });
        self.then_statement.emit(instructions);
        instructions.push(Jump {
            label: end_label.clone(),
        });
        instructions.set_label(&else_label);
        if let Some(else_statement) = &self.else_statement {
            else_statement.emit(instructions);
        }
        instructions.set_label(&end_label);
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let end_label = instructions.get_unique_label();
        let default_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((default_label.clone(), end_label.clone()));

        // push target to stack
        self.target.emit(instructions);
        if self.target.is_return_reference() {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }
        // and push variable for 'if the pattern matched already?'
        instructions.push(PushStack {
            operand: Operand::Value(VariableData::UInt8(0)),
        });

        // body
        self.statement.emit(instructions);
        // check if the pattern matched and default is defined
        if instructions.labels.get(&default_label).is_some() {
            // if not matched, set pattern matched to true and goto default
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -1),
                operand_to: Operand::Register(0),
            });
            instructions.push(JumpZero {
                label: default_label.clone(),
                operand_cond: Operand::Register(0),
            });
        }

        // end label here, cleanup
        instructions.set_label(&end_label);
        // pop pattern-matched state and target from stack
        instructions.push(SubAssign {
            lhs: Operand::Register(STACK_POINTER_REGISTER),
            rhs: Operand::Value(VariableData::UInt64(2)),
        });

        instructions
            .label_stack
            .pop()
            .expect("Switch: label_stack is empty");
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let case_end_label = instructions.get_unique_label();
        let comparison_skip_label = instructions.get_unique_label();

        // copy state from stack
        instructions.push(MoveRegister {
            operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -1),
            operand_to: Operand::Register(0),
        });
        // if the pattern matched already, skip comparison
        instructions.push(JumpNonZero {
            label: comparison_skip_label.clone(),
            operand_cond: Operand::Register(0),
        });

        // comparison start here
        // evaluate value
        self.value.emit(instructions);
        // register1 = value
        if self.value.is_return_reference() {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            });
        } else {
            instructions.push(MoveRegister {
                operand_from: Operand::Register(0),
                operand_to: Operand::Register(1),
            });
        }
        // register0 = target
        instructions.push(MoveRegister {
            operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -2),
            operand_to: Operand::Register(0),
        });
        // register2 = result of comparison
        instructions.push(Equal {
            lhs: Operand::Register(0),
            rhs: Operand::Register(1),
            to: Operand::Register(2),
        });
        instructions.push(JumpZero {
            label: case_end_label.clone(),
            operand_cond: Operand::Register(2),
        });

        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt8(1)),
            operand_to: Operand::Derefed(STACK_POINTER_REGISTER, -1),
        });

        instructions.set_label(&comparison_skip_label);
        self.statement.emit(instructions);

        instructions.set_label(&case_end_label);
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let default_end_label = instructions.get_unique_label();
        let (default_label, _) = instructions
            .label_stack
            .last()
            .expect("Default: label_stack is empty")
            .clone();
        // skip default statement
        instructions.push(Jump {
            label: default_end_label.clone(),
        });
        instructions.set_label(&default_label);
        // set pattern matched
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt8(1)),
            operand_to: Operand::Derefed(STACK_POINTER_REGISTER, -1),
        });
        self.statement.emit(instructions);
        instructions.set_label(&default_end_label);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ContinueStatement;
impl Statement for ContinueStatement {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let continue_label = &instructions
            .label_stack
            .last()
            .expect("Continue: label_stack is empty")
            .0;
        instructions.push(Jump {
            label: continue_label.clone(),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct BreakStatement;
impl Statement for BreakStatement {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let end_label = &instructions
            .label_stack
            .last()
            .expect("Break: label_stack is empty")
            .1;
        instructions.push(Jump {
            label: end_label.clone(),
        });
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let start_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((start_label.clone(), end_label.clone()));

        instructions.set_label(&start_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference() {
            instructions.push(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Register(0),
            });
        }
        self.statement.emit(instructions);
        instructions.push(Jump {
            label: start_label.clone(),
        });
        instructions.set_label(&end_label);

        instructions
            .label_stack
            .pop()
            .expect("While: label_stack is empty");
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let start_label = instructions.get_unique_label();
        let continue_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        instructions
            .label_stack
            .push((continue_label.clone(), end_label.clone()));

        // start_label:
        //    do { body ... }
        // continue_label:
        //    while (cond);
        // end_label:

        instructions.set_label(&start_label);
        self.statement.emit(instructions);

        instructions.set_label(&continue_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference() {
            instructions.push(JumpNonZero {
                label: start_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(JumpNonZero {
                label: start_label.clone(),
                operand_cond: Operand::Register(0),
            });
        }
        instructions.set_label(&end_label);

        instructions
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let cond_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();
        let continue_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((continue_label.clone(), end_label.clone()));

        // init
        //   COND:
        // cond
        // body
        //   CONTINUE:
        // next
        // jump COND
        //   END:

        self.init.emit(instructions);
        instructions.set_label(&cond_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference() {
            instructions.push(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Register(0),
            });
        }
        self.statement.emit(instructions);
        instructions.set_label(&continue_label);
        if let Some(next) = &self.next {
            next.emit(instructions);
        }
        instructions.push(Jump {
            label: cond_label.clone(),
        });
        instructions.set_label(&end_label);

        instructions
            .label_stack
            .pop()
            .expect("For: label_stack is empty");
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Jump {
            label: self.label.clone(),
        });
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        if let Some(expr) = &self.expr {
            expr.emit(instructions);
            // force return as value
            if expr.is_return_reference() {
                instructions.push(MoveRegister {
                    operand_from: Operand::Derefed(0, 0),
                    operand_to: Operand::Register(0),
                });
            }
        }
        instructions.push(Return {});
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        for declaration in &self.vars {
            if declaration.0.is_none() && declaration.2.is_none() {
                // Struct & Enum & Union declaration
                panic!("Struct & Enum & Union declaration is not allowed in declaration statement");
                // match &declaration.1 {
                // TypeInfo::Struct(t) => {
                //     if t.name.is_none() {
                //         println!("Anonymous struct in declaration statement; ignored it");
                //     } else {
                //         let old = instructions.cur_scope_mut().type_infos.insert(
                //             t.name.as_ref().unwrap().clone(),
                //             TypeInfo::Struct(t.clone()),
                //         );
                //         if old.is_some() {
                //             panic!("Struct {} already exists", t.name.as_ref().unwrap());
                //         }
                //     }
                // }
                // TypeInfo::Union(t) => {
                //     if t.name.is_none() {
                //         println!("Anonymous union in declaration statement; ignored it");
                //     } else {
                //         let old = instructions.cur_scope_mut().type_infos.insert(
                //             t.name.as_ref().unwrap().clone(),
                //             TypeInfo::Union(t.clone()),
                //         );
                //         if old.is_some() {
                //             panic!("Union {} already exists", t.name.as_ref().unwrap());
                //         }
                //     }
                // }
                // TypeInfo::Enum(t) => {
                //     if t.name.is_none() {
                //         println!("Anonymous enum in declaration statement; ignored it");
                //     } else {
                //         let old = instructions.cur_scope_mut().type_infos.insert(
                //             t.name.as_ref().unwrap().clone(),
                //             TypeInfo::Enum(t.clone()),
                //         );
                //         if old.is_some() {
                //             panic!("Enum {} already exists", t.name.as_ref().unwrap());
                //         }
                //     }
                // }
                // _ => panic!("Invalid type for anonymous declaration"),
                // }
            } else if declaration.0.is_none() {
                panic!("Anonymous variable is not allowed in declaration statement");
            } else {
                // variable declaration

                if let Some(initial_value) = &declaration.2 {
                    // variable with initial value

                    // check type
                    match &declaration.1 {
                        TypeInfo::Function(_, _) => {
                            panic!( "Function declaration cannot have initial value; something went wrong");
                        }
                        TypeInfo::Struct(_sinfo) => {
                            panic!(
                                "Struct declaration in declaration statement is not implemented"
                            );
                        }
                        TypeInfo::Union(_uinfo) => {
                            panic!("Union declaration in declaration statement is not implemented");
                        }

                        // array
                        TypeInfo::Array(type_, size) => {
                            // initializer must be initializer list
                            let initial_value = initial_value
                                .as_any()
                                .downcast_ref::<InitializerListExpression>()
                                .expect("Array initializer must be initializer list");

                            let size = match size {
                                Some(size) => {
                                    if initial_value.initializers.len() > *size {
                                        panic!("Too many initializers for array");
                                    }
                                    *size
                                }
                                None => initial_value.initializers.len(),
                            };
                            if size == 0 {
                                panic!("Array size must be greater than 0");
                            }
                            let init_with_default = size - initial_value.initializers.len();

                            // link name to stack
                            instructions.declare_variable(
                                declaration.0.as_ref().unwrap(),
                                &TypeInfo::Array(type_.clone(), Some(size)),
                                size,
                            );

                            // init with initializer
                            for initializer in initial_value.initializers.iter() {
                                // register0 = initial value
                                initializer.emit(instructions);

                                // register1 = (type-casting) register0
                                if initializer.is_return_reference() {
                                    instructions.push(Assign {
                                        lhs_type: *type_.clone(),
                                        lhs: Operand::Register(1),
                                        rhs: Operand::Derefed(0, 0),
                                    });
                                } else {
                                    instructions.push(Assign {
                                        lhs_type: *type_.clone(),
                                        lhs: Operand::Register(1),
                                        rhs: Operand::Register(0),
                                    });
                                }
                                // push to stack
                                instructions.push(PushStack {
                                    operand: Operand::Register(1),
                                });
                            }
                            for _ in 0..init_with_default {
                                // push to stack
                                instructions.push(PushStack {
                                    operand: Operand::Value(VariableData::init_default(type_)),
                                });
                            }
                        }

                        // primitive types + pointer
                        TypeInfo::UInt8
                        | TypeInfo::UInt16
                        | TypeInfo::UInt32
                        | TypeInfo::UInt64
                        | TypeInfo::Int8
                        | TypeInfo::Int16
                        | TypeInfo::Int32
                        | TypeInfo::Int64
                        | TypeInfo::Float32
                        | TypeInfo::Float64
                        | TypeInfo::Pointer(_) => {
                            // link name to stack
                            instructions.declare_variable(
                                declaration.0.as_ref().unwrap(),
                                &declaration.1,
                                1,
                            );

                            // register0 = initial value
                            initial_value.emit(instructions);

                            // register1 = (type-casting) register0
                            if initial_value.is_return_reference() {
                                instructions.push(Assign {
                                    lhs_type: declaration.1.clone(),
                                    lhs: Operand::Register(1),
                                    rhs: Operand::Derefed(0, 0),
                                });
                            } else {
                                instructions.push(Assign {
                                    lhs_type: declaration.1.clone(),
                                    lhs: Operand::Register(1),
                                    rhs: Operand::Register(0),
                                });
                            }
                            // push register1 to stack
                            instructions.push(PushStack {
                                operand: Operand::Register(1),
                            });
                        }
                        _ => panic!("Invalid type for variable declaration"),
                    }
                } else {
                    // variable without initial value

                    match &declaration.1 {
                        TypeInfo::Function(return_type, params) => {
                            // check if its already declared
                            let old = instructions.functions.get(declaration.0.as_ref().unwrap());
                            if let Some(old) = old {
                                // function is declared

                                // check parameter types are same
                                let param_equal =
                                    old.params.iter().map(|(_, type_)| type_).eq(params.iter());
                                if param_equal == false {
                                    panic!(
                                        "Function {} is already declared with different parameter types",
                                        declaration.0.as_ref().unwrap()
                                    );
                                }

                                // check return type is same
                                if &old.return_type != return_type.as_ref() {
                                    panic!(
                                        "Function {} is already declared with different return type",
                                        declaration.0.as_ref().unwrap()
                                    );
                                }
                            } else {
                                // function is not declared
                                let params: Vec<_> = params
                                    .iter()
                                    .map(|typeinfo| (None, typeinfo.clone()))
                                    .collect();
                                let function_data = FunctionInfo {
                                    return_type: *return_type.clone(),
                                    params,
                                    is_defined: false,
                                };
                                instructions
                                    .functions
                                    .insert(declaration.0.as_ref().unwrap().clone(), function_data);
                            }
                        }
                        TypeInfo::Struct(_sinfo) => {
                            panic!(
                                "Struct declaration in declaration statement is not implemented"
                            );
                        }
                        TypeInfo::Union(_uinfo) => {
                            panic!("Union declaration in declaration statement is not implemented");
                        }

                        TypeInfo::Array(type_, size) => {
                            let size =
                                size.expect("Array declaration without initializer must have size");
                            if size == 0 {
                                panic!("Array size must be greater than 0");
                            }

                            // link name to stack
                            instructions.declare_variable(
                                declaration.0.as_ref().unwrap(),
                                &TypeInfo::Array(type_.clone(), Some(size)),
                                size,
                            );

                            for _ in 0..size {
                                // push to stack
                                instructions.push(PushStack {
                                    operand: Operand::Value(VariableData::init_default(type_)),
                                });
                            }
                        }

                        // primitive types + pointer
                        TypeInfo::UInt8
                        | TypeInfo::UInt16
                        | TypeInfo::UInt32
                        | TypeInfo::UInt64
                        | TypeInfo::Int8
                        | TypeInfo::Int16
                        | TypeInfo::Int32
                        | TypeInfo::Int64
                        | TypeInfo::Float32
                        | TypeInfo::Float64
                        | TypeInfo::Pointer(_) => {
                            // link name to stack
                            instructions.declare_variable(
                                declaration.0.as_ref().unwrap(),
                                &declaration.1,
                                1,
                            );

                            // push default value to stack
                            instructions.push(PushStack {
                                operand: Operand::Value(VariableData::init_default(&declaration.1)),
                            });
                        }
                        _ => {}
                    }
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let address = instructions.instructions.len();

        if instructions.function_scope.is_some() {
            panic!("nested function is not allowed");
        }

        let function_data = FunctionInfo {
            return_type: self.return_type.clone(),
            params: self.params.clone(),
            is_defined: true,
        };
        let old = instructions
            .functions
            .insert(self.name.clone(), function_data);
        if let Some(old) = old {
            // check if old was declaration
            if old.is_defined {
                panic!("redefinition of function {}", &self.name);
            }
        }
        instructions.set_label(&self.name);

        instructions.function_scope = Some(FunctionScope::new());
        instructions.push_scope();

        // argument initialization
        // function's arguments are pushed to stack before call ( and MUST BE )
        // ===== top of stack: return_address -> arg1 -> arg2 ... =====
        //                           ^ top of stack

        for (id, param) in self.params.iter().enumerate() {
            if param.0.is_some() {
                instructions.link_variable(param.0.as_ref().unwrap(), &param.1, -(id as isize) - 3);
            }
        }

        // push base pointer
        instructions.push(PushStack {
            operand: Operand::Register(STACK_POINTER_BASE_REGISTER),
        });
        // move base
        // rbp = rsp
        instructions.push(MoveRegister {
            operand_from: Operand::Register(STACK_POINTER_REGISTER),
            operand_to: Operand::Register(STACK_POINTER_BASE_REGISTER),
        });
        // here, [rbp-1] is old base pointer
        // here, [rbp-2] is return address

        // body
        self.body.emit(instructions);

        // end of body
        // if return type is void, add return statement
        // else, add panic statement for missing return statement
        if self.return_type == TypeInfo::Void {
            // force add return statement
            let return_statement = ReturnStatement { expr: None };
            return_statement.emit(instructions);
        } else {
            // panic
            instructions.push(Panic {
                message: format!(
                    "Function {} must return a {:?} value",
                    &self.name, &self.return_type
                ),
            });
        }

        instructions.pop_scope();
        instructions.function_scope = None;
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        for statement in &self.statements {
            statement.emit(instructions);
        }

        // find main function
        let main = instructions
            .functions
            .get("main")
            .expect("main function not found");

        let startaddress = instructions.instructions.len();
        instructions.start_address = startaddress;
        instructions.push(Call {
            label: "main".to_string(),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}
