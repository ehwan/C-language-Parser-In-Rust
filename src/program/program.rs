use super::instruction::{Instruction, Null};
use super::variable::VariableData;

use std::collections::HashMap;
use std::rc::Rc;
use std::{boxed::Box, cell::RefCell};

use crate::ast::typename::TypeInfo;

pub struct FunctionData {
    pub return_type: TypeInfo,
    pub params: Vec<(Option<String>, TypeInfo)>,
}

#[derive(Default)]
pub struct Scope {
    pub type_infos: HashMap<String, Rc<TypeInfo>>,
    pub variables: HashMap<String, Rc<RefCell<(TypeInfo, VariableData)>>>,
}

pub struct Program {
    /// ======================= for instructions generation =======================
    /// function map
    pub functions: HashMap<String, FunctionData>,
    /// label map
    pub labels: HashMap<String, usize>,
    /// label, anonymous name generation
    pub unique_id: usize,

    /// ======================= for execute =======================
    pub scopes: Vec<Scope>,
    pub stack: Vec<Rc<RefCell<(TypeInfo, VariableData)>>>,
    pub registers: [Rc<RefCell<(TypeInfo, VariableData)>>; 4],

    pub current_instruction: usize,
}

impl Program {
    pub fn new() -> Program {
        let mut ret = Program {
            functions: HashMap::new(),
            scopes: Vec::new(),
            stack: Vec::new(),
            labels: HashMap::new(),

            registers: [
                Rc::new(RefCell::new((TypeInfo::Int32, VariableData::Int32(0)))),
                Rc::new(RefCell::new((TypeInfo::Int32, VariableData::Int32(0)))),
                Rc::new(RefCell::new((TypeInfo::Int32, VariableData::Int32(0)))),
                Rc::new(RefCell::new((TypeInfo::Int32, VariableData::Int32(0)))),
            ],

            current_instruction: 0,
            unique_id: 0,
        };
        ret.scopes.push(Default::default()); // default global scope
        ret
    }
    pub fn new_scope(&mut self) {
        self.scopes.push(Default::default());
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn cur_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("cur_scope_mut: scopes is empty")
    }
    pub fn cur_scope(&self) -> &Scope {
        self.scopes.last().expect("cur_scope: scopes is empty")
    }

    pub fn get_unique_id(&mut self) -> usize {
        let ret = self.unique_id;
        self.unique_id += 1;
        ret
    }
    pub fn get_unique_label(&mut self) -> String {
        format!(".__L{}__", self.get_unique_id())
    }

    pub fn search_variable<'a>(
        &self,
        name: &'a str,
    ) -> Option<Rc<RefCell<(TypeInfo, VariableData)>>> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Some(var.clone());
            }
        }
        None
    }

    pub fn set_label(&mut self, name: String, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.labels.insert(name, instructions.len());
        instructions.push(Box::new(Null {}));
    }
    pub fn search_typeinfo<'a>(&self, name: &'a str) -> Option<Rc<TypeInfo>> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.type_infos.get(name) {
                return Some(info.clone());
            }
        }
        None
    }
    pub fn push_to_stack(&mut self, var: Rc<RefCell<(TypeInfo, VariableData)>>) {
        self.stack.push(var);
    }
    pub fn pop_from_stack(&mut self) -> Rc<RefCell<(TypeInfo, VariableData)>> {
        self.stack.pop().expect("pop_from_stack: stack is empty")
    }

    pub fn execute(&mut self, instructions: &Vec<Box<dyn Instruction>>) {
        self.current_instruction = instructions.len();

        // push end of instructions as a return address
        let ret_addr = Rc::new(RefCell::new((
            TypeInfo::UInt64,
            VariableData::UInt64(instructions.len() as u64),
        )));
        self.push_to_stack(ret_addr);

        if let Some(main_address) = self.labels.get("main") {
            self.current_instruction = *main_address;
            while self.current_instruction < instructions.len() {
                instructions[self.current_instruction].execute(self);
                self.current_instruction += 1;
            }
        } else {
            panic!("main function not found");
        }
    }
}
