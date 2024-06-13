use super::DefineLabel;
use super::Instruction;
use crate::ast::typename::TypeInfo;
use crate::virtualmachine::scope::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub return_type: TypeInfo,
    pub params: Vec<(Option<String>, TypeInfo)>,
    pub is_defined: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum VariableOffset {
    Global(usize), // absolute address in stack
    Local(isize),  // relative address from rbp
}

#[derive(Debug)]
pub struct InstructionGenerator {
    pub instructions: Vec<Box<dyn Instruction>>,

    /// function map
    pub functions: HashMap<String, FunctionInfo>,
    /// label map
    pub labels: HashMap<String, usize>,
    /// label, anonymous name generation
    pub unique_id: usize,
    /// label stack for continue, break
    ///                  default, break for switch statement
    pub label_stack: Vec<(String, String)>,

    /// global variables must have absolute address in stack
    /// local variables have relative address from rbp
    pub global_scope: Scope,

    /// function can have multiple scopes;
    /// we have to count the number of variable declaration
    /// for stack allocation
    pub function_scope: Option<FunctionScope>,
    /// variable scopes
    pub scopes: Vec<Scope>,

    /// start address of program
    pub start_address: usize,
}
impl InstructionGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            functions: HashMap::new(),
            labels: HashMap::new(),
            unique_id: 0,
            start_address: 0,
            scopes: Vec::new(),
            function_scope: None,
            global_scope: Scope::new(),
            label_stack: Vec::new(),
        }
    }
    /// push new instruction
    pub fn push<I: Instruction + 'static>(&mut self, instruction: I) {
        self.instructions.push(Box::new(instruction));
    }

    /// for unique label generation
    pub fn get_unique_id(&mut self) -> usize {
        let ret = self.unique_id;
        self.unique_id += 1;
        ret
    }
    /// for unique label generation
    pub fn get_unique_label(&mut self) -> String {
        format!(".__L{}__", self.get_unique_id())
    }

    /// variable scopes
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    pub fn pop_scope(&mut self) {
        let scope = self.scopes.pop().expect("pop_scope: no scope");
        self.function_scope
            .as_mut()
            .expect("pop_scope: no function scope")
            .declared_variable_count -= scope.declared_variable_count;
    }
    /// make new named variable on current scope
    /// this allocates stack space
    pub fn declare_variable(&mut self, name: &str, type_info: &TypeInfo) {
        let (offset, scope) = if let Some(func_scope) = &mut self.function_scope {
            let offset = func_scope.declared_variable_count;
            func_scope.declared_variable_count += 1;

            func_scope.max_variable_count = func_scope
                .max_variable_count
                .max(func_scope.declared_variable_count);

            (offset, self.scopes.last_mut().unwrap())
        } else {
            // if there is no function scope, it is global variable
            let len = self.global_scope.variables.len();
            (len, &mut self.global_scope)
        };
        let old = scope
            .variables
            .insert(name.to_string(), (type_info.clone(), offset as isize));
        if let Some(_) = old {
            panic!("variable {} is already declared", name);
        }
        scope.declared_variable_count += 1;
    }
    // if variable's data is already in stack (e.g. function arguments)
    pub fn link_variable(&mut self, name: &str, type_info: &TypeInfo, offset: isize) {
        let scope = self.scopes.last_mut().unwrap();
        let old = scope
            .variables
            .insert(name.to_string(), (type_info.clone(), offset));
        if let Some(_) = old {
            panic!("variable {} is already declared", name);
        }
    }
    pub fn search_variable(&self, name: &str) -> Option<(&TypeInfo, VariableOffset)> {
        for scope in self.scopes.iter().rev() {
            if let Some((type_info, offset)) = scope.variables.get(name) {
                return Some((type_info, VariableOffset::Local(*offset as isize)));
            }
        }
        if let Some((type_info, offset)) = self.global_scope.variables.get(name) {
            return Some((type_info, VariableOffset::Global(*offset as usize)));
        }
        None
    }

    // link label name to current instruction address
    pub fn set_label(&mut self, label: &str) {
        let old = self
            .labels
            .insert(label.to_string(), self.instructions.len());
        if let Some(_) = old {
            panic!("label {} is already defined", label);
        }
        self.push(DefineLabel {
            label: label.to_string(),
        });
    }
}
