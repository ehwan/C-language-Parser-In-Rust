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

/// return type for search_variable by name
#[derive(Debug, Clone, Copy)]
pub enum VariableOffset {
    Global(usize), // absolute address in stack
    Local(isize),  // relative address from rbp
}

#[derive(Debug)]
pub struct InstructionGenerator {
    /// generated instructions
    pub instructions: Vec<Box<dyn Instruction>>,

    /// function map
    pub functions: HashMap<String, FunctionInfo>,
    /// label map
    pub labels: HashMap<String, usize>,
    /// for label, anonymous name generation
    pub unique_id: usize,
    /// label stack for continue, break
    ///                  default, break for switch statement
    pub label_stack: Vec<(String, String)>,

    /// global variables must have absolute address in stack
    /// local variables have relative address from rbp
    pub global_scope: Scope,

    /// function may have multiple scopes inside.
    /// we have to count the number of variable declaration
    /// for stack allocation
    pub function_scope: Option<FunctionScope>,
    /// local variable scopes
    pub scopes: Vec<Scope>,

    /// start address of program
    pub start_address: usize,

    /// where constant data is stored
    /// for same-address system with stack data, it will be merged into stack upon program execution
    ///
    /// commonly, this is used for read-only data
    /// but we don't have `const` qualifier yet
    /// so it is mutable ...
    pub text_section: Vec<u8>,
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
            text_section: Vec::new(),
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

    /// make new variable scopes
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }
    /// pop variable scopes
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("pop_scope: no scope");
    }

    /// make new named variable on current scope
    /// this does not allocate stack memory, just for variable counting
    pub fn declare_variable(&mut self, name: &str, type_info: &TypeInfo, count: usize) {
        let type_info = self.get_true_typeinfo(type_info);
        // if there is function scope, it is local variable
        let (offset, scope) = if let Some(func_scope) = &mut self.function_scope {
            let offset = func_scope.declared_variable_count;
            func_scope.declared_variable_count += count;

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
            .insert(name.to_string(), (type_info, offset as isize));
        if let Some(_) = old {
            panic!("variable {} is already declared", name);
        }
        scope.declared_variable_count += count;
    }
    /// if variable's data is already in stack (e.g. function arguments)
    /// this deos not count up variable_count
    /// only for stack address-variable mapping
    pub fn link_variable(&mut self, name: &str, type_info: &TypeInfo, offset_from_rbp: isize) {
        let type_info = self.get_true_typeinfo(type_info);
        let scope = self.scopes.last_mut().unwrap();
        let old = scope
            .variables
            .insert(name.to_string(), (type_info, offset_from_rbp));
        if let Some(_) = old {
            panic!("variable {} is already declared", name);
        }
    }
    /// search variable by name across scopes
    pub fn search_variable(&self, name: &str) -> Option<(TypeInfo, VariableOffset)> {
        for scope in self.scopes.iter().rev() {
            if let Some((type_info, offset)) = scope.variables.get(name) {
                return Some((
                    self.get_true_typeinfo(type_info),
                    VariableOffset::Local(*offset as isize),
                ));
            }
        }
        if let Some((type_info, offset)) = self.global_scope.variables.get(name) {
            return Some((
                self.get_true_typeinfo(type_info),
                VariableOffset::Global(*offset as usize),
            ));
        }
        None
    }
    /// search typeinfo by name across scopes
    pub fn search_type(&self, name: &str) -> Option<TypeInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(type_info) = scope.type_infos.get(name) {
                return Some(self.get_true_typeinfo(type_info));
            }
        }
        if let Some(type_info) = self.global_scope.type_infos.get(name) {
            return Some(self.get_true_typeinfo(type_info));
        }
        None
    }
    /// for struct type (and typedef-ed type in future)
    /// there may be no field information in typeinfo ( `struct MyStruct a;` )
    /// so we need to find the definition of struct
    pub fn get_true_typeinfo(&self, type_info: &TypeInfo) -> TypeInfo {
        match type_info {
            TypeInfo::Void
            | TypeInfo::UInt8
            | TypeInfo::UInt16
            | TypeInfo::UInt32
            | TypeInfo::UInt64
            | TypeInfo::Int8
            | TypeInfo::Int16
            | TypeInfo::Int32
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_) => type_info.clone(),
            TypeInfo::Array(t, len) => TypeInfo::Array(Box::new(self.get_true_typeinfo(t)), *len),
            TypeInfo::Const(t) => TypeInfo::Const(Box::new(self.get_true_typeinfo(t))),
            TypeInfo::Function(return_type, params) => {
                let mut new_params = Vec::new();
                for t in params.iter() {
                    new_params.push(self.get_true_typeinfo(t));
                }
                TypeInfo::Function(Box::new(self.get_true_typeinfo(return_type)), new_params)
            }

            TypeInfo::Struct(sinfo) => {
                if sinfo.fields.is_some() {
                    return type_info.clone();
                }
                if sinfo.name.is_none() {
                    panic!("get_true_typeinfo: anonymous struct");
                }

                let searched = self.search_type(sinfo.name.as_ref().unwrap()).expect(
                    format!(
                        "get_true_typeinfo: struct {} is not defined",
                        sinfo.name.as_ref().unwrap()
                    )
                    .as_str(),
                );
                if let TypeInfo::Struct(sinfo) = searched {
                    return TypeInfo::Struct(sinfo.clone());
                } else {
                    panic!(
                        "get_true_typeinfo: {} is not struct",
                        sinfo.name.as_ref().unwrap()
                    );
                }
            }
            TypeInfo::Identifier(name) => self
                .search_type(name)
                .expect(format!("get_true_typeinfo: type {} is not defined", name).as_str()),
            _ => panic!("get_true_typeinfo: not implemented {:?}", type_info),
        }
    }

    /// link label name to current instruction address
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
