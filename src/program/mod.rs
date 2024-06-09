pub mod function;
pub mod instruction;
pub mod variable;

use instruction::Instruction;

use std::boxed::Box;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::typename::{StructInfo, TypeInfo};

#[derive(Debug, Clone)]
pub enum VariableData {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    Struct(StructData),
    Union(UnionData),
    Enum(EnumData),
    Pointer(Rc<VariableData>),
}

#[derive(Debug, Clone)]
pub struct StructData {
    pub fields: HashMap<String, VariableData>,
}
#[derive(Debug, Clone)]
pub struct UnionData {
    pub fields: HashMap<String, VariableData>,
}

#[derive(Debug, Clone)]
pub struct EnumData {
    pub value: i64,
}

#[derive(Debug, Clone, Default)]
pub struct Scope {
    pub type_infos: HashMap<String, TypeInfo>,
    pub variables: HashMap<String, usize>, // (name, index in stack)
    pub stack: Vec<Rc<(TypeInfo, VariableData)>>,
}

pub struct Program {
    pub functions: HashMap<String, usize>,
    pub instructions: Vec<Box<dyn Instruction>>,
    pub scopes: Vec<Scope>,

    // for struct/union/enum declaration
    string_stack: String,
}

impl Program {
    pub fn new() -> Program {
        let mut ret = Program {
            functions: HashMap::new(),
            instructions: Vec::new(),
            scopes: Vec::new(),

            string_stack: String::new(),
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

    pub fn get_variable<'a>(&self, name: &'a str) -> Option<Rc<(TypeInfo, VariableData)>> {
        for scope in self.scopes.iter().rev() {
            if let Some(index) = scope.variables.get(name) {
                return Some(Rc::clone(&scope.stack[*index]));
            }
        }
        None
    }
    pub fn get_type_info<'a>(&self, name: &'a str) -> Option<TypeInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.type_infos.get(name) {
                return Some(info.clone());
            }
        }
        None
    }
}
