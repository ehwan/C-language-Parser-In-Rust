use crate::ast::ast::TypeSpecifier;

use super::instruction::Instruction;
use super::variable::Variable;

use std::boxed::Box;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    pub variables: HashMap<String, usize>,
}
impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
        }
    }
}

pub struct Program {
    pub functions: HashMap<String, usize>,
    pub instructions: Vec<Box<dyn Instruction>>,
    pub scopes: Vec<Scope>,
    pub stack: Vec<Variable>,
}

impl Program {
    pub fn new() -> Program {
        let mut ret = Program {
            functions: HashMap::new(),
            instructions: Vec::new(),
            scopes: Vec::new(),
            stack: Vec::new(),
        };
        ret.scopes.push(Scope::new()); // default global scope
        ret
    }

    pub fn get_variable(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_ptr) = scope.variables.get(name) {
                return Some(*var_ptr);
            }
        }
        None
    }
    pub fn new_scope(&mut self) {
        self.scopes.push(Scope::new());
    }
}
