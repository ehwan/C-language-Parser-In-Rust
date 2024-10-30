use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::VariablePool;
use super::{CVType, FunctionType, LabelInfo, VariableInfo};

#[derive(Debug, Clone)]
pub enum Scope {
    Switch(SwitchScope),
    Loop(LoopScope),
    Block(BlockScope),
    Variable(VariableScope),
}

/// for switch statement, `break` and `default`
#[derive(Debug, Clone)]
pub struct SwitchScope {
    pub id: usize,
    // is `default` defined
    pub default: bool,
}

/// for any loop statement, `break` and `continue`
#[derive(Debug, Clone)]
pub struct LoopScope {
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub id: usize,

    // `typedef`s and `struct`, `union`, `enum` definitions
    pub typedefs: HashMap<String, CVType>,
}

#[derive(Debug, Clone)]
pub struct VariableScope {
    pub name: String,
    pub info: VariableInfo,
}

#[derive(Clone)]
pub struct FunctionScope {
    pub name: String,
    pub type_: FunctionType,
    pub labels: HashMap<String, Rc<RefCell<LabelInfo>>>,
    pub pool: VariablePool,
}
impl FunctionScope {
    pub fn new(name: String, type_: FunctionType) -> Self {
        FunctionScope {
            name,
            type_,
            labels: HashMap::new(),
            pool: VariablePool::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub body: Box<super::Statement>,
    pub stack_size: usize,
}
#[derive(Debug, Clone)]
pub struct GlobalScope {
    pub variables: HashMap<String, VariableInfo>,
    pub pool: VariablePool,
    pub functions: Vec<Rc<RefCell<Option<FunctionDefinition>>>>,
    // `typedef`s and `struct`, `union`, `enum` definitions
    pub typedefs: HashMap<String, CVType>,
}
impl GlobalScope {
    pub fn new() -> Self {
        GlobalScope {
            variables: HashMap::new(),
            pool: VariablePool::new(),
            functions: Vec::new(),
            typedefs: HashMap::new(),
        }
    }
}
