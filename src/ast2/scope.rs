use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{FunctionType, LabelInfo};

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
}

#[derive(Debug, Clone)]
pub struct VariableScope {
    pub name: String,
}

#[derive(Clone)]
pub struct FunctionScope {
    pub name: String,
    pub type_: FunctionType,
    pub stack_size: usize,
    pub labels: HashMap<String, Rc<RefCell<LabelInfo>>>,
}
impl FunctionScope {
    pub fn new(name: String, type_: FunctionType) -> Self {
        FunctionScope {
            name,
            type_,
            stack_size: 0,
            labels: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalScope {
    pub stack_size: usize,
}
impl GlobalScope {
    pub fn new() -> Self {
        GlobalScope { stack_size: 0 }
    }
}
