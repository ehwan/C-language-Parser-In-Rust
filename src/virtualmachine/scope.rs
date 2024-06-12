use crate::ast::typename::TypeInfo;
use std::collections::HashMap;

/// for variable scope
#[derive(Default, Debug)]
pub struct Scope {
    // for typedef & declaration
    pub type_infos: HashMap<String, TypeInfo>,

    // for variable declaration; (type, offset from rbp)
    pub variables: HashMap<String, (TypeInfo, isize)>,

    pub declared_variable_count: usize,
}
impl Scope {
    pub fn new() -> Self {
        Self {
            type_infos: HashMap::new(),
            variables: HashMap::new(),
            declared_variable_count: 0,
        }
    }
}

/// scope for function
/// this is for variable counting; allocating stack
/// so need to need normal scopr for variable declaration
#[derive(Debug, Default)]
pub struct FunctionScope {
    pub declared_variable_count: usize,
    pub max_variable_count: usize,
}
impl FunctionScope {
    pub fn new() -> Self {
        Self {
            declared_variable_count: 0,
            max_variable_count: 0,
        }
    }
}
