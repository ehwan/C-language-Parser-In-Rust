use crate::token::Token;
use std::collections::HashMap;
use std::vec::Vec;

pub enum MacroData {
    DirectReplace(Vec<Token>),
    Function(usize, Vec<Token>),
}

pub struct IfStackData {
    pub processed: bool, // if some branch of the #if-#elif chain has been processed
    pub current: bool,   // if current branch is active
}

pub struct PreprocessorContext {
    pub define_map: HashMap<String, MacroData>,

    pub if_stack: Vec<IfStackData>,
}
impl PreprocessorContext {
    pub fn new() -> Self {
        Self {
            define_map: HashMap::new(),
            if_stack: Vec::new(),
        }
    }

    // check if the current context should emit tokens;
    // returns false if it is false branch of an #if directive
    pub fn should_emit(&self) -> bool {
        if self.if_stack.is_empty() {
            true
        } else {
            self.if_stack.last().unwrap().current
        }
    }
}
