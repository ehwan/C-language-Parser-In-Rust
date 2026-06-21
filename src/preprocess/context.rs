use crate::token::Token;
use std::collections::HashMap;
use std::vec::Vec;

pub enum MacroDefinition {
    ObjectLike {
        replacement_list: Vec<Token>,
    },
    FunctionLike {
        arity: usize,
        replacement_list: Vec<Token>,
    },
}

pub struct ConditionalInclusionFrame {
    pub enclosing_region_active: bool,
    pub prior_group_selected: bool,
    pub current_region_active: bool,
}

pub struct PreprocessorContext {
    pub macro_environment: HashMap<String, MacroDefinition>,

    pub conditional_inclusion_stack: Vec<ConditionalInclusionFrame>,
}
impl PreprocessorContext {
    pub fn new() -> Self {
        Self {
            macro_environment: HashMap::new(),
            conditional_inclusion_stack: Vec::new(),
        }
    }

    pub fn current_region_is_active(&self) -> bool {
        if self.conditional_inclusion_stack.is_empty() {
            true
        } else {
            self.conditional_inclusion_stack
                .last()
                .unwrap()
                .current_region_active
        }
    }

    pub fn enter_conditional_inclusion(&mut self, condition_holds: bool) {
        let enclosing_region_active = self.current_region_is_active();
        let selected_group = enclosing_region_active && condition_holds;
        self.conditional_inclusion_stack
            .push(ConditionalInclusionFrame {
                enclosing_region_active,
                prior_group_selected: selected_group,
                current_region_active: selected_group,
            });
    }

    pub fn enter_elif_group(&mut self, condition_holds: bool) {
        let frame = self
            .conditional_inclusion_stack
            .last_mut()
            .expect("#elif without #if");

        let selected_group =
            frame.enclosing_region_active && !frame.prior_group_selected && condition_holds;
        frame.current_region_active = selected_group;
        frame.prior_group_selected |= selected_group;
    }

    pub fn enter_else_group(&mut self) {
        let frame = self
            .conditional_inclusion_stack
            .last_mut()
            .expect("#else without #if");

        let selected_group = frame.enclosing_region_active && !frame.prior_group_selected;
        frame.current_region_active = selected_group;
        frame.prior_group_selected = true;
    }

    pub fn exit_conditional_inclusion(&mut self) {
        self.conditional_inclusion_stack
            .pop()
            .expect("#endif without #if");
    }
}
