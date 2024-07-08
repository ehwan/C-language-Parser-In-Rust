use rusty_parser as rp;

use std::fmt::Debug;
use std::vec::Vec;

use crate::token::Token;

use super::context::*;
use super::parser::PreprocessorParser;

pub trait PreprocessedTokenLine: Debug {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token>;

    fn is_empty(&self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct Define {
    pub name: String,
    pub replacement: Vec<Token>,
}
impl PreprocessedTokenLine for Define {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }
        let old = ctx.define_map.insert(
            self.name.clone(),
            MacroData::DirectReplace(self.replacement.clone()),
        );
        if old.is_some() {
            panic!("Macro {} is already defined", self.name);
        }
        Vec::new()
    }
}

#[derive(Debug)]
pub struct DefineFunction {
    pub name: String,
    pub param_count: usize,
    pub replacement: Vec<Token>,
}
impl PreprocessedTokenLine for DefineFunction {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }

        let old = ctx.define_map.insert(
            self.name.clone(),
            MacroData::Function(self.param_count, self.replacement.clone()),
        );
        if old.is_some() {
            panic!("Macro {} is already defined", self.name);
        }
        Vec::new()
    }
}

#[derive(Debug)]
pub struct IfDef {
    pub name: String,
}
impl PreprocessedTokenLine for IfDef {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }

        if ctx.define_map.contains_key(&self.name) {
            ctx.if_stack.push(IfStackData {
                processed: true,
                current: true,
            });
        } else {
            ctx.if_stack.push(IfStackData {
                processed: false,
                current: false,
            });
        }
        Vec::new()
    }
}
#[derive(Debug)]
pub struct IfNDef {
    pub name: String,
}
impl PreprocessedTokenLine for IfNDef {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }

        if ctx.define_map.contains_key(&self.name) {
            ctx.if_stack.push(IfStackData {
                processed: false,
                current: false,
            });
        } else {
            ctx.if_stack.push(IfStackData {
                processed: true,
                current: true,
            });
        }
        Vec::new()
    }
}
#[derive(Debug)]
pub struct Else {}
impl PreprocessedTokenLine for Else {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        let branch_data = ctx.if_stack.last_mut().expect("#else without #if");
        if branch_data.current {
            branch_data.current = false;
        } else if branch_data.processed == false {
            branch_data.processed = true;
            branch_data.current = true;
        }
        Vec::new()
    }
}
#[derive(Debug)]
pub struct EndIf {}
impl PreprocessedTokenLine for EndIf {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        ctx.if_stack.pop().expect("#endif without #if");
        Vec::new()
    }
}

#[derive(Debug)]
pub struct If {
    pub expression_tokens: Vec<Token>,
}
impl PreprocessedTokenLine for If {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token> {
        let tokens = parser.replace_recursive(&self.expression_tokens, ctx);
        let expression = rp::parse(&parser.expression, tokens.iter().cloned())
            .output
            .expect("Failed to parse expression")
            .0;

        let val = expression.eval(ctx);
        if ctx.should_emit() == false {
            return Vec::new();
        }

        if val != 0 {
            ctx.if_stack.push(IfStackData {
                processed: true,
                current: true,
            });
        } else {
            ctx.if_stack.push(IfStackData {
                processed: false,
                current: false,
            });
        }
        Vec::new()
    }
}
#[derive(Debug)]
pub struct ElIf {
    pub expression_tokens: Vec<Token>,
}
impl PreprocessedTokenLine for ElIf {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token> {
        let tokens = parser.replace_recursive(&self.expression_tokens, ctx);
        let expression = rp::parse(&parser.expression, tokens.iter().cloned())
            .output
            .expect("Failed to parse expression")
            .0;

        let val = expression.eval(ctx);
        let branch_data = ctx.if_stack.last_mut().expect("#else without #if");

        if val == 0 {
            branch_data.current = false;
        } else {
            if branch_data.current {
                branch_data.current = false;
            } else if branch_data.processed == false {
                branch_data.processed = true;
                branch_data.current = true;
            }
        }

        Vec::new()
    }
}

#[derive(Debug)]
pub struct Undef {
    pub name: String,
}
impl PreprocessedTokenLine for Undef {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }

        let old = ctx.define_map.remove(&self.name);
        if old.is_none() {
            panic!("Macro {} is not defined", self.name);
        }
        Vec::new()
    }
}

#[derive(Debug)]
pub struct RawTokens {
    pub tokens: Vec<Token>,
}
impl PreprocessedTokenLine for RawTokens {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.should_emit() == false {
            return Vec::new();
        }

        // replace macro to real tokens here
        parser.replace_recursive(&self.tokens, ctx)
    }

    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}
