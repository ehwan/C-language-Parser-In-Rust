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
        if ctx.current_region_is_active() == false {
            return Vec::new();
        }
        let old = ctx.macro_environment.insert(
            self.name.clone(),
            MacroDefinition::ObjectLike {
                replacement_list: self.replacement.clone(),
            },
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
        if ctx.current_region_is_active() == false {
            return Vec::new();
        }

        let old = ctx.macro_environment.insert(
            self.name.clone(),
            MacroDefinition::FunctionLike {
                arity: self.param_count,
                replacement_list: self.replacement.clone(),
            },
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
        let condition_holds =
            ctx.current_region_is_active() && ctx.macro_environment.contains_key(&self.name);
        ctx.enter_conditional_inclusion(condition_holds);
        Vec::new()
    }
}
#[derive(Debug)]
pub struct IfNDef {
    pub name: String,
}
impl PreprocessedTokenLine for IfNDef {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        let condition_holds =
            ctx.current_region_is_active() && !ctx.macro_environment.contains_key(&self.name);
        ctx.enter_conditional_inclusion(condition_holds);
        Vec::new()
    }
}
#[derive(Debug)]
pub struct Else {}
impl PreprocessedTokenLine for Else {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        ctx.enter_else_group();
        Vec::new()
    }
}
#[derive(Debug)]
pub struct EndIf {}
impl PreprocessedTokenLine for EndIf {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        ctx.exit_conditional_inclusion();
        Vec::new()
    }
}

#[derive(Debug)]
pub struct If {
    pub expression_tokens: Vec<Token>,
}
impl PreprocessedTokenLine for If {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.current_region_is_active() == false {
            ctx.enter_conditional_inclusion(false);
            return Vec::new();
        }

        let tokens = parser.replace_recursive(&self.expression_tokens, ctx);
        let expression = parser.parse_expression(&tokens);

        let val = expression.eval(ctx);
        ctx.enter_conditional_inclusion(val != 0);
        Vec::new()
    }
}
#[derive(Debug)]
pub struct ElIf {
    pub expression_tokens: Vec<Token>,
}
impl PreprocessedTokenLine for ElIf {
    fn emit(&self, ctx: &mut PreprocessorContext, parser: &PreprocessorParser) -> Vec<Token> {
        let should_evaluate = ctx
            .conditional_inclusion_stack
            .last()
            .map(|frame| frame.enclosing_region_active && !frame.prior_group_selected)
            .expect("#elif without #if");

        if should_evaluate == false {
            ctx.enter_elif_group(false);
            return Vec::new();
        }

        let tokens = parser.replace_recursive(&self.expression_tokens, ctx);
        let expression = parser.parse_expression(&tokens);

        let val = expression.eval(ctx);
        ctx.enter_elif_group(val != 0);

        Vec::new()
    }
}

#[derive(Debug)]
pub struct Undef {
    pub name: String,
}
impl PreprocessedTokenLine for Undef {
    fn emit(&self, ctx: &mut PreprocessorContext, _parser: &PreprocessorParser) -> Vec<Token> {
        if ctx.current_region_is_active() == false {
            return Vec::new();
        }

        let old = ctx.macro_environment.remove(&self.name);
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
        if ctx.current_region_is_active() == false {
            return Vec::new();
        }

        // replace macro to real tokens here
        parser.replace_recursive(&self.tokens, ctx)
    }

    fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}
