use super::identifier;
use super::trie;
use super::Token;

use rp::IntoParser;
use rusty_parser as rp;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

pub enum MacroData {
    DirectReplace(Vec<Token>),
    Function(usize, Vec<Token>),
}

pub struct MacroReplacer {
    pub macro_argument_item: Rc<RefCell<rp::DynBoxSlice<(Vec<Token>,), Token>>>,
    pub macro_argument_item_list: Rc<RefCell<rp::DynBoxSlice<(Vec<Vec<Token>>,), Token>>>,
}

impl MacroReplacer {
    pub fn new() -> Self {
        let macro_argument_item: Rc<RefCell<rp::DynBoxSlice<(Vec<Token>,), Token>>> =
            Default::default();
        let macro_argument_item_list: Rc<RefCell<rp::DynBoxSlice<(Vec<Vec<Token>>,), Token>>> =
            Default::default();

        let single_item = rp::any()
            .not(rp::or!(rp::one(Token::Comma), rp::one(Token::RightParen)))
            .repeat(0..);
        let parenthesized_list = rp::seq!(
            rp::one(Token::LeftParen).void(),
            macro_argument_item_list.clone(),
            rp::one(Token::RightParen).void()
        )
        .map(|mut args: Vec<Vec<Token>>| {
            let mut v = Vec::new();
            v.push(Token::LeftParen);
            for (idx, arg) in args.iter_mut().enumerate() {
                if idx > 0 {
                    v.push(Token::Comma);
                }
                v.append(arg);
            }
            v.push(Token::RightParen);
            v
        });
        // macro_argument_item.borrow_mut().assign(single_item);
        macro_argument_item
            .borrow_mut()
            .assign(rp::or!(parenthesized_list, single_item));

        let single_item_list = macro_argument_item
            .clone()
            .map(|item: Vec<Token>| -> Vec<Vec<Token>> { vec![item] })
            .reduce_left(
                rp::seq!(rp::one(Token::Comma).void(), macro_argument_item.clone()),
                |mut v: Vec<Vec<Token>>, item: Vec<Token>| {
                    v.push(item);
                    v
                },
            );
        macro_argument_item_list
            .borrow_mut()
            .assign(single_item_list);

        Self {
            macro_argument_item,
            macro_argument_item_list,
        }
    }

    /// replace macro in given tokens
    /// this is not recursive macro replacement
    /// returns true if there was any replacement occured
    pub fn replace(
        &self,
        define_map: &HashMap<String, MacroData>,
        tokens: &mut Vec<Token>,
    ) -> bool {
        let mut v = Vec::new();
        let mut it = tokens.iter().cloned();

        let mut replaced = false;
        while let Some(token) = it.next() {
            match token {
                Token::Identifier(name) => {
                    let macro_data = define_map.get(&name);
                    if let Some(MacroData::DirectReplace(replacement)) = macro_data {
                        // identifier links to normal macro
                        let mut replacement = replacement.clone();
                        v.append(&mut replacement);
                        replaced = true;
                    } else if let Some(MacroData::Function(param_count, replacement)) = macro_data {
                        // identifier links to function-like macro
                        // check arguments

                        let parser = rp::seq!(
                            rp::one(Token::LeftParen).void(),
                            self.macro_argument_item_list.clone(),
                            rp::one(Token::RightParen).void()
                        );
                        let args_res = rp::parse(&parser, it);
                        if let Some((args,)) = args_res.output {
                            if args.len() != *param_count {
                                panic!("Invalid number of arguments for macro {}", name);
                            }

                            for replacement_token in replacement.iter() {
                                if let Token::PreprocessorPlaceholder(arg_idx) = replacement_token {
                                    v.append(&mut args[*arg_idx].clone());
                                } else {
                                    v.push(replacement_token.clone());
                                }
                            }
                        } else {
                            panic!("Invalid arguments for macro {}", name);
                        }
                        it = args_res.it;
                        replaced = true;
                    } else {
                        v.push(Token::Identifier(name));
                    }
                }
                _ => {
                    v.push(token);
                }
            }
        }

        *tokens = v;
        replaced
    }
    /// replace macro in given tokens
    /// this is recursive macro replacement
    pub fn replace_recursive(
        &self,
        define_map: &HashMap<String, MacroData>,
        tokens: &mut Vec<Token>,
    ) {
        while self.replace(define_map, tokens) {}
    }
}

/// preprocessing phase
/// this will process #define, #ifdef, #ifndef, #else, #endif
/// keywords are still kept as Token::Identifier
pub fn preprocess_phase1(tokens: &[Token]) -> Vec<Token> {
    // split lines
    let mut token_lines = Vec::new();
    let mut cur_line = Vec::new();
    for token in tokens.iter() {
        if token == &Token::NewLine {
            if cur_line.is_empty() == false {
                token_lines.push(cur_line);
                cur_line = Vec::new();
            }
        } else {
            cur_line.push(token.clone());
        }
    }
    if cur_line.is_empty() == false {
        token_lines.push(cur_line);
    }

    let identifier_parser = rp::check(|t| {
        if let Token::Identifier(name) = t {
            Some(name)
        } else {
            None
        }
    });
    // ( macro_name: String, replace_tokens: Vec<Token> )
    let define_identifier_parser = rp::seq!(
        rp::one(Token::PreprocessorDefine).void(),
        identifier_parser,
        rp::any().repeat(0..)
    );
    // identifier, identifier, ...
    let identifiers = identifier_parser.map(|s: String| vec![s]).reduce_left(
        rp::seq!(rp::one(Token::Comma).void(), identifier_parser.clone()),
        |mut v: Vec<String>, s: String| {
            v.push(s);
            v
        },
    );

    // ( macro_name: String, macro_params: Vec<String>, macro_value: Vec<Token> )
    let define_function_parser = rp::seq!(
        rp::one(Token::PreprocessorDefine).void(),
        identifier_parser.clone(),
        rp::one(Token::LeftParen).void(),
        identifiers,
        rp::one(Token::RightParen).void(),
        rp::any().repeat(0..)
    );

    // preprocessing
    let mut define_map: HashMap<String, MacroData> = HashMap::new();
    let mut if_block_stack: Vec<bool> = Vec::new();

    let macro_replacer = MacroReplacer::new();

    for token_line in token_lines.iter_mut() {
        // #else
        if token_line.get(0) == Some(&Token::PreprocessorElse) {
            if token_line.len() != 1 {
                panic!("Invalid #else statement");
            }
            if if_block_stack.is_empty() {
                panic!("Invalid #else statement");
            }
            let last = if_block_stack.pop().unwrap();
            if_block_stack.push(!last);

            token_line.clear();
        }
        // #endif
        else if token_line.get(0) == Some(&Token::PreprocessorEndIf) {
            if token_line.len() != 1 {
                panic!("Invalid #endif statement");
            }
            if if_block_stack.is_empty() {
                panic!("Invalid #endif statement");
            }
            if_block_stack.pop();

            token_line.clear();
        }

        if if_block_stack.last().or(Some(&true)) == Some(&false) {
            token_line.clear();
            continue;
        }

        // #ifdef
        if token_line.get(0) == Some(&Token::PreprocessorIfDef) {
            if token_line.len() != 2 {
                panic!("Invalid #ifdef statement");
            }
            if let Token::Identifier(ref macro_name) = *token_line.get(1).unwrap() {
                if define_map.contains_key(macro_name) {
                    println!("Macro {} is defined", macro_name);
                    if_block_stack.push(true);
                } else {
                    println!("Macro {} is Not defined", macro_name);
                    if_block_stack.push(false);
                }
            }

            token_line.clear();
        }
        // #ifndef
        else if token_line.get(0) == Some(&Token::PreprocessorIfNDef) {
            if token_line.len() != 2 {
                panic!("Invalid #ifndef statement");
            }
            if let Token::Identifier(ref macro_name) = *token_line.get(1).unwrap() {
                if define_map.contains_key(macro_name) {
                    println!("Macro {} is defined", macro_name);
                    if_block_stack.push(false);
                } else {
                    println!("Macro {} is not defined", macro_name);
                    if_block_stack.push(true);
                }
            }

            token_line.clear();
        }
        // #define IDENT( IDENT, IDENT, ... ) Replacement
        else if let Some((name, params, mut replacement_tokens)) =
            rp::parse(&define_function_parser, token_line.iter().cloned()).output
        {
            // if this line starts with '#define IDENT(IDENT, IDENT ... )', this line is a macro definition
            for (idx, param_name) in params.iter().enumerate() {
                for token in replacement_tokens.iter_mut() {
                    if let Token::Identifier(ref s) = token {
                        if s == param_name {
                            *token = Token::PreprocessorPlaceholder(idx);
                        }
                    }
                }
            }
            println!(
                "Function-Macro Defined: {}({:?}) -> {:?}",
                name, params, replacement_tokens
            );
            define_map.insert(name, MacroData::Function(params.len(), replacement_tokens));

            token_line.clear();
        } else if let Some((name, replacement_tokens)) =
            rp::parse(&define_identifier_parser, token_line.iter().cloned()).output
        {
            // if this line starts with '#define IDENT', this line is a macro definition
            println!("Macro Defined: {} -> {:?}", name, replacement_tokens);
            define_map.insert(name, MacroData::DirectReplace(replacement_tokens));
            token_line.clear();
        } else {
            // else, unwrap the macro recursively
            macro_replacer.replace_recursive(&define_map, token_line);
        }
    }
    if if_block_stack.is_empty() == false {
        panic!("#if block is not closed");
    }

    // identifier to keyword
    let keyword_trie = rp::seq!(trie::build_keyword_trie(), rp::end());
    for token_line in token_lines.iter_mut() {
        for token in token_line.iter_mut() {
            if let Token::Identifier(ref mut s) = token {
                let keyword_matched = rp::parse(&keyword_trie, s.chars());
                // if it was keyword, change it
                if let Some((t,)) = keyword_matched.output {
                    *token = t;
                }
            }
        }
    }

    token_lines.iter().flatten().cloned().collect()
}
