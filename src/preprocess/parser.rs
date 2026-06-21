use super::context::*;
use super::expression::PreprocessorExpression;
use super::parser_lr_expression_expanded as expression_parser;
use super::parser_lr_file_expanded as file_parser;
use super::parser_lr_macro_expanded as macro_parser;
use super::preprocessor::*;
use crate::token::trie::ident_to_keyword_map;
use crate::token::Token;

pub struct PreprocessorParser {}

impl PreprocessorParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn tokenize(&self, source: &str) -> Vec<Token> {
        crate::token::tokenize::tokenize(source)
    }

    pub fn parse_lines(&self, tokens: &[Token]) -> Vec<Box<dyn PreprocessedTokenLine>> {
        let mut parser_context = file_parser::PreprocessingFileContext::new(());
        for token in tokens.iter().cloned() {
            parser_context
                .feed(token)
                .expect("Failed to parse preprocessor tokens");
        }

        parser_context
            .accept()
            .expect("Failed to parse preprocessor tokens")
            .0
    }

    pub fn parse_expression(&self, tokens: &[Token]) -> Box<dyn PreprocessorExpression> {
        let mut parser_context = expression_parser::PreprocessingExpressionContext::new(());
        for token in tokens
            .iter()
            .filter(|token| token != &&Token::Whitespace)
            .cloned()
        {
            parser_context
                .feed(token)
                .expect("Failed to parse preprocessing expression");
        }

        parser_context
            .accept()
            .expect("Failed to parse preprocessing expression")
            .0
    }

    pub fn preprocess(&self, lines: &[Box<dyn PreprocessedTokenLine>]) -> Vec<Vec<Token>> {
        let mut context = PreprocessorContext::new();
        let mut preprocessing_token_stream = Vec::new();

        for line in lines.iter() {
            let line_tokens = line.emit(&mut context, self);
            if line_tokens.is_empty() == false {
                preprocessing_token_stream.push(line_tokens);
            }
        }

        if context.conditional_inclusion_stack.is_empty() == false {
            panic!("#if block is not closed");
        }

        let keyword_environment = ident_to_keyword_map();
        for line in preprocessing_token_stream.iter_mut() {
            for token in line.iter_mut() {
                if let Token::Identifier(name) = token {
                    if let Some(keyword_token) = keyword_environment.get(name) {
                        *token = keyword_token.clone();
                    }
                }
            }
        }

        preprocessing_token_stream
    }

    pub fn replace(
        &self,
        source_tokens: &[Token],
        ctx: &PreprocessorContext,
    ) -> (bool, Vec<Token>) {
        let mut target_tokens = Vec::new();
        let mut source_index = 0;
        let mut substitution_occurred = false;

        while let Some(token) = source_tokens.get(source_index).cloned() {
            match token {
                Token::Identifier(name) => {
                    let macro_definition = ctx.macro_environment.get(&name);
                    match macro_definition {
                        Some(MacroDefinition::ObjectLike { replacement_list }) => {
                            target_tokens.extend(replacement_list.iter().cloned());
                            source_index += 1;
                            substitution_occurred = true;
                        }
                        Some(MacroDefinition::FunctionLike {
                            arity,
                            replacement_list,
                        }) => {
                            let invocation_suffix = &source_tokens[source_index + 1..];
                            let (mut arguments, residual_tokens) =
                                self.parse_macro_invocation_tail(invocation_suffix);

                            if *arity == 1 && arguments.is_empty() {
                                arguments.push(Vec::new());
                            }

                            if arguments.len() != *arity {
                                panic!("Invalid number of arguments for macro {}", name);
                            }

                            for replacement_token in replacement_list.iter() {
                                if let Token::PreprocessorPlaceholder(argument_index) =
                                    replacement_token
                                {
                                    target_tokens
                                        .extend(arguments[*argument_index].iter().cloned());
                                } else {
                                    target_tokens.push(replacement_token.clone());
                                }
                            }

                            let consumed_suffix_len =
                                invocation_suffix.len() - residual_tokens.len();
                            source_index += 1 + consumed_suffix_len;
                            substitution_occurred = true;
                        }
                        None => {
                            target_tokens.push(Token::Identifier(name));
                            source_index += 1;
                        }
                    }
                }
                _ => {
                    target_tokens.push(token);
                    source_index += 1;
                }
            }
        }

        (substitution_occurred, target_tokens)
    }

    pub fn replace_recursive(
        &self,
        source_tokens: &[Token],
        ctx: &PreprocessorContext,
    ) -> Vec<Token> {
        let mut current_tokens = source_tokens.to_vec();
        loop {
            let (substitution_occurred, next_tokens) = self.replace(&current_tokens, ctx);
            if substitution_occurred == false {
                break next_tokens;
            }
            current_tokens = next_tokens;
        }
    }

    fn parse_macro_invocation_tail(&self, tokens: &[Token]) -> (Vec<Vec<Token>>, Vec<Token>) {
        let mut parser_context = macro_parser::MacroInvocationTailContext::new(());
        for token in tokens.iter().cloned() {
            parser_context
                .feed(token)
                .expect("Failed to parse function-like macro invocation");
        }

        parser_context
            .accept()
            .expect("Failed to parse function-like macro invocation")
            .0
    }
}
