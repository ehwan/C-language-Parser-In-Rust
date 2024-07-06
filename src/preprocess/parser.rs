use std::boxed::Box;
use std::cell::RefCell;
use std::rc::Rc;

use rp::IntoParser;
use rusty_parser as rp;

use super::context::*;
use super::expression::*;
use super::preprocessor::*;
use crate::token::Token;

pub struct PreprocessorParser {
    /// compile-time expression parser
    pub expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>>,

    /// comma-separated list of macro arguments
    pub macro_argument_item_list: Rc<RefCell<rp::DynBoxSlice<(Vec<Vec<Token>>,), Token>>>,

    /// parse one line of token stream and make AST for preprocessing
    pub line: Rc<RefCell<rp::DynBoxSlice<(Box<dyn PreprocessedTokenLine>,), Token>>>,
}

impl PreprocessorParser {
    pub fn new() -> Self {
        let s = Self {
            expression: Default::default(),
            macro_argument_item_list: Default::default(),
            line: Default::default(),
        };

        s.expression_parser();
        s.arg_list_parser();
        s.line_parser();

        s
    }

    fn expression_parser(&self) {
        let primary_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let unary_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let multiplicative_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let additive_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let shift_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let relational_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let equality_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let and_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let exclusive_or_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let inclusive_or_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let logical_and_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let logical_or_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();
        let conditional_expression: Rc<
            RefCell<rp::DynBoxSlice<(Box<dyn PreprocessorExpression>,), Token>>,
        > = Default::default();

        // =======================
        // Primary expression
        // =======================
        {
            let integer_constant = rp::check(|t: Token| -> Option<i64> {
                match t {
                    Token::ConstantUnsignedInteger(i) => Some(i as i64),
                    Token::ConstantInteger(i) => Some(i as i64),
                    Token::ConstantCharacter(ch) => Some(ch as i64),
                    Token::ConstantLong(l) => Some(l as i64),
                    Token::ConstantUnsignedLong(l) => Some(l as i64),
                    _ => None,
                }
            })
            .map(|i: i64| -> Box<dyn PreprocessorExpression> { Box::new(Constant { value: i }) });

            let defined = rp::seq!(
                rp::one(Token::Identifier("defined".to_string())).void(),
                rp::one(Token::LeftParen).void(),
                rp::check(|t: Token| -> Option<String> {
                    if let Token::Identifier(s) = t {
                        Some(s)
                    } else {
                        None
                    }
                }),
                rp::one(Token::RightParen).void()
            )
            .map(|name: String| -> Box<dyn PreprocessorExpression> { Box::new(Defined { name }) });

            primary_expression.borrow_mut().assign(rp::or!(
                integer_constant,
                defined,
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    self.expression.clone(),
                    rp::one(Token::RightParen).void()
                )
            ));
        }

        // =======================
        // Unary expression
        // =======================
        {
            /*
            unary_operator
                    : '&'
                    | '*'
                    | '+'
                    | '-'
                    | '~'
                    | '!'
                    ;
                    */
            let unary_operator = rp::or!(
                rp::one(Token::Plus).output(UnaryOperator::Plus),
                rp::one(Token::Minus).output(UnaryOperator::Minus),
                rp::one(Token::Tilde).output(UnaryOperator::BitwiseNot),
                rp::one(Token::Exclamation).output(UnaryOperator::LogicalNot)
            );

            /*
            unary_expression
            : postfix_expression
            | unary_operator cast_expression
            ;
            */
            let unary_expression_ = rp::or!(
                rp::seq!(unary_operator, primary_expression.clone()).map(
                    |op: UnaryOperator,
                     expr: Box<dyn PreprocessorExpression>|
                     -> Box<dyn PreprocessorExpression> {
                        Box::new(UnaryExpression { op, src: expr })
                    }
                ),
                primary_expression.clone()
            );

            unary_expression.borrow_mut().assign(unary_expression_);
        }

        {
            /*
            multiplicative_expression
            : unary_expression
            | multiplicative_expression '*' cast_expression
            | multiplicative_expression '/' cast_expression
            | multiplicative_expression '%' cast_expression
            ;
            */
            let op = rp::or!(
                rp::one(Token::Star).output(BinaryOperator::Mul),
                rp::one(Token::Slash).output(BinaryOperator::Div),
                rp::one(Token::Percent).output(BinaryOperator::Mod)
            );

            let multiplicative_ = unary_expression.clone().reduce_left(
                rp::seq!(op, unary_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            multiplicative_expression
                .borrow_mut()
                .assign(multiplicative_);
        }
        {
            /*
            additive_expression
            : multiplicative_expression
            | additive_expression '+' multiplicative_expression
            | additive_expression '-' multiplicative_expression
            ;
            */
            let op = rp::or!(
                rp::one(Token::Plus).output(BinaryOperator::Add),
                rp::one(Token::Minus).output(BinaryOperator::Sub)
            );
            let additive = multiplicative_expression.clone().reduce_left(
                rp::seq!(op, multiplicative_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            additive_expression.borrow_mut().assign(additive);
        }
        {
            /*
            shift_expression
            : additive_expression
            | shift_expression LEFT_OP additive_expression
            | shift_expression RIGHT_OP additive_expression
            ;
            */
            let op = rp::or!(
                rp::one(Token::LeftOp).output(BinaryOperator::ShiftLeft),
                rp::one(Token::RightOp).output(BinaryOperator::ShiftRight)
            );
            let shift = additive_expression.clone().reduce_left(
                rp::seq!(op, additive_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            shift_expression.borrow_mut().assign(shift);
        }
        {
            /*
            relational_expression
            : shift_expression
            | relational_expression '<' shift_expression
            | relational_expression '>' shift_expression
            | relational_expression LE_OP shift_expression
            | relational_expression GE_OP shift_expression
            ;
            */
            let op = rp::or!(
                rp::one(Token::LessThan).output(BinaryOperator::LessThan),
                rp::one(Token::GreaterThan).output(BinaryOperator::GreaterThan),
                rp::one(Token::LeOp).output(BinaryOperator::LessThanOrEqual),
                rp::one(Token::GeOp).output(BinaryOperator::GreaterThanOrEqual)
            );
            let relational = shift_expression.clone().reduce_left(
                rp::seq!(op, shift_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            relational_expression.borrow_mut().assign(relational);
        }
        {
            /*
            equality_expression
            : relational_expression
            | equality_expression EQ_OP relational_expression
            | equality_expression NE_OP relational_expression
            ;
            */
            let op = rp::or!(
                rp::one(Token::EqOp).output(BinaryOperator::Equal),
                rp::one(Token::NeOp).output(BinaryOperator::NotEqual)
            );
            let equality = relational_expression.clone().reduce_left(
                rp::seq!(op, relational_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            equality_expression.borrow_mut().assign(equality);
        }
        {
            /*
            and_expression
            : equality_expression
            | and_expression '&' equality_expression
            ;
            */
            let op = rp::one(Token::Ampersand).output(BinaryOperator::BitwiseAnd);
            let and = equality_expression.clone().reduce_left(
                rp::seq!(op, equality_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            and_expression.borrow_mut().assign(and);
        }
        {
            /*
            exclusive_or_expression
            : and_expression
            | exclusive_or_expression '^' and_expression
            ;
            */
            let op = rp::one(Token::Caret).output(BinaryOperator::BitwiseXor);
            let xor = and_expression.clone().reduce_left(
                rp::seq!(op, and_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            exclusive_or_expression.borrow_mut().assign(xor);
        }
        {
            /*
            inclusive_or_expression
            : exclusive_or_expression
            | inclusive_or_expression '|' exclusive_or_expression
            ;
            */
            let op = rp::one(Token::Pipe).output(BinaryOperator::BitwiseOr);
            let or = exclusive_or_expression.clone().reduce_left(
                rp::seq!(op, exclusive_or_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            inclusive_or_expression.borrow_mut().assign(or);
        }
        {
            /*
            logical_and_expression
            : inclusive_or_expression
            | logical_and_expression AND_OP inclusive_or_expression
            ;
            */
            let op = rp::one(Token::AndOp).output(BinaryOperator::LogicalAnd);
            let logical_and = inclusive_or_expression.clone().reduce_left(
                rp::seq!(op, inclusive_or_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            logical_and_expression.borrow_mut().assign(logical_and);
        }
        {
            /*
            logical_or_expression
            : logical_and_expression
            | logical_or_expression OR_OP logical_and_expression
            ;
            */

            let op = rp::one(Token::OrOp).output(BinaryOperator::LogicalOr);
            let logical_or = logical_and_expression.clone().reduce_left(
                rp::seq!(op, logical_and_expression.clone()),
                |lhs: Box<dyn PreprocessorExpression>,
                 op: BinaryOperator,
                 rhs: Box<dyn PreprocessorExpression>|
                 -> Box<dyn PreprocessorExpression> {
                    Box::new(BinaryExpression { op, lhs, rhs })
                },
            );
            logical_or_expression.borrow_mut().assign(logical_or);
        }
        {
            /*
            conditional_expression
            : logical_or_expression
            | logical_or_expression '?' expression ':' conditional_expression
            ;
            */
            let conditional = rp::seq!(
                logical_or_expression.clone(),
                rp::seq!(
                    rp::one(Token::Question).void(),
                    self.expression.clone(),
                    rp::one(Token::Colon).void(),
                    conditional_expression.clone()
                )
                .optional()
            )
            .map(
                |cond: Box<dyn PreprocessorExpression>,
                 truefalse: Option<(
                    Box<dyn PreprocessorExpression>,
                    Box<dyn PreprocessorExpression>,
                )>|
                 -> Box<dyn PreprocessorExpression> {
                    if let Some((true_expr, false_expr)) = truefalse {
                        Box::new(ConditionalExpression {
                            cond,
                            then_expr: true_expr,
                            else_expr: false_expr,
                        })
                    } else {
                        cond
                    }
                },
            );
            conditional_expression
                .borrow_mut()
                .assign(conditional.clone());
            self.expression.borrow_mut().assign(conditional);
        }
    }

    fn arg_list_parser(&self) {
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
        self.macro_argument_item_list
            .borrow_mut()
            .assign(single_item_list);
    }

    fn line_parser(&self) {
        // String
        let identifier_parser = rp::check(|t| {
            if let Token::Identifier(name) = t {
                Some(name)
            } else {
                None
            }
        });
        // identifier, identifier, ...
        // Vec<String>
        let identifiers_parser = identifier_parser.map(|s: String| vec![s]).reduce_left(
            rp::seq!(rp::one(Token::Comma).void(), identifier_parser.clone()),
            |mut v: Vec<String>, s: String| {
                v.push(s);
                v
            },
        );

        // Vec<Token> excluing lineend
        let tokens_to_end = rp::seq!(
            rp::any().not(rp::one(Token::NewLine)).repeat(0..),
            rp::one(Token::NewLine).void()
        );

        // ( macro_name: String, replace_tokens: Vec<Token> )
        let define_identifier_parser = rp::seq!(
            rp::one(Token::PreprocessorDefine).void(),
            identifier_parser,
            tokens_to_end.clone()
        )
        .map(
            |name: String, replacement: Vec<Token>| -> Box<dyn PreprocessedTokenLine> {
                Box::new(Define { name, replacement })
            },
        );

        // ( macro_name: String, macro_params: Vec<String>, macro_value: Vec<Token> )
        let define_function_parser = rp::seq!(
            rp::one(Token::PreprocessorDefine).void(),
            identifier_parser.clone(),
            rp::one(Token::LeftParen).void(),
            identifiers_parser,
            rp::one(Token::RightParen).void(),
            tokens_to_end.clone()
        )
        .map(
            |name: String,
             params: Vec<String>,
             mut replacement: Vec<Token>|
             -> Box<dyn PreprocessedTokenLine> {
                for (idx, param_name) in params.iter().enumerate() {
                    for token in replacement.iter_mut() {
                        if let Token::Identifier(ref s) = token {
                            if s == param_name {
                                *token = Token::PreprocessorPlaceholder(idx);
                            }
                        }
                    }
                }
                Box::new(DefineFunction {
                    name,
                    param_count: params.len(),
                    replacement: replacement,
                })
            },
        );

        let undef_parser = rp::seq!(
            rp::one(Token::PreprocessorUndef).void(),
            identifier_parser,
            rp::one(Token::NewLine).void()
        )
        .map(|name: String| -> Box<dyn PreprocessedTokenLine> { Box::new(Undef { name }) });

        let ifdef_parser = rp::seq!(
            rp::one(Token::PreprocessorIfDef).void(),
            identifier_parser,
            rp::one(Token::NewLine).void()
        )
        .map(|name: String| -> Box<dyn PreprocessedTokenLine> { Box::new(IfDef { name }) });

        let ifndef_parser = rp::seq!(
            rp::one(Token::PreprocessorIfNDef).void(),
            identifier_parser,
            rp::one(Token::NewLine).void()
        )
        .map(|name: String| -> Box<dyn PreprocessedTokenLine> { Box::new(IfNDef { name }) });

        let else_parser = rp::seq!(
            rp::one(Token::PreprocessorElse).void(),
            rp::one(Token::NewLine).void()
        )
        .map(|| -> Box<dyn PreprocessedTokenLine> { Box::new(Else {}) });

        let endif_parser = rp::seq!(
            rp::one(Token::PreprocessorEndIf).void(),
            rp::one(Token::NewLine).void()
        )
        .map(|| -> Box<dyn PreprocessedTokenLine> { Box::new(EndIf {}) });

        let if_parser = rp::seq!(rp::one(Token::PreprocessorIf).void(), tokens_to_end.clone()).map(
            |expr_tokens: Vec<Token>| -> Box<dyn PreprocessedTokenLine> {
                Box::new(If {
                    expression_tokens: expr_tokens,
                })
            },
        );

        let elif_parser = rp::seq!(
            rp::one(Token::PreprocessorElIf).void(),
            tokens_to_end.clone()
        )
        .map(
            |expr_tokens: Vec<Token>| -> Box<dyn PreprocessedTokenLine> {
                Box::new(ElIf {
                    expression_tokens: expr_tokens,
                })
            },
        );

        // token stream as-is without any processing, excluding lineend
        let raw_parser =
            tokens_to_end
                .clone()
                .map(|tokens: Vec<Token>| -> Box<dyn PreprocessedTokenLine> {
                    Box::new(RawTokens { tokens })
                });

        self.line.borrow_mut().assign(rp::or!(
            define_function_parser,
            define_identifier_parser,
            undef_parser,
            ifdef_parser,
            ifndef_parser,
            if_parser,
            elif_parser,
            else_parser,
            endif_parser,
            raw_parser
        ));
    }

    pub fn parse_lines(&self, tokens: &[Token]) -> Vec<Box<dyn PreprocessedTokenLine>> {
        let lines_parser = rp::seq!(self.line.clone().repeat(0..), rp::end());

        // build ast
        let lines_result = rp::parse(&lines_parser, tokens.iter().cloned())
            .output
            .expect("Failed to parse preprocessor tokens")
            .0;

        // remove empty lines
        lines_result
            .into_iter()
            .filter(|line| line.is_empty() == false)
            .collect()
    }
    pub fn preprocess(&self, lines: &[Box<dyn PreprocessedTokenLine>]) -> Vec<Vec<Token>> {
        let mut context = PreprocessorContext::new();

        // preprocessed token stream
        let mut lines_token = Vec::new();

        // preprocessing phase
        for line in lines.iter() {
            let line_token = line.emit(&mut context, self);
            if line_token.is_empty() == false {
                lines_token.push(line_token);
            }
        }

        // check errors
        if context.if_stack.is_empty() == false {
            panic!("#if block is not closed");
        }

        lines_token
    }

    /// replace macro in given tokens
    /// this is not recursive macro replacement
    /// returns true if there was any replacement occured
    pub fn replace(&self, src_tokens: &[Token], ctx: &PreprocessorContext) -> (bool, Vec<Token>) {
        let mut tokens = Vec::new();
        let mut it = src_tokens.iter().cloned();

        let mut replaced = false;
        while let Some(token) = it.next() {
            match token {
                // check if it is macro
                Token::Identifier(name) => {
                    let macro_data = ctx.define_map.get(&name);
                    if let Some(MacroData::DirectReplace(replacement)) = macro_data {
                        // identifier links to normal macro
                        let mut replacement = replacement.clone();
                        tokens.append(&mut replacement);
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
                                    tokens.append(&mut args[*arg_idx].clone());
                                } else {
                                    tokens.push(replacement_token.clone());
                                }
                            }
                        } else {
                            panic!("Invalid arguments for macro {}", name);
                        }
                        it = args_res.it;
                        replaced = true;
                    } else {
                        tokens.push(Token::Identifier(name));
                    }
                }
                _ => {
                    tokens.push(token);
                }
            }
        }

        (replaced, tokens)
    }
    /// replace macro in given tokens
    /// this is recursive macro replacement
    pub fn replace_recursive(&self, src_tokens: &[Token], ctx: &PreprocessorContext) -> Vec<Token> {
        let mut tokens = src_tokens.to_vec();
        loop {
            let (replaced, new_tokens) = self.replace(&tokens, ctx);
            if replaced == false {
                break new_tokens;
            }
            tokens = new_tokens;
        }
    }
}
