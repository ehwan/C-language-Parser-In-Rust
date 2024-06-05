use super::ast::*;
use crate::token::Token;

use std::{any::Any, borrow::Borrow};

use rusty_parser::{self as rp, IntoParser};

pub struct ASTParser {
    primary_expression: DynASTParser,
    postfix_expression: DynASTParser,
    unary_expression: DynASTParser,
    cast_expression: DynASTParser,
    multiplicative_expression: DynASTParser,
    additive_expression: DynASTParser,
    shift_expression: DynASTParser,
    relational_expression: DynASTParser,
    equality_expression: DynASTParser,
    and_expression: DynASTParser,
    exclusive_or_expression: DynASTParser,
    inclusive_or_expression: DynASTParser,
    logical_and_expression: DynASTParser,
    logical_or_expression: DynASTParser,
    conditional_expression: DynASTParser,
    assignment_expression: DynASTParser,
    expression: DynASTParser,
    constant_expression: DynASTParser,
    declaration: DynASTParser,
    declarator: DynASTParser,
    initializer: DynASTParser,
    initializer_list: DynASTParser,
    statement: DynASTParser,
    labeled_statement: DynASTParser,
    compound_statement: DynASTParser,
    expression_statement: DynASTParser,
    selection_statement: DynASTParser,
    iteration_statement: DynASTParser,
    jump_statement: DynASTParser,
    translation_unit: DynASTParser,
    function_definition: DynASTParser,
    struct_specifier: DynASTParser,
}

impl ASTParser {
    pub fn new() -> Self {
        // This is a dummy parser that always panics.
        // dummy parser must be assigned later.
        let dummy = rp::constant(()).map(|| -> Box<dyn AST> {
            panic!("dummy parser!!");
        });

        let mut s = Self {
            primary_expression: dummy.clone().box_slice().refcell().rc(),
            postfix_expression: dummy.clone().box_slice().refcell().rc(),
            unary_expression: dummy.clone().box_slice().refcell().rc(),
            cast_expression: dummy.clone().box_slice().refcell().rc(),
            multiplicative_expression: dummy.clone().box_slice().refcell().rc(),
            additive_expression: dummy.clone().box_slice().refcell().rc(),
            shift_expression: dummy.clone().box_slice().refcell().rc(),
            relational_expression: dummy.clone().box_slice().refcell().rc(),
            equality_expression: dummy.clone().box_slice().refcell().rc(),
            and_expression: dummy.clone().box_slice().refcell().rc(),
            exclusive_or_expression: dummy.clone().box_slice().refcell().rc(),
            inclusive_or_expression: dummy.clone().box_slice().refcell().rc(),
            logical_and_expression: dummy.clone().box_slice().refcell().rc(),
            logical_or_expression: dummy.clone().box_slice().refcell().rc(),
            conditional_expression: dummy.clone().box_slice().refcell().rc(),
            assignment_expression: dummy.clone().box_slice().refcell().rc(),
            expression: dummy.clone().box_slice().refcell().rc(),
            constant_expression: dummy.clone().box_slice().refcell().rc(),
            declaration: dummy.clone().box_slice().refcell().rc(),
            declarator: dummy.clone().box_slice().refcell().rc(),
            initializer: dummy.clone().box_slice().refcell().rc(),
            initializer_list: dummy.clone().box_slice().refcell().rc(),
            statement: dummy.clone().box_slice().refcell().rc(),
            labeled_statement: dummy.clone().box_slice().refcell().rc(),
            compound_statement: dummy.clone().box_slice().refcell().rc(),
            expression_statement: dummy.clone().box_slice().refcell().rc(),
            selection_statement: dummy.clone().box_slice().refcell().rc(),
            iteration_statement: dummy.clone().box_slice().refcell().rc(),
            jump_statement: dummy.clone().box_slice().refcell().rc(),
            translation_unit: dummy.clone().box_slice().refcell().rc(),
            function_definition: dummy.clone().box_slice().refcell().rc(),
            struct_specifier: dummy.clone().box_slice().refcell().rc(),
        };

        let type_specifier = rp::or!(
            rp::one(Token::Int).map(|_| TypeSpecifier::Int),
            rp::one(Token::Char).map(|_| TypeSpecifier::Char),
            rp::one(Token::Float).map(|_| TypeSpecifier::Float),
            rp::one(Token::Double).map(|_| TypeSpecifier::Double),
            rp::one(Token::Void).map(|_| TypeSpecifier::Void),
            rp::one(Token::Short).map(|_| TypeSpecifier::Short),
            rp::one(Token::Long).map(|_| TypeSpecifier::Long),
            rp::one(Token::Signed).map(|_| TypeSpecifier::Signed),
            rp::one(Token::Unsigned).map(|_| TypeSpecifier::Unsigned),
            s.struct_specifier.clone().map(|s| TypeSpecifier::Struct(s))
        );

        {
            /*
            struct_declaration
            : type_specifier declarator+',' ';'
            ;
            */

            let declarators = s
                .declarator
                .clone()
                .map(|decl: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                    let mut v = Vec::new();
                    v.push(decl);
                    v
                })
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), s.declarator.clone()),
                    |mut v: Vec<Box<dyn AST>>, decl: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                        v.push(decl);
                        v
                    },
                );
            let struct_declaration = rp::seq!(
                type_specifier.clone(),
                declarators,
                rp::one(Token::SemiColon).void()
            )
            .map(
                |type_: TypeSpecifier, decls: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    Box::new(StructMemberDeclarationAST {
                        type_specifier: type_,
                        declarators: decls,
                    })
                },
            );

            /*
            struct_specifier
            : STRUCT IDENTIFIER '{' struct_declaration+ '}'
            | STRUCT '{' struct_declaration+ '}'
            | STRUCT IDENTIFIER
            ;
            */

            let struct_specifier = rp::seq!(
                rp::one(Token::Struct).void(),
                rp::or!(
                    rp::seq!(
                        rp::check(|t: Token| -> Option<String> {
                            if let Token::Identifier(s) = t {
                                Some(s)
                            } else {
                                None
                            }
                        }),
                        rp::one(Token::LeftBrace).void(),
                        struct_declaration.clone().repeat(1..),
                        rp::one(Token::RightBrace).void()
                    )
                    .map(
                        |name: String, decls: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                            Box::new(StructDeclAndSpecifierAST {
                                name: Some(name),
                                declarations: decls,
                            })
                        }
                    ),
                    rp::seq!(
                        rp::one(Token::LeftBrace).void(),
                        struct_declaration.clone().repeat(1..),
                        rp::one(Token::RightBrace).void()
                    )
                    .map(|decls: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                        Box::new(StructDeclAndSpecifierAST {
                            name: None,
                            declarations: decls,
                        })
                    }),
                    rp::check(|t: Token| -> Option<String> {
                        if let Token::Identifier(s) = t {
                            Some(s)
                        } else {
                            None
                        }
                    })
                    .map(|name: String| -> Box<dyn AST> { Box::new(StructSpecifierAST { name }) })
                )
            );
            s.struct_specifier.borrow_mut().assign(struct_specifier);
        }

        // =======================
        // Primary expression
        // =======================
        {
            let identifier = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                if let Token::Identifier(s) = t {
                    Some(Box::new(PrimaryIdentifierAST { name: s }))
                } else {
                    None
                }
            });
            let integer_constant = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                match t {
                    Token::ConstantInteger(i) => Some(Box::new(ConstantIntegerAST { value: i })),
                    Token::ConstantCharacter(ch) => {
                        Some(Box::new(ConstantIntegerAST { value: ch as u64 }))
                    }
                    _ => None,
                }
            });
            let float_constant = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                match t {
                    Token::ConstantFloat(f) => Some(Box::new(ConstantFloatAST { value: f })),
                    _ => None,
                }
            });
            let string_literal = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                match t {
                    Token::StringLiteral(s) => Some(Box::new(StringLiteralAST { value: s })),
                    _ => None,
                }
            });

            /*
              primary_expression
            : IDENTIFIER
            | CONSTANT
            | STRING_LITERAL
            | '(' expression ')'
            ;
            */
            s.primary_expression.borrow_mut().assign(rp::or!(
                identifier,
                integer_constant,
                float_constant,
                string_literal,
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    s.expression.clone(),
                    rp::one(Token::RightParen).void()
                )
            ));
        }

        // =======================
        // Postfix expression
        // =======================
        {
            enum PostfixType {
                Bracket(Box<dyn AST>),
                Paren(Vec<Box<dyn AST>>),
                Dot(String),
                Arrow(String),
                Inc,
                Dec,
            }

            let bracket = rp::seq!(
                rp::one(Token::LeftBracket).void(),
                s.expression.clone(),
                rp::one(Token::RightBracket).void()
            )
            .map(|e: Box<dyn AST>| PostfixType::Bracket(e));

            let paren = rp::seq!(
                rp::one(Token::LeftParen).void(),
                rp::one(Token::RightParen).void()
            )
            .map(|| PostfixType::Paren(Vec::new()));

            let argument_expression_list = rp::seq!(
                s.assignment_expression.clone(),
                rp::seq!(
                    rp::one(Token::Comma).void(),
                    s.assignment_expression.clone()
                )
                .repeat(0..)
            );

            let paren_with_args = rp::seq!(
                rp::one(Token::LeftParen).void(),
                argument_expression_list,
                rp::one(Token::RightParen).void()
            )
            .map(|arg0: Box<dyn AST>, args: Vec<Box<dyn AST>>| {
                let mut ret: Vec<Box<dyn AST>> = Vec::with_capacity(args.len() + 1);
                ret.push(arg0);
                for a in args {
                    ret.push(a);
                }
                PostfixType::Paren(ret)
            });

            let dot = rp::seq!(
                rp::one(Token::Dot).void(),
                rp::check(|t: Token| -> Option<String> {
                    if let Token::Identifier(s) = t {
                        Some(s)
                    } else {
                        None
                    }
                })
            )
            .map(|s: String| PostfixType::Dot(s));

            let ptr_op = rp::seq!(
                rp::one(Token::PtrOp).void(),
                rp::check(|t: Token| -> Option<String> {
                    if let Token::Identifier(s) = t {
                        Some(s)
                    } else {
                        None
                    }
                })
            )
            .map(|s: String| PostfixType::Arrow(s));

            let inc_op = rp::check(|t: Token| -> Option<PostfixType> {
                if let Token::IncOp = t {
                    Some(PostfixType::Inc)
                } else {
                    None
                }
            });
            let dec_op = rp::check(|t: Token| -> Option<PostfixType> {
                if let Token::DecOp = t {
                    Some(PostfixType::Dec)
                } else {
                    None
                }
            });

            /*
            postfix_expression
            : primary_expression
            | postfix_expression '[' expression ']'
            | postfix_expression '(' ')'
            | postfix_expression '(' argument_expression_list ')'
            | postfix_expression '.' IDENTIFIER
            | postfix_expression PTR_OP IDENTIFIER
            | postfix_expression INC_OP
            | postfix_expression DEC_OP
            ;
            */
            let postfix_expression = s.primary_expression.clone().reduce_left(
                rp::or!(bracket, paren, paren_with_args, dot, ptr_op, inc_op, dec_op),
                |lhs: Box<dyn AST>, rhs: PostfixType| -> Box<dyn AST> {
                    match rhs {
                        PostfixType::Bracket(e) => Box::new(PostBracketAST { src: lhs, index: e }),
                        PostfixType::Paren(args) => {
                            let args = Box::new(ArgumentExpressionListAST { args });
                            Box::new(PostParen { src: lhs, args })
                        }
                        PostfixType::Dot(s) => Box::new(PostMemberAST {
                            src: lhs,
                            member: s,
                        }),
                        PostfixType::Arrow(s) => Box::new(PostPointerAST {
                            src: lhs,
                            member: s,
                        }),
                        PostfixType::Inc => Box::new(PostIncrementAST { src: lhs }),
                        PostfixType::Dec => Box::new(PostDecrementAST { src: lhs }),
                    }
                },
            );

            s.postfix_expression.borrow_mut().assign(postfix_expression);
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
                rp::one(Token::Ampersand).output(UnaryOperator::AddressOf),
                rp::one(Token::Star).output(UnaryOperator::Dereference),
                rp::one(Token::Plus).output(UnaryOperator::Plus),
                rp::one(Token::Minus).output(UnaryOperator::Minus),
                rp::one(Token::Tilde).output(UnaryOperator::BitwiseNot),
                rp::one(Token::Exclamation).output(UnaryOperator::LogicalNot)
            );
            /*
            cast_expression
            : unary_expression
            | '(' type_specifier ')' cast_expression
            ;
            */
            let cast_expression = rp::or!(
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    type_specifier.clone(),
                    rp::one(Token::RightParen).void(),
                    s.cast_expression.clone()
                )
                .map(
                    |typename: TypeSpecifier, cast_expression: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(CastExpressionAST {
                            src: cast_expression,
                            typename,
                        })
                    }
                ),
                s.unary_expression.clone()
            );
            s.cast_expression.borrow_mut().assign(cast_expression);

            let sizeof_type = rp::seq!(
                rp::one(Token::Sizeof).void(),
                rp::one(Token::LeftParen).void(),
                type_specifier.clone(),
                rp::one(Token::RightParen).void()
            )
            .map(|typename: TypeSpecifier| -> Box<dyn AST> {
                Box::new(SizeofTypeAST { typename })
            });

            let sizeof_expr = rp::seq!(
                rp::one(Token::Sizeof).void(),
                rp::one(Token::LeftParen).void(),
                s.unary_expression.clone(),
                rp::one(Token::RightParen).void()
            )
            .map(|expr: Box<dyn AST>| -> Box<dyn AST> { Box::new(SizeofExprAST { expr }) });

            /*
            unary_expression
            : postfix_expression
            | INC_OP unary_expression
            | DEC_OP unary_expression
            | unary_operator cast_expression
            | SIZEOF unary_expression
            | SIZEOF '(' type_specifier ')'
            ;
            */
            let unary_expression = rp::or!(
                sizeof_expr,
                sizeof_type,
                rp::seq!(unary_operator, s.cast_expression.clone()).map(
                    |op: UnaryOperator, expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST { op, src: expr })
                    }
                ),
                rp::seq!(rp::one(Token::IncOp).void(), s.unary_expression.clone()).map(
                    |expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST {
                            op: UnaryOperator::Increment,
                            src: expr,
                        })
                    }
                ),
                rp::seq!(rp::one(Token::DecOp).void(), s.unary_expression.clone()).map(
                    |expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST {
                            op: UnaryOperator::Decrement,
                            src: expr,
                        })
                    }
                ),
                s.postfix_expression.clone()
            );

            s.unary_expression.borrow_mut().assign(unary_expression);
        }

        {
            /*
            multiplicative_expression
            : cast_expression
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

            let multiplicative = rp::seq!(
                s.cast_expression.clone(),
                rp::seq!(op, s.cast_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.multiplicative_expression
                .borrow_mut()
                .assign(multiplicative);
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
            let additive = rp::seq!(
                s.multiplicative_expression.clone(),
                rp::seq!(op, s.multiplicative_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.additive_expression.borrow_mut().assign(additive);
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
            let shift = rp::seq!(
                s.additive_expression.clone(),
                rp::seq!(op, s.additive_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.shift_expression.borrow_mut().assign(shift);
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
            let relational = rp::seq!(
                s.shift_expression.clone(),
                rp::seq!(op, s.shift_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.relational_expression.borrow_mut().assign(relational);
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
            let equality = rp::seq!(
                s.relational_expression.clone(),
                rp::seq!(op, s.relational_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.equality_expression.borrow_mut().assign(equality);
        }
        {
            /*
            and_expression
            : equality_expression
            | and_expression '&' equality_expression
            ;
            */
            let op = rp::one(Token::AndOp).output(BinaryOperator::BitwiseAnd);
            let and = rp::seq!(
                s.equality_expression.clone(),
                rp::seq!(op, s.equality_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.and_expression.borrow_mut().assign(and);
        }
        {
            /*
            exclusive_or_expression
            : and_expression
            | exclusive_or_expression '^' and_expression
            ;
            */
            let op = rp::one(Token::Caret).output(BinaryOperator::BitwiseXor);
            let xor = rp::seq!(
                s.and_expression.clone(),
                rp::seq!(op, s.and_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.exclusive_or_expression.borrow_mut().assign(xor);
        }
        {
            /*
            inclusive_or_expression
            : exclusive_or_expression
            | inclusive_or_expression '|' exclusive_or_expression
            ;
            */
            let op = rp::one(Token::Pipe).output(BinaryOperator::BitwiseOr);
            let or = rp::seq!(
                s.exclusive_or_expression.clone(),
                rp::seq!(op, s.exclusive_or_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.inclusive_or_expression.borrow_mut().assign(or);
        }
        {
            /*
            logical_and_expression
            : inclusive_or_expression
            | logical_and_expression AND_OP inclusive_or_expression
            ;
            */
            let op = rp::one(Token::AndOp).output(BinaryOperator::LogicalAnd);
            let logical_and = rp::seq!(
                s.inclusive_or_expression.clone(),
                rp::seq!(op, s.inclusive_or_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.logical_and_expression.borrow_mut().assign(logical_and);
        }
        {
            /*
            logical_or_expression
            : logical_and_expression
            | logical_or_expression OR_OP logical_and_expression
            ;
            */
            let op = rp::one(Token::OrOp).output(BinaryOperator::LogicalOr);
            let logical_or = rp::seq!(
                s.logical_and_expression.clone(),
                rp::seq!(op, s.logical_and_expression.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<(BinaryOperator, Box<dyn AST>)>| -> Box<dyn AST> {
                    let mut ret = first;
                    for (op, expr) in rest {
                        ret = Box::new(BinaryExpressionAST {
                            op,
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );
            s.logical_or_expression.borrow_mut().assign(logical_or);
        }
        {
            /*
            conditional_expression
            : logical_or_expression
            | logical_or_expression '?' expression ':' conditional_expression
            ;
            */
            let conditional = rp::seq!(
                s.logical_or_expression.clone(),
                rp::seq!(
                    rp::one(Token::Question).void(),
                    s.expression.clone(),
                    rp::one(Token::Colon).void(),
                    s.conditional_expression.clone()
                )
                .optional()
            )
            .map(
                |cond: Box<dyn AST>,
                 truefalse: Option<(Box<dyn AST>, Box<dyn AST>)>|
                 -> Box<dyn AST> {
                    if let Some((true_expr, false_expr)) = truefalse {
                        Box::new(ConditionalExpressionAST {
                            cond,
                            then_expr: true_expr,
                            else_expr: false_expr,
                        })
                    } else {
                        cond
                    }
                },
            );
            s.conditional_expression.borrow_mut().assign(conditional);
        }
        {
            /*
            assignment_operator
            : '='
            | MUL_ASSIGN
            | DIV_ASSIGN
            | MOD_ASSIGN
            | ADD_ASSIGN
            | SUB_ASSIGN
            | LEFT_ASSIGN
            | RIGHT_ASSIGN
            | AND_ASSIGN
            | XOR_ASSIGN
            | OR_ASSIGN
            ;
            */
            let assignment_operator = rp::or!(
                rp::one(Token::Equal).output(BinaryOperator::Assign),
                rp::one(Token::MulAssign).output(BinaryOperator::MulAssign),
                rp::one(Token::DivAssign).output(BinaryOperator::DivAssign),
                rp::one(Token::ModAssign).output(BinaryOperator::ModAssign),
                rp::one(Token::AddAssign).output(BinaryOperator::AddAssign),
                rp::one(Token::SubAssign).output(BinaryOperator::SubAssign),
                rp::one(Token::LeftAssign).output(BinaryOperator::ShiftLeftAssign),
                rp::one(Token::RightAssign).output(BinaryOperator::ShiftRightAssign),
                rp::one(Token::AndAssign).output(BinaryOperator::BitwiseAndAssign),
                rp::one(Token::XorAssign).output(BinaryOperator::BitwiseXorAssign),
                rp::one(Token::OrAssign).output(BinaryOperator::BitwiseOrAssign)
            );
            /*
            assignment_expression
            : conditional_expression
            | unary_expression assignment_operator assignment_expression
            */
            let assignment = rp::or!(
                rp::seq!(
                    s.unary_expression.clone(),
                    assignment_operator,
                    s.assignment_expression.clone()
                )
                .map(
                    |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(BinaryExpressionAST { op, lhs, rhs })
                    }
                ),
                s.conditional_expression.clone()
            );
            s.assignment_expression.borrow_mut().assign(assignment);
        }
        {
            /*
            expression
            : assignment_expression
            | expression ',' assignment_expression
            ;
            */
            let expression = rp::seq!(
                s.assignment_expression.clone(),
                rp::seq!(
                    rp::one(Token::Comma).void(),
                    s.assignment_expression.clone()
                )
                .repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    let mut ret = first;
                    for expr in rest {
                        ret = Box::new(CommaExpressionAST {
                            lhs: ret,
                            rhs: expr,
                        });
                    }
                    ret
                },
            );

            s.expression.borrow_mut().assign(expression);
        }
        {
            /*
            constant_expression
            : conditional_expression
            ;
            */
            s.constant_expression = s.conditional_expression.clone();
        }

        {}
        {
            /*
            declarator
            : IDENTIFIER
            | '(' declarator ')'
            | declarator '[' constant_expression ']' // array
            | declarator '[' ']' // array
            | declarator '(' parameter_declaration*',' ')' // function
            ;
            */

            let identifier = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                if let Token::Identifier(s) = t {
                    Some(Box::new(DeclaratorIdentifierAST { name: s }))
                } else {
                    None
                }
            });

            enum DeclaratorRightDecorator {
                Array(Option<Box<dyn AST>>),
                Function(Vec<Box<dyn AST>>),
            }

            let paren = rp::seq!(
                rp::one(Token::LeftParen).void(),
                s.declarator.clone(),
                rp::one(Token::RightParen).void()
            );

            let array_decorator = rp::seq!(
                rp::one(Token::LeftBracket).void(),
                s.constant_expression.clone().optional(),
                rp::one(Token::RightBracket).void()
            )
            .map(|expr: Option<Box<dyn AST>>| -> DeclaratorRightDecorator {
                DeclaratorRightDecorator::Array(expr)
            });

            let parameter_declaration =
                rp::seq!(type_specifier.clone(), s.declarator.clone().optional()).map(
                    |specifier: TypeSpecifier, decl: Option<Box<dyn AST>>| -> Box<dyn AST> {
                        Box::new(ParameterDeclarationAST {
                            specifiers: specifier,
                            declarator: decl,
                        })
                    },
                );
            let parameter_declarations = rp::seq!(
                parameter_declaration.clone(),
                rp::seq!(rp::one(Token::Comma).void(), parameter_declaration.clone()).repeat(0..)
            )
            .optional()
            .map(
                |decls: Option<(Box<dyn AST>, Vec<Box<dyn AST>>)>| -> Vec<Box<dyn AST>> {
                    if let Some(decls) = decls {
                        let mut v = Vec::with_capacity(decls.1.len() + 1);
                        v.push(decls.0);
                        for decl in decls.1 {
                            v.push(decl);
                        }
                        v
                    } else {
                        Vec::new()
                    }
                },
            );

            let func_decorator = rp::seq!(
                rp::one(Token::LeftParen).void(),
                parameter_declarations,
                rp::one(Token::RightParen).void()
            )
            .map(|decls: Vec<Box<dyn AST>>| -> DeclaratorRightDecorator {
                DeclaratorRightDecorator::Function(decls)
            });

            let right_decorator = rp::or!(array_decorator, func_decorator);

            let declarator = rp::seq!(rp::or!(identifier, paren), right_decorator.repeat(0..)).map(
                |src: Box<dyn AST>, decorators: Vec<DeclaratorRightDecorator>| -> Box<dyn AST> {
                    let mut ret = src;
                    for dec in decorators {
                        ret = match dec {
                            DeclaratorRightDecorator::Array(size_expr) => {
                                Box::new(DeclaratorArrayAST {
                                    src: ret,
                                    size: size_expr,
                                })
                            }
                            DeclaratorRightDecorator::Function(params) => {
                                Box::new(DeclaratorFunctionAST {
                                    decl: ret,
                                    args: params,
                                })
                            }
                        };
                    }
                    ret
                },
            );

            s.declarator.borrow_mut().assign(declarator);
        }

        {
            /*
            init_declarator
            : declarator
            | declarator '=' initializer
            ;
            */
            let init_declarator = rp::seq!(
                s.declarator.clone(),
                rp::seq!(rp::one(Token::Equal).void(), s.initializer.clone()).optional()
            )
            .map(
                |decl: Box<dyn AST>, init: Option<Box<dyn AST>>| -> Box<dyn AST> {
                    if let Some(init) = init {
                        Box::new(InitDeclaratorAST {
                            declarator: decl,
                            initializer: init,
                        })
                    } else {
                        decl
                    }
                },
            );

            let init_declarators = rp::seq!(
                init_declarator.clone(),
                rp::seq!(rp::one(Token::Comma).void(), init_declarator.clone()).repeat(0..)
            )
            .optional()
            .map(
                |decls: Option<(Box<dyn AST>, Vec<Box<dyn AST>>)>| -> Vec<Box<dyn AST>> {
                    if let Some((first, rest)) = decls {
                        let mut v = Vec::with_capacity(rest.len() + 1);
                        v.push(first);
                        for decl in rest {
                            v.push(decl);
                        }
                        v
                    } else {
                        Vec::new()
                    }
                },
            );

            /*
            declaration
            : type_specifier init_declarator*',' ';'
            ;
            */
            let declaration = rp::seq!(
                type_specifier.clone(),
                init_declarators,
                rp::one(Token::SemiColon).void()
            )
            .map(
                |specifier: TypeSpecifier, decls: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    Box::new(DeclarationAST {
                        specifier,
                        init_declarators: decls,
                    })
                },
            );

            s.declaration.borrow_mut().assign(declaration);
        }
        {
            /*
            initializer
            : assignment_expression
            | '{' initializer_list '}'
            | '{' initializer_list ',' '}'
            ;
            */
            let initializer = rp::or!(
                s.assignment_expression.clone(),
                rp::seq!(
                    rp::one(Token::LeftBrace).void(),
                    s.initializer_list.clone(),
                    rp::one(Token::Comma).optional().void(),
                    rp::one(Token::RightBrace).void()
                )
            );
            s.initializer.borrow_mut().assign(initializer);

            /*
            initializer_list
            : initializer
            | initializer_list ',' initializer
            ;
            */
            let initializer_list = rp::seq!(
                s.initializer.clone(),
                rp::seq!(rp::one(Token::Comma).void(), s.initializer.clone()).repeat(0..)
            )
            .map(
                |first: Box<dyn AST>, rest: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    let mut v: Vec<Box<dyn AST>> = Vec::with_capacity(rest.len() + 1);
                    v.push(first);
                    for expr in rest {
                        v.push(expr);
                    }
                    Box::new(InitializerListAST { initializers: v })
                },
            );

            s.initializer_list.borrow_mut().assign(initializer_list);
        }
        {
            /*
            statement
            : labeled_statement
            | compound_statement
            | expression_statement
            | selection_statement
            | iteration_statement
            | jump_statement
            | declaration
            ;
            */
            let statement = rp::or!(
                s.labeled_statement.clone(),
                s.compound_statement.clone(),
                s.expression_statement.clone(),
                s.selection_statement.clone(),
                s.iteration_statement.clone(),
                s.jump_statement.clone(),
                s.declaration.clone()
            );
            s.statement.borrow_mut().assign(statement);
        }
        {
            /*
            labeled_statement
            : IDENTIFIER ':' statement
            | CASE constant_expression ':' statement
            | DEFAULT ':' statement
            ;
            */
            let labeled_statement = rp::or!(
                rp::seq!(
                    rp::check(|t: Token| -> Option<String> {
                        if let Token::Identifier(s) = t {
                            Some(s)
                        } else {
                            None
                        }
                    }),
                    rp::one(Token::Colon).void(),
                    s.statement.clone()
                )
                .map(|s: String, stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(LabeledStatementAST {
                        label: s,
                        statement: stmt,
                    })
                }),
                rp::seq!(
                    rp::one(Token::Case).void(),
                    s.constant_expression.clone(),
                    rp::one(Token::Colon).void(),
                    s.statement.clone()
                )
                .map(|expr: Box<dyn AST>, stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(CaseStatementAST {
                        value: expr,
                        statement: stmt,
                    })
                }),
                rp::seq!(
                    rp::one(Token::Default).void(),
                    rp::one(Token::Colon).void(),
                    s.statement.clone()
                )
                .map(|stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(DefaultStatementAST { statement: stmt })
                })
            );
            s.labeled_statement.borrow_mut().assign(labeled_statement);
        }
        {
            /*
            compound_statement
            :  '{'  statement*  '}'
            ;
            */

            let compound_statement = rp::seq!(
                rp::one(Token::LeftBrace).void(),
                s.statement.clone().repeat(0..).map(
                    |compound: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                        Box::new(CompoundStatementAST {
                            statements: compound,
                        })
                    }
                ),
                rp::one(Token::RightBrace).void()
            );
            s.compound_statement.borrow_mut().assign(compound_statement);
        }
        {
            /*
            expression_statement
            : ';'
            | expression ';'
            ;
            */

            let expression_statement = rp::seq!(
                s.expression.clone().optional(),
                rp::one(Token::SemiColon).void()
            )
            .map(|expr: Option<Box<dyn AST>>| -> Box<dyn AST> {
                if let Some(expr) = expr {
                    expr
                } else {
                    Box::new(NullAST {})
                }
            });
            s.expression_statement
                .borrow_mut()
                .assign(expression_statement);
        }
        {
            /*
            selection_statement
            : IF '(' expression ')' statement
            | IF '(' expression ')' statement ELSE statement
            | SWITCH '(' expression ')' statement
            ;
            */
            let selection_statement = rp::or!(
                rp::seq!(
                    rp::one(Token::If).void(),
                    rp::one(Token::LeftParen).void(),
                    s.expression.clone(),
                    rp::one(Token::RightParen).void(),
                    s.statement.clone(),
                    rp::seq!(rp::one(Token::Else).void(), s.statement.clone()).optional()
                )
                .map(
                    |cond: Box<dyn AST>,
                     stmt: Box<dyn AST>,
                     else_stmt: Option<Box<dyn AST>>|
                     -> Box<dyn AST> {
                        Box::new(IfStatementAST {
                            cond,
                            then_statement: stmt,
                            else_statement: else_stmt,
                        })
                    }
                ),
                rp::seq!(
                    rp::one(Token::Switch).void(),
                    rp::one(Token::LeftParen).void(),
                    s.expression.clone(),
                    rp::one(Token::RightParen).void(),
                    s.statement.clone()
                )
                .map(|cond: Box<dyn AST>, stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(SwitchStatementAST {
                        cond,
                        statement: stmt,
                    })
                })
            );
            s.selection_statement
                .borrow_mut()
                .assign(selection_statement);
        }
        {
            /*
            iteration_statement
            : WHILE '(' expression ')' statement
            | DO statement WHILE '(' expression ')' ';'
            | FOR '(' expression_statement expression_statement ')' statement
            | FOR '(' expression_statement expression_statement expression ')' statement
            ;
            */

            let while_statement = rp::seq!(
                rp::one(Token::While).void(),
                rp::one(Token::LeftParen).void(),
                s.expression.clone(),
                rp::one(Token::RightParen).void(),
                s.statement.clone()
            )
            .map(|cond: Box<dyn AST>, stmt: Box<dyn AST>| -> Box<dyn AST> {
                Box::new(WhileStatementAST {
                    cond,
                    statement: stmt,
                })
            });

            let do_while_statement = rp::seq!(
                rp::one(Token::Do).void(),
                s.statement.clone(),
                rp::one(Token::While).void(),
                rp::one(Token::LeftParen).void(),
                s.expression.clone(),
                rp::one(Token::RightParen).void(),
                rp::one(Token::SemiColon).void()
            )
            .map(|stmt: Box<dyn AST>, cond: Box<dyn AST>| -> Box<dyn AST> {
                Box::new(DoWhileStatementAST {
                    cond,
                    statement: stmt,
                })
            });

            let for_statement = rp::seq!(
                rp::one(Token::For).void(),
                rp::one(Token::LeftParen).void(),
                s.expression_statement.clone(),
                s.expression_statement.clone(),
                s.expression.clone().optional(),
                rp::one(Token::RightParen).void(),
                s.statement.clone()
            )
            .map(
                |init: Box<dyn AST>,
                 cond: Box<dyn AST>,
                 next: Option<Box<dyn AST>>,
                 stmt: Box<dyn AST>|
                 -> Box<dyn AST> {
                    Box::new(ForStatementAST {
                        init,
                        cond,
                        next: if let Some(next) = next {
                            next
                        } else {
                            Box::new(NullAST {})
                        },
                        statement: stmt,
                    })
                },
            );

            let iteration_statement = rp::or!(while_statement, do_while_statement, for_statement);
            s.iteration_statement
                .borrow_mut()
                .assign(iteration_statement);
        }

        {
            /*
            jump_statement
            : GOTO IDENTIFIER ';'
            | CONTINUE ';'
            | BREAK ';'
            | RETURN ';'
            | RETURN expression ';'
            ;
            */

            let jump_statement = rp::or!(
                rp::seq!(
                    rp::one(Token::Goto).void(),
                    rp::check(|t: Token| -> Option<String> {
                        if let Token::Identifier(s) = t {
                            Some(s)
                        } else {
                            None
                        }
                    }),
                    rp::one(Token::SemiColon).void()
                )
                .map(|s: String| -> Box<dyn AST> { Box::new(GotoStatementAST { label: s }) }),
                rp::seq!(
                    rp::one(Token::Continue).void(),
                    rp::one(Token::SemiColon).void()
                )
                .map(|| -> Box<dyn AST> { Box::new(ContinueStatementAST {}) }),
                rp::seq!(
                    rp::one(Token::Break).void(),
                    rp::one(Token::SemiColon).void()
                )
                .map(|| -> Box<dyn AST> { Box::new(BreakStatementAST {}) }),
                rp::seq!(
                    rp::one(Token::Return).void(),
                    s.expression.clone().optional(),
                    rp::one(Token::SemiColon).void()
                )
                .map(|expr: Option<Box<dyn AST>>| -> Box<dyn AST> {
                    Box::new(ReturnStatementAST { expr: expr })
                })
            );

            s.jump_statement.borrow_mut().assign(jump_statement);
        }
        {
            /*
            translation_unit
            : external_declaration
            | translation_unit external_declaration
            ;
            */

            /*
            external_declaration
              : function_definition
              | declaration
              ;
            */
            let external_declaration =
                rp::or!(s.function_definition.clone(), s.declaration.clone());

            let translation_unit = external_declaration.clone().repeat(1..);
            let translation_unit =
                translation_unit.map(|decls: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    Box::new(TranslationUnitAST {
                        declarations: decls,
                    })
                });
            s.translation_unit.borrow_mut().assign(translation_unit);
        }
        {
            /*
            function_definition
            : type_specifier? IDENTIFIER '(' PARAM_LIST ')' compound_statement
            ;
            */

            let funcdef = rp::seq!(
                type_specifier.clone().optional(),
                s.declarator.clone(),
                s.compound_statement.clone()
            )
            .map(
                |spec: Option<TypeSpecifier>,
                 decl: Box<dyn AST>,
                 stmt: Box<dyn AST>|
                 -> Box<dyn AST> {
                    Box::new(FunctionDefinitionAST {
                        return_type: spec.or(Some(TypeSpecifier::Void)).unwrap(),
                        funcdecl: decl,
                        body: stmt,
                    })
                },
            );

            s.function_definition.borrow_mut().assign(funcdef);
        }

        s
        // TODO declare
    }

    pub fn parse(&self, tokens: Vec<Token>) -> Box<dyn AST> {
        let result = rp::parse(&self.translation_unit, tokens.iter().cloned());
        result.output.expect("Failed to parse").0
    }
}

impl Default for ASTParser {
    fn default() -> Self {
        Self::new()
    }
}
