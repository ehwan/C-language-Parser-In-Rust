use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::ast::*;
use super::declarator::*;
use super::expression::*;
use super::statement::*;
use super::typename::*;

use crate::token::Token;

use rusty_parser::{self as rp, IntoParser};

pub struct ASTParser {
    type_name: Rc<RefCell<rp::DynBoxSlice<(TypeInfo,), Token>>>, // ? Only Single typename, no pointer
    type_specifier: Rc<RefCell<rp::DynBoxSlice<(TypeInfo,), Token>>>, // OK

    expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, //OK
    assignment_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    constant_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    initializer: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK

    statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    compound_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    declaration: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    parameter_list: Rc<RefCell<rp::DynBoxSlice<(Vec<(TypeInfo, Option<Box<dyn AST>>)>,), Token>>>, // OK

    declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    init_declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK
    abstract_declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>>, // OK

    translation_unit: Rc<RefCell<rp::DynBoxSlice<(Box<TranslationUnitAST>,), Token>>>, // OK
}

impl ASTParser {
    pub fn new() -> Self {
        let mut s = Self {
            type_name: Default::default(),
            type_specifier: Default::default(),

            expression: Default::default(),
            constant_expression: Default::default(),
            assignment_expression: Default::default(),
            initializer: Default::default(),

            statement: Default::default(),
            compound_statement: Default::default(),
            declaration: Default::default(),
            parameter_list: Default::default(),

            declarator: Default::default(),
            init_declarator: Default::default(),
            abstract_declarator: Default::default(),

            translation_unit: Default::default(),
        };

        s.expression_parser();
        s.statement_parser();
        s.declarator();
        s.type_name();
        s.translation_unit_parser();

        s
    }

    pub fn parse(&self, input: Vec<Token>) -> Box<TranslationUnitAST> {
        let res = rp::parse(&self.translation_unit, input.iter().cloned());
        if let Some((out,)) = res.output {
            return out;
        }
        panic!("Failed to parse AST");
    }
    fn type_name(&mut self) {
        /*
        unsigned (char | short | int | long long | long | )
        signed (char | short | int | long long |  )
        char
        short
        int
        long long
        long
        float
        double
        */
        let unsigned_specifier = rp::seq!(
            rp::one(Token::Unsigned).void(),
            rp::or!(
                rp::one(Token::Char).void().map(|| TypeInfo::UInt8),
                rp::one(Token::Short).void().map(|| TypeInfo::UInt16),
                rp::one(Token::Int).void().map(|| TypeInfo::UInt32),
                rp::seq!(rp::one(Token::Long), rp::one(Token::Long))
                    .void()
                    .map(|| TypeInfo::UInt64),
                rp::one(Token::Long).void().map(|| TypeInfo::UInt64)
            )
            .optional_or(TypeInfo::UInt32)
        );
        let signed_specifier = rp::seq!(
            rp::one(Token::Unsigned).void(),
            rp::or!(
                rp::one(Token::Char).void().map(|| TypeInfo::Int8),
                rp::one(Token::Short).void().map(|| TypeInfo::Int16),
                rp::one(Token::Int).void().map(|| TypeInfo::Int32),
                rp::seq!(rp::one(Token::Long), rp::one(Token::Long))
                    .void()
                    .map(|| TypeInfo::Int64),
                rp::one(Token::Long).void().map(|| TypeInfo::Int64)
            )
            .optional_or(TypeInfo::Int32)
        );
        let floating_specifier = rp::or!(
            rp::one(Token::Float).void().map(|| TypeInfo::Float32),
            rp::one(Token::Double).void().map(|| TypeInfo::Float64)
        );
        let primitive_specifier = rp::or!(
            rp::one(Token::Void).void().map(|| TypeInfo::Void),
            rp::one(Token::Char).void().map(|| TypeInfo::Int8),
            rp::one(Token::Short).void().map(|| TypeInfo::Int16),
            rp::one(Token::Int).void().map(|| TypeInfo::Int32),
            rp::seq!(rp::one(Token::Long), rp::one(Token::Long))
                .void()
                .map(|| TypeInfo::Int64),
            rp::one(Token::Long).void().map(|| TypeInfo::Int64),
            unsigned_specifier,
            signed_specifier,
            floating_specifier
        );
        let struct_specifier: Rc<RefCell<rp::DynBoxSlice<(StructInfo,), Token>>> =
            Default::default();
        let union_specifier: Rc<RefCell<rp::DynBoxSlice<(UnionInfo,), Token>>> = Default::default();
        let enum_specifier: Rc<RefCell<rp::DynBoxSlice<(EnumInfo,), Token>>> = Default::default();

        /*
        type_specifier
        : VOID
        | CHAR
        | SHORT
        | INT
        | LONG
        | FLOAT
        | DOUBLE
        | SIGNED
        | UNSIGNED
        | struct_or_union_specifier
        | enum_specifier
        | TYPE_NAME
        ;
        */
        let type_specifier = rp::or!(
            primitive_specifier,
            struct_specifier.clone().map(|s| TypeInfo::Struct(s)),
            union_specifier.clone().map(|s| TypeInfo::Union(s)),
            enum_specifier.clone().map(|e| TypeInfo::Enum(e)),
            rp::check(|t: Token| -> Option<TypeInfo> {
                if let Token::Identifier(s) = t {
                    Some(TypeInfo::Identifier(s))
                } else {
                    None
                }
            })
        );

        self.type_specifier.borrow_mut().assign(type_specifier);

        let type_name = rp::seq!(
            self.type_specifier.clone(),
            self.abstract_declarator.clone().optional()
        )
        .map(
            |specifier: TypeInfo, declarator: Option<Box<dyn AST>>| -> TypeInfo {
                if let Some(declarator) = declarator {
                    declarator.get_typeinfo_from_declarator(specifier)
                } else {
                    specifier
                }
            },
        );
        self.type_name.borrow_mut().assign(type_name);

        /*
        struct_member_declaration
        : type_specifier declarator+',' ';'
        ;
        */
        let declarators = self
            .declarator
            .clone()
            .map(|d: Box<dyn AST>| -> Vec<Box<dyn AST>> { vec![d] })
            .reduce_left(
                rp::seq!(rp::one(Token::Comma).void(), self.declarator.clone()),
                |mut v: Vec<Box<dyn AST>>, d: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                    v.push(d);
                    v
                },
            );

        let struct_member_declaration = rp::seq!(
            self.type_specifier.clone(),
            declarators,
            rp::one(Token::SemiColon).void()
        )
        .map(
            |specifier: TypeInfo, declarators: Vec<Box<dyn AST>>| -> Vec<(String, TypeInfo)> {
                let mut ret = Vec::new();
                for declarator in declarators.into_iter() {
                    let var = declarator.get_typeinfo_from_direct_declarator(specifier.clone());
                    ret.push(var)
                }
                ret
            },
        );

        /*
        struct_or_union_specifier
        : struct_or_union IDENTIFIER '{' struct_member_declaration* '}'
        | struct_or_union '{' struct_member_declaration* '}'
        | struct_or_union IDENTIFIER
        ;
        */
        let struct_specifier1 = rp::seq!(
            rp::one(Token::Struct).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            }),
            rp::one(Token::LeftBrace).void(),
            struct_member_declaration.clone().repeat(0..),
            rp::one(Token::RightBrace).void()
        )
        .map(
            |name: String, memberss: Vec<Vec<(String, TypeInfo)>>| -> StructInfo {
                let mut fields: HashMap<String, TypeInfo> = HashMap::new();
                for members in memberss.into_iter() {
                    for member in members.into_iter() {
                        let old = fields.insert(member.0.clone(), member.1);
                        if old.is_some() {
                            panic!("Duplicated field name: {}", member.0);
                        }
                    }
                }
                StructInfo {
                    name: Some(name),
                    fields: Some(fields),
                }
            },
        );
        let struct_specifier2 = rp::seq!(
            rp::one(Token::Struct).void(),
            rp::one(Token::LeftBrace).void(),
            struct_member_declaration.clone().repeat(0..),
            rp::one(Token::RightBrace).void()
        )
        .map(|memberss: Vec<Vec<(String, TypeInfo)>>| -> StructInfo {
            let mut fields: HashMap<String, TypeInfo> = HashMap::new();
            for members in memberss.into_iter() {
                for member in members.into_iter() {
                    let old = fields.insert(member.0.clone(), member.1);
                    if old.is_some() {
                        panic!("Duplicated field name: {}", member.0);
                    }
                }
            }
            StructInfo {
                name: None,
                fields: Some(fields),
            }
        });
        let struct_specifier3 = rp::seq!(
            rp::one(Token::Struct).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            })
        )
        .map(|name: String| -> StructInfo {
            StructInfo {
                name: Some(name),
                fields: None,
            }
        });

        struct_specifier.borrow_mut().assign(rp::or!(
            struct_specifier1,
            struct_specifier2,
            struct_specifier3
        ));

        /*
        struct_or_union_specifier
        : struct_or_union IDENTIFIER '{' struct_member_declaration* '}'
        | struct_or_union '{' struct_member_declaration* '}'
        | struct_or_union IDENTIFIER
        ;
        */
        let union_specifier1 = rp::seq!(
            rp::one(Token::Union).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            }),
            rp::one(Token::LeftBrace).void(),
            struct_member_declaration.clone().repeat(0..),
            rp::one(Token::RightBrace).void()
        )
        .map(
            |name: String, memberss: Vec<Vec<(String, TypeInfo)>>| -> UnionInfo {
                let mut fields: HashMap<String, TypeInfo> = HashMap::new();
                for members in memberss.into_iter() {
                    for member in members.into_iter() {
                        let old = fields.insert(member.0.clone(), member.1);
                        if old.is_some() {
                            panic!("Duplicated field name: {}", member.0);
                        }
                    }
                }
                UnionInfo {
                    name: Some(name),
                    fields: Some(fields),
                }
            },
        );
        let union_specifier2 = rp::seq!(
            rp::one(Token::Union).void(),
            rp::one(Token::LeftBrace).void(),
            struct_member_declaration.clone().repeat(0..),
            rp::one(Token::RightBrace).void()
        )
        .map(|memberss: Vec<Vec<(String, TypeInfo)>>| -> UnionInfo {
            let mut fields: HashMap<String, TypeInfo> = HashMap::new();
            for members in memberss.into_iter() {
                for member in members.into_iter() {
                    let old = fields.insert(member.0.clone(), member.1);
                    if old.is_some() {
                        panic!("Duplicated field name: {}", member.0);
                    }
                }
            }
            UnionInfo {
                name: None,
                fields: Some(fields),
            }
        });
        let union_specifier3 = rp::seq!(
            rp::one(Token::Union).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            })
        )
        .map(|name: String| -> UnionInfo {
            UnionInfo {
                name: Some(name),
                fields: None,
            }
        });

        union_specifier.borrow_mut().assign(rp::or!(
            union_specifier1,
            union_specifier2,
            union_specifier3
        ));

        /*
        enumerator
        : IDENTIFIER
        | IDENTIFIER '=' constant_expression
        ;
        */
        struct Enumerator {
            pub name: String,
            pub value: Option<Box<dyn AST>>,
        }
        let enumerator = rp::seq!(
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            }),
            rp::seq!(
                rp::one(Token::Equal).void(),
                self.constant_expression.clone()
            )
            .optional()
        )
        .map(|name: String, value: Option<Box<dyn AST>>| Enumerator { name, value });

        /*
        enumerator_list
        : enumerator
        | enumerator_list ',' enumerator
        ;
        */
        let enumerator_list = enumerator
            .clone()
            .map(|e| -> Vec<Enumerator> { vec![e] })
            .reduce_left(
                rp::seq!(rp::one(Token::Comma).void(), enumerator.clone()),
                |mut v: Vec<Enumerator>, e: Enumerator| -> Vec<Enumerator> {
                    v.push(e);
                    v
                },
            );
        /*
        enum_specifier
        : ENUM '{' enumerator_list '}'
        | ENUM IDENTIFIER '{' enumerator_list '}'
        | ENUM IDENTIFIER
        ;
        */
        let enum_specifier1 = rp::seq!(
            rp::one(Token::Enum).void(),
            rp::one(Token::LeftBrace).void(),
            enumerator_list.clone(),
            rp::one(Token::RightBrace).void()
        )
        .map(|enumerators: Vec<Enumerator>| -> EnumInfo {
            let mut fields: HashMap<String, i64> = HashMap::new();
            let mut last_enum_value: Option<i64> = None;
            for enumerator in enumerators.into_iter() {
                let mut enum_value: i64 = 0;
                if let Some(value) = enumerator.value {
                    enum_value = value
                        .get_constant_i64()
                        .expect("Enumerator value must be constant expression");
                } else {
                    if last_enum_value.is_none() {
                        enum_value = 0;
                    } else {
                        enum_value = last_enum_value.unwrap() + 1;
                    }
                }
                let old = fields.insert(enumerator.name.clone(), enum_value);
                if old.is_some() {
                    panic!("Duplicated enumerator name: {}", enumerator.name);
                }
                last_enum_value = Some(enum_value);
            }
            EnumInfo {
                name: None,
                fields: Some(fields),
            }
        });

        let enum_specifier2 = rp::seq!(
            rp::one(Token::Enum).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            }),
            rp::one(Token::LeftBrace).void(),
            enumerator_list.clone(),
            rp::one(Token::RightBrace).void()
        )
        .map(|name: String, enumerators: Vec<Enumerator>| -> EnumInfo {
            let mut fields: HashMap<String, i64> = HashMap::new();
            let mut last_enum_value: Option<i64> = None;
            for enumerator in enumerators.into_iter() {
                let mut enum_value: i64 = 0;
                if let Some(value) = enumerator.value {
                    enum_value = value
                        .get_constant_i64()
                        .expect("Enumerator value must be constant expression");
                } else {
                    if last_enum_value.is_none() {
                        enum_value = 0;
                    } else {
                        enum_value = last_enum_value.unwrap() + 1;
                    }
                }
                let old = fields.insert(enumerator.name.clone(), enum_value);
                if old.is_some() {
                    panic!("Duplicated enumerator name: {}", enumerator.name);
                }
                last_enum_value = Some(enum_value);
            }
            EnumInfo {
                name: Some(name),
                fields: Some(fields),
            }
        });
        let enum_specifier3 = rp::seq!(
            rp::one(Token::Enum).void(),
            rp::check(|t: Token| -> Option<String> {
                if let Token::Identifier(s) = t {
                    Some(s)
                } else {
                    None
                }
            })
        )
        .map(|name: String| -> EnumInfo {
            EnumInfo {
                name: Some(name),
                fields: None,
            }
        });

        enum_specifier.borrow_mut().assign(rp::or!(
            enum_specifier1,
            enum_specifier2,
            enum_specifier3
        ));
    }
    fn expression_parser(&mut self) {
        let primary_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let postfix_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let unary_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let cast_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let multiplicative_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let additive_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let shift_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let relational_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let equality_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let and_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let exclusive_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let inclusive_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let logical_and_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let logical_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let conditional_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();

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
                        Some(Box::new(ConstantCharacterAST { value: ch }))
                    }
                    Token::ConstantLong(l) => Some(Box::new(ConstantLongAST { value: l })),
                    _ => None,
                }
            });
            let float_constant = rp::check(|t: Token| -> Option<Box<dyn AST>> {
                match t {
                    Token::ConstantFloat(f) => Some(Box::new(ConstantFloatAST { value: f })),
                    Token::ConstantDouble(d) => Some(Box::new(ConstantDoubleAST { value: d })),
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
            primary_expression.borrow_mut().assign(rp::or!(
                identifier,
                integer_constant,
                float_constant,
                string_literal,
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    self.expression.clone(),
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
                self.expression.clone(),
                rp::one(Token::RightBracket).void()
            )
            .map(|e: Box<dyn AST>| PostfixType::Bracket(e));

            let argument_list1 = self
                .assignment_expression
                .clone()
                .map(|e| -> Vec<Box<dyn AST>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(
                        rp::one(Token::Comma).void(),
                        self.assignment_expression.clone()
                    ),
                    |mut v: Vec<Box<dyn AST>>, e: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                        v.push(e);
                        v
                    },
                );
            let argument_list0 = argument_list1.optional().map(
                |args: Option<Vec<Box<dyn AST>>>| -> Vec<Box<dyn AST>> {
                    if let Some(args) = args {
                        args
                    } else {
                        Vec::new()
                    }
                },
            );

            let paren = rp::seq!(
                rp::one(Token::LeftParen).void(),
                argument_list0,
                rp::one(Token::RightParen).void()
            )
            .map(|args| PostfixType::Paren(args));

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
            postfix_expression
                .borrow_mut()
                .assign(primary_expression.clone().reduce_left(
                    rp::or!(bracket, paren, dot, ptr_op, inc_op, dec_op),
                    |lhs: Box<dyn AST>, rhs: PostfixType| -> Box<dyn AST> {
                        match rhs {
                            PostfixType::Bracket(e) => {
                                Box::new(PostBracketAST { src: lhs, index: e })
                            }
                            PostfixType::Paren(args) => Box::new(PostParen { src: lhs, args }),
                            PostfixType::Dot(s) => Box::new(PostMemberAST {
                                src: lhs,
                                member: s,
                            }),
                            PostfixType::Arrow(s) => Box::new(PostArrowAST {
                                src: lhs,
                                member: s,
                            }),
                            PostfixType::Inc => Box::new(PostIncrementAST { src: lhs }),
                            PostfixType::Dec => Box::new(PostDecrementAST { src: lhs }),
                        }
                    },
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
                rp::one(Token::Ampersand).output(UnaryOperator::AddressOf),
                rp::one(Token::Star).output(UnaryOperator::Dereference),
                rp::one(Token::Plus).output(UnaryOperator::Plus),
                rp::one(Token::Minus).output(UnaryOperator::Minus),
                rp::one(Token::Tilde).output(UnaryOperator::BitwiseNot),
                rp::one(Token::Exclamation).output(UnaryOperator::LogicalNot)
            );

            let sizeof_type = rp::seq!(
                rp::one(Token::Sizeof).void(),
                rp::one(Token::LeftParen).void(),
                self.type_name.clone(),
                rp::one(Token::RightParen).void()
            )
            .map(|typeinfo: TypeInfo| -> Box<dyn AST> { Box::new(SizeofTypeAST { typeinfo }) });

            let sizeof_expr = rp::seq!(rp::one(Token::Sizeof).void(), unary_expression.clone())
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
            let unary_expression1 = rp::or!(
                sizeof_expr,
                sizeof_type,
                rp::seq!(unary_operator, cast_expression.clone()).map(
                    |op: UnaryOperator, expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST { op, src: expr })
                    }
                ),
                rp::seq!(rp::one(Token::IncOp).void(), unary_expression.clone()).map(
                    |expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST {
                            op: UnaryOperator::Increment,
                            src: expr,
                        })
                    }
                ),
                rp::seq!(rp::one(Token::DecOp).void(), unary_expression.clone()).map(
                    |expr: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(UnaryExpressionAST {
                            op: UnaryOperator::Decrement,
                            src: expr,
                        })
                    }
                ),
                postfix_expression.clone()
            );

            unary_expression.borrow_mut().assign(unary_expression1);
        }

        {
            /*
            cast_expression
            : unary_expression
            | '(' type_name')' cast_expression
            ;
            */
            cast_expression
                .borrow_mut()
                .assign(unary_expression.clone().reduce_right(
                    rp::seq!(
                        rp::one(Token::LeftParen).void(),
                        self.type_name.clone(),
                        rp::one(Token::RightParen).void()
                    ),
                    |typeinfo: TypeInfo, cast_expression: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(CastExpressionAST {
                            src: cast_expression,
                            typeinfo,
                        })
                    },
                ));
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

            let multiplicative_ = cast_expression.clone().reduce_left(
                rp::seq!(op, cast_expression.clone()),
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
            let op = rp::one(Token::AndOp).output(BinaryOperator::BitwiseAnd);
            let and = equality_expression.clone().reduce_left(
                rp::seq!(op, equality_expression.clone()),
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
                |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(BinaryExpressionAST { op, lhs, rhs })
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
            conditional_expression.borrow_mut().assign(conditional);
            self.constant_expression
                .borrow_mut()
                .assign(conditional_expression.clone());
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
                    unary_expression.clone(),
                    assignment_operator,
                    self.assignment_expression.clone()
                )
                .map(
                    |lhs: Box<dyn AST>, op: BinaryOperator, rhs: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(BinaryExpressionAST { op, lhs, rhs })
                    }
                ),
                conditional_expression.clone()
            );
            self.assignment_expression.borrow_mut().assign(assignment);
        }
        {
            /*
            expression
            : assignment_expression
            | expression ',' assignment_expression
            ;
            */
            let expression = self.assignment_expression.clone().reduce_left(
                rp::seq!(rp::one(Token::Comma).void(), self.expression.clone()),
                |lhs: Box<dyn AST>, rhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(CommaExpressionAST { lhs, rhs })
                },
            );
            self.expression.borrow_mut().assign(expression);
        }

        {
            /*
            initializer
            : assignment_expression
            | '{' initializer_list '}'
            | '{' initializer_list ',' '}'
            ;

            initializer_list
            : initializer
            | initializer_list ',' initializer
            ;
            */
            let initalizer_list = self
                .initializer
                .clone()
                .map(|e: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), self.initializer.clone()),
                    |mut v: Vec<Box<dyn AST>>, e: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                        v.push(e);
                        v
                    },
                )
                .map(|v: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                    Box::new(InitializerListExpressionAST { initializers: v })
                });

            let initializer_ = rp::or!(
                rp::seq!(
                    rp::one(Token::LeftBrace).void(),
                    initalizer_list,
                    rp::one(Token::RightBrace).void(),
                    rp::one(Token::Comma).optional().void()
                ),
                self.assignment_expression.clone()
            );
            self.initializer.borrow_mut().assign(initializer_);
        }
    }

    fn declarator(&mut self) {
        let identifier = rp::check(|t: Token| -> Option<String> {
            if let Token::Identifier(s) = t {
                Some(s)
            } else {
                None
            }
        });

        /*
        direct_declarator
        : IDENTIFIER
        | '(' declarator ')'
        | direct_declarator '[' constant_expression ']'
        | direct_declarator '[' ']'
        | direct_declarator '(' parameter_type_list ')'
        | direct_declarator '(' ')'
        ;
        */
        let direct_declarator_leaf = rp::or!(
            identifier
                .clone()
                .map(|s: String| -> Box<dyn AST> { Box::new(IdentifierDeclaratorAST { name: s }) }),
            rp::seq!(
                rp::one(Token::LeftParen).void(),
                self.declarator.clone(),
                rp::one(Token::RightParen).void()
            )
        );
        enum DirectDeclaratorType {
            Array(Option<Box<dyn AST>>),                     // length
            Function(Vec<(TypeInfo, Option<Box<dyn AST>>)>), // parameter types
        }
        let direct_declarator = direct_declarator_leaf.reduce_left(
            rp::or!(
                rp::seq!(
                    rp::one(Token::LeftBracket).void(),
                    self.constant_expression.clone(),
                    rp::one(Token::RightBracket).void()
                )
                .map(|len: Box<dyn AST>| -> DirectDeclaratorType {
                    DirectDeclaratorType::Array(Some(len))
                }),
                rp::seq!(
                    rp::one(Token::LeftBracket).void(),
                    rp::one(Token::RightBracket).void()
                )
                .map(|| -> DirectDeclaratorType { DirectDeclaratorType::Array(None) }),
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    rp::one(Token::RightParen).void()
                )
                .map(|| -> DirectDeclaratorType { DirectDeclaratorType::Function(Vec::new()) }),
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    self.parameter_list.clone(),
                    rp::one(Token::RightParen).void()
                )
                .map(
                    |params: Vec<(TypeInfo, Option<Box<dyn AST>>)>| -> DirectDeclaratorType {
                        DirectDeclaratorType::Function(params)
                    }
                )
            ),
            |lhs: Box<dyn AST>, op: DirectDeclaratorType| -> Box<dyn AST> {
                match op {
                    DirectDeclaratorType::Array(Some(len)) => {
                        Box::new(DirectArrayFixedDeclaratorAST {
                            declarator: lhs,
                            size: len,
                        })
                    }
                    DirectDeclaratorType::Array(None) => {
                        Box::new(DirectArrayUnboundedDeclaratorAST { declarator: lhs })
                    }
                    DirectDeclaratorType::Function(params) => {
                        Box::new(DirectFunctionDeclaratorAST {
                            declarator: lhs,
                            params,
                        })
                    }
                }
            },
        );
        /*
        pointer
        : '*' pointer
        ;
        */
        /*
        declarator
        : pointer direct_declarator
        | direct_declarator
        ;
        */

        let declarator_ = direct_declarator.reduce_right(
            rp::one(Token::Star).void(),
            |decl: Box<dyn AST>| -> Box<dyn AST> {
                Box::new(PointerDeclaratorAST { declarator: decl })
            },
        );

        self.declarator.borrow_mut().assign(declarator_);

        /*
        init_declarator
        : declarator
        | declarator '=' initializer
        ;
        */
        let init_declarator_ = rp::seq!(
            self.declarator.clone(),
            rp::seq!(rp::one(Token::Equal).void(), self.initializer.clone()).optional()
        )
        .map(
            |decl: Box<dyn AST>, init: Option<Box<dyn AST>>| -> Box<dyn AST> {
                match init {
                    Some(init) => Box::new(InitDeclaratorAST {
                        declarator: decl,
                        initializer: init,
                    }),
                    None => decl,
                }
            },
        );
        self.init_declarator.borrow_mut().assign(init_declarator_);
        {
            /*

            direct_abstract_declarator
            : '(' abstract_declarator ')'
            | '[' ']'
            | '[' constant_expression ']'
            | direct_abstract_declarator '[' ']'
            | direct_abstract_declarator '[' constant_expression ']'
            | '(' ')'
            | '(' parameter_type_list ')'
            | direct_abstract_declarator '(' ')'
            | direct_abstract_declarator '(' parameter_type_list ')'
            ;
            */

            enum DirectAbstractDeclaratorLeaf {
                Array(Option<Box<dyn AST>>),
                Function(Vec<(TypeInfo, Option<Box<dyn AST>>)>),
            }
            let direct_abstract_declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
                Default::default();

            let direct_abstract_declarator_leaf_array = rp::or!(
                rp::seq!(
                    rp::one(Token::LeftBracket).void(),
                    rp::one(Token::RightBracket).void()
                )
                .map(|| -> DirectAbstractDeclaratorLeaf {
                    DirectAbstractDeclaratorLeaf::Array(None)
                }),
                rp::seq!(
                    rp::one(Token::LeftBracket).void(),
                    self.constant_expression.clone(),
                    rp::one(Token::RightBracket).void()
                )
                .map(|len: Box<dyn AST>| -> DirectAbstractDeclaratorLeaf {
                    DirectAbstractDeclaratorLeaf::Array(Some(len))
                })
            );
            let direct_abstract_declarator_leaf_function = rp::or!(
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    rp::one(Token::RightParen).void()
                )
                .map(|| -> DirectAbstractDeclaratorLeaf {
                    DirectAbstractDeclaratorLeaf::Function(Vec::new())
                }),
                rp::seq!(
                    rp::one(Token::LeftParen).void(),
                    self.parameter_list.clone(),
                    rp::one(Token::RightParen).void()
                )
                .map(
                    |params: Vec<(TypeInfo,Option<Box<dyn AST>>)>| -> DirectAbstractDeclaratorLeaf {
                        DirectAbstractDeclaratorLeaf::Function(params)
                    }
                )
            );
            let direct_abstract_declarator_leaf_paren = rp::seq!(
                rp::one(Token::LeftParen).void(),
                direct_abstract_declarator.clone(),
                rp::one(Token::RightParen).void()
            );
            let direct_abstract_declarator_decorator = rp::or!(
                direct_abstract_declarator_leaf_array,
                direct_abstract_declarator_leaf_function
            );

            let direct_abstract_declarator_leaf = rp::or!(
                direct_abstract_declarator_leaf_paren,
                direct_abstract_declarator_decorator.clone().map(
                    |op: DirectAbstractDeclaratorLeaf| -> Box<dyn AST> {
                        match op {
                            DirectAbstractDeclaratorLeaf::Array(len) => {
                                if let Some(len) = len {
                                    Box::new(AbstractArrayFixedDeclaratorAST {
                                        declarator: None,
                                        size: len,
                                    })
                                } else {
                                    Box::new(AbstractArrayUnboundedDeclaratorAST {
                                        declarator: None,
                                    })
                                }
                            }
                            DirectAbstractDeclaratorLeaf::Function(params) => {
                                Box::new(AbstractFunctionDeclaratorAST {
                                    declarator: None,
                                    params,
                                })
                            }
                        }
                    },
                )
            );

            let direct_abstract_declarator_ = direct_abstract_declarator_leaf.reduce_left(
                direct_abstract_declarator_decorator,
                |lhs: Box<dyn AST>, op: DirectAbstractDeclaratorLeaf| -> Box<dyn AST> {
                    match op {
                        DirectAbstractDeclaratorLeaf::Array(len) => {
                            if let Some(len) = len {
                                Box::new(AbstractArrayFixedDeclaratorAST {
                                    declarator: Some(lhs),
                                    size: len,
                                })
                            } else {
                                Box::new(AbstractArrayUnboundedDeclaratorAST {
                                    declarator: Some(lhs),
                                })
                            }
                        }
                        DirectAbstractDeclaratorLeaf::Function(params) => {
                            Box::new(AbstractFunctionDeclaratorAST {
                                declarator: Some(lhs),
                                params,
                            })
                        }
                    }
                },
            );
            direct_abstract_declarator
                .borrow_mut()
                .assign(direct_abstract_declarator_);
            /*
            abstract_declarator
            : pointer
            | direct_abstract_declarator
            | pointer direct_abstract_declarator
            ;
            */
            /*
            pointer
            : '*' pointer
            ;
            */
            let pointered = direct_abstract_declarator.clone().reduce_right(
                rp::one(Token::Star).void(),
                |lhs: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(AbstractPointerDeclaratorAST {
                        declarator: Some(lhs),
                    })
                },
            );
            let pointer = rp::one(Token::Star)
                .void()
                .map(|| -> Box<dyn AST> {
                    Box::new(AbstractPointerDeclaratorAST { declarator: None })
                })
                .reduce_right(
                    rp::one(Token::Star).void(),
                    |lhs: Box<dyn AST>| -> Box<dyn AST> {
                        Box::new(AbstractPointerDeclaratorAST {
                            declarator: Some(lhs),
                        })
                    },
                );
            let abstract_declarator_ = rp::or!(pointered, pointer, direct_abstract_declarator);
            self.abstract_declarator
                .borrow_mut()
                .assign(abstract_declarator_);
        }
    }

    fn statement_parser(&mut self) {
        let identifier = rp::check(|t: Token| -> Option<String> {
            if let Token::Identifier(s) = t {
                Some(s)
            } else {
                None
            }
        });
        let labeled_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let expression_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let selection_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let iteration_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
        let jump_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();

        {
            let parameter_declaration = rp::seq!(
                self.type_specifier.clone(),
                rp::or!(self.declarator.clone(), self.abstract_declarator.clone()).optional()
            );

            let parameter_list_ = parameter_declaration
                .clone()
                .map(
                    |typeinfo: TypeInfo,
                     declarator: Option<Box<dyn AST>>|
                     -> Vec<(TypeInfo, Option<Box<dyn AST>>)> {
                        vec![(typeinfo, declarator)]
                    },
                )
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), parameter_declaration),
                    |mut v: Vec<(TypeInfo, Option<Box<dyn AST>>)>,
                     typeinfo: TypeInfo,
                     declarator: Option<Box<dyn AST>>|
                     -> Vec<(TypeInfo, Option<Box<dyn AST>>)> {
                        v.push((typeinfo, declarator));
                        v
                    },
                );
            self.parameter_list.borrow_mut().assign(parameter_list_);
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
                self.declaration.clone(),
                labeled_statement.clone(),
                self.compound_statement.clone(),
                selection_statement.clone(),
                iteration_statement.clone(),
                jump_statement.clone(),
                expression_statement.clone()
            );
            self.statement.borrow_mut().assign(statement);
        }
        {
            /*
            labeled_statement
            : IDENTIFIER ':' statement
            | CASE constant_expression ':' statement
            | DEFAULT ':' statement
            ;
            */
            let labeled_statement_ = rp::or!(
                rp::seq!(
                    identifier,
                    rp::one(Token::Colon).void(),
                    self.statement.clone()
                )
                .map(|s: String, stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(LabeledStatementAST {
                        label: s,
                        statement: stmt,
                    })
                }),
                rp::seq!(
                    rp::one(Token::Case).void(),
                    self.constant_expression.clone(),
                    rp::one(Token::Colon).void(),
                    self.statement.clone()
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
                    self.statement.clone()
                )
                .map(|stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(DefaultStatementAST { statement: stmt })
                })
            );
            labeled_statement.borrow_mut().assign(labeled_statement_);
        }
        {
            /*
            compound_statement
            :  '{'  statement*  '}'
            ;
            */

            let compound_statement_ = rp::seq!(
                rp::one(Token::LeftBrace).void(),
                self.statement.clone().repeat(0..).map(
                    |compound: Vec<Box<dyn AST>>| -> Box<dyn AST> {
                        Box::new(CompoundStatementAST {
                            statements: compound,
                        })
                    }
                ),
                rp::one(Token::RightBrace).void()
            );
            self.compound_statement
                .borrow_mut()
                .assign(compound_statement_);
        }
        {
            /*
            expression_statement
            : ';'
            | expression ';'
            ;
            */

            let expression_statement_ = rp::seq!(
                self.expression.clone().optional(),
                rp::one(Token::SemiColon).void()
            )
            .map(|expr: Option<Box<dyn AST>>| -> Box<dyn AST> {
                if let Some(expr) = expr {
                    Box::new(ExpressionStatementAST { expression: expr })
                } else {
                    Box::new(NullStatementAST {})
                }
            });
            expression_statement
                .borrow_mut()
                .assign(expression_statement_);
        }
        {
            /*
            selection_statement
            : IF '(' expression ')' statement
            | IF '(' expression ')' statement ELSE statement
            | SWITCH '(' expression ')' statement
            ;
            */
            let selection_statement_ = rp::or!(
                rp::seq!(
                    rp::one(Token::If).void(),
                    rp::one(Token::LeftParen).void(),
                    self.expression.clone(),
                    rp::one(Token::RightParen).void(),
                    self.statement.clone(),
                    rp::seq!(rp::one(Token::Else).void(), self.statement.clone()).optional()
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
                    self.expression.clone(),
                    rp::one(Token::RightParen).void(),
                    self.statement.clone()
                )
                .map(|cond: Box<dyn AST>, stmt: Box<dyn AST>| -> Box<dyn AST> {
                    Box::new(SwitchStatementAST {
                        cond,
                        statement: stmt,
                    })
                })
            );
            selection_statement
                .borrow_mut()
                .assign(selection_statement_);
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
                self.expression.clone(),
                rp::one(Token::RightParen).void(),
                self.statement.clone()
            )
            .map(|cond: Box<dyn AST>, stmt: Box<dyn AST>| -> Box<dyn AST> {
                Box::new(WhileStatementAST {
                    cond,
                    statement: stmt,
                })
            });

            let do_while_statement = rp::seq!(
                rp::one(Token::Do).void(),
                self.statement.clone(),
                rp::one(Token::While).void(),
                rp::one(Token::LeftParen).void(),
                self.expression.clone(),
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
                expression_statement.clone(),
                expression_statement.clone(),
                self.expression.clone().optional(),
                rp::one(Token::RightParen).void(),
                self.statement.clone()
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
                        next,
                        statement: stmt,
                    })
                },
            );

            let iteration_statement_ = rp::or!(while_statement, do_while_statement, for_statement);
            iteration_statement
                .borrow_mut()
                .assign(iteration_statement_);
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

            let goto_statement = rp::seq!(
                rp::one(Token::Goto).void(),
                identifier,
                rp::one(Token::SemiColon).void()
            )
            .map(|s: String| -> Box<dyn AST> { Box::new(GotoStatementAST { label: s }) });

            let continue_statement = rp::seq!(
                rp::one(Token::Continue).void(),
                rp::one(Token::SemiColon).void()
            )
            .map(|| -> Box<dyn AST> { Box::new(ContinueStatementAST {}) });

            let break_statement = rp::seq!(
                rp::one(Token::Break).void(),
                rp::one(Token::SemiColon).void()
            )
            .map(|| -> Box<dyn AST> { Box::new(BreakStatementAST {}) });

            let return_statement = rp::seq!(
                rp::one(Token::Return).void(),
                self.expression.clone().optional(),
                rp::one(Token::SemiColon).void()
            )
            .map(|expr: Option<Box<dyn AST>>| -> Box<dyn AST> {
                Box::new(ReturnStatementAST { expr })
            });

            let jump_statement_ = rp::or!(
                goto_statement,
                continue_statement,
                break_statement,
                return_statement
            );

            jump_statement.borrow_mut().assign(jump_statement_);
        }

        {
            /*

            init_declarator_list
            : init_declarator
            | init_declarator_list ',' init_declarator
            ;
            */
            let init_declarator_list = self
                .init_declarator
                .clone()
                .map(|e: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), self.init_declarator.clone()),
                    |mut v: Vec<Box<dyn AST>>, e: Box<dyn AST>| -> Vec<Box<dyn AST>> {
                        v.push(e);
                        v
                    },
                );
            /*
            declaration
            : specifier_list ';'
            | specifier_list init_declarator_list ';'
            ;
            */
            let declaration = rp::seq!(
                self.type_specifier.clone(),
                init_declarator_list.optional(),
                rp::one(Token::SemiColon).void()
            )
            .map(
                |typeinfo: TypeInfo, decls: Option<Vec<Box<dyn AST>>>| -> Box<dyn AST> {
                    if let Some(decls) = decls {
                        Box::new(DeclarationVarsStatementAST {
                            typeinfo,
                            declarators: decls,
                        })
                    } else {
                        Box::new(DeclarationStatementAST { typeinfo })
                    }
                },
            );
            self.declaration.borrow_mut().assign(declaration);
        }
    }

    fn translation_unit_parser(&mut self) {
        let function_definition: Rc<RefCell<rp::DynBoxSlice<(Box<dyn AST>,), Token>>> =
            Default::default();
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
                rp::or!(function_definition.clone(), self.declaration.clone());

            let translation_unit_ = external_declaration.repeat(1..).map(
                |decls: Vec<Box<dyn AST>>| -> Box<TranslationUnitAST> {
                    Box::new(TranslationUnitAST { statements: decls })
                },
            );
            self.translation_unit.borrow_mut().assign(translation_unit_);
        }
        {
            /*
            function_definition
            : specifier_list declarator compound_statement
            | declarator compound_statement
            ;
            ;
            */

            let funcdef = rp::seq!(
                self.type_specifier.clone().optional(),
                self.declarator.clone(),
                self.compound_statement.clone()
            )
            .map(
                |returntype: Option<TypeInfo>,
                 decl: Box<dyn AST>,
                 stmt: Box<dyn AST>|
                 -> Box<dyn AST> {
                    Box::new(FunctionDefinitionStatementAST {
                        return_type: returntype.or(Some(TypeInfo::Void)).unwrap(),
                        declarator: decl,
                        body: stmt,
                    })
                },
            );

            function_definition.borrow_mut().assign(funcdef);
        }
    }
}
