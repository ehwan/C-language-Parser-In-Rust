use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::declarator::*;
use super::expression::*;
use super::statement::*;
use super::typename::*;

use crate::token::Token;

use rusty_parser::{self as rp, IntoParser};

pub struct ASTParser {
    type_name: Rc<RefCell<rp::DynBoxSlice<(TypeInfo,), Token>>>, // ? Only Single typename, no pointer
    type_specifier: Rc<RefCell<rp::DynBoxSlice<(TypeInfo,), Token>>>, // OK
    parameter_list: Rc<RefCell<rp::DynBoxSlice<(Vec<(Option<String>, TypeInfo)>,), Token>>>, // OK
    declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Declarator>,), Token>>>, // OK
    init_declarator: Rc<RefCell<rp::DynBoxSlice<(Box<InitDeclarator>,), Token>>>, // OK
    abstract_declarator: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Declarator>,), Token>>>, // OK
    pointer: Rc<RefCell<rp::DynBoxSlice<(Vec<Token>,), Token>>>,

    expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>>, //OK
    assignment_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>>, // OK
    constant_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>>, // OK
    initializer: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>>, // OK

    statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>>, // OK
    compound_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>>, // OK
    declaration: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>>, // OK

    translation_unit: Rc<RefCell<rp::DynBoxSlice<(Box<TranslationUnit>,), Token>>>, // OK
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
            pointer: Default::default(),

            translation_unit: Default::default(),
        };

        s.expression_parser();
        s.statement_parser();
        s.declarator();
        s.type_name();
        s.translation_unit_parser();

        s
    }

    pub fn parse(&self, input: Vec<Token>) -> Box<TranslationUnit> {
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

        let consts = rp::one(Token::Const).repeat(0..);

        let type_specifier = rp::seq!(consts.clone(), type_specifier, consts).map(
            |consts: Vec<Token>, specifier: TypeInfo, consts2: Vec<Token>| -> TypeInfo {
                if consts.is_empty() == false || consts2.is_empty() == false {
                    TypeInfo::Const(Box::new(specifier))
                } else {
                    specifier
                }
            },
        );

        self.type_specifier.borrow_mut().assign(type_specifier);

        let type_name = rp::seq!(
            self.type_specifier.clone(),
            self.abstract_declarator.clone().optional()
        )
        .map(
            |specifier: TypeInfo, declarator: Option<Box<dyn Declarator>>| -> TypeInfo {
                if let Some(declarator) = declarator {
                    declarator.resolve_typeinfo(specifier).1
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
            .map(|d: Box<dyn Declarator>| -> Vec<Box<dyn Declarator>> { vec![d] })
            .reduce_left(
                rp::seq!(rp::one(Token::Comma).void(), self.declarator.clone()),
                |mut v: Vec<Box<dyn Declarator>>,
                 d: Box<dyn Declarator>|
                 -> Vec<Box<dyn Declarator>> {
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
            |specifier: TypeInfo,
             declarators: Vec<Box<dyn Declarator>>|
             -> Vec<(String, TypeInfo)> {
                let mut ret = Vec::new();
                for declarator in declarators.into_iter() {
                    let var = declarator.resolve_typeinfo(specifier.clone());
                    ret.push((var.0.expect("Variable name is required"), var.1));
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
                let mut fields: Vec<(TypeInfo, String, usize)> = Vec::new();
                let mut offset: usize = 0;
                for members in memberss.into_iter() {
                    for member in members.into_iter() {
                        fields.push((member.1.clone(), member.0.clone(), offset));
                        offset += member.1.number_of_primitives();

                        // TODO duplicate check
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
            let mut fields: Vec<(TypeInfo, String, usize)> = Vec::new();
            let mut offset: usize = 0;
            for members in memberss.into_iter() {
                for member in members.into_iter() {
                    fields.push((member.1.clone(), member.0.clone(), offset));
                    offset += member.1.number_of_primitives();

                    // TODO duplicate check
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
            pub value: Option<Box<dyn Expression>>,
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
        .map(|name: String, value: Option<Box<dyn Expression>>| Enumerator { name, value });

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
                let enum_value: i64 = if let Some(value) = enumerator.value {
                    value
                        .get_constant_i64()
                        .expect("Enumerator value must be constant expression")
                } else {
                    if last_enum_value.is_none() {
                        0
                    } else {
                        last_enum_value.unwrap() + 1
                    }
                };
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
                let enum_value: i64 = if let Some(value) = enumerator.value {
                    value
                        .get_constant_i64()
                        .expect("Enumerator value must be constant expression")
                } else {
                    if last_enum_value.is_none() {
                        0
                    } else {
                        last_enum_value.unwrap() + 1
                    }
                };
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
        let primary_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let postfix_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let unary_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let cast_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let multiplicative_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let additive_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let shift_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let relational_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let equality_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let and_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let exclusive_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let inclusive_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let logical_and_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let logical_or_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let conditional_expression: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();

        // =======================
        // Primary expression
        // =======================
        {
            let identifier = rp::check(|t: Token| -> Option<Box<dyn Expression>> {
                if let Token::Identifier(s) = t {
                    Some(Box::new(PrimaryIdentifier { name: s }))
                } else {
                    None
                }
            });
            let integer_constant = rp::check(|t: Token| -> Option<Box<dyn Expression>> {
                match t {
                    Token::ConstantUnsignedInteger(i) => {
                        Some(Box::new(ConstantUnsignedInteger { value: i }))
                    }
                    Token::ConstantInteger(i) => Some(Box::new(ConstantInteger { value: i })),
                    Token::ConstantCharacter(ch) => Some(Box::new(ConstantCharacter { value: ch })),
                    Token::ConstantLong(l) => Some(Box::new(ConstantLong { value: l })),
                    Token::ConstantUnsignedLong(l) => {
                        Some(Box::new(ConstantUnsignedLong { value: l }))
                    }
                    _ => None,
                }
            });
            let float_constant = rp::check(|t: Token| -> Option<Box<dyn Expression>> {
                match t {
                    Token::ConstantFloat(f) => Some(Box::new(ConstantFloat { value: f })),
                    Token::ConstantDouble(d) => Some(Box::new(ConstantDouble { value: d })),
                    _ => None,
                }
            });
            let string_literal = rp::check(|t: Token| -> Option<Box<dyn Expression>> {
                match t {
                    Token::StringLiteral(s) => Some(Box::new(StringLiteral { value: s })),
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
                Bracket(Box<dyn Expression>),
                Paren(Vec<Box<dyn Expression>>),
                Dot(String),
                Arrow(String),
                Inc,
                Dec,
            }

            let bracket = rp::seq!(
                rp::one(Token::LeftBracket).void(),
                self.expression.clone(),
                rp::one(Token::RightBracket).void().or_else(|| -> () {
                    panic!("Expected ']' after index expression");
                })
            )
            .map(|e: Box<dyn Expression>| PostfixType::Bracket(e));

            let argument_list1 = self
                .assignment_expression
                .clone()
                .map(|e| -> Vec<Box<dyn Expression>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(
                        rp::one(Token::Comma).void(),
                        self.assignment_expression.clone()
                    ),
                    |mut v: Vec<Box<dyn Expression>>,
                     e: Box<dyn Expression>|
                     -> Vec<Box<dyn Expression>> {
                        v.push(e);
                        v
                    },
                );
            let argument_list0 = argument_list1.optional().map(
                |args: Option<Vec<Box<dyn Expression>>>| -> Vec<Box<dyn Expression>> {
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
                rp::one(Token::RightParen).void().or_else(|| -> () {
                    panic!("Expected ')' for function call");
                })
            )
            .map(|args| PostfixType::Paren(args));

            let dot = rp::seq!(
                rp::one(Token::Dot).void(),
                rp::check(|t: Token| -> Option<String> {
                    if let Token::Identifier(s) = t {
                        Some(s)
                    } else {
                        panic!("Expected identifier after '.'");
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
                        panic!("Expected identifier after '->'");
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
                    |lhs: Box<dyn Expression>, rhs: PostfixType| -> Box<dyn Expression> {
                        match rhs {
                            PostfixType::Bracket(e) => Box::new(PostBracket { src: lhs, index: e }),
                            PostfixType::Paren(args) => Box::new(PostParen { src: lhs, args }),
                            PostfixType::Dot(s) => Box::new(PostMember {
                                src: lhs,
                                member: s,
                            }),
                            PostfixType::Arrow(s) => Box::new(PostArrow {
                                src: lhs,
                                member: s,
                            }),
                            PostfixType::Inc => Box::new(PostIncrement { src: lhs }),
                            PostfixType::Dec => Box::new(PostDecrement { src: lhs }),
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
                rp::one(Token::LeftParen).void().or_else(|| -> () {
                    panic!("Expected '(' after sizeof");
                }),
                self.type_name.clone().or_else(|| -> TypeInfo {
                    panic!("Expected type name after sizeof(");
                }),
                rp::one(Token::RightParen).void().or_else(|| -> () {
                    panic!("Expected ')' after sizeof(type)");
                })
            )
            .map(|typeinfo: TypeInfo| -> Box<dyn Expression> { Box::new(SizeofType { typeinfo }) });

            let sizeof_expr = rp::seq!(rp::one(Token::Sizeof).void(), unary_expression.clone())
                .map(|expr: Box<dyn Expression>| -> Box<dyn Expression> {
                    Box::new(SizeofExpr { expr })
                });

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
                    |op: UnaryOperator, expr: Box<dyn Expression>| -> Box<dyn Expression> {
                        Box::new(UnaryExpression { op, src: expr })
                    }
                ),
                rp::seq!(rp::one(Token::IncOp).void(), unary_expression.clone()).map(
                    |expr: Box<dyn Expression>| -> Box<dyn Expression> {
                        Box::new(UnaryExpression {
                            op: UnaryOperator::Increment,
                            src: expr,
                        })
                    }
                ),
                rp::seq!(rp::one(Token::DecOp).void(), unary_expression.clone()).map(
                    |expr: Box<dyn Expression>| -> Box<dyn Expression> {
                        Box::new(UnaryExpression {
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
                    |typeinfo: TypeInfo,
                     cast_expression: Box<dyn Expression>|
                     -> Box<dyn Expression> {
                        Box::new(CastExpression {
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(MultiplicativeExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(AdditiveExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(ShiftExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(ComparisonExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(ComparisonExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(BitwiseExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(BitwiseExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(BitwiseExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(LogicalBinaryExpression { op, lhs, rhs })
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
                |lhs: Box<dyn Expression>,
                 op: BinaryOperator,
                 rhs: Box<dyn Expression>|
                 -> Box<dyn Expression> {
                    Box::new(LogicalBinaryExpression { op, lhs, rhs })
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
                    self.expression.clone().or_else(|| -> Box<dyn Expression> {
                        panic!("Invalid expression after '?' in conditional expression");
                    }),
                    rp::one(Token::Colon).void().or_else(|| -> () {
                        panic!("Colon ':' expected after '?' in conditional expression");
                    }),
                    conditional_expression
                        .clone()
                        .or_else(|| -> Box<dyn Expression> {
                            panic!("Invalid expression after ':' in conditional expression");
                        })
                )
                .optional()
            )
            .map(
                |cond: Box<dyn Expression>,
                 truefalse: Option<(Box<dyn Expression>, Box<dyn Expression>)>|
                 -> Box<dyn Expression> {
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
                    |lhs: Box<dyn Expression>,
                     op: BinaryOperator,
                     rhs: Box<dyn Expression>|
                     -> Box<dyn Expression> {
                        if op == BinaryOperator::Assign {
                            Box::new(AssignExpression { lhs, rhs })
                        } else {
                            Box::new(AssignOpExpression { op, lhs, rhs })
                        }
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
                |lhs: Box<dyn Expression>, rhs: Box<dyn Expression>| -> Box<dyn Expression> {
                    Box::new(CommaExpression { lhs, rhs })
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
                .map(|e: Box<dyn Expression>| -> Vec<Box<dyn Expression>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), self.initializer.clone()),
                    |mut v: Vec<Box<dyn Expression>>,
                     e: Box<dyn Expression>|
                     -> Vec<Box<dyn Expression>> {
                        v.push(e);
                        v
                    },
                )
                .map(|v: Vec<Box<dyn Expression>>| -> Box<dyn Expression> {
                    Box::new(InitializerListExpression { initializers: v })
                });

            let initializer_ = rp::or!(
                rp::seq!(
                    rp::one(Token::LeftBrace).void(),
                    initalizer_list,
                    rp::one(Token::Comma).optional().void(),
                    rp::one(Token::RightBrace).void()
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
            identifier.clone().map(|s: String| -> Box<dyn Declarator> {
                Box::new(IdentifierDeclarator { name: s })
            }),
            rp::seq!(
                rp::one(Token::LeftParen).void(),
                self.declarator.clone(),
                rp::one(Token::RightParen).void()
            )
        );
        enum DirectDeclaratorType {
            Array(Option<Box<dyn Expression>>),        // length
            Function(Vec<(Option<String>, TypeInfo)>), // parameters
        }
        let direct_declarator = direct_declarator_leaf.reduce_left(
            rp::or!(
                rp::seq!(
                    rp::one(Token::LeftBracket).void(),
                    self.constant_expression.clone(),
                    rp::one(Token::RightBracket).void()
                )
                .map(|len: Box<dyn Expression>| -> DirectDeclaratorType {
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
                    |params: Vec<(Option<String>, TypeInfo)>| -> DirectDeclaratorType {
                        DirectDeclaratorType::Function(params)
                    }
                )
            ),
            |lhs: Box<dyn Declarator>, op: DirectDeclaratorType| -> Box<dyn Declarator> {
                match op {
                    DirectDeclaratorType::Array(Some(len)) => {
                        Box::new(DirectArrayFixedDeclarator {
                            declarator: lhs,
                            size: len,
                        })
                    }
                    DirectDeclaratorType::Array(None) => {
                        Box::new(DirectArrayUnboundedDeclarator { declarator: lhs })
                    }
                    DirectDeclaratorType::Function(params) => Box::new(DirectFunctionDeclarator {
                        declarator: lhs,
                        params,
                    }),
                }
            },
        );
        /*
        pointer
        : '*'
        | '*' CONST pointer
        | '*' CONST
        | '*' pointer
        ;
        */
        let pointer_ = rp::seq!(
            rp::one(Token::Star).void(),
            rp::or!(
                rp::seq!(rp::one(Token::Const).void(), self.pointer.clone()).map(
                    |mut decorators: Vec<Token>| -> Vec<Token> {
                        let mut ret = Vec::new();
                        ret.push(Token::Const);
                        ret.append(&mut decorators);
                        ret
                    }
                ),
                rp::one(Token::Const).map(|t| -> Vec<Token> { vec![t] }),
                self.pointer.clone()
            )
            .optional()
        )
        .map(|decorators: Option<Vec<Token>>| -> Vec<Token> {
            if let Some(mut decorators) = decorators {
                let mut ret = Vec::new();
                ret.push(Token::Star);
                ret.append(&mut decorators);
                ret
            } else {
                vec![Token::Star]
            }
        });
        self.pointer.borrow_mut().assign(pointer_);

        /*
        declarator
        : pointer direct_declarator
        | direct_declarator
        ;
        */

        let declarator_ = rp::or!(
            rp::seq!(self.pointer.clone(), direct_declarator.clone()).map(
                |ptr: Vec<Token>, mut decl: Box<dyn Declarator>| -> Box<dyn Declarator> {
                    for t in ptr.iter() {
                        match t {
                            Token::Star => decl = Box::new(PointerDeclarator { declarator: decl }),
                            Token::Const => decl = Box::new(ConstDeclarator { declarator: decl }),
                            _ => unreachable!("Invalid token in pointer or const"),
                        }
                    }
                    decl
                }
            ),
            direct_declarator.clone()
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
            |decl: Box<dyn Declarator>, init: Option<Box<dyn Expression>>| -> Box<InitDeclarator> {
                Box::new(InitDeclarator {
                    declarator: decl,
                    initializer: init,
                })
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
                Array(Option<Box<dyn Expression>>),
                Function(Vec<(Option<String>, TypeInfo)>),
            }
            let direct_abstract_declarator: Rc<
                RefCell<rp::DynBoxSlice<(Box<dyn Declarator>,), Token>>,
            > = Default::default();

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
                .map(|len: Box<dyn Expression>| -> DirectAbstractDeclaratorLeaf {
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
                    |params: Vec<(Option<String>, TypeInfo)>| -> DirectAbstractDeclaratorLeaf {
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
                    |op: DirectAbstractDeclaratorLeaf| -> Box<dyn Declarator> {
                        match op {
                            DirectAbstractDeclaratorLeaf::Array(len) => {
                                if let Some(len) = len {
                                    Box::new(AbstractArrayFixedDeclarator {
                                        declarator: None,
                                        size: len,
                                    })
                                } else {
                                    Box::new(AbstractArrayUnboundedDeclarator { declarator: None })
                                }
                            }
                            DirectAbstractDeclaratorLeaf::Function(params) => {
                                Box::new(AbstractFunctionDeclarator {
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
                |lhs: Box<dyn Declarator>,
                 op: DirectAbstractDeclaratorLeaf|
                 -> Box<dyn Declarator> {
                    match op {
                        DirectAbstractDeclaratorLeaf::Array(len) => {
                            if let Some(len) = len {
                                Box::new(AbstractArrayFixedDeclarator {
                                    declarator: Some(lhs),
                                    size: len,
                                })
                            } else {
                                Box::new(AbstractArrayUnboundedDeclarator {
                                    declarator: Some(lhs),
                                })
                            }
                        }
                        DirectAbstractDeclaratorLeaf::Function(params) => {
                            Box::new(AbstractFunctionDeclarator {
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
            let pointered = rp::seq!(
                self.pointer.clone(),
                direct_abstract_declarator.clone().optional()
            )
            .map(
                |ptr: Vec<Token>, decl: Option<Box<dyn Declarator>>| -> Box<dyn Declarator> {
                    let mut decl = decl;
                    for t in ptr.iter() {
                        match t {
                            Token::Star => {
                                decl =
                                    Some(Box::new(AbstractPointerDeclarator { declarator: decl }));
                            }
                            Token::Const => {
                                decl = Some(Box::new(AbstractConstDeclarator { declarator: decl }));
                            }
                            _ => unreachable!("Invalid token in pointer"),
                        }
                    }
                    decl.unwrap()
                },
            );
            let abstract_declarator_ = rp::or!(pointered, direct_abstract_declarator.clone());
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
        let labeled_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>> =
            Default::default();
        let expression_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Expression>,), Token>>> =
            Default::default();
        let selection_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>> =
            Default::default();
        let iteration_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>> =
            Default::default();
        let jump_statement: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>> =
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
                     declarator: Option<Box<dyn Declarator>>|
                     -> Vec<(TypeInfo, Option<Box<dyn Declarator>>)> {
                        vec![(typeinfo, declarator)]
                    },
                )
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), parameter_declaration),
                    |mut v: Vec<(TypeInfo, Option<Box<dyn Declarator>>)>,
                     typeinfo: TypeInfo,
                     declarator: Option<Box<dyn Declarator>>|
                     -> Vec<(TypeInfo, Option<Box<dyn Declarator>>)> {
                        v.push((typeinfo, declarator));
                        v
                    },
                )
                .map(
                    |v: Vec<(TypeInfo, Option<Box<dyn Declarator>>)>| -> Vec<(Option<String>, TypeInfo)> {
                        let mut ret = Vec::new();
                        for (typeinfo, declarator) in v.into_iter() {
                            if let Some(declarator) = declarator {
                                let (name, typeinfo_) = declarator
                                    .resolve_typeinfo(typeinfo.clone());
                                ret.push((name, typeinfo_));
                            } else {
                                ret.push((None, typeinfo));
                            }
                        }
                        ret
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
                expression_statement
                    .clone()
                    .map(|expr| -> Box<dyn Statement> {
                        Box::new(ExpressionStatement { expression: expr })
                    }),
                self.declaration.clone(),
                labeled_statement.clone(),
                self.compound_statement.clone(),
                selection_statement.clone(),
                iteration_statement.clone(),
                jump_statement.clone()
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
                    self.statement.clone().or_else(|| -> Box<dyn Statement> {
                        panic!("Invalid Statement after label");
                    })
                )
                .map(
                    |s: String, stmt: Box<dyn Statement>| -> Box<dyn Statement> {
                        Box::new(LabeledStatement {
                            label: s,
                            statement: stmt,
                        })
                    }
                ),
                rp::seq!(
                    rp::one(Token::Case).void(),
                    self.constant_expression
                        .clone()
                        .or_else(|| -> Box<dyn Expression> {
                            panic!("Invalid expression after 'case'");
                        }),
                    rp::one(Token::Colon).void().or_else(|| -> () {
                        panic!(": is expected after 'case'");
                    }),
                    self.statement.clone().or_else(|| -> Box<dyn Statement> {
                        panic!("Invalid Statement after 'case'");
                    })
                )
                .map(
                    |expr: Box<dyn Expression>, stmt: Box<dyn Statement>| -> Box<dyn Statement> {
                        Box::new(CaseStatement {
                            value: expr,
                            statement: stmt,
                        })
                    }
                ),
                rp::seq!(
                    rp::one(Token::Default).void(),
                    rp::one(Token::Colon).void().or_else(|| -> () {
                        panic!(": is expected after 'default'");
                    }),
                    self.statement.clone().or_else(|| -> Box<dyn Statement> {
                        panic!("Invalid Statement after 'default'");
                    })
                )
                .map(|stmt: Box<dyn Statement>| -> Box<dyn Statement> {
                    Box::new(DefaultStatement { statement: stmt })
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
                    |compound: Vec<Box<dyn Statement>>| -> Box<dyn Statement> {
                        Box::new(CompoundStatement {
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
            .map(|expr: Option<Box<dyn Expression>>| -> Box<dyn Expression> {
                if let Some(expr) = expr {
                    expr
                } else {
                    Box::new(VoidExpression {})
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
                    rp::one(Token::LeftParen).void().or_else(|| -> () {
                        panic!("'(' is expected after 'if'");
                    }),
                    self.expression.clone().or_else(|| -> Box<dyn Expression> {
                        panic!("Invalid expression after 'if'");
                    }),
                    rp::one(Token::RightParen).void().or_else(|| -> () {
                        panic!("')' is expected after 'if'");
                    }),
                    self.statement.clone().or_else(|| -> Box<dyn Statement> {
                        panic!("Invalid body statement after 'if'");
                    }),
                    rp::seq!(
                        rp::one(Token::Else).void(),
                        self.statement.clone().or_else(|| -> Box<dyn Statement> {
                            panic!("Invalid body statement after 'else'");
                        })
                    )
                    .optional()
                )
                .map(
                    |cond: Box<dyn Expression>,
                     stmt: Box<dyn Statement>,
                     else_stmt: Option<Box<dyn Statement>>|
                     -> Box<dyn Statement> {
                        Box::new(IfStatement {
                            cond,
                            then_statement: stmt,
                            else_statement: else_stmt,
                        })
                    }
                ),
                rp::seq!(
                    rp::one(Token::Switch).void(),
                    rp::one(Token::LeftParen).void().or_else(|| -> () {
                        panic!("'(' is expected after 'switch'");
                    }),
                    self.expression.clone().or_else(|| -> Box<dyn Expression> {
                        panic!("Invalid expression after 'switch'");
                    }),
                    rp::one(Token::RightParen).void().or_else(|| -> () {
                        panic!("')' is expected after 'switch'");
                    }),
                    self.statement.clone().or_else(|| -> Box<dyn Statement> {
                        panic!("Invalid body statement after 'switch'");
                    })
                )
                .map(
                    |target: Box<dyn Expression>, stmt: Box<dyn Statement>| -> Box<dyn Statement> {
                        Box::new(SwitchStatement {
                            target,
                            statement: stmt,
                        })
                    }
                )
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
                rp::one(Token::LeftParen).void().or_else(|| -> () {
                    panic!("'(' is expected after 'while'");
                }),
                self.expression.clone().or_else(|| -> Box<dyn Expression> {
                    panic!("Invalid expression after 'while'");
                }),
                rp::one(Token::RightParen).void().or_else(|| -> () {
                    panic!("')' is expected after 'while'");
                }),
                self.statement.clone().or_else(|| -> Box<dyn Statement> {
                    panic!("Invalid body statement after 'while'");
                })
            )
            .map(
                |cond: Box<dyn Expression>, stmt: Box<dyn Statement>| -> Box<dyn Statement> {
                    Box::new(WhileStatement {
                        cond,
                        statement: stmt,
                    })
                },
            );

            let do_while_statement = rp::seq!(
                rp::one(Token::Do).void(),
                self.statement.clone().or_else(|| -> Box<dyn Statement> {
                    panic!("Invalid body statement for 'do-while'");
                }),
                rp::one(Token::While).void(),
                rp::one(Token::LeftParen).void().or_else(|| -> () {
                    panic!("'(' is expected after 'do-while'");
                }),
                self.expression.clone().or_else(|| -> Box<dyn Expression> {
                    panic!("Invalid expression after 'do-while'");
                }),
                rp::one(Token::RightParen).void().or_else(|| -> () {
                    panic!("')' is expected after 'do-while'");
                }),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'do-while'");
                })
            )
            .map(
                |stmt: Box<dyn Statement>, cond: Box<dyn Expression>| -> Box<dyn Statement> {
                    Box::new(DoWhileStatement {
                        cond,
                        statement: stmt,
                    })
                },
            );

            let for_statement = rp::seq!(
                rp::one(Token::For).void(),
                rp::one(Token::LeftParen).void().or_else(|| -> () {
                    panic!("'(' is expected after 'for'");
                }),
                expression_statement
                    .clone()
                    .or_else(|| -> Box<dyn Expression> {
                        panic!("Invalid 1st statement for 'for' statement");
                    }),
                expression_statement
                    .clone()
                    .or_else(|| -> Box<dyn Expression> {
                        panic!("Invalid 2nd statement for 'for' statement");
                    }),
                self.expression.clone().optional(),
                rp::one(Token::RightParen).void().or_else(|| -> () {
                    panic!("')' is expected after 'for'");
                }),
                self.statement.clone().or_else(|| -> Box<dyn Statement> {
                    panic!("Invalid body statement for 'for' statement");
                })
            )
            .map(
                |init: Box<dyn Expression>,
                 cond: Box<dyn Expression>,
                 next: Option<Box<dyn Expression>>,
                 stmt: Box<dyn Statement>|
                 -> Box<dyn Statement> {
                    Box::new(ForStatement {
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
                identifier.or_else(|| -> String {
                    panic!("IDENTIFIER label must be followed by 'goto'");
                }),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'goto'");
                })
            )
            .map(|s: String| -> Box<dyn Statement> { Box::new(GotoStatement { label: s }) });

            let continue_statement = rp::seq!(
                rp::one(Token::Continue).void(),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'continue'");
                })
            )
            .map(|| -> Box<dyn Statement> { Box::new(ContinueStatement {}) });

            let break_statement = rp::seq!(
                rp::one(Token::Break).void(),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'break'");
                })
            )
            .map(|| -> Box<dyn Statement> { Box::new(BreakStatement {}) });

            let return_statement = rp::seq!(
                rp::one(Token::Return).void(),
                self.expression.clone().optional(),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'return'");
                })
            )
            .map(|expr: Option<Box<dyn Expression>>| -> Box<dyn Statement> {
                Box::new(ReturnStatement { expr })
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
                .map(|e: Box<InitDeclarator>| -> Vec<Box<InitDeclarator>> {
                    let mut ret = Vec::new();
                    ret.push(e);
                    ret
                })
                .reduce_left(
                    rp::seq!(rp::one(Token::Comma).void(), self.init_declarator.clone()),
                    |mut v: Vec<Box<InitDeclarator>>,
                     e: Box<InitDeclarator>|
                     -> Vec<Box<InitDeclarator>> {
                        v.push(e);
                        v
                    },
                );
            /*
            declaration
            : specifier_list ';'
            | specifier_list init_declarator_list ';'
            | TYPEDEF specifier_list init_declarator_list ';'
            ;
            */
            let declaration = rp::seq!(
                self.type_specifier.clone(),
                init_declarator_list.clone().optional(),
                rp::one(Token::SemiColon).void()
            )
            .map(
                |typeinfo: TypeInfo,
                 decls: Option<Vec<Box<InitDeclarator>>>|
                 -> Box<dyn Statement> {
                    if let Some(decls) = decls {
                        // variable definition
                        let mut ret = Vec::with_capacity(decls.len());
                        for decl in decls.into_iter() {
                            let (name, typeinfo_) = decl.resolve_typeinfo(typeinfo.clone());
                            ret.push((
                                name.expect("Declaration must have name"),
                                typeinfo_,
                                decl.initializer,
                            ));
                        }
                        // check if it's a function declaration
                        if ret.len() == 1 {
                            if let TypeInfo::Function(return_type, params) = &ret[0].1 {
                                return Box::new(FunctionDeclaration {
                                    name: ret[0].0.clone(),
                                    return_type: *return_type.clone(),
                                    params: params.clone(),
                                });
                            }
                        }
                        Box::new(DeclarationStatement { vars: ret })
                    } else {
                        // type definition
                        Box::new(TypeDefinition { typeinfo })
                    }
                },
            );
            let typedef_declaration = rp::seq!(
                rp::one(Token::Typedef).void(),
                self.type_specifier.clone().or_else(|| -> TypeInfo {
                    panic!("Typedef must have a type specifier");
                }),
                init_declarator_list
                    .clone()
                    .or_else(|| -> Vec<Box<InitDeclarator>> {
                        panic!("Typedef must have a declarator");
                    }),
                rp::one(Token::SemiColon).void().or_else(|| -> () {
                    panic!("';' is expected after 'typedef'");
                })
            )
            .map(
                |typeinfo: TypeInfo, decls: Vec<Box<InitDeclarator>>| -> Box<dyn Statement> {
                    if decls.len() != 1 {
                        panic!("Typedef declaration must have exactly one declarator");
                    }
                    let decl = &decls[0];
                    if decl.initializer.is_some() {
                        panic!("Typedef declaration cannot have initializer");
                    }
                    let (name, typeinfo) = decl.declarator.resolve_typeinfo(typeinfo);

                    Box::new(TypedefStatement {
                        name: name.expect("Typedef must have a target name"),
                        typeinfo,
                    })
                },
            );
            self.declaration
                .borrow_mut()
                .assign(rp::or!(declaration, typedef_declaration));
        }
    }

    fn translation_unit_parser(&mut self) {
        let function_definition: Rc<RefCell<rp::DynBoxSlice<(Box<dyn Statement>,), Token>>> =
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
                |decls: Vec<Box<dyn Statement>>| -> Box<TranslationUnit> {
                    Box::new(TranslationUnit { statements: decls })
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
                 decl: Box<dyn Declarator>,
                 stmt: Box<dyn Statement>|
                 -> Box<dyn Statement> {
                    let direct_func_decl = decl
                        .as_any()
                        .downcast_ref::<DirectFunctionDeclarator>()
                        .expect("Function definition must have a function declarator");
                    let name = &direct_func_decl
                        .declarator
                        .as_any()
                        .downcast_ref::<IdentifierDeclarator>()
                        .expect("Function definition must have a function declarator with an identifier").name;
                    Box::new(FunctionDefinitionStatement {
                        return_type: returntype.or(Some(TypeInfo::Void)).unwrap(),
                        name: name.clone(),
                        params: direct_func_decl.params.clone(),
                        body: stmt,
                    })
                },
            );

            function_definition.borrow_mut().assign(funcdef);
        }
    }
}
