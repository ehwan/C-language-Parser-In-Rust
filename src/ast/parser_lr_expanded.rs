
// ================================User Codes Begin================================
#![allow(non_camel_case_types)]
use super::declarator;
use super::declarator::Declarator;
use super::expression;
use super::expression::Expression;
use super::statement;
use super::statement::Statement;
use crate::token::Token;

// =================================User Codes End=================================
/*
====================================Grammar=====================================

# of terminal classes: 88
# of states: 341

0: Constant -> constant_character
1: Constant -> constant_integer
2: Constant -> constant_long
3: Constant -> constant_unsigned_integer
4: Constant -> constant_unsigned_long
5: Constant -> constant_float
6: Constant -> constant_double
7: primary_expression -> ident
8: primary_expression -> Constant
9: primary_expression -> string_literal
10: primary_expression -> lparen expression rparen
11: postfix_expression -> primary_expression
12: postfix_expression -> postfix_expression lbracket expression rbracket
13: postfix_expression -> postfix_expression lparen rparen
14: postfix_expression -> postfix_expression lparen argument_expression_list rparen
15: postfix_expression -> postfix_expression dot ident
16: postfix_expression -> postfix_expression ptr_op ident
17: postfix_expression -> postfix_expression inc_op
18: postfix_expression -> postfix_expression dec_op
19: argument_expression_list -> assignment_expression
20: argument_expression_list -> argument_expression_list comma assignment_expression
21: unary_expression -> postfix_expression
22: unary_expression -> inc_op unary_expression
23: unary_expression -> dec_op unary_expression
24: unary_expression -> ampersand cast_expression
25: unary_expression -> star cast_expression
26: unary_expression -> plus cast_expression
27: unary_expression -> minus cast_expression
28: unary_expression -> tilde cast_expression
29: unary_expression -> exclamation cast_expression
30: unary_expression -> sizeof unary_expression
31: unary_expression -> sizeof lparen type_name rparen
32: cast_expression -> unary_expression
33: cast_expression -> lparen type_name rparen cast_expression
34: multiplicative_expression -> cast_expression
35: multiplicative_expression -> multiplicative_expression star cast_expression
36: multiplicative_expression -> multiplicative_expression slash cast_expression
37: multiplicative_expression -> multiplicative_expression percent cast_expression
38: additive_expression -> multiplicative_expression
39: additive_expression -> additive_expression plus multiplicative_expression
40: additive_expression -> additive_expression minus multiplicative_expression
41: shift_expression -> additive_expression
42: shift_expression -> shift_expression left_op additive_expression
43: shift_expression -> shift_expression right_op additive_expression
44: relational_expression -> shift_expression
45: relational_expression -> relational_expression less shift_expression
46: relational_expression -> relational_expression greater shift_expression
47: relational_expression -> relational_expression le shift_expression
48: relational_expression -> relational_expression ge shift_expression
49: equality_expression -> relational_expression
50: equality_expression -> equality_expression eq relational_expression
51: equality_expression -> equality_expression ne relational_expression
52: and_expression -> equality_expression
53: and_expression -> and_expression ampersand equality_expression
54: exclusive_or_expression -> and_expression
55: exclusive_or_expression -> exclusive_or_expression caret and_expression
56: inclusive_or_expression -> exclusive_or_expression
57: inclusive_or_expression -> inclusive_or_expression pipe exclusive_or_expression
58: logical_and_expression -> inclusive_or_expression
59: logical_and_expression -> logical_and_expression and_op inclusive_or_expression
60: logical_or_expression -> logical_and_expression
61: logical_or_expression -> logical_or_expression or_op logical_and_expression
62: conditional_expression -> logical_or_expression
63: conditional_expression -> logical_or_expression question expression colon conditional_expression
64: assignment_expression -> conditional_expression
65: assignment_expression -> unary_expression assign assignment_expression
66: assignment_expression -> unary_expression mul_assign assignment_expression
67: assignment_expression -> unary_expression div_assign assignment_expression
68: assignment_expression -> unary_expression mod_assign assignment_expression
69: assignment_expression -> unary_expression add_assign assignment_expression
70: assignment_expression -> unary_expression sub_assign assignment_expression
71: assignment_expression -> unary_expression left_assign assignment_expression
72: assignment_expression -> unary_expression right_assign assignment_expression
73: assignment_expression -> unary_expression and_assign assignment_expression
74: assignment_expression -> unary_expression xor_assign assignment_expression
75: assignment_expression -> unary_expression or_assign assignment_expression
76: expression -> assignment_expression
77: expression -> expression comma assignment_expression
78: initializer -> assignment_expression
79: initializer -> lbrace initializer_list rbrace
80: initializer -> lbrace initializer_list comma rbrace
81: initializer_list -> initializer
82: initializer_list -> initializer_list comma initializer
83: labeled_statement -> ident colon statement
84: labeled_statement -> case conditional_expression colon statement
85: labeled_statement -> default colon statement
86: statement_or_declaration -> statement
87: statement_or_declaration -> declaration
88: compound_statement -> lbrace statement_or_declaration* rbrace
89: expression_statement -> semicolon
90: expression_statement -> expression semicolon
91: selection_statement -> if_ lparen expression rparen statement
92: selection_statement -> if_ lparen expression rparen statement else_ statement
93: selection_statement -> switch lparen expression rparen statement
94: declaration_or_expression -> declaration
95: declaration_or_expression -> expression_statement
96: iteration_statement -> while_ lparen expression rparen statement
97: iteration_statement -> do_ statement while_ lparen expression rparen semicolon
98: iteration_statement -> for_ lparen declaration_or_expression expression_statement rparen statement
99: iteration_statement -> for_ lparen declaration_or_expression expression_statement expression rparen statement
100: jump_statement -> goto_ ident semicolon
101: jump_statement -> continue_ semicolon
102: jump_statement -> break_ semicolon
103: jump_statement -> return_ semicolon
104: jump_statement -> return_ expression semicolon
105: declaration -> declaration_specifier+ semicolon
106: declaration -> declaration_specifier+ init_declarator_list semicolon
107: function_definition -> declaration_specifier+ declarator compound_statement
108: function_definition -> declarator compound_statement
109: statement -> labeled_statement
110: statement -> compound_statement
111: statement -> expression_statement
112: statement -> selection_statement
113: statement -> iteration_statement
114: statement -> jump_statement
115: external_declaration -> function_definition
116: external_declaration -> declaration
117: translation_unit -> external_declaration*
118: type_qualifier -> const_
119: type_qualifier -> volatile
120: declarator -> direct_declarator
121: declarator -> star type_qualifier* declarator
122: direct_declarator -> ident
123: direct_declarator -> lparen declarator rparen
124: direct_declarator -> direct_declarator lbracket conditional_expression rbracket
125: direct_declarator -> direct_declarator lbracket rbracket
126: direct_declarator -> direct_declarator lparen parameter_type_list rparen
127: direct_declarator -> direct_declarator lparen rparen
128: abstract_declarator -> star type_qualifier* abstract_declarator
129: abstract_declarator -> star type_qualifier*
130: abstract_declarator -> direct_abstract_declarator
131: direct_abstract_declarator -> lparen abstract_declarator rparen
132: direct_abstract_declarator -> lbracket rbracket
133: direct_abstract_declarator -> lbracket conditional_expression rbracket
134: direct_abstract_declarator -> direct_abstract_declarator lbracket rbracket
135: direct_abstract_declarator -> direct_abstract_declarator lbracket conditional_expression rbracket
136: direct_abstract_declarator -> lparen rparen
137: direct_abstract_declarator -> lparen parameter_type_list rparen
138: direct_abstract_declarator -> direct_abstract_declarator lparen rparen
139: direct_abstract_declarator -> direct_abstract_declarator lparen parameter_type_list rparen
140: specifier_qualifier -> type_qualifier
141: specifier_qualifier -> type_specifier
142: type_name -> specifier_qualifier+ abstract_declarator?
143: type_specifier -> void_
144: type_specifier -> char_
145: type_specifier -> short_
146: type_specifier -> int_
147: type_specifier -> long_
148: type_specifier -> float_
149: type_specifier -> double_
150: type_specifier -> signed
151: type_specifier -> unsigned
152: type_specifier -> ident
153: type_specifier -> struct_or_union_specifier
154: type_specifier -> enum_specifier
155: storage_class_specifier -> typedef
156: storage_class_specifier -> extern_
157: storage_class_specifier -> static_
158: storage_class_specifier -> auto
159: storage_class_specifier -> register
160: declaration_specifier -> storage_class_specifier
161: declaration_specifier -> type_specifier
162: declaration_specifier -> type_qualifier
163: parameter_declaration -> declaration_specifier+ declarator
164: parameter_declaration -> declaration_specifier+ abstract_declarator
165: parameter_declaration -> declaration_specifier+
166: parameter_list -> parameter_declaration
167: parameter_list -> parameter_list comma parameter_declaration
168: parameter_type_list -> parameter_list
169: parameter_type_list -> parameter_list comma ellipsis
170: struct_or_union -> struct_
171: struct_or_union -> union_
172: struct_declarator -> declarator
173: struct_declarator -> declarator? colon conditional_expression
174: struct_declarator_list -> struct_declarator
175: struct_declarator_list -> struct_declarator_list comma struct_declarator
176: struct_declaration -> specifier_qualifier+ struct_declarator_list semicolon
177: struct_or_union_specifier -> struct_or_union ident? lbrace struct_declaration* rbrace
178: struct_or_union_specifier -> struct_or_union ident
179: enum_specifier -> enum_ ident? lbrace enumerator_list rbrace
180: enum_specifier -> enum_ ident
181: enumerator_list -> enumerator
182: enumerator_list -> enumerator_list comma enumerator
183: enumerator -> ident
184: enumerator -> ident assign conditional_expression
185: init_declarator -> declarator
186: init_declarator -> declarator assign initializer
187: init_declarator_list -> init_declarator
188: init_declarator_list -> init_declarator_list comma init_declarator
189: statement_or_declaration+ -> statement_or_declaration
190: statement_or_declaration+ -> statement_or_declaration+ statement_or_declaration
191: statement_or_declaration* -> statement_or_declaration+
192: statement_or_declaration* -> 
193: declaration_specifier+ -> declaration_specifier
194: declaration_specifier+ -> declaration_specifier+ declaration_specifier
195: external_declaration+ -> external_declaration
196: external_declaration+ -> external_declaration+ external_declaration
197: external_declaration* -> external_declaration+
198: external_declaration* -> 
199: type_qualifier+ -> type_qualifier
200: type_qualifier+ -> type_qualifier+ type_qualifier
201: type_qualifier* -> type_qualifier+
202: type_qualifier* -> 
203: specifier_qualifier+ -> specifier_qualifier
204: specifier_qualifier+ -> specifier_qualifier+ specifier_qualifier
205: abstract_declarator? -> abstract_declarator
206: abstract_declarator? -> 
207: declarator? -> declarator
208: declarator? -> 
209: ident? -> ident
210: ident? -> 
211: struct_declaration+ -> struct_declaration
212: struct_declaration+ -> struct_declaration+ struct_declaration
213: struct_declaration* -> struct_declaration+
214: struct_declaration* -> 
215: Augmented -> VirtualStart(0) translation_unit eof

*/
// =============================Generated Codes Begin==============================
#[allow(non_camel_case_types, dead_code)]
pub type translation_unitContext = ::rusty_lr::parser::nondeterministic::Context<
    Parser,
    Data,
    translation_unitExtracter,
    u16,
    2usize,
>;
#[allow(non_camel_case_types, dead_code)]
pub type Rule = ::rusty_lr::production::Production<TerminalClasses, NonTerminals>;
#[allow(non_camel_case_types, dead_code)]
pub type Tables = ::rusty_lr::parser::table::DenseFlatTables<
    TerminalClasses,
    NonTerminals,
    ::rusty_lr::parser::table::ArrayVec<u8, 2usize>,
    u16,
>;
#[allow(non_camel_case_types, dead_code)]
pub type ParseError = ::rusty_lr::parser::nondeterministic::ParseError<
    Token,
    ::rusty_lr::DefaultLocation,
    ::rusty_lr::DefaultReduceActionError,
>;
/// A enum that represents terminal classes
#[allow(non_camel_case_types, dead_code)]
#[derive(
    Clone,
    Copy,
    std::hash::Hash,
    std::cmp::PartialEq,
    std::cmp::Eq,
    std::cmp::PartialOrd,
    std::cmp::Ord
)]
#[repr(usize)]
pub enum TerminalClasses {
    ident,
    lparen,
    rparen,
    string_literal,
    constant_character,
    constant_integer,
    constant_long,
    constant_unsigned_integer,
    constant_unsigned_long,
    constant_float,
    constant_double,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    comma,
    semicolon,
    ellipsis,
    question,
    colon,
    dot,
    ptr_op,
    inc_op,
    dec_op,
    sizeof,
    ampersand,
    exclamation,
    tilde,
    minus,
    plus,
    star,
    slash,
    percent,
    left_op,
    right_op,
    less,
    greater,
    caret,
    pipe,
    le,
    ge,
    eq,
    ne,
    and_op,
    or_op,
    assign,
    mul_assign,
    div_assign,
    mod_assign,
    add_assign,
    sub_assign,
    left_assign,
    right_assign,
    and_assign,
    xor_assign,
    or_assign,
    case,
    default,
    if_,
    else_,
    switch,
    while_,
    do_,
    for_,
    goto_,
    continue_,
    break_,
    return_,
    typedef,
    extern_,
    static_,
    auto,
    register,
    const_,
    volatile,
    void_,
    char_,
    short_,
    int_,
    long_,
    float_,
    double_,
    signed,
    unsigned,
    struct_,
    union_,
    enum_,
    __rustylr_other_terminals,
    error,
    eof,
    VirtualStart0,
}
impl TerminalClasses {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 91usize, "Terminal class index {} is out of bounds (max {})", value,
            91usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl ::rusty_lr::parser::terminalclass::TerminalClass for TerminalClasses {
    type Term = Token;
    const ERROR: Self = Self::error;
    const EOF: Self = Self::eof;
    fn as_str(&self) -> &'static str {
        match self {
            TerminalClasses::ident => "ident",
            TerminalClasses::lparen => "lparen",
            TerminalClasses::rparen => "rparen",
            TerminalClasses::string_literal => "string_literal",
            TerminalClasses::constant_character => "constant_character",
            TerminalClasses::constant_integer => "constant_integer",
            TerminalClasses::constant_long => "constant_long",
            TerminalClasses::constant_unsigned_integer => "constant_unsigned_integer",
            TerminalClasses::constant_unsigned_long => "constant_unsigned_long",
            TerminalClasses::constant_float => "constant_float",
            TerminalClasses::constant_double => "constant_double",
            TerminalClasses::lbracket => "lbracket",
            TerminalClasses::rbracket => "rbracket",
            TerminalClasses::lbrace => "lbrace",
            TerminalClasses::rbrace => "rbrace",
            TerminalClasses::comma => "comma",
            TerminalClasses::semicolon => "semicolon",
            TerminalClasses::ellipsis => "ellipsis",
            TerminalClasses::question => "question",
            TerminalClasses::colon => "colon",
            TerminalClasses::dot => "dot",
            TerminalClasses::ptr_op => "ptr_op",
            TerminalClasses::inc_op => "inc_op",
            TerminalClasses::dec_op => "dec_op",
            TerminalClasses::sizeof => "sizeof",
            TerminalClasses::ampersand => "ampersand",
            TerminalClasses::exclamation => "exclamation",
            TerminalClasses::tilde => "tilde",
            TerminalClasses::minus => "minus",
            TerminalClasses::plus => "plus",
            TerminalClasses::star => "star",
            TerminalClasses::slash => "slash",
            TerminalClasses::percent => "percent",
            TerminalClasses::left_op => "left_op",
            TerminalClasses::right_op => "right_op",
            TerminalClasses::less => "less",
            TerminalClasses::greater => "greater",
            TerminalClasses::caret => "caret",
            TerminalClasses::pipe => "pipe",
            TerminalClasses::le => "le",
            TerminalClasses::ge => "ge",
            TerminalClasses::eq => "eq",
            TerminalClasses::ne => "ne",
            TerminalClasses::and_op => "and_op",
            TerminalClasses::or_op => "or_op",
            TerminalClasses::assign => "assign",
            TerminalClasses::mul_assign => "mul_assign",
            TerminalClasses::div_assign => "div_assign",
            TerminalClasses::mod_assign => "mod_assign",
            TerminalClasses::add_assign => "add_assign",
            TerminalClasses::sub_assign => "sub_assign",
            TerminalClasses::left_assign => "left_assign",
            TerminalClasses::right_assign => "right_assign",
            TerminalClasses::and_assign => "and_assign",
            TerminalClasses::xor_assign => "xor_assign",
            TerminalClasses::or_assign => "or_assign",
            TerminalClasses::case => "case",
            TerminalClasses::default => "default",
            TerminalClasses::if_ => "if_",
            TerminalClasses::else_ => "else_",
            TerminalClasses::switch => "switch",
            TerminalClasses::while_ => "while_",
            TerminalClasses::do_ => "do_",
            TerminalClasses::for_ => "for_",
            TerminalClasses::goto_ => "goto_",
            TerminalClasses::continue_ => "continue_",
            TerminalClasses::break_ => "break_",
            TerminalClasses::return_ => "return_",
            TerminalClasses::typedef => "typedef",
            TerminalClasses::extern_ => "extern_",
            TerminalClasses::static_ => "static_",
            TerminalClasses::auto => "auto",
            TerminalClasses::register => "register",
            TerminalClasses::const_ => "const_",
            TerminalClasses::volatile => "volatile",
            TerminalClasses::void_ => "void_",
            TerminalClasses::char_ => "char_",
            TerminalClasses::short_ => "short_",
            TerminalClasses::int_ => "int_",
            TerminalClasses::long_ => "long_",
            TerminalClasses::float_ => "float_",
            TerminalClasses::double_ => "double_",
            TerminalClasses::signed => "signed",
            TerminalClasses::unsigned => "unsigned",
            TerminalClasses::struct_ => "struct_",
            TerminalClasses::union_ => "union_",
            TerminalClasses::enum_ => "enum_",
            TerminalClasses::__rustylr_other_terminals => "__rustylr_other_terminals",
            TerminalClasses::error => "error",
            TerminalClasses::eof => "eof",
            TerminalClasses::VirtualStart0 => "virtual_start",
        }
    }
    fn to_usize(&self) -> usize {
        *self as usize
    }
    fn from_term(terminal: &Self::Term) -> Self {
        #[allow(unreachable_patterns, unused_variables)]
        match terminal {
            Token::Identifier(_) => TerminalClasses::ident,
            Token::LeftParen => TerminalClasses::lparen,
            Token::RightParen => TerminalClasses::rparen,
            Token::StringLiteral(_) => TerminalClasses::string_literal,
            Token::ConstantCharacter(_) => TerminalClasses::constant_character,
            Token::ConstantInteger(_) => TerminalClasses::constant_integer,
            Token::ConstantLong(_) => TerminalClasses::constant_long,
            Token::ConstantUnsignedInteger(_) => {
                TerminalClasses::constant_unsigned_integer
            }
            Token::ConstantUnsignedLong(_) => TerminalClasses::constant_unsigned_long,
            Token::ConstantFloat(_) => TerminalClasses::constant_float,
            Token::ConstantDouble(_) => TerminalClasses::constant_double,
            Token::LeftBracket => TerminalClasses::lbracket,
            Token::RightBracket => TerminalClasses::rbracket,
            Token::LeftBrace => TerminalClasses::lbrace,
            Token::RightBrace => TerminalClasses::rbrace,
            Token::Comma => TerminalClasses::comma,
            Token::SemiColon => TerminalClasses::semicolon,
            Token::Ellipsis => TerminalClasses::ellipsis,
            Token::Question => TerminalClasses::question,
            Token::Colon => TerminalClasses::colon,
            Token::Dot => TerminalClasses::dot,
            Token::PtrOp => TerminalClasses::ptr_op,
            Token::IncOp => TerminalClasses::inc_op,
            Token::DecOp => TerminalClasses::dec_op,
            Token::Sizeof => TerminalClasses::sizeof,
            Token::Ampersand => TerminalClasses::ampersand,
            Token::Exclamation => TerminalClasses::exclamation,
            Token::Tilde => TerminalClasses::tilde,
            Token::Minus => TerminalClasses::minus,
            Token::Plus => TerminalClasses::plus,
            Token::Star => TerminalClasses::star,
            Token::Slash => TerminalClasses::slash,
            Token::Percent => TerminalClasses::percent,
            Token::LeftOp => TerminalClasses::left_op,
            Token::RightOp => TerminalClasses::right_op,
            Token::LessThan => TerminalClasses::less,
            Token::GreaterThan => TerminalClasses::greater,
            Token::Caret => TerminalClasses::caret,
            Token::Pipe => TerminalClasses::pipe,
            Token::LeOp => TerminalClasses::le,
            Token::GeOp => TerminalClasses::ge,
            Token::EqOp => TerminalClasses::eq,
            Token::NeOp => TerminalClasses::ne,
            Token::AndOp => TerminalClasses::and_op,
            Token::OrOp => TerminalClasses::or_op,
            Token::Equal => TerminalClasses::assign,
            Token::MulAssign => TerminalClasses::mul_assign,
            Token::DivAssign => TerminalClasses::div_assign,
            Token::ModAssign => TerminalClasses::mod_assign,
            Token::AddAssign => TerminalClasses::add_assign,
            Token::SubAssign => TerminalClasses::sub_assign,
            Token::LeftAssign => TerminalClasses::left_assign,
            Token::RightAssign => TerminalClasses::right_assign,
            Token::AndAssign => TerminalClasses::and_assign,
            Token::XorAssign => TerminalClasses::xor_assign,
            Token::OrAssign => TerminalClasses::or_assign,
            Token::Case => TerminalClasses::case,
            Token::Default => TerminalClasses::default,
            Token::If => TerminalClasses::if_,
            Token::Else => TerminalClasses::else_,
            Token::Switch => TerminalClasses::switch,
            Token::While => TerminalClasses::while_,
            Token::Do => TerminalClasses::do_,
            Token::For => TerminalClasses::for_,
            Token::Goto => TerminalClasses::goto_,
            Token::Continue => TerminalClasses::continue_,
            Token::Break => TerminalClasses::break_,
            Token::Return => TerminalClasses::return_,
            Token::Typedef => TerminalClasses::typedef,
            Token::Extern => TerminalClasses::extern_,
            Token::Static => TerminalClasses::static_,
            Token::Auto => TerminalClasses::auto,
            Token::Register => TerminalClasses::register,
            Token::Const => TerminalClasses::const_,
            Token::Volatile => TerminalClasses::volatile,
            Token::Void => TerminalClasses::void_,
            Token::Char => TerminalClasses::char_,
            Token::Short => TerminalClasses::short_,
            Token::Int => TerminalClasses::int_,
            Token::Long => TerminalClasses::long_,
            Token::Float => TerminalClasses::float_,
            Token::Double => TerminalClasses::double_,
            Token::Signed => TerminalClasses::signed,
            Token::Unsigned => TerminalClasses::unsigned,
            Token::Struct => TerminalClasses::struct_,
            Token::Union => TerminalClasses::union_,
            Token::Enum => TerminalClasses::enum_,
            _ => TerminalClasses::__rustylr_other_terminals,
        }
    }
    fn from_virtual_start(branch_idx: u32) -> Self {
        match branch_idx {
            0u32 => Self::VirtualStart0,
            _ => panic!("Invalid virtual start branch index: {}", branch_idx),
        }
    }
}
impl std::fmt::Display for TerminalClasses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::terminalclass::TerminalClass;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for TerminalClasses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::terminalclass::TerminalClass;
        write!(f, "{}", self.as_str())
    }
}
/// An enum that represents non-terminal symbols
#[allow(non_camel_case_types, dead_code)]
#[derive(
    Clone,
    Copy,
    std::hash::Hash,
    std::cmp::PartialEq,
    std::cmp::Eq,
    std::cmp::PartialOrd,
    std::cmp::Ord
)]
#[repr(usize)]
pub enum NonTerminals {
    Constant,
    primary_expression,
    postfix_expression,
    argument_expression_list,
    unary_expression,
    cast_expression,
    multiplicative_expression,
    additive_expression,
    shift_expression,
    relational_expression,
    equality_expression,
    and_expression,
    exclusive_or_expression,
    inclusive_or_expression,
    logical_and_expression,
    logical_or_expression,
    conditional_expression,
    assignment_expression,
    expression,
    initializer,
    initializer_list,
    labeled_statement,
    statement_or_declaration,
    compound_statement,
    expression_statement,
    selection_statement,
    declaration_or_expression,
    iteration_statement,
    jump_statement,
    declaration,
    function_definition,
    statement,
    external_declaration,
    translation_unit,
    type_qualifier,
    declarator,
    direct_declarator,
    abstract_declarator,
    direct_abstract_declarator,
    specifier_qualifier,
    type_name,
    type_specifier,
    storage_class_specifier,
    declaration_specifier,
    parameter_declaration,
    parameter_list,
    parameter_type_list,
    struct_or_union,
    struct_declarator,
    struct_declarator_list,
    struct_declaration,
    struct_or_union_specifier,
    enum_specifier,
    enumerator_list,
    enumerator,
    init_declarator,
    init_declarator_list,
    _statement_or_declarationPlus58,
    _statement_or_declarationStar59,
    _declaration_specifierPlus60,
    _external_declarationPlus61,
    _external_declarationStar62,
    _type_qualifierPlus63,
    _type_qualifierStar64,
    _specifier_qualifierPlus65,
    _abstract_declaratorQuestion66,
    _declaratorQuestion67,
    _identQuestion68,
    _struct_declarationPlus69,
    _struct_declarationStar70,
    Augmented,
}
impl NonTerminals {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 71usize, "Non-terminal index {} is out of bounds (max {})", value,
            71usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl std::fmt::Display for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl ::rusty_lr::parser::nonterminal::NonTerminal for NonTerminals {
    fn as_str(&self) -> &'static str {
        match self {
            NonTerminals::Constant => "Constant",
            NonTerminals::primary_expression => "primary_expression",
            NonTerminals::postfix_expression => "postfix_expression",
            NonTerminals::argument_expression_list => "argument_expression_list",
            NonTerminals::unary_expression => "unary_expression",
            NonTerminals::cast_expression => "cast_expression",
            NonTerminals::multiplicative_expression => "multiplicative_expression",
            NonTerminals::additive_expression => "additive_expression",
            NonTerminals::shift_expression => "shift_expression",
            NonTerminals::relational_expression => "relational_expression",
            NonTerminals::equality_expression => "equality_expression",
            NonTerminals::and_expression => "and_expression",
            NonTerminals::exclusive_or_expression => "exclusive_or_expression",
            NonTerminals::inclusive_or_expression => "inclusive_or_expression",
            NonTerminals::logical_and_expression => "logical_and_expression",
            NonTerminals::logical_or_expression => "logical_or_expression",
            NonTerminals::conditional_expression => "conditional_expression",
            NonTerminals::assignment_expression => "assignment_expression",
            NonTerminals::expression => "expression",
            NonTerminals::initializer => "initializer",
            NonTerminals::initializer_list => "initializer_list",
            NonTerminals::labeled_statement => "labeled_statement",
            NonTerminals::statement_or_declaration => "statement_or_declaration",
            NonTerminals::compound_statement => "compound_statement",
            NonTerminals::expression_statement => "expression_statement",
            NonTerminals::selection_statement => "selection_statement",
            NonTerminals::declaration_or_expression => "declaration_or_expression",
            NonTerminals::iteration_statement => "iteration_statement",
            NonTerminals::jump_statement => "jump_statement",
            NonTerminals::declaration => "declaration",
            NonTerminals::function_definition => "function_definition",
            NonTerminals::statement => "statement",
            NonTerminals::external_declaration => "external_declaration",
            NonTerminals::translation_unit => "translation_unit",
            NonTerminals::type_qualifier => "type_qualifier",
            NonTerminals::declarator => "declarator",
            NonTerminals::direct_declarator => "direct_declarator",
            NonTerminals::abstract_declarator => "abstract_declarator",
            NonTerminals::direct_abstract_declarator => "direct_abstract_declarator",
            NonTerminals::specifier_qualifier => "specifier_qualifier",
            NonTerminals::type_name => "type_name",
            NonTerminals::type_specifier => "type_specifier",
            NonTerminals::storage_class_specifier => "storage_class_specifier",
            NonTerminals::declaration_specifier => "declaration_specifier",
            NonTerminals::parameter_declaration => "parameter_declaration",
            NonTerminals::parameter_list => "parameter_list",
            NonTerminals::parameter_type_list => "parameter_type_list",
            NonTerminals::struct_or_union => "struct_or_union",
            NonTerminals::struct_declarator => "struct_declarator",
            NonTerminals::struct_declarator_list => "struct_declarator_list",
            NonTerminals::struct_declaration => "struct_declaration",
            NonTerminals::struct_or_union_specifier => "struct_or_union_specifier",
            NonTerminals::enum_specifier => "enum_specifier",
            NonTerminals::enumerator_list => "enumerator_list",
            NonTerminals::enumerator => "enumerator",
            NonTerminals::init_declarator => "init_declarator",
            NonTerminals::init_declarator_list => "init_declarator_list",
            NonTerminals::_statement_or_declarationPlus58 => "statement_or_declaration+",
            NonTerminals::_statement_or_declarationStar59 => "statement_or_declaration*",
            NonTerminals::_declaration_specifierPlus60 => "declaration_specifier+",
            NonTerminals::_external_declarationPlus61 => "external_declaration+",
            NonTerminals::_external_declarationStar62 => "external_declaration*",
            NonTerminals::_type_qualifierPlus63 => "type_qualifier+",
            NonTerminals::_type_qualifierStar64 => "type_qualifier*",
            NonTerminals::_specifier_qualifierPlus65 => "specifier_qualifier+",
            NonTerminals::_abstract_declaratorQuestion66 => "abstract_declarator?",
            NonTerminals::_declaratorQuestion67 => "declarator?",
            NonTerminals::_identQuestion68 => "ident?",
            NonTerminals::_struct_declarationPlus69 => "struct_declaration+",
            NonTerminals::_struct_declarationStar70 => "struct_declaration*",
            NonTerminals::Augmented => "Augmented",
        }
    }
    fn nonterm_type(&self) -> Option<::rusty_lr::parser::nonterminal::NonTerminalType> {
        match self {
            NonTerminals::Constant => None,
            NonTerminals::primary_expression => None,
            NonTerminals::postfix_expression => None,
            NonTerminals::argument_expression_list => None,
            NonTerminals::unary_expression => None,
            NonTerminals::cast_expression => None,
            NonTerminals::multiplicative_expression => None,
            NonTerminals::additive_expression => None,
            NonTerminals::shift_expression => None,
            NonTerminals::relational_expression => None,
            NonTerminals::equality_expression => None,
            NonTerminals::and_expression => None,
            NonTerminals::exclusive_or_expression => None,
            NonTerminals::inclusive_or_expression => None,
            NonTerminals::logical_and_expression => None,
            NonTerminals::logical_or_expression => None,
            NonTerminals::conditional_expression => None,
            NonTerminals::assignment_expression => None,
            NonTerminals::expression => None,
            NonTerminals::initializer => None,
            NonTerminals::initializer_list => None,
            NonTerminals::labeled_statement => None,
            NonTerminals::statement_or_declaration => None,
            NonTerminals::compound_statement => None,
            NonTerminals::expression_statement => None,
            NonTerminals::selection_statement => None,
            NonTerminals::declaration_or_expression => None,
            NonTerminals::iteration_statement => None,
            NonTerminals::jump_statement => None,
            NonTerminals::declaration => None,
            NonTerminals::function_definition => None,
            NonTerminals::statement => None,
            NonTerminals::external_declaration => None,
            NonTerminals::translation_unit => None,
            NonTerminals::type_qualifier => None,
            NonTerminals::declarator => None,
            NonTerminals::direct_declarator => None,
            NonTerminals::abstract_declarator => None,
            NonTerminals::direct_abstract_declarator => None,
            NonTerminals::specifier_qualifier => None,
            NonTerminals::type_name => None,
            NonTerminals::type_specifier => None,
            NonTerminals::storage_class_specifier => None,
            NonTerminals::declaration_specifier => None,
            NonTerminals::parameter_declaration => None,
            NonTerminals::parameter_list => None,
            NonTerminals::parameter_type_list => None,
            NonTerminals::struct_or_union => None,
            NonTerminals::struct_declarator => None,
            NonTerminals::struct_declarator_list => None,
            NonTerminals::struct_declaration => None,
            NonTerminals::struct_or_union_specifier => None,
            NonTerminals::enum_specifier => None,
            NonTerminals::enumerator_list => None,
            NonTerminals::enumerator => None,
            NonTerminals::init_declarator => None,
            NonTerminals::init_declarator_list => None,
            NonTerminals::_statement_or_declarationPlus58 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_statement_or_declarationStar59 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_declaration_specifierPlus60 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_external_declarationPlus61 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_external_declarationStar62 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_type_qualifierPlus63 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_type_qualifierStar64 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_specifier_qualifierPlus65 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_abstract_declaratorQuestion66 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::_declaratorQuestion67 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::_identQuestion68 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::_struct_declarationPlus69 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_struct_declarationStar70 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::Augmented => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Augmented)
            }
        }
    }
    fn to_usize(&self) -> usize {
        *self as usize
    }
}
/// enum for each non-terminal and terminal symbol, that actually hold data
#[rustfmt::skip]
#[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
#[derive(Clone)]
pub enum Data {
    __terminals(Token),
    __variant1(Expression),
    __variant2(Vec<Expression>),
    __variant3(Statement),
    __variant4(statement::TranslationUnit),
    __variant5(declarator::TypeQualifier),
    __variant6(Declarator),
    __variant7(declarator::SpecifierQualifier),
    __variant8(declarator::Typename),
    __variant9(declarator::TypeSpecifier),
    __variant10(declarator::StorageClassSpecifier),
    __variant11(declarator::DeclarationSpecifier),
    __variant12(declarator::ParameterDeclaration),
    __variant13(Vec<declarator::ParameterDeclaration>),
    __variant14(declarator::ParameterList),
    __variant15(bool),
    __variant16(Vec<Declarator>),
    __variant17(declarator::StructDeclaration),
    __variant18(declarator::StructOrUnionSpecifier),
    __variant19(declarator::EnumSpecifier),
    __variant20(Vec<declarator::Enumerator>),
    __variant21(declarator::Enumerator),
    __variant22(declarator::DeclInit),
    __variant23(Vec<declarator::DeclInit>),
    __variant24(Vec<Statement>),
    __variant25(Vec<declarator::DeclarationSpecifier>),
    __variant26(Vec<declarator::TypeQualifier>),
    __variant27(Vec<declarator::SpecifierQualifier>),
    __variant28(Option<Declarator>),
    __variant29(Option<Token>),
    __variant30(Vec<declarator::StructDeclaration>),
    Empty,
}
impl ::std::fmt::Debug for Data {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::__terminals(..) => f.write_str(stringify!(__terminals)),
            Self::__variant1(..) => f.write_str(stringify!(__variant1)),
            Self::__variant2(..) => f.write_str(stringify!(__variant2)),
            Self::__variant3(..) => f.write_str(stringify!(__variant3)),
            Self::__variant4(..) => f.write_str(stringify!(__variant4)),
            Self::__variant5(..) => f.write_str(stringify!(__variant5)),
            Self::__variant6(..) => f.write_str(stringify!(__variant6)),
            Self::__variant7(..) => f.write_str(stringify!(__variant7)),
            Self::__variant8(..) => f.write_str(stringify!(__variant8)),
            Self::__variant9(..) => f.write_str(stringify!(__variant9)),
            Self::__variant10(..) => f.write_str(stringify!(__variant10)),
            Self::__variant11(..) => f.write_str(stringify!(__variant11)),
            Self::__variant12(..) => f.write_str(stringify!(__variant12)),
            Self::__variant13(..) => f.write_str(stringify!(__variant13)),
            Self::__variant14(..) => f.write_str(stringify!(__variant14)),
            Self::__variant15(..) => f.write_str(stringify!(__variant15)),
            Self::__variant16(..) => f.write_str(stringify!(__variant16)),
            Self::__variant17(..) => f.write_str(stringify!(__variant17)),
            Self::__variant18(..) => f.write_str(stringify!(__variant18)),
            Self::__variant19(..) => f.write_str(stringify!(__variant19)),
            Self::__variant20(..) => f.write_str(stringify!(__variant20)),
            Self::__variant21(..) => f.write_str(stringify!(__variant21)),
            Self::__variant22(..) => f.write_str(stringify!(__variant22)),
            Self::__variant23(..) => f.write_str(stringify!(__variant23)),
            Self::__variant24(..) => f.write_str(stringify!(__variant24)),
            Self::__variant25(..) => f.write_str(stringify!(__variant25)),
            Self::__variant26(..) => f.write_str(stringify!(__variant26)),
            Self::__variant27(..) => f.write_str(stringify!(__variant27)),
            Self::__variant28(..) => f.write_str(stringify!(__variant28)),
            Self::__variant29(..) => f.write_str(stringify!(__variant29)),
            Self::__variant30(..) => f.write_str(stringify!(__variant30)),
            Self::Empty => f.write_str("Empty"),
        }
    }
}
#[doc(hidden)]
#[allow(non_camel_case_types, dead_code)]
pub struct translation_unitExtracter;
impl ::rusty_lr::parser::semantic_value::StartExtractor<Data>
for translation_unitExtracter {
    type StartType = statement::TranslationUnit;
    const BRANCH_INDEX: u32 = 0u32;
    fn extract(value: Data) -> Option<Self::StartType> {
        #[allow(unreachable_patterns, unused_variables)]
        match value {
            Data::__variant4(val) => Some(val),
            _ => None,
        }
    }
}
#[rustfmt::skip]
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut,
    dead_code,
    unreachable_patterns
)]
impl Data {
    ///Constant -> constant_character
    #[inline]
    fn reduce_Constant_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_character = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantCharacter(value) = constant_character {
                Expression::ConstantCharacter(expression::ExprConstantCharacter {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_integer
    #[inline]
    fn reduce_Constant_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_integer = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantInteger(value) = constant_integer {
                Expression::ConstantInteger(expression::ExprConstantInteger {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_long
    #[inline]
    fn reduce_Constant_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_long = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantLong(value) = constant_long {
                Expression::ConstantLong(expression::ExprConstantLong {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_unsigned_integer
    #[inline]
    fn reduce_Constant_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_unsigned_integer = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantUnsignedInteger(value) = constant_unsigned_integer {
                Expression::ConstantUnsignedInteger(expression::ExprConstantUnsignedInteger {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_unsigned_long
    #[inline]
    fn reduce_Constant_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_unsigned_long = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantUnsignedLong(value) = constant_unsigned_long {
                Expression::ConstantUnsignedLong(expression::ExprConstantUnsignedLong {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_float
    #[inline]
    fn reduce_Constant_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_float = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantFloat(value) = constant_float {
                Expression::ConstantFloat(expression::ExprConstantFloat {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Constant -> constant_double
    #[inline]
    fn reduce_Constant_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut constant_double = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::ConstantDouble(value) = constant_double {
                Expression::ConstantDouble(expression::ExprConstantDouble {
                    value,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///primary_expression -> ident
    #[inline]
    fn reduce_primary_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::Identifier(name) = ident {
                Expression::Identifier(expression::ExprIdentifier { name })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///primary_expression -> string_literal
    #[inline]
    fn reduce_primary_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut string_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::StringLiteral(value) = string_literal {
                Expression::String(expression::ExprString { value })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///primary_expression -> lparen expression rparen
    #[inline]
    fn reduce_primary_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression lbracket expression rbracket
    #[inline]
    fn reduce_postfix_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Bracket(expression::ExprBracket {
                src: Box::new(postfix_expression),
                index: Box::new(expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression lparen rparen
    #[inline]
    fn reduce_postfix_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Paren(expression::ExprParen {
                src: Box::new(postfix_expression),
                args: Vec::new(),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression lparen argument_expression_list rparen
    #[inline]
    fn reduce_postfix_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut argument_expression_list = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Paren(expression::ExprParen {
                src: Box::new(postfix_expression),
                args: argument_expression_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression dot ident
    #[inline]
    fn reduce_postfix_expression_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::Identifier(name) = ident {
                Expression::Member(expression::ExprMember {
                    src: Box::new(postfix_expression),
                    member: name,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression ptr_op ident
    #[inline]
    fn reduce_postfix_expression_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::Identifier(name) = ident {
                Expression::Arrow(expression::ExprArrow {
                    src: Box::new(postfix_expression),
                    member: name,
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression inc_op
    #[inline]
    fn reduce_postfix_expression_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::IncrementPost,
                src: Box::new(postfix_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///postfix_expression -> postfix_expression dec_op
    #[inline]
    fn reduce_postfix_expression_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::DecrementPost,
                src: Box::new(postfix_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///argument_expression_list -> assignment_expression
    #[inline]
    fn reduce_argument_expression_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![assignment_expression] };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///argument_expression_list -> argument_expression_list comma assignment_expression
    #[inline]
    fn reduce_argument_expression_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut argument_expression_list = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            argument_expression_list.push(assignment_expression);
            argument_expression_list
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> postfix_expression
    #[inline]
    fn reduce_unary_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut postfix_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = postfix_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> inc_op unary_expression
    #[inline]
    fn reduce_unary_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::IncrementPre,
                src: Box::new(unary_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> dec_op unary_expression
    #[inline]
    fn reduce_unary_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::DecrementPre,
                src: Box::new(unary_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> ampersand cast_expression
    #[inline]
    fn reduce_unary_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::AddressOf,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> star cast_expression
    #[inline]
    fn reduce_unary_expression_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::Dereference,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> plus cast_expression
    #[inline]
    fn reduce_unary_expression_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::Plus,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> minus cast_expression
    #[inline]
    fn reduce_unary_expression_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::Minus,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> tilde cast_expression
    #[inline]
    fn reduce_unary_expression_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::BitwiseNot,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> exclamation cast_expression
    #[inline]
    fn reduce_unary_expression_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Unary(expression::ExprUnary {
                op: expression::ExprUnaryOperator::LogicalNot,
                src: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> sizeof unary_expression
    #[inline]
    fn reduce_unary_expression_9(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::SizeofExpr(expression::ExprSizeOfExpr {
                expr: Box::new(unary_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///unary_expression -> sizeof lparen type_name rparen
    #[inline]
    fn reduce_unary_expression_10(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant8(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut type_name = match __data_stack.pop().unwrap() {
            Data::__variant8(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Expression::SizeofType(expression::ExprSizeOfType {
                typename: type_name,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///cast_expression -> unary_expression
    #[inline]
    fn reduce_cast_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = unary_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///cast_expression -> lparen type_name rparen cast_expression
    #[inline]
    fn reduce_cast_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant8(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut type_name = match __data_stack.pop().unwrap() {
            Data::__variant8(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::Cast(expression::ExprCast {
                src: Box::new(cast_expression),
                typename: type_name,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///multiplicative_expression -> multiplicative_expression star cast_expression
    #[inline]
    fn reduce_multiplicative_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Mul,
                lhs: Box::new(multiplicative_expression),
                rhs: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///multiplicative_expression -> multiplicative_expression slash cast_expression
    #[inline]
    fn reduce_multiplicative_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Div,
                lhs: Box::new(multiplicative_expression),
                rhs: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///multiplicative_expression -> multiplicative_expression percent cast_expression
    #[inline]
    fn reduce_multiplicative_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut cast_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Mod,
                lhs: Box::new(multiplicative_expression),
                rhs: Box::new(cast_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///additive_expression -> multiplicative_expression
    #[inline]
    fn reduce_additive_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = multiplicative_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///additive_expression -> additive_expression plus multiplicative_expression
    #[inline]
    fn reduce_additive_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut additive_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Add,
                lhs: Box::new(additive_expression),
                rhs: Box::new(multiplicative_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///additive_expression -> additive_expression minus multiplicative_expression
    #[inline]
    fn reduce_additive_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut multiplicative_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut additive_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Sub,
                lhs: Box::new(additive_expression),
                rhs: Box::new(multiplicative_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///shift_expression -> additive_expression
    #[inline]
    fn reduce_shift_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut additive_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = additive_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///shift_expression -> shift_expression left_op additive_expression
    #[inline]
    fn reduce_shift_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut additive_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::ShiftLeft,
                lhs: Box::new(shift_expression),
                rhs: Box::new(additive_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///shift_expression -> shift_expression right_op additive_expression
    #[inline]
    fn reduce_shift_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut additive_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::ShiftRight,
                lhs: Box::new(shift_expression),
                rhs: Box::new(additive_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///relational_expression -> shift_expression
    #[inline]
    fn reduce_relational_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = shift_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///relational_expression -> relational_expression less shift_expression
    #[inline]
    fn reduce_relational_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::LessThan,
                lhs: Box::new(relational_expression),
                rhs: Box::new(shift_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///relational_expression -> relational_expression greater shift_expression
    #[inline]
    fn reduce_relational_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::GreaterThan,
                lhs: Box::new(relational_expression),
                rhs: Box::new(shift_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///relational_expression -> relational_expression le shift_expression
    #[inline]
    fn reduce_relational_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::LessThanOrEqual,
                lhs: Box::new(relational_expression),
                rhs: Box::new(shift_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///relational_expression -> relational_expression ge shift_expression
    #[inline]
    fn reduce_relational_expression_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut shift_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::GreaterThanOrEqual,
                lhs: Box::new(relational_expression),
                rhs: Box::new(shift_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///equality_expression -> relational_expression
    #[inline]
    fn reduce_equality_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = relational_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///equality_expression -> equality_expression eq relational_expression
    #[inline]
    fn reduce_equality_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut equality_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Equal,
                lhs: Box::new(equality_expression),
                rhs: Box::new(relational_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///equality_expression -> equality_expression ne relational_expression
    #[inline]
    fn reduce_equality_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut relational_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut equality_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::NotEqual,
                lhs: Box::new(equality_expression),
                rhs: Box::new(relational_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///and_expression -> equality_expression
    #[inline]
    fn reduce_and_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut equality_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = equality_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///and_expression -> and_expression ampersand equality_expression
    #[inline]
    fn reduce_and_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut equality_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseAnd,
                lhs: Box::new(and_expression),
                rhs: Box::new(equality_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///exclusive_or_expression -> and_expression
    #[inline]
    fn reduce_exclusive_or_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = and_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///exclusive_or_expression -> exclusive_or_expression caret and_expression
    #[inline]
    fn reduce_exclusive_or_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut exclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseXor,
                lhs: Box::new(exclusive_or_expression),
                rhs: Box::new(and_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///inclusive_or_expression -> exclusive_or_expression
    #[inline]
    fn reduce_inclusive_or_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut exclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = exclusive_or_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///inclusive_or_expression -> inclusive_or_expression pipe exclusive_or_expression
    #[inline]
    fn reduce_inclusive_or_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut exclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut inclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseOr,
                lhs: Box::new(inclusive_or_expression),
                rhs: Box::new(exclusive_or_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///logical_and_expression -> inclusive_or_expression
    #[inline]
    fn reduce_logical_and_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut inclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = inclusive_or_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///logical_and_expression -> logical_and_expression and_op inclusive_or_expression
    #[inline]
    fn reduce_logical_and_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut inclusive_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut logical_and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::LogicalAnd,
                lhs: Box::new(logical_and_expression),
                rhs: Box::new(inclusive_or_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///logical_or_expression -> logical_and_expression
    #[inline]
    fn reduce_logical_or_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut logical_and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = logical_and_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///logical_or_expression -> logical_or_expression or_op logical_and_expression
    #[inline]
    fn reduce_logical_or_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut logical_and_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut logical_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::LogicalOr,
                lhs: Box::new(logical_or_expression),
                rhs: Box::new(logical_and_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///conditional_expression -> logical_or_expression
    #[inline]
    fn reduce_conditional_expression_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut logical_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = logical_or_expression;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///conditional_expression -> logical_or_expression question expression colon conditional_expression
    #[inline]
    fn reduce_conditional_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        let mut conditional_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut logical_or_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Conditional(expression::ExprConditional {
                cond: Box::new(logical_or_expression),
                then_expr: Box::new(expression),
                else_expr: Box::new(conditional_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Assign(false),
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression mul_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::MulAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression div_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::DivAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression mod_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::ModAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression add_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::AddAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression sub_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::SubAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression left_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::ShiftLeftAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression right_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::ShiftRightAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression and_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_9(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseAndAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression xor_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_10(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseXorAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///assignment_expression -> unary_expression or_assign assignment_expression
    #[inline]
    fn reduce_assignment_expression_11(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut unary_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::BitwiseOrAssign,
                lhs: Box::new(unary_expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///expression -> expression comma assignment_expression
    #[inline]
    fn reduce_expression_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut assignment_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Expression::Binary(expression::ExprBinary {
                op: expression::ExprBinaryOperator::Comma,
                lhs: Box::new(expression),
                rhs: Box::new(assignment_expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///initializer -> lbrace initializer_list rbrace
    #[inline]
    fn reduce_initializer_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut initializer_list = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::InitializerList(expression::ExprInitializerList {
                initializers: initializer_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///initializer -> lbrace initializer_list comma rbrace
    #[inline]
    fn reduce_initializer_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut initializer_list = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Expression::InitializerList(expression::ExprInitializerList {
                initializers: initializer_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///initializer_list -> initializer
    #[inline]
    fn reduce_initializer_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut initializer = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![initializer] };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///initializer_list -> initializer_list comma initializer
    #[inline]
    fn reduce_initializer_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut initializer = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut initializer_list = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            initializer_list.push(initializer);
            initializer_list
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///labeled_statement -> ident colon statement
    #[inline]
    fn reduce_labeled_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::Identifier(label) = ident {
                Statement::Labeled(statement::StmtLabeled {
                    label,
                    statement: Box::new(statement),
                })
            } else {
                unreachable!()
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///labeled_statement -> case conditional_expression colon statement
    #[inline]
    fn reduce_labeled_statement_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Statement::Case(statement::StmtCase {
                value: constant_expression,
                statement: Box::new(statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///labeled_statement -> default colon statement
    #[inline]
    fn reduce_labeled_statement_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::Default(statement::StmtDefault {
                statement: Box::new(statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///compound_statement -> lbrace statement_or_declaration* rbrace
    #[inline]
    fn reduce_compound_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant24(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut statement_or_declaration = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Statement::Compound(statement::StmtCompound {
                statements: statement_or_declaration,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///expression_statement -> semicolon
    #[inline]
    fn reduce_expression_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { Statement::Null(statement::StmtNull {}) };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///expression_statement -> expression semicolon
    #[inline]
    fn reduce_expression_statement_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Statement::Expression(statement::StmtExpression {
                expression: expression,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///selection_statement -> if_ lparen expression rparen statement
    #[inline]
    fn reduce_selection_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::If(statement::StmtIf {
                cond: expression,
                then_statement: Box::new(statement),
                else_statement: None,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///selection_statement -> if_ lparen expression rparen statement else_ statement
    #[inline]
    fn reduce_selection_statement_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 7);
        let mut elsestmt = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut thenstmt = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::If(statement::StmtIf {
                cond: expression,
                then_statement: Box::new(thenstmt),
                else_statement: Some(Box::new(elsestmt)),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///selection_statement -> switch lparen expression rparen statement
    #[inline]
    fn reduce_selection_statement_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::Switch(statement::StmtSwitch {
                target: expression,
                statement: Box::new(statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///iteration_statement -> while_ lparen expression rparen statement
    #[inline]
    fn reduce_iteration_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::While(statement::StmtWhile {
                cond: expression,
                statement: Box::new(statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///iteration_statement -> do_ statement while_ lparen expression rparen semicolon
    #[inline]
    fn reduce_iteration_statement_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 7);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let mut statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Statement::DoWhile(statement::StmtDoWhile {
                cond: expression,
                statement: Box::new(statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///iteration_statement -> for_ lparen declaration_or_expression expression_statement rparen statement
    #[inline]
    fn reduce_iteration_statement_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 6);
        let mut body = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut cond = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut init = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            let Statement::Expression(cond) = cond else { unreachable!() };
            Statement::For(statement::StmtFor {
                init: Box::new(init),
                cond: cond.expression,
                next: None,
                statement: Box::new(body),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///iteration_statement -> for_ lparen declaration_or_expression expression_statement expression rparen statement
    #[inline]
    fn reduce_iteration_statement_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 7);
        let mut body = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut next = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let mut cond = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut init = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            let Statement::Expression(cond) = cond else { unreachable!() };
            Statement::For(statement::StmtFor {
                init: Box::new(init),
                cond: cond.expression,
                next: Some(next),
                statement: Box::new(body),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///jump_statement -> goto_ ident semicolon
    #[inline]
    fn reduce_jump_statement_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Token::Identifier(ident) = ident else { unreachable!() };
            Statement::Goto(statement::StmtGoto {
                label: ident,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///jump_statement -> continue_ semicolon
    #[inline]
    fn reduce_jump_statement_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = { Statement::Continue(statement::StmtContinue {}) };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///jump_statement -> break_ semicolon
    #[inline]
    fn reduce_jump_statement_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = { Statement::Break(statement::StmtBreak {}) };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///jump_statement -> return_ semicolon
    #[inline]
    fn reduce_jump_statement_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Statement::Return(statement::StmtReturn {
                expr: None,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///jump_statement -> return_ expression semicolon
    #[inline]
    fn reduce_jump_statement_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Statement::Return(statement::StmtReturn {
                expr: Some(expression),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration -> declaration_specifier+ semicolon
    #[inline]
    fn reduce_declaration_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Statement::Declaration(statement::StmtDeclaration {
                specs: declaration_specifier,
                inits: None,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration -> declaration_specifier+ init_declarator_list semicolon
    #[inline]
    fn reduce_declaration_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant23(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut inits = match __data_stack.pop().unwrap() {
            Data::__variant23(val) => val,
            _ => unreachable!(),
        };
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Statement::Declaration(statement::StmtDeclaration {
                specs: declaration_specifier,
                inits: Some(inits),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///function_definition -> declaration_specifier+ declarator compound_statement
    #[inline]
    fn reduce_function_definition_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut compound_statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Statement::FunctionDefinition(statement::StmtFunctionDefinition {
                specs: Some(declaration_specifier),
                decl: declarator,
                body: Box::new(compound_statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///function_definition -> declarator compound_statement
    #[inline]
    fn reduce_function_definition_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut compound_statement = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Statement::FunctionDefinition(statement::StmtFunctionDefinition {
                specs: None,
                decl: declarator,
                body: Box::new(compound_statement),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///translation_unit -> external_declaration*
    #[inline]
    fn reduce_translation_unit_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant24(_)))
            );
        }
        __location_stack.pop();
        let mut external_declaration = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            statement::TranslationUnit {
                statements: external_declaration,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant4(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier -> const_
    #[inline]
    fn reduce_type_qualifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeQualifier::Const };
        if __push_data {
            __data_stack.push(Self::__variant5(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier -> volatile
    #[inline]
    fn reduce_type_qualifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeQualifier::Volatile };
        if __push_data {
            __data_stack.push(Self::__variant5(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declarator -> direct_declarator
    #[inline]
    fn reduce_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut direct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = direct_declarator;
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declarator -> star type_qualifier* declarator
    #[inline]
    fn reduce_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant26(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let mut type_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant26(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            while let Some(type_qual) = type_qualifier.pop() {
                match type_qual {
                    declarator::TypeQualifier::Const => {
                        declarator = Declarator::Const(declarator::DeclConst {
                            declarator: Some(Box::new(declarator)),
                        });
                    }
                    declarator::TypeQualifier::Volatile => {
                        declarator = Declarator::Volatile(declarator::DeclVolatile {
                            declarator: Some(Box::new(declarator)),
                        });
                    }
                }
            }
            declarator = Declarator::Pointer(declarator::DeclPointer {
                declarator: Some(Box::new(declarator)),
            });
            declarator
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> ident
    #[inline]
    fn reduce_direct_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Token::Identifier(name) = ident else { unreachable!() };
            Declarator::Identifier(declarator::DeclIdentifier { name })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> lparen declarator rparen
    #[inline]
    fn reduce_direct_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = declarator;
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> direct_declarator lbracket conditional_expression rbracket
    #[inline]
    fn reduce_direct_declarator_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut direct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::ArrayFixed(declarator::DeclArrayFixed {
                declarator: Some(Box::new(direct_declarator)),
                size: constant_expression,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> direct_declarator lbracket rbracket
    #[inline]
    fn reduce_direct_declarator_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut direct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded {
                declarator: Some(Box::new(direct_declarator)),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> direct_declarator lparen parameter_type_list rparen
    #[inline]
    fn reduce_direct_declarator_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant14(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut parameter_type_list = match __data_stack.pop().unwrap() {
            Data::__variant14(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut direct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: Some(Box::new(direct_declarator)),
                params: parameter_type_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_declarator -> direct_declarator lparen rparen
    #[inline]
    fn reduce_direct_declarator_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut direct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: Some(Box::new(direct_declarator)),
                params: declarator::ParameterList {
                    params: Vec::new(),
                    variadic: false,
                },
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///abstract_declarator -> star type_qualifier* abstract_declarator
    #[inline]
    fn reduce_abstract_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant26(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let mut type_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant26(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let mut declarator = abstract_declarator;
            while let Some(type_qual) = type_qualifier.pop() {
                match type_qual {
                    declarator::TypeQualifier::Const => {
                        declarator = Declarator::Const(declarator::DeclConst {
                            declarator: Some(Box::new(declarator)),
                        });
                    }
                    declarator::TypeQualifier::Volatile => {
                        declarator = Declarator::Volatile(declarator::DeclVolatile {
                            declarator: Some(Box::new(declarator)),
                        });
                    }
                }
            }
            declarator = Declarator::Pointer(declarator::DeclPointer {
                declarator: Some(Box::new(declarator)),
            });
            declarator
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///abstract_declarator -> star type_qualifier*
    #[inline]
    fn reduce_abstract_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant26(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut type_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant26(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let mut declarator = None;
            while let Some(type_qual) = type_qualifier.pop() {
                match type_qual {
                    declarator::TypeQualifier::Const => {
                        declarator = Some(
                            Declarator::Const(declarator::DeclConst {
                                declarator: declarator.map(Box::new),
                            }),
                        );
                    }
                    declarator::TypeQualifier::Volatile => {
                        declarator = Some(
                            Declarator::Volatile(declarator::DeclVolatile {
                                declarator: declarator.map(Box::new),
                            }),
                        );
                    }
                }
            }
            Declarator::Pointer(declarator::DeclPointer {
                declarator: declarator.map(Box::new),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///abstract_declarator -> direct_abstract_declarator
    #[inline]
    fn reduce_abstract_declarator_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut direct_abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = direct_abstract_declarator;
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> lparen abstract_declarator rparen
    #[inline]
    fn reduce_direct_abstract_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = abstract_declarator;
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> lbracket rbracket
    #[inline]
    fn reduce_direct_abstract_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded {
                declarator: None,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> lbracket conditional_expression rbracket
    #[inline]
    fn reduce_direct_abstract_declarator_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Declarator::ArrayFixed(declarator::DeclArrayFixed {
                declarator: None,
                size: constant_expression,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> direct_abstract_declarator lbracket rbracket
    #[inline]
    fn reduce_direct_abstract_declarator_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut direct_abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded {
                declarator: Some(Box::new(direct_abstract_declarator)),
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> direct_abstract_declarator lbracket conditional_expression rbracket
    #[inline]
    fn reduce_direct_abstract_declarator_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut direct_abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::ArrayFixed(declarator::DeclArrayFixed {
                declarator: Some(Box::new(direct_abstract_declarator)),
                size: constant_expression,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> lparen rparen
    #[inline]
    fn reduce_direct_abstract_declarator_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: None,
                params: declarator::ParameterList {
                    params: Vec::new(),
                    variadic: false,
                },
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> lparen parameter_type_list rparen
    #[inline]
    fn reduce_direct_abstract_declarator_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant14(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut parameter_type_list = match __data_stack.pop().unwrap() {
            Data::__variant14(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: None,
                params: parameter_type_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> direct_abstract_declarator lparen rparen
    #[inline]
    fn reduce_direct_abstract_declarator_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut direct_abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: Some(Box::new(direct_abstract_declarator)),
                params: declarator::ParameterList {
                    params: Vec::new(),
                    variadic: false,
                },
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///direct_abstract_declarator -> direct_abstract_declarator lparen parameter_type_list rparen
    #[inline]
    fn reduce_direct_abstract_declarator_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant14(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.pop();
        let mut parameter_type_list = match __data_stack.pop().unwrap() {
            Data::__variant14(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut direct_abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::Function(declarator::DeclFunction {
                declarator: Some(Box::new(direct_abstract_declarator)),
                params: parameter_type_list,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///specifier_qualifier -> type_qualifier
    #[inline]
    fn reduce_specifier_qualifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
        }
        __location_stack.pop();
        let mut type_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => val,
            _ => unreachable!(),
        };
        let __res = { declarator::SpecifierQualifier::TypeQualifier(type_qualifier) };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///specifier_qualifier -> type_specifier
    #[inline]
    fn reduce_specifier_qualifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
        }
        __location_stack.pop();
        let mut type_specifier = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = { declarator::SpecifierQualifier::TypeSpecifier(type_specifier) };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_name -> specifier_qualifier+ abstract_declarator?
    #[inline]
    fn reduce_type_name_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant28(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant27(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant28(val) => val,
            _ => unreachable!(),
        };
        let mut specifier_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant27(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::Typename {
                specs: specifier_qualifier,
                declarator: abstract_declarator.map(Box::new),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant8(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> void_
    #[inline]
    fn reduce_type_specifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Void };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> char_
    #[inline]
    fn reduce_type_specifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Char };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> short_
    #[inline]
    fn reduce_type_specifier_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Short };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> int_
    #[inline]
    fn reduce_type_specifier_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Int };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> long_
    #[inline]
    fn reduce_type_specifier_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Long };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> float_
    #[inline]
    fn reduce_type_specifier_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Float };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> double_
    #[inline]
    fn reduce_type_specifier_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Double };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> signed
    #[inline]
    fn reduce_type_specifier_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Signed };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> unsigned
    #[inline]
    fn reduce_type_specifier_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::TypeSpecifier::Unsigned };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> ident
    #[inline]
    fn reduce_type_specifier_9(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Token::Identifier(ident) = ident else { unreachable!() };
            declarator::TypeSpecifier::Typename(ident)
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> struct_or_union_specifier
    #[inline]
    fn reduce_type_specifier_10(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant18(_)))
            );
        }
        __location_stack.pop();
        let mut struct_or_union_specifier = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::TypeSpecifier::StructOrUnion(struct_or_union_specifier)
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_specifier -> enum_specifier
    #[inline]
    fn reduce_type_specifier_11(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant19(_)))
            );
        }
        __location_stack.pop();
        let mut enum_specifier = match __data_stack.pop().unwrap() {
            Data::__variant19(val) => val,
            _ => unreachable!(),
        };
        let __res = { declarator::TypeSpecifier::Enum(enum_specifier) };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///storage_class_specifier -> typedef
    #[inline]
    fn reduce_storage_class_specifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::StorageClassSpecifier::Typedef };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///storage_class_specifier -> extern_
    #[inline]
    fn reduce_storage_class_specifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::StorageClassSpecifier::Extern };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///storage_class_specifier -> static_
    #[inline]
    fn reduce_storage_class_specifier_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::StorageClassSpecifier::Static };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///storage_class_specifier -> auto
    #[inline]
    fn reduce_storage_class_specifier_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::StorageClassSpecifier::Auto };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///storage_class_specifier -> register
    #[inline]
    fn reduce_storage_class_specifier_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { declarator::StorageClassSpecifier::Register };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration_specifier -> storage_class_specifier
    #[inline]
    fn reduce_declaration_specifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant10(_)))
            );
        }
        __location_stack.pop();
        let mut storage_class_specifier = match __data_stack.pop().unwrap() {
            Data::__variant10(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::DeclarationSpecifier::StorageClassSpecifier(
                storage_class_specifier,
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration_specifier -> type_specifier
    #[inline]
    fn reduce_declaration_specifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
        }
        __location_stack.pop();
        let mut type_specifier = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = { declarator::DeclarationSpecifier::TypeSpecifier(type_specifier) };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration_specifier -> type_qualifier
    #[inline]
    fn reduce_declaration_specifier_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
        }
        __location_stack.pop();
        let mut type_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => val,
            _ => unreachable!(),
        };
        let __res = { declarator::DeclarationSpecifier::TypeQualifier(type_qualifier) };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_declaration -> declaration_specifier+ declarator
    #[inline]
    fn reduce_parameter_declaration_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::ParameterDeclaration {
                specs: declaration_specifier,
                declarator: Some(Box::new(declarator)),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_declaration -> declaration_specifier+ abstract_declarator
    #[inline]
    fn reduce_parameter_declaration_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut abstract_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::ParameterDeclaration {
                specs: declaration_specifier,
                declarator: Some(Box::new(abstract_declarator)),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_declaration -> declaration_specifier+
    #[inline]
    fn reduce_parameter_declaration_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.pop();
        let mut declaration_specifier = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::ParameterDeclaration {
                specs: declaration_specifier,
                declarator: None,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_list -> parameter_declaration
    #[inline]
    fn reduce_parameter_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant12(_)))
            );
        }
        __location_stack.pop();
        let mut parameter_declaration = match __data_stack.pop().unwrap() {
            Data::__variant12(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![parameter_declaration] };
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_list -> parameter_list comma parameter_declaration
    #[inline]
    fn reduce_parameter_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant12(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant13(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut parameter_declaration = match __data_stack.pop().unwrap() {
            Data::__variant12(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut parameter_list = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            parameter_list.push(parameter_declaration);
            parameter_list
        };
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_type_list -> parameter_list
    #[inline]
    fn reduce_parameter_type_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant13(_)))
            );
        }
        __location_stack.pop();
        let mut parameter_list = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::ParameterList {
                params: parameter_list,
                variadic: false,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant14(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///parameter_type_list -> parameter_list comma ellipsis
    #[inline]
    fn reduce_parameter_type_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant13(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut parameter_list = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::ParameterList {
                params: parameter_list,
                variadic: true,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant14(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_or_union -> struct_
    #[inline]
    fn reduce_struct_or_union_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { true };
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_or_union -> union_
    #[inline]
    fn reduce_struct_or_union_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { false };
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declarator -> declarator
    #[inline]
    fn reduce_struct_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = declarator;
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declarator -> declarator? colon conditional_expression
    #[inline]
    fn reduce_struct_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant28(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant28(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Declarator::BitField(declarator::DeclBitField {
                declarator: declarator.map(Box::new),
                width: constant_expression,
            })
        };
        if __push_data {
            __data_stack.push(Self::__variant6(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declarator_list -> struct_declarator
    #[inline]
    fn reduce_struct_declarator_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut struct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![struct_declarator] };
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declarator_list -> struct_declarator_list comma struct_declarator
    #[inline]
    fn reduce_struct_declarator_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant16(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut struct_declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut struct_declarator_list = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            struct_declarator_list.push(struct_declarator);
            struct_declarator_list
        };
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declaration -> specifier_qualifier+ struct_declarator_list semicolon
    #[inline]
    fn reduce_struct_declaration_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant16(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant27(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut struct_declarator_list = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        let mut specifier_qualifier = match __data_stack.pop().unwrap() {
            Data::__variant27(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::StructDeclaration {
                specs: specifier_qualifier,
                declarators: struct_declarator_list,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant17(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_or_union_specifier -> struct_or_union ident? lbrace struct_declaration* rbrace
    #[inline]
    fn reduce_struct_or_union_specifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant30(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant29(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant15(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        __data_stack.pop();
        let mut struct_declaration = match __data_stack.pop().unwrap() {
            Data::__variant30(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__variant29(val) => val,
            _ => unreachable!(),
        };
        let mut struct_or_union = match __data_stack.pop().unwrap() {
            Data::__variant15(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let name = ident
                .map(|name| {
                    let Token::Identifier(name) = name else { unreachable!() };
                    name
                });
            declarator::StructOrUnionSpecifier {
                is_struct: struct_or_union,
                name,
                decls: Some(struct_declaration),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant18(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_or_union_specifier -> struct_or_union ident
    #[inline]
    fn reduce_struct_or_union_specifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant15(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let mut struct_or_union = match __data_stack.pop().unwrap() {
            Data::__variant15(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Token::Identifier(name) = ident else { unreachable!() };
            declarator::StructOrUnionSpecifier {
                is_struct: struct_or_union,
                name: Some(name),
                decls: None,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant18(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enum_specifier -> enum_ ident? lbrace enumerator_list rbrace
    #[inline]
    fn reduce_enum_specifier_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant20(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant29(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        __data_stack.pop();
        let mut enumerator_list = match __data_stack.pop().unwrap() {
            Data::__variant20(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__variant29(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let name = ident
                .map(|name| {
                    let Token::Identifier(name) = name else { unreachable!() };
                    name
                });
            declarator::EnumSpecifier {
                name,
                enumerators: Some(enumerator_list),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant19(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enum_specifier -> enum_ ident
    #[inline]
    fn reduce_enum_specifier_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Token::Identifier(name) = ident else { unreachable!() };
            declarator::EnumSpecifier {
                name: Some(name),
                enumerators: None,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant19(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enumerator_list -> enumerator
    #[inline]
    fn reduce_enumerator_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant21(_)))
            );
        }
        __location_stack.pop();
        let mut enumerator = match __data_stack.pop().unwrap() {
            Data::__variant21(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![enumerator] };
        if __push_data {
            __data_stack.push(Self::__variant20(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enumerator_list -> enumerator_list comma enumerator
    #[inline]
    fn reduce_enumerator_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant21(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant20(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut enumerator = match __data_stack.pop().unwrap() {
            Data::__variant21(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut enumerator_list = match __data_stack.pop().unwrap() {
            Data::__variant20(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            enumerator_list.push(enumerator);
            enumerator_list
        };
        if __push_data {
            __data_stack.push(Self::__variant20(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enumerator -> ident
    #[inline]
    fn reduce_enumerator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Token::Identifier(name) = ident else { unreachable!() };
            declarator::Enumerator {
                name,
                value: None,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant21(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///enumerator -> ident assign conditional_expression
    #[inline]
    fn reduce_enumerator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut constant_expression = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Token::Identifier(name) = ident else { unreachable!() };
            declarator::Enumerator {
                name,
                value: Some(constant_expression),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant21(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///init_declarator -> declarator
    #[inline]
    fn reduce_init_declarator_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::DeclInit {
                declarator: Box::new(declarator),
                initializer: None,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant22(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///init_declarator -> declarator assign initializer
    #[inline]
    fn reduce_init_declarator_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut initializer = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut declarator = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            declarator::DeclInit {
                declarator: Box::new(declarator),
                initializer: Some(initializer),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant22(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///init_declarator_list -> init_declarator
    #[inline]
    fn reduce_init_declarator_list_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant22(_)))
            );
        }
        __location_stack.pop();
        let mut init_declarator = match __data_stack.pop().unwrap() {
            Data::__variant22(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![init_declarator] };
        if __push_data {
            __data_stack.push(Self::__variant23(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///init_declarator_list -> init_declarator_list comma init_declarator
    #[inline]
    fn reduce_init_declarator_list_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant22(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant23(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut init_declarator = match __data_stack.pop().unwrap() {
            Data::__variant22(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut init_declarator_list = match __data_stack.pop().unwrap() {
            Data::__variant23(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            init_declarator_list.push(init_declarator);
            init_declarator_list
        };
        if __push_data {
            __data_stack.push(Self::__variant23(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///statement_or_declaration+ -> statement_or_declaration
    #[inline]
    fn reduce__statement_or_declarationPlus58_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///statement_or_declaration+ -> statement_or_declaration+ statement_or_declaration
    #[inline]
    fn reduce__statement_or_declarationPlus58_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant24(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///statement_or_declaration* -> statement_or_declaration+
    #[inline]
    fn reduce__statement_or_declarationStar59_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant24(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///statement_or_declaration* ->
    #[inline]
    fn reduce__statement_or_declarationStar59_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration_specifier+ -> declaration_specifier
    #[inline]
    fn reduce__declaration_specifierPlus60_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant11(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant11(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant25(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declaration_specifier+ -> declaration_specifier+ declaration_specifier
    #[inline]
    fn reduce__declaration_specifierPlus60_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant11(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant25(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant11(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant25(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant25(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///external_declaration+ -> external_declaration
    #[inline]
    fn reduce__external_declarationPlus61_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///external_declaration+ -> external_declaration+ external_declaration
    #[inline]
    fn reduce__external_declarationPlus61_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant24(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///external_declaration* -> external_declaration+
    #[inline]
    fn reduce__external_declarationStar62_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant24(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant24(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///external_declaration* ->
    #[inline]
    fn reduce__external_declarationStar62_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant24(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier+ -> type_qualifier
    #[inline]
    fn reduce__type_qualifierPlus63_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant26(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier+ -> type_qualifier+ type_qualifier
    #[inline]
    fn reduce__type_qualifierPlus63_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant26(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant26(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant26(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier* -> type_qualifier+
    #[inline]
    fn reduce__type_qualifierStar64_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant26(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant26(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant26(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///type_qualifier* ->
    #[inline]
    fn reduce__type_qualifierStar64_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant26(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///specifier_qualifier+ -> specifier_qualifier
    #[inline]
    fn reduce__specifier_qualifierPlus65_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant7(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant7(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant27(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///specifier_qualifier+ -> specifier_qualifier+ specifier_qualifier
    #[inline]
    fn reduce__specifier_qualifierPlus65_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant7(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant27(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant7(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant27(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant27(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///abstract_declarator? -> abstract_declarator
    #[inline]
    fn reduce__abstract_declaratorQuestion66_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = Some(A);
        if __push_data {
            __data_stack.push(Self::__variant28(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///abstract_declarator? ->
    #[inline]
    fn reduce__abstract_declaratorQuestion66_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant28(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declarator? -> declarator
    #[inline]
    fn reduce__declaratorQuestion67_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => val,
            _ => unreachable!(),
        };
        let __res = Some(A);
        if __push_data {
            __data_stack.push(Self::__variant28(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///declarator? ->
    #[inline]
    fn reduce__declaratorQuestion67_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant28(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///ident? -> ident
    #[inline]
    fn reduce__identQuestion68_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = Some(A);
        if __push_data {
            __data_stack.push(Self::__variant29(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///ident? ->
    #[inline]
    fn reduce__identQuestion68_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant29(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declaration+ -> struct_declaration
    #[inline]
    fn reduce__struct_declarationPlus69_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant17(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant17(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant30(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declaration+ -> struct_declaration+ struct_declaration
    #[inline]
    fn reduce__struct_declarationPlus69_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant17(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant30(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant17(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant30(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant30(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declaration* -> struct_declaration+
    #[inline]
    fn reduce__struct_declarationStar70_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant30(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant30(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant30(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///struct_declaration* ->
    #[inline]
    fn reduce__struct_declarationStar70_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant30(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
}
#[rustfmt::skip]
#[allow(
    unused_braces,
    unused_parens,
    non_snake_case,
    non_camel_case_types,
    unused_variables
)]
impl ::rusty_lr::parser::semantic_value::SemanticValue for Data {
    type Term = Token;
    type NonTerm = NonTerminals;
    type ReduceActionError = ::rusty_lr::DefaultReduceActionError;
    type UserData = ();
    type Location = ::rusty_lr::DefaultLocation;
    fn new_empty() -> Self {
        Self::Empty
    }
    fn new_terminal(term: Self::Term) -> Self {
        Self::__terminals(term)
    }
    fn reduce_action(
        data_stack: &mut Vec<Self>,
        location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        push_data: bool,
        rule_index: usize,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Self::Term>,
        user_data: &mut Self::UserData,
        location0: &mut Self::Location,
    ) -> Result<(), Self::ReduceActionError> {
        match rule_index {
            0usize => {
                Self::reduce_Constant_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            1usize => {
                Self::reduce_Constant_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            2usize => {
                Self::reduce_Constant_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            3usize => {
                Self::reduce_Constant_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            4usize => {
                Self::reduce_Constant_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            5usize => {
                Self::reduce_Constant_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            6usize => {
                Self::reduce_Constant_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            7usize => {
                Self::reduce_primary_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            9usize => {
                Self::reduce_primary_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            10usize => {
                Self::reduce_primary_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            12usize => {
                Self::reduce_postfix_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            13usize => {
                Self::reduce_postfix_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            14usize => {
                Self::reduce_postfix_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            15usize => {
                Self::reduce_postfix_expression_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            16usize => {
                Self::reduce_postfix_expression_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            17usize => {
                Self::reduce_postfix_expression_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            18usize => {
                Self::reduce_postfix_expression_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            19usize => {
                Self::reduce_argument_expression_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            20usize => {
                Self::reduce_argument_expression_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            21usize => {
                Self::reduce_unary_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            22usize => {
                Self::reduce_unary_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            23usize => {
                Self::reduce_unary_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            24usize => {
                Self::reduce_unary_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            25usize => {
                Self::reduce_unary_expression_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            26usize => {
                Self::reduce_unary_expression_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            27usize => {
                Self::reduce_unary_expression_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            28usize => {
                Self::reduce_unary_expression_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            29usize => {
                Self::reduce_unary_expression_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            30usize => {
                Self::reduce_unary_expression_9(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            31usize => {
                Self::reduce_unary_expression_10(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            32usize => {
                Self::reduce_cast_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            33usize => {
                Self::reduce_cast_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            35usize => {
                Self::reduce_multiplicative_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            36usize => {
                Self::reduce_multiplicative_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            37usize => {
                Self::reduce_multiplicative_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            38usize => {
                Self::reduce_additive_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            39usize => {
                Self::reduce_additive_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            40usize => {
                Self::reduce_additive_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            41usize => {
                Self::reduce_shift_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            42usize => {
                Self::reduce_shift_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            43usize => {
                Self::reduce_shift_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            44usize => {
                Self::reduce_relational_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            45usize => {
                Self::reduce_relational_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            46usize => {
                Self::reduce_relational_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            47usize => {
                Self::reduce_relational_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            48usize => {
                Self::reduce_relational_expression_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            49usize => {
                Self::reduce_equality_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            50usize => {
                Self::reduce_equality_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            51usize => {
                Self::reduce_equality_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            52usize => {
                Self::reduce_and_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            53usize => {
                Self::reduce_and_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            54usize => {
                Self::reduce_exclusive_or_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            55usize => {
                Self::reduce_exclusive_or_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            56usize => {
                Self::reduce_inclusive_or_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            57usize => {
                Self::reduce_inclusive_or_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            58usize => {
                Self::reduce_logical_and_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            59usize => {
                Self::reduce_logical_and_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            60usize => {
                Self::reduce_logical_or_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            61usize => {
                Self::reduce_logical_or_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            62usize => {
                Self::reduce_conditional_expression_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            63usize => {
                Self::reduce_conditional_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            65usize => {
                Self::reduce_assignment_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            66usize => {
                Self::reduce_assignment_expression_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            67usize => {
                Self::reduce_assignment_expression_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            68usize => {
                Self::reduce_assignment_expression_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            69usize => {
                Self::reduce_assignment_expression_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            70usize => {
                Self::reduce_assignment_expression_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            71usize => {
                Self::reduce_assignment_expression_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            72usize => {
                Self::reduce_assignment_expression_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            73usize => {
                Self::reduce_assignment_expression_9(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            74usize => {
                Self::reduce_assignment_expression_10(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            75usize => {
                Self::reduce_assignment_expression_11(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            77usize => {
                Self::reduce_expression_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            79usize => {
                Self::reduce_initializer_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            80usize => {
                Self::reduce_initializer_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            81usize => {
                Self::reduce_initializer_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            82usize => {
                Self::reduce_initializer_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            83usize => {
                Self::reduce_labeled_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            84usize => {
                Self::reduce_labeled_statement_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            85usize => {
                Self::reduce_labeled_statement_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            88usize => {
                Self::reduce_compound_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            89usize => {
                Self::reduce_expression_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            90usize => {
                Self::reduce_expression_statement_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            91usize => {
                Self::reduce_selection_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            92usize => {
                Self::reduce_selection_statement_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            93usize => {
                Self::reduce_selection_statement_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            96usize => {
                Self::reduce_iteration_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            97usize => {
                Self::reduce_iteration_statement_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            98usize => {
                Self::reduce_iteration_statement_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            99usize => {
                Self::reduce_iteration_statement_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            100usize => {
                Self::reduce_jump_statement_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            101usize => {
                Self::reduce_jump_statement_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            102usize => {
                Self::reduce_jump_statement_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            103usize => {
                Self::reduce_jump_statement_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            104usize => {
                Self::reduce_jump_statement_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            105usize => {
                Self::reduce_declaration_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            106usize => {
                Self::reduce_declaration_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            107usize => {
                Self::reduce_function_definition_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            108usize => {
                Self::reduce_function_definition_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            117usize => {
                Self::reduce_translation_unit_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            118usize => {
                Self::reduce_type_qualifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            119usize => {
                Self::reduce_type_qualifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            120usize => {
                Self::reduce_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            121usize => {
                Self::reduce_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            122usize => {
                Self::reduce_direct_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            123usize => {
                Self::reduce_direct_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            124usize => {
                Self::reduce_direct_declarator_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            125usize => {
                Self::reduce_direct_declarator_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            126usize => {
                Self::reduce_direct_declarator_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            127usize => {
                Self::reduce_direct_declarator_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            128usize => {
                Self::reduce_abstract_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            129usize => {
                Self::reduce_abstract_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            130usize => {
                Self::reduce_abstract_declarator_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            131usize => {
                Self::reduce_direct_abstract_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            132usize => {
                Self::reduce_direct_abstract_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            133usize => {
                Self::reduce_direct_abstract_declarator_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            134usize => {
                Self::reduce_direct_abstract_declarator_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            135usize => {
                Self::reduce_direct_abstract_declarator_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            136usize => {
                Self::reduce_direct_abstract_declarator_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            137usize => {
                Self::reduce_direct_abstract_declarator_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            138usize => {
                Self::reduce_direct_abstract_declarator_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            139usize => {
                Self::reduce_direct_abstract_declarator_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            140usize => {
                Self::reduce_specifier_qualifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            141usize => {
                Self::reduce_specifier_qualifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            142usize => {
                Self::reduce_type_name_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            143usize => {
                Self::reduce_type_specifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            144usize => {
                Self::reduce_type_specifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            145usize => {
                Self::reduce_type_specifier_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            146usize => {
                Self::reduce_type_specifier_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            147usize => {
                Self::reduce_type_specifier_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            148usize => {
                Self::reduce_type_specifier_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            149usize => {
                Self::reduce_type_specifier_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            150usize => {
                Self::reduce_type_specifier_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            151usize => {
                Self::reduce_type_specifier_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            152usize => {
                Self::reduce_type_specifier_9(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            153usize => {
                Self::reduce_type_specifier_10(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            154usize => {
                Self::reduce_type_specifier_11(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            155usize => {
                Self::reduce_storage_class_specifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            156usize => {
                Self::reduce_storage_class_specifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            157usize => {
                Self::reduce_storage_class_specifier_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            158usize => {
                Self::reduce_storage_class_specifier_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            159usize => {
                Self::reduce_storage_class_specifier_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            160usize => {
                Self::reduce_declaration_specifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            161usize => {
                Self::reduce_declaration_specifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            162usize => {
                Self::reduce_declaration_specifier_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            163usize => {
                Self::reduce_parameter_declaration_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            164usize => {
                Self::reduce_parameter_declaration_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            165usize => {
                Self::reduce_parameter_declaration_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            166usize => {
                Self::reduce_parameter_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            167usize => {
                Self::reduce_parameter_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            168usize => {
                Self::reduce_parameter_type_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            169usize => {
                Self::reduce_parameter_type_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            170usize => {
                Self::reduce_struct_or_union_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            171usize => {
                Self::reduce_struct_or_union_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            172usize => {
                Self::reduce_struct_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            173usize => {
                Self::reduce_struct_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            174usize => {
                Self::reduce_struct_declarator_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            175usize => {
                Self::reduce_struct_declarator_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            176usize => {
                Self::reduce_struct_declaration_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            177usize => {
                Self::reduce_struct_or_union_specifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            178usize => {
                Self::reduce_struct_or_union_specifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            179usize => {
                Self::reduce_enum_specifier_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            180usize => {
                Self::reduce_enum_specifier_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            181usize => {
                Self::reduce_enumerator_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            182usize => {
                Self::reduce_enumerator_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            183usize => {
                Self::reduce_enumerator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            184usize => {
                Self::reduce_enumerator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            185usize => {
                Self::reduce_init_declarator_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            186usize => {
                Self::reduce_init_declarator_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            187usize => {
                Self::reduce_init_declarator_list_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            188usize => {
                Self::reduce_init_declarator_list_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            189usize => {
                Self::reduce__statement_or_declarationPlus58_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            190usize => {
                Self::reduce__statement_or_declarationPlus58_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            191usize => {
                Self::reduce__statement_or_declarationStar59_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            192usize => {
                Self::reduce__statement_or_declarationStar59_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            193usize => {
                Self::reduce__declaration_specifierPlus60_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            194usize => {
                Self::reduce__declaration_specifierPlus60_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            195usize => {
                Self::reduce__external_declarationPlus61_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            196usize => {
                Self::reduce__external_declarationPlus61_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            197usize => {
                Self::reduce__external_declarationStar62_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            198usize => {
                Self::reduce__external_declarationStar62_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            199usize => {
                Self::reduce__type_qualifierPlus63_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            200usize => {
                Self::reduce__type_qualifierPlus63_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            201usize => {
                Self::reduce__type_qualifierStar64_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            202usize => {
                Self::reduce__type_qualifierStar64_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            203usize => {
                Self::reduce__specifier_qualifierPlus65_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            204usize => {
                Self::reduce__specifier_qualifierPlus65_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            205usize => {
                Self::reduce__abstract_declaratorQuestion66_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            206usize => {
                Self::reduce__abstract_declaratorQuestion66_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            207usize => {
                Self::reduce__declaratorQuestion67_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            208usize => {
                Self::reduce__declaratorQuestion67_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            209usize => {
                Self::reduce__identQuestion68_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            210usize => {
                Self::reduce__identQuestion68_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            211usize => {
                Self::reduce__struct_declarationPlus69_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            212usize => {
                Self::reduce__struct_declarationPlus69_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            213usize => {
                Self::reduce__struct_declarationStar70_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            214usize => {
                Self::reduce__struct_declarationStar70_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            _ => {
                unreachable!("Invalid Rule: {}", rule_index);
            }
        }
    }
}
/// A lightweight parser struct that references the static parser tables and production rules.
///
/// Since this struct only holds `'static` references to shared, read-only static parser tables,
/// it is extremely cheap to instantiate, copy, or clone, and takes very little space.
#[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
#[derive(Clone, Copy)]
pub struct Parser;
unsafe impl ::std::marker::Send for Parser {}
unsafe impl ::std::marker::Sync for Parser {}
#[rustfmt::skip]
impl ::rusty_lr::parser::Parser for Parser {
    type Term = Token;
    type TermClass = TerminalClasses;
    type NonTerm = NonTerminals;
    type StateIndex = u16;
    type ReduceRules = ::rusty_lr::parser::table::ArrayVec<u8, 2usize>;
    type Tables = Tables;
    const ERROR_USED: bool = false;
    fn get_tables() -> &'static Tables {
        static TABLES: std::sync::OnceLock<Tables> = std::sync::OnceLock::new();
        TABLES
            .get_or_init(|| {
                static RULE_NAMES: &[u32] = &[
                    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4,
                    4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9,
                    9, 9, 9, 10, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16,
                    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 19, 19, 19,
                    20, 20, 21, 21, 21, 22, 22, 23, 24, 24, 25, 25, 25, 26, 26, 27, 27,
                    27, 27, 28, 28, 28, 28, 28, 29, 29, 30, 30, 31, 31, 31, 31, 31, 31,
                    32, 32, 33, 34, 34, 35, 35, 36, 36, 36, 36, 36, 36, 37, 37, 37, 38,
                    38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 40, 41, 41, 41, 41, 41, 41,
                    41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42, 43, 43, 43, 44, 44, 44,
                    45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 50, 51, 51, 52, 52, 53, 53,
                    54, 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 59, 60, 60, 61, 61, 62,
                    62, 63, 63, 64, 64, 65, 65, 66, 66, 67, 67, 68, 68, 69, 69, 70,
                ];
                static RULE_LENGTHS: &[u32] = &[
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 4, 3, 4, 3, 3, 2, 2, 1, 3, 1, 2,
                    2, 2, 2, 2, 2, 2, 2, 2, 4, 1, 4, 1, 3, 3, 3, 1, 3, 3, 1, 3, 3, 1, 3,
                    3, 3, 3, 1, 3, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 5, 1, 3, 3, 3, 3,
                    3, 3, 3, 3, 3, 3, 3, 1, 3, 1, 3, 4, 1, 3, 3, 4, 3, 1, 1, 3, 1, 2, 5,
                    7, 5, 1, 1, 5, 7, 6, 7, 3, 2, 2, 2, 3, 2, 3, 3, 2, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 3, 1, 3, 4, 3, 4, 3, 3, 2, 1, 3, 2, 3, 3, 4, 2, 3,
                    3, 4, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 2, 2, 1, 1, 3, 1, 3, 1, 1, 1, 3, 1, 3, 3, 5, 2, 5, 2, 1, 3, 1,
                    3, 1, 3, 1, 3, 1, 2, 1, 0, 1, 2, 1, 2, 1, 0, 1, 2, 1, 0, 1, 2, 1, 0,
                    1, 0, 1, 0, 1, 2, 1, 0, 3,
                ];
                static SHIFT_TERM_DATA: &[u32] = &[
                    2147516506, 2147549184, 98305, 163870, 557124, 589893, 622662,
                    655431, 688200, 196681, 229450, 720971, 753740, 786509, 819278,
                    852047, 884816, 917585, 950354, 983123, 1015892, 1048661, 1081430,
                    2147614720, 98305, 163870, 196681, 229450, 196681, 229450,
                    2147614720, 98305, 163870, 458753, 7569419, 2147975168, 524290,
                    557124, 589893, 622662, 655431, 688200, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 2148597760, 1179661, 2148696064, 1245229,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148827136, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    196681, 229450, 720971, 753740, 786509, 819278, 852047, 884816,
                    917585, 950354, 983123, 1015892, 1048661, 1081430, 2148761600,
                    1671169, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2148761600,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2148761600,
                    1671169, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2148761600,
                    1769473, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2148827136,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 196681, 229450,
                    720971, 753740, 786509, 819278, 852047, 884816, 917585, 950354,
                    983123, 1015892, 1048661, 1081430, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2031617, 5439499, 5668884,
                    5734421, 5799958, 5832727, 2148761600, 1310721, 2064386, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2129922, 2162703, 2148761600,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2228269,
                    6389806, 6455343, 6520880, 6586417, 6651954, 6717491, 6783028,
                    6848565, 6914102, 6979639, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2293790, 2981919, 3047456,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2359310, 2392079, 2148696064, 2523164, 2916381, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2588705,
                    2850850, 2148761600, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 2654243, 2785316, 3112999, 3178536, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2719785,
                    3244074, 2148761600, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 2654243, 2785316, 3112999, 3178536, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2588705,
                    2850850, 2148761600, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 2523164, 2916381, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2293790, 2981919, 3047456,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2588705, 2850850, 2148761600, 1310721, 2148859907, 2148892676,
                    2148925445, 2148958214, 2148990983, 2149023752, 2149056521,
                    2149089290, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 2588705, 2850850, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2654243,
                    2785316, 3112999, 3178536, 3342361, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 2719785, 3244074, 3440677,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    3342361, 3538982, 2148761600, 1310721, 2148859907, 2148892676,
                    2148925445, 2148958214, 2148990983, 2149023752, 2149056521,
                    2149089290, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 3440677, 3637291, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 3538982,
                    3735570, 6291500, 2148761600, 1310721, 2148859907, 2148892676,
                    2148925445, 2148958214, 2148990983, 2149023752, 2149056521,
                    2149089290, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 3801090, 3833871, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 3997698,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2151579648, 4161549, 2147975168, 196681, 229450, 720971, 753740,
                    786509, 819278, 852047, 884816, 917585, 950354, 983123, 1015892,
                    1048661, 1081430, 2147549184, 98305, 163870, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 4358146, 4587535, 2147975168, 4620305,
                    557124, 589893, 622662, 655431, 688200, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 2147975168, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 4784142, 2147549184, 4849665, 4915211,
                    7438366, 557124, 589893, 622662, 655431, 688200, 196681, 229450,
                    720971, 753740, 786509, 819278, 852047, 884816, 917585, 950354,
                    983123, 1015892, 1048661, 1081430, 2147549184, 4849665, 4882434,
                    4915211, 7438366, 557124, 589893, 622662, 655431, 688200, 196681,
                    229450, 720971, 753740, 786509, 819278, 852047, 884816, 917585,
                    950354, 983123, 1015892, 1048661, 1081430, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 7340044, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    4980738, 2147975168, 5046273, 4915211, 5079070, 196681, 229450,
                    720971, 753740, 786509, 819278, 852047, 884816, 917585, 950354,
                    983123, 1015892, 1048661, 1081430, 2147975168, 5046273, 4882434,
                    4915211, 5079070, 557124, 589893, 622662, 655431, 688200, 196681,
                    229450, 720971, 753740, 786509, 819278, 852047, 884816, 917585,
                    950354, 983123, 1015892, 1048661, 1081430, 196681, 229450, 5046273,
                    4915211, 5079070, 5210113, 5341195, 2147975168, 5242882, 557124,
                    589893, 622662, 655431, 688200, 196681, 229450, 720971, 753740,
                    786509, 819278, 852047, 884816, 917585, 950354, 983123, 1015892,
                    1048661, 1081430, 5308418, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 5373964, 1638422, 1703959, 1736728, 1802265,
                    1835034, 1867803, 1900572, 1933341, 1966110, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2293790,
                    2981919, 3047456, 2523164, 2916381, 2588705, 2850850, 5636108,
                    3833871, 2153185280, 2153250816, 3833871, 6225939, 2148761600,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2148761600,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 3637291,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    7077900, 7143426, 7208962, 7405580, 196681, 229450, 2147614720,
                    4849665, 4915211, 7438366, 7536642, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 7602188, 1638422, 1703959, 1736728, 1802265,
                    1835034, 1867803, 1900572, 1933341, 1966110, 7667724, 7897103,
                    8060944, 2147614720, 98305, 163870, 7995411, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 2155708505,
                    8290317, 2155806720, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 8290317,
                    8421392, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 8454200, 8552505, 8618042, 8749116,
                    8880189, 9011262, 9044031, 9273408, 9371713, 9437250, 9502787,
                    557124, 589893, 622662, 655431, 688200, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 8355859, 2155872256, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 8290317, 8421392, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 8454200,
                    8552505, 8618042, 8749116, 8880189, 9011262, 9044031, 9273408,
                    9371713, 9437250, 9502787, 8355859, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 8519699, 2155872256, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8290317, 8421392, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 8454200, 8552505, 8618042, 8749116, 8880189, 9011262,
                    9044031, 9273408, 9371713, 9437250, 9502787, 8585235, 2155872256,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8290317, 8421392, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 8454200, 8552505, 8618042, 8749116, 8880189, 9011262,
                    9044031, 9273408, 9371713, 9437250, 9502787, 8650753, 2148761600,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 8716290,
                    3833871, 2155872256, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 8290317,
                    8421392, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 8454200, 8552505, 8618042, 8749116,
                    8880189, 9011262, 9044031, 9273408, 9371713, 9437250, 9502787,
                    8781825, 2148761600, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 8847362, 3833871, 2155872256, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 8290317, 8421392, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 8454200,
                    8552505, 8618042, 8749116, 8880189, 9011262, 9044031, 9273408,
                    9371713, 9437250, 9502787, 8912897, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 1638422, 1703959, 1736728, 1802265, 1835034,
                    1867803, 1900572, 1933341, 1966110, 8978434, 3833871, 2155872256,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8290317, 8421392, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 8454200, 8552505, 8618042, 8749116, 8880189, 9011262,
                    9044031, 9273408, 9371713, 9437250, 9502787, 2155872256, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8290317, 8421392, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 8454200, 8552505, 8618042, 8749116, 8880189, 9011262,
                    9044031, 9273408, 9371713, 9437250, 9502787, 9076737, 2148827136,
                    1310721, 2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8421392, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    557124, 589893, 622662, 655431, 688200, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 3833871, 9142288, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 8421392, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    2148761600, 1310721, 9240578, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 1638422,
                    1703959, 1736728, 1802265, 1835034, 1867803, 1900572, 1933341,
                    1966110, 2155872256, 1310721, 2148859907, 2148892676, 2148925445,
                    2148958214, 2148990983, 2149023752, 2149056521, 2149089290, 8290317,
                    8421392, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 8454200, 8552505, 8618042, 8749116,
                    8880189, 9011262, 9044031, 9273408, 9371713, 9437250, 9502787,
                    2156789760, 9338896, 9404432, 9469968, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 9535504, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    3833871, 9601040, 9699330, 3833871, 2155872256, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 8290317, 8421392, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 8454200,
                    8552505, 8618042, 8749116, 8880189, 9011262, 9044031, 9273408,
                    9371713, 9437250, 9502787, 2147549184, 98305, 9797648, 163870,
                    557124, 589893, 622662, 655431, 688200, 196681, 229450, 720971,
                    753740, 786509, 819278, 852047, 884816, 917585, 950354, 983123,
                    1015892, 1048661, 1081430, 9863213, 2148761600, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 9895949, 1638422, 1703959, 1736728, 1802265,
                    1835034, 1867803, 1900572, 1933341, 1966110, 2148761600, 1310721,
                    2148859907, 2148892676, 2148925445, 2148958214, 2148990983,
                    2149023752, 2149056521, 2149089290, 9895949, 1638422, 1703959,
                    1736728, 1802265, 1835034, 1867803, 1900572, 1933341, 1966110,
                    9994254, 10027023, 2148761600, 1310721, 2148859907, 2148892676,
                    2148925445, 2148958214, 2148990983, 2149023752, 2149056521,
                    2149089290, 9895949, 10059790, 1638422, 1703959, 1736728, 1802265,
                    1835034, 1867803, 1900572, 1933341, 1966110, 10223631, 10289168,
                    2147614720, 98305, 163870, 2155806720, 1310721, 2148859907,
                    2148892676, 2148925445, 2148958214, 2148990983, 2149023752,
                    2149056521, 2149089290, 8290317, 8421392, 1638422, 1703959, 1736728,
                    1802265, 1835034, 1867803, 1900572, 1933341, 1966110, 8454200,
                    8552505, 8618042, 8749116, 8880189, 9011262, 9044031, 9273408,
                    9371713, 9437250, 9502787, 557124, 589893, 622662, 655431, 688200,
                    196681, 229450, 720971, 753740, 786509, 819278, 852047, 884816,
                    917585, 950354, 983123, 1015892, 1048661, 1081430, 10453006,
                    10518589, 10551297, 2148761600, 1310721, 2148859907, 2148892676,
                    2148925445, 2148958214, 2148990983, 2149023752, 2149056521,
                    2149089290, 1638422, 1703959, 1736728, 1802265, 1835034, 1867803,
                    1900572, 1933341, 1966110, 10616834, 3833871, 10649616, 10780731,
                    2155872256, 1310721, 2148859907, 2148892676, 2148925445, 2148958214,
                    2148990983, 2149023752, 2149056521, 2149089290, 8290317, 8421392,
                    1638422, 1703959, 1736728, 1802265, 1835034, 1867803, 1900572,
                    1933341, 1966110, 8454200, 8552505, 8618042, 8749116, 8880189,
                    9011262, 9044031, 9273408, 9371713, 9437250, 9502787, 2147549184,
                    98305, 9797648, 163870, 557124, 589893, 622662, 655431, 688200,
                    196681, 229450, 720971, 753740, 786509, 819278, 852047, 884816,
                    917585, 950354, 983123, 1015892, 1048661, 1081430, 8290317, 9863213,
                    2147549184, 98305, 163870, 557124, 589893, 622662, 655431, 688200,
                    196681, 229450, 720971, 753740, 786509, 819278, 852047, 884816,
                    917585, 950354, 983123, 1015892, 1048661, 1081430,
                ];
                static SHIFT_TERM_OFFSETS: &[u32] = &[
                    0, 1, 23, 23, 26, 26, 28, 28, 28, 28, 30, 30, 33, 33, 35, 56, 56, 56,
                    56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 57,
                    57, 58, 59, 60, 79, 79, 112, 112, 112, 112, 112, 112, 112, 112, 112,
                    112, 131, 150, 169, 188, 221, 240, 259, 278, 297, 316, 335, 341, 361,
                    361, 363, 363, 382, 393, 412, 415, 434, 436, 436, 437, 437, 437, 439,
                    458, 460, 479, 483, 502, 504, 523, 527, 546, 548, 567, 569, 588, 591,
                    610, 610, 629, 629, 648, 650, 669, 671, 690, 694, 695, 714, 716, 717,
                    736, 737, 738, 757, 758, 759, 778, 779, 781, 800, 802, 802, 821, 821,
                    821, 821, 822, 841, 841, 842, 842, 843, 858, 858, 858, 858, 875, 876,
                    876, 876, 876, 876, 876, 876, 877, 898, 898, 898, 913, 913, 914, 914,
                    937, 961, 961, 981, 982, 982, 1000, 1024, 1026, 1029, 1029, 1031,
                    1052, 1052, 1053, 1053, 1073, 1073, 1073, 1092, 1092, 1095, 1097,
                    1099, 1101, 1101, 1102, 1102, 1103, 1103, 1103, 1103, 1103, 1103,
                    1103, 1103, 1103, 1103, 1103, 1103, 1103, 1103, 1105, 1124, 1124,
                    1143, 1144, 1144, 1163, 1163, 1182, 1182, 1201, 1201, 1220, 1220,
                    1239, 1239, 1258, 1258, 1277, 1277, 1296, 1296, 1315, 1315, 1334,
                    1334, 1335, 1335, 1336, 1336, 1337, 1337, 1337, 1337, 1337, 1337,
                    1338, 1338, 1340, 1344, 1345, 1345, 1365, 1365, 1366, 1366, 1366,
                    1366, 1366, 1366, 1366, 1368, 1371, 1371, 1372, 1391, 1391, 1391,
                    1391, 1391, 1391, 1392, 1392, 1393, 1444, 1445, 1477, 1478, 1478,
                    1497, 1498, 1530, 1531, 1563, 1564, 1583, 1585, 1617, 1618, 1637,
                    1639, 1671, 1672, 1691, 1693, 1725, 1757, 1758, 1797, 1799, 1799,
                    1819, 1839, 1871, 1872, 1873, 1873, 1874, 1874, 1875, 1875, 1895,
                    1895, 1897, 1897, 1897, 1899, 1931, 1931, 1954, 1954, 1955, 1975,
                    1995, 1995, 1997, 1997, 2018, 2018, 2018, 2018, 2018, 2020, 2023,
                    2023, 2023, 2023, 2074, 2074, 2075, 2075, 2076, 2077, 2096, 2098,
                    2099, 2099, 2099, 2099, 2100, 2132, 2132, 2132, 2132, 2132, 2132,
                    2155, 2157, 2157, 2179, 2179, 2179,
                ];
                static SHIFT_NONTERM_DATA: &[u32] = &[
                    2155642909, 2155642910, 2155642912, 2155675681, 2151874594,
                    2155741219, 2147909668, 2151907369, 2151940138, 2151972907,
                    2151546927, 2151710771, 2151743540, 2158460987, 2158559292,
                    2158624829, 2151809059, 2147909668, 2147745826, 2147778622,
                    2147844159, 2147811362, 2147876899, 2147909668, 2151874594,
                    2151907369, 2151940138, 2151972907, 2152005676, 2152038445,
                    2154987566, 2151546927, 2151710771, 2151743540, 2152300603,
                    2148630595, 2149810229, 2149941302, 2149482496, 2149482497,
                    2149482498, 2149744644, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2155610128, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2151251984,
                    2151251985, 2151251986, 2151383074, 2151415847, 2151448616,
                    2151514153, 2151546927, 2151710771, 2151743540, 2152497216,
                    2149482496, 2149482497, 2149482498, 2153644036, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2151251984,
                    2151251985, 2151251986, 2149482496, 2149482497, 2149482498,
                    2153611268, 2149482496, 2149482497, 2149482498, 2153578500,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2151251984, 2151251985, 2151251986, 2151383074, 2151415847,
                    2152431656, 2151514153, 2151546927, 2151710771, 2151743540,
                    2152497216, 2149482496, 2149482497, 2149482498, 2153545732,
                    2153545733, 2149482496, 2149482497, 2149482498, 2153480196,
                    2153480197, 2149482496, 2149482497, 2149482498, 2153447428,
                    2153447429, 2149482496, 2149482497, 2149482498, 2153414660,
                    2153414661, 2149482496, 2149482497, 2149482498, 2153381892,
                    2153381893, 2149482496, 2149482497, 2149482498, 2153349124,
                    2153349125, 2149482496, 2149482497, 2149482498, 2149580803,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2152890384, 2152890385, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2155577360,
                    2155577361, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2153840656, 2153840657, 2149482496, 2149482497,
                    2149482498, 2152955908, 2152955909, 2149908534, 2149482496,
                    2149482497, 2149482498, 2152988676, 2152988677, 2152988678,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2153021447, 2149482496, 2149482497, 2149482498,
                    2149744644, 2149744645, 2149744646, 2149974023, 2153054216,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150236169, 2149482496,
                    2149482497, 2149482498, 2149744644, 2149744645, 2149744646,
                    2149974023, 2150301704, 2149482496, 2149482497, 2149482498,
                    2149744644, 2149744645, 2149744646, 2150367239, 2149482496,
                    2149482497, 2149482498, 2150432772, 2150432773, 2150432774,
                    2149482496, 2149482497, 2149482498, 2150498308, 2150498309,
                    2149482496, 2149482497, 2149482498, 2150563844, 2150563845,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150629384, 2149482496, 2149482497,
                    2149482498, 2149744644, 2149744645, 2149744646, 2149974023,
                    2150694920, 2149482496, 2149482497, 2149482498, 2149744644,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150760457,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150858762,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150957067, 2149482496, 2149482497, 2149482498, 2149744644,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2151055372, 2149482496, 2149482497,
                    2149482498, 2149744644, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2151153677, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2153676816, 2153676817, 2153676818, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2151350288,
                    2151350289, 2149482496, 2149482497, 2149482498, 2153512964,
                    2153512965, 2151612483, 2151383074, 2151415847, 2151514153,
                    2151546927, 2151678002, 2151710771, 2151743540, 2151776320,
                    2152169540, 2152235077, 2151383074, 2155282467, 2147909668,
                    2154758183, 2151514153, 2151546927, 2155315248, 2155348017,
                    2151710771, 2151743540, 2155446338, 2151874594, 2151907369,
                    2151940138, 2151972907, 2152136748, 2151546927, 2151710771,
                    2151743540, 2152300603, 2151383074, 2151415847, 2151514153,
                    2151546927, 2152202290, 2151710771, 2151743540, 2151776320,
                    2151874594, 2155184163, 2147909668, 2155216933, 2152661030,
                    2151907369, 2151940138, 2155249707, 2151546927, 2151710771,
                    2151743540, 2151874594, 2151809059, 2147909668, 2154594341,
                    2152661030, 2151907369, 2151940138, 2151972907, 2152005676,
                    2152038445, 2154659886, 2151546927, 2151710771, 2151743540,
                    2152300603, 2149482496, 2149482497, 2149482498, 2149744644,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2154856464, 2151383074, 2154725413, 2152661030,
                    2154758183, 2151514153, 2151546927, 2151710771, 2151743540,
                    2154790977, 2151874594, 2154594341, 2152661030, 2151907369,
                    2151940138, 2151972907, 2152005676, 2152038445, 2154659886,
                    2151546927, 2151710771, 2151743540, 2152300603, 2147745826,
                    2147778622, 2152595519, 2152628261, 2152661030, 2151874594,
                    2151907369, 2151940138, 2151972907, 2152005676, 2152038445,
                    2152759342, 2151546927, 2151710771, 2151743540, 2152300603,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2154528784, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2153086992, 2153086993, 2153086994, 2149482496,
                    2149482497, 2149482498, 2149744644, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2153742352,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2153807886, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2153906192,
                    2153906193, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2153971728, 2153971729, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2154037264, 2154037265,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2154102800, 2154102801, 2149482496, 2149482497, 2149482498,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2154168336, 2154168337, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2154233872,
                    2154233873, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2154299408, 2154299409, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2154364944, 2154364945,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2154430480, 2154430481, 2149482496, 2149482497, 2149482498,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2154496016, 2154496017, 2147745826,
                    2147778622, 2154954815, 2147876899, 2147909668, 2152628261,
                    2152661030, 2149482496, 2149482497, 2149482498, 2149744644,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2155118608, 2155282467, 2147909668, 2155413552,
                    2155446338, 2149482496, 2149482497, 2149482498, 2149744644,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2155511824, 2158428183, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2156593168, 2156593169,
                    2156593170, 2157805589, 2157805590, 2157805591, 2157805592,
                    2157805593, 2157805595, 2157805596, 2157805597, 2157805599,
                    2151874594, 2151907369, 2151940138, 2151972907, 2151546927,
                    2151710771, 2151743540, 2157838393, 2157903930, 2157248571,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2156593168, 2156593169, 2156593170, 2158395413, 2158395415,
                    2158395416, 2158395417, 2158395419, 2158395420, 2158395423,
                    2149482496, 2149482497, 2149482498, 2149744644, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2155970576, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2156593168, 2156593169, 2156593170, 2158362645,
                    2158362647, 2158362648, 2158362649, 2158362651, 2158362652,
                    2158362655, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2156593168, 2156593169, 2156593170, 2158329877,
                    2158329879, 2158329880, 2158329881, 2158329883, 2158329884,
                    2158329887, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2156167184, 2156167185, 2156167186, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2156593168,
                    2156593169, 2156593170, 2158231573, 2158231575, 2158231576,
                    2158231577, 2158231579, 2158231580, 2158231583, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2156298256,
                    2156298257, 2156298258, 2149482496, 2149482497, 2149482498,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2156593168, 2156593169, 2156593170,
                    2158198805, 2158198807, 2158198808, 2158198809, 2158198811,
                    2158198812, 2158198815, 2149482496, 2149482497, 2149482498,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2156429328, 2156429329, 2156429330,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2156593168, 2156593169, 2156593170, 2158166037, 2158166039,
                    2158166040, 2158166041, 2158166043, 2158166044, 2158166047,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2156593168, 2156593169, 2156593170, 2157969429, 2157969431,
                    2157969432, 2157969433, 2157969435, 2157969436, 2157969439,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2156593168, 2156593169, 2156593170, 2156658712, 2156658714,
                    2156658717, 2151874594, 2151907369, 2151940138, 2151972907,
                    2151546927, 2151710771, 2151743540, 2157248571, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2156593168,
                    2156593169, 2156593170, 2156691480, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2157150224, 2157150225,
                    2157150226, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2156593168, 2156593169, 2156593170, 2157117461,
                    2157117463, 2157117464, 2157117465, 2157117467, 2157117468,
                    2157117471, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2157051920, 2157051921, 2157051922, 2149482496,
                    2149482497, 2149482498, 2149679108, 2149744645, 2149744646,
                    2149974023, 2150039560, 2150105097, 2150170634, 2150793227,
                    2150891532, 2150989837, 2151088142, 2151186447, 2156593168,
                    2156593169, 2156593170, 2157215765, 2157215767, 2157215768,
                    2157215769, 2157215771, 2157215772, 2157215775, 2151874594,
                    2157314083, 2147909668, 2151907369, 2151940138, 2155249707,
                    2151546927, 2151710771, 2151743540, 2157641783, 2157674552,
                    2149482496, 2149482497, 2149482498, 2149679108, 2149744645,
                    2149744646, 2149974023, 2150039560, 2150105097, 2150170634,
                    2150793227, 2150891532, 2150989837, 2151088142, 2151186447,
                    2157608976, 2157608977, 2157608979, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2157412368, 2157412369,
                    2157412371, 2157445140, 2149482496, 2149482497, 2149482498,
                    2149679108, 2149744645, 2149744646, 2149974023, 2150039560,
                    2150105097, 2150170634, 2150793227, 2150891532, 2150989837,
                    2151088142, 2151186447, 2157576208, 2157576209, 2157576211,
                    2157314083, 2147909668, 2157740087, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2156593168, 2156593169,
                    2156593170, 2157871125, 2157871126, 2157871127, 2157871128,
                    2157871129, 2157871131, 2157871132, 2157871133, 2157871135,
                    2151874594, 2151907369, 2151940138, 2151972907, 2151546927,
                    2151710771, 2151743540, 2157248571, 2149482496, 2149482497,
                    2149482498, 2149679108, 2149744645, 2149744646, 2149974023,
                    2150039560, 2150105097, 2150170634, 2150793227, 2150891532,
                    2150989837, 2151088142, 2151186447, 2158067728, 2158067729,
                    2158067730, 2149482496, 2149482497, 2149482498, 2149679108,
                    2149744645, 2149744646, 2149974023, 2150039560, 2150105097,
                    2150170634, 2150793227, 2150891532, 2150989837, 2151088142,
                    2151186447, 2156593168, 2156593169, 2156593170, 2158297109,
                    2158297111, 2158297112, 2158297113, 2158297115, 2158297116,
                    2158297119, 2151874594, 2158493731, 2147909668, 2151907369,
                    2151940138, 2155249707, 2151546927, 2151710771, 2151743540,
                    2157641783, 2157674552, 2158526487, 2158592029, 2158592030,
                    2158592032, 2151874594, 2155741219, 2147909668, 2151907369,
                    2151940138, 2151972907, 2151546927, 2151710771, 2151743540,
                    2158460987,
                ];
                static SHIFT_NONTERM_OFFSETS: &[u32] = &[
                    0, 0, 16, 16, 18, 18, 21, 21, 21, 21, 22, 22, 24, 24, 24, 35, 35, 35,
                    35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36,
                    36, 36, 38, 38, 54, 54, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 84,
                    102, 106, 110, 136, 141, 146, 151, 156, 161, 166, 166, 184, 184, 184,
                    184, 201, 201, 218, 218, 223, 223, 223, 224, 224, 224, 224, 230, 230,
                    237, 237, 245, 245, 254, 254, 262, 262, 269, 269, 275, 275, 280, 280,
                    285, 285, 293, 293, 301, 301, 310, 310, 310, 320, 320, 320, 331, 331,
                    331, 343, 343, 343, 356, 356, 356, 374, 374, 374, 391, 391, 391, 391,
                    391, 396, 396, 397, 397, 397, 407, 407, 407, 407, 418, 418, 418, 418,
                    418, 418, 418, 418, 418, 427, 427, 427, 435, 435, 435, 435, 446, 461,
                    461, 477, 477, 477, 486, 499, 502, 504, 504, 504, 515, 515, 515, 515,
                    531, 531, 531, 549, 549, 549, 549, 549, 549, 549, 549, 549, 549, 549,
                    549, 549, 549, 549, 549, 549, 549, 549, 549, 549, 549, 549, 549, 565,
                    565, 579, 579, 579, 596, 596, 613, 613, 630, 630, 647, 647, 664, 664,
                    681, 681, 698, 698, 715, 715, 732, 732, 749, 749, 749, 749, 749, 749,
                    749, 749, 749, 749, 749, 749, 749, 749, 752, 756, 756, 756, 772, 772,
                    772, 772, 772, 772, 772, 772, 772, 772, 776, 776, 776, 792, 792, 792,
                    792, 792, 792, 792, 792, 793, 830, 830, 855, 855, 855, 871, 871, 896,
                    896, 921, 921, 939, 939, 964, 964, 982, 982, 1007, 1007, 1025, 1025,
                    1050, 1075, 1075, 1104, 1104, 1104, 1123, 1141, 1166, 1166, 1166,
                    1166, 1166, 1166, 1166, 1166, 1184, 1184, 1184, 1184, 1184, 1184,
                    1209, 1209, 1220, 1220, 1220, 1238, 1257, 1257, 1257, 1257, 1275,
                    1275, 1275, 1275, 1275, 1275, 1278, 1278, 1278, 1278, 1313, 1313,
                    1313, 1313, 1313, 1313, 1331, 1331, 1331, 1331, 1331, 1331, 1331,
                    1356, 1356, 1356, 1356, 1356, 1356, 1367, 1368, 1368, 1381, 1381,
                    1381,
                ];
                static REDUCE_DATA: &[u32] = &[
                    89, 1, 198, 0, 1, 152, 1, 2, 122, 152, 2, 2, 122, 152, 11, 2, 122,
                    152, 13, 1, 122, 15, 2, 122, 152, 16, 2, 122, 152, 19, 2, 122, 152,
                    30, 1, 152, 45, 1, 122, 68, 1, 152, 69, 1, 152, 70, 1, 152, 71, 1,
                    152, 72, 1, 152, 73, 1, 152, 74, 1, 152, 75, 1, 152, 76, 1, 152, 77,
                    1, 152, 78, 1, 152, 79, 1, 152, 80, 1, 152, 81, 1, 152, 82, 1, 152,
                    83, 1, 152, 84, 1, 152, 85, 1, 152, 86, 1, 152, 1, 1, 122, 2, 1, 122,
                    11, 1, 122, 13, 1, 122, 15, 1, 122, 16, 1, 122, 19, 1, 122, 45, 1,
                    122, 0, 1, 202, 1, 1, 202, 30, 1, 202, 0, 1, 118, 1, 1, 118, 2, 1,
                    118, 11, 1, 118, 15, 1, 118, 16, 1, 118, 19, 1, 118, 30, 1, 118, 68,
                    1, 118, 69, 1, 118, 70, 1, 118, 71, 1, 118, 72, 1, 118, 73, 1, 118,
                    74, 1, 118, 75, 1, 118, 76, 1, 118, 77, 1, 118, 78, 1, 118, 79, 1,
                    118, 80, 1, 118, 81, 1, 118, 82, 1, 118, 83, 1, 118, 84, 1, 118, 85,
                    1, 118, 86, 1, 118, 0, 1, 119, 1, 1, 119, 2, 1, 119, 11, 1, 119, 15,
                    1, 119, 16, 1, 119, 19, 1, 119, 30, 1, 119, 68, 1, 119, 69, 1, 119,
                    70, 1, 119, 71, 1, 119, 72, 1, 119, 73, 1, 119, 74, 1, 119, 75, 1,
                    119, 76, 1, 119, 77, 1, 119, 78, 1, 119, 79, 1, 119, 80, 1, 119, 81,
                    1, 119, 82, 1, 119, 83, 1, 119, 84, 1, 119, 85, 1, 119, 86, 1, 119,
                    0, 1, 199, 1, 1, 199, 2, 1, 199, 11, 1, 199, 15, 1, 199, 30, 1, 199,
                    73, 1, 199, 74, 1, 199, 0, 1, 201, 1, 1, 201, 2, 1, 201, 11, 1, 201,
                    15, 1, 201, 30, 1, 201, 0, 1, 200, 1, 1, 200, 2, 1, 200, 11, 1, 200,
                    15, 1, 200, 30, 1, 200, 73, 1, 200, 74, 1, 200, 2, 1, 121, 13, 1,
                    121, 15, 1, 121, 16, 1, 121, 19, 1, 121, 45, 1, 121, 2, 1, 120, 13,
                    1, 120, 15, 1, 120, 16, 1, 120, 19, 1, 120, 45, 1, 120, 0, 1, 152, 1,
                    1, 152, 2, 1, 152, 11, 1, 152, 15, 1, 152, 19, 1, 152, 30, 1, 152,
                    68, 1, 152, 69, 1, 152, 70, 1, 152, 71, 1, 152, 72, 1, 152, 73, 1,
                    152, 74, 1, 152, 75, 1, 152, 76, 1, 152, 77, 1, 152, 78, 1, 152, 79,
                    1, 152, 80, 1, 152, 81, 1, 152, 82, 1, 152, 83, 1, 152, 84, 1, 152,
                    85, 1, 152, 86, 1, 152, 1, 1, 127, 2, 1, 127, 11, 1, 127, 13, 1, 127,
                    15, 1, 127, 16, 1, 127, 19, 1, 127, 45, 1, 127, 0, 1, 155, 1, 1, 155,
                    2, 1, 155, 11, 1, 155, 15, 1, 155, 16, 1, 155, 30, 1, 155, 68, 1,
                    155, 69, 1, 155, 70, 1, 155, 71, 1, 155, 72, 1, 155, 73, 1, 155, 74,
                    1, 155, 75, 1, 155, 76, 1, 155, 77, 1, 155, 78, 1, 155, 79, 1, 155,
                    80, 1, 155, 81, 1, 155, 82, 1, 155, 83, 1, 155, 84, 1, 155, 85, 1,
                    155, 86, 1, 155, 0, 1, 156, 1, 1, 156, 2, 1, 156, 11, 1, 156, 15, 1,
                    156, 16, 1, 156, 30, 1, 156, 68, 1, 156, 69, 1, 156, 70, 1, 156, 71,
                    1, 156, 72, 1, 156, 73, 1, 156, 74, 1, 156, 75, 1, 156, 76, 1, 156,
                    77, 1, 156, 78, 1, 156, 79, 1, 156, 80, 1, 156, 81, 1, 156, 82, 1,
                    156, 83, 1, 156, 84, 1, 156, 85, 1, 156, 86, 1, 156, 0, 1, 157, 1, 1,
                    157, 2, 1, 157, 11, 1, 157, 15, 1, 157, 16, 1, 157, 30, 1, 157, 68,
                    1, 157, 69, 1, 157, 70, 1, 157, 71, 1, 157, 72, 1, 157, 73, 1, 157,
                    74, 1, 157, 75, 1, 157, 76, 1, 157, 77, 1, 157, 78, 1, 157, 79, 1,
                    157, 80, 1, 157, 81, 1, 157, 82, 1, 157, 83, 1, 157, 84, 1, 157, 85,
                    1, 157, 86, 1, 157, 0, 1, 158, 1, 1, 158, 2, 1, 158, 11, 1, 158, 15,
                    1, 158, 16, 1, 158, 30, 1, 158, 68, 1, 158, 69, 1, 158, 70, 1, 158,
                    71, 1, 158, 72, 1, 158, 73, 1, 158, 74, 1, 158, 75, 1, 158, 76, 1,
                    158, 77, 1, 158, 78, 1, 158, 79, 1, 158, 80, 1, 158, 81, 1, 158, 82,
                    1, 158, 83, 1, 158, 84, 1, 158, 85, 1, 158, 86, 1, 158, 0, 1, 159, 1,
                    1, 159, 2, 1, 159, 11, 1, 159, 15, 1, 159, 16, 1, 159, 30, 1, 159,
                    68, 1, 159, 69, 1, 159, 70, 1, 159, 71, 1, 159, 72, 1, 159, 73, 1,
                    159, 74, 1, 159, 75, 1, 159, 76, 1, 159, 77, 1, 159, 78, 1, 159, 79,
                    1, 159, 80, 1, 159, 81, 1, 159, 82, 1, 159, 83, 1, 159, 84, 1, 159,
                    85, 1, 159, 86, 1, 159, 0, 1, 143, 1, 1, 143, 2, 1, 143, 11, 1, 143,
                    15, 1, 143, 16, 1, 143, 19, 1, 143, 30, 1, 143, 68, 1, 143, 69, 1,
                    143, 70, 1, 143, 71, 1, 143, 72, 1, 143, 73, 1, 143, 74, 1, 143, 75,
                    1, 143, 76, 1, 143, 77, 1, 143, 78, 1, 143, 79, 1, 143, 80, 1, 143,
                    81, 1, 143, 82, 1, 143, 83, 1, 143, 84, 1, 143, 85, 1, 143, 86, 1,
                    143, 0, 1, 144, 1, 1, 144, 2, 1, 144, 11, 1, 144, 15, 1, 144, 16, 1,
                    144, 19, 1, 144, 30, 1, 144, 68, 1, 144, 69, 1, 144, 70, 1, 144, 71,
                    1, 144, 72, 1, 144, 73, 1, 144, 74, 1, 144, 75, 1, 144, 76, 1, 144,
                    77, 1, 144, 78, 1, 144, 79, 1, 144, 80, 1, 144, 81, 1, 144, 82, 1,
                    144, 83, 1, 144, 84, 1, 144, 85, 1, 144, 86, 1, 144, 0, 1, 145, 1, 1,
                    145, 2, 1, 145, 11, 1, 145, 15, 1, 145, 16, 1, 145, 19, 1, 145, 30,
                    1, 145, 68, 1, 145, 69, 1, 145, 70, 1, 145, 71, 1, 145, 72, 1, 145,
                    73, 1, 145, 74, 1, 145, 75, 1, 145, 76, 1, 145, 77, 1, 145, 78, 1,
                    145, 79, 1, 145, 80, 1, 145, 81, 1, 145, 82, 1, 145, 83, 1, 145, 84,
                    1, 145, 85, 1, 145, 86, 1, 145, 0, 1, 146, 1, 1, 146, 2, 1, 146, 11,
                    1, 146, 15, 1, 146, 16, 1, 146, 19, 1, 146, 30, 1, 146, 68, 1, 146,
                    69, 1, 146, 70, 1, 146, 71, 1, 146, 72, 1, 146, 73, 1, 146, 74, 1,
                    146, 75, 1, 146, 76, 1, 146, 77, 1, 146, 78, 1, 146, 79, 1, 146, 80,
                    1, 146, 81, 1, 146, 82, 1, 146, 83, 1, 146, 84, 1, 146, 85, 1, 146,
                    86, 1, 146, 0, 1, 147, 1, 1, 147, 2, 1, 147, 11, 1, 147, 15, 1, 147,
                    16, 1, 147, 19, 1, 147, 30, 1, 147, 68, 1, 147, 69, 1, 147, 70, 1,
                    147, 71, 1, 147, 72, 1, 147, 73, 1, 147, 74, 1, 147, 75, 1, 147, 76,
                    1, 147, 77, 1, 147, 78, 1, 147, 79, 1, 147, 80, 1, 147, 81, 1, 147,
                    82, 1, 147, 83, 1, 147, 84, 1, 147, 85, 1, 147, 86, 1, 147, 0, 1,
                    148, 1, 1, 148, 2, 1, 148, 11, 1, 148, 15, 1, 148, 16, 1, 148, 19, 1,
                    148, 30, 1, 148, 68, 1, 148, 69, 1, 148, 70, 1, 148, 71, 1, 148, 72,
                    1, 148, 73, 1, 148, 74, 1, 148, 75, 1, 148, 76, 1, 148, 77, 1, 148,
                    78, 1, 148, 79, 1, 148, 80, 1, 148, 81, 1, 148, 82, 1, 148, 83, 1,
                    148, 84, 1, 148, 85, 1, 148, 86, 1, 148, 0, 1, 149, 1, 1, 149, 2, 1,
                    149, 11, 1, 149, 15, 1, 149, 16, 1, 149, 19, 1, 149, 30, 1, 149, 68,
                    1, 149, 69, 1, 149, 70, 1, 149, 71, 1, 149, 72, 1, 149, 73, 1, 149,
                    74, 1, 149, 75, 1, 149, 76, 1, 149, 77, 1, 149, 78, 1, 149, 79, 1,
                    149, 80, 1, 149, 81, 1, 149, 82, 1, 149, 83, 1, 149, 84, 1, 149, 85,
                    1, 149, 86, 1, 149, 0, 1, 150, 1, 1, 150, 2, 1, 150, 11, 1, 150, 15,
                    1, 150, 16, 1, 150, 19, 1, 150, 30, 1, 150, 68, 1, 150, 69, 1, 150,
                    70, 1, 150, 71, 1, 150, 72, 1, 150, 73, 1, 150, 74, 1, 150, 75, 1,
                    150, 76, 1, 150, 77, 1, 150, 78, 1, 150, 79, 1, 150, 80, 1, 150, 81,
                    1, 150, 82, 1, 150, 83, 1, 150, 84, 1, 150, 85, 1, 150, 86, 1, 150,
                    0, 1, 151, 1, 1, 151, 2, 1, 151, 11, 1, 151, 15, 1, 151, 16, 1, 151,
                    19, 1, 151, 30, 1, 151, 68, 1, 151, 69, 1, 151, 70, 1, 151, 71, 1,
                    151, 72, 1, 151, 73, 1, 151, 74, 1, 151, 75, 1, 151, 76, 1, 151, 77,
                    1, 151, 78, 1, 151, 79, 1, 151, 80, 1, 151, 81, 1, 151, 82, 1, 151,
                    83, 1, 151, 84, 1, 151, 85, 1, 151, 86, 1, 151, 0, 1, 170, 13, 1,
                    170, 0, 1, 171, 13, 1, 171, 13, 1, 210, 0, 1, 180, 1, 1, 180, 2, 1,
                    180, 11, 1, 180, 13, 1, 209, 15, 1, 180, 16, 1, 180, 19, 1, 180, 30,
                    1, 180, 68, 1, 180, 69, 1, 180, 70, 1, 180, 71, 1, 180, 72, 1, 180,
                    73, 1, 180, 74, 1, 180, 75, 1, 180, 76, 1, 180, 77, 1, 180, 78, 1,
                    180, 79, 1, 180, 80, 1, 180, 81, 1, 180, 82, 1, 180, 83, 1, 180, 84,
                    1, 180, 85, 1, 180, 86, 1, 180, 14, 1, 183, 15, 1, 183, 1, 1, 7, 2,
                    1, 7, 11, 1, 7, 12, 1, 7, 14, 1, 7, 15, 1, 7, 16, 1, 7, 18, 1, 7, 19,
                    1, 7, 20, 1, 7, 21, 1, 7, 22, 1, 7, 23, 1, 7, 25, 1, 7, 28, 1, 7, 29,
                    1, 7, 30, 1, 7, 31, 1, 7, 32, 1, 7, 33, 1, 7, 34, 1, 7, 35, 1, 7, 36,
                    1, 7, 37, 1, 7, 38, 1, 7, 39, 1, 7, 40, 1, 7, 41, 1, 7, 42, 1, 7, 43,
                    1, 7, 44, 1, 7, 45, 1, 7, 46, 1, 7, 47, 1, 7, 48, 1, 7, 49, 1, 7, 50,
                    1, 7, 51, 1, 7, 52, 1, 7, 53, 1, 7, 54, 1, 7, 55, 1, 7, 0, 1, 152, 1,
                    2, 7, 152, 2, 2, 7, 152, 11, 2, 7, 152, 15, 1, 7, 16, 2, 7, 152, 18,
                    1, 7, 20, 1, 7, 21, 1, 7, 22, 1, 7, 23, 1, 7, 25, 1, 7, 28, 1, 7, 29,
                    1, 7, 30, 2, 7, 152, 31, 1, 7, 32, 1, 7, 33, 1, 7, 34, 1, 7, 35, 1,
                    7, 36, 1, 7, 37, 1, 7, 38, 1, 7, 39, 1, 7, 40, 1, 7, 41, 1, 7, 42, 1,
                    7, 43, 1, 7, 44, 1, 7, 45, 1, 7, 46, 1, 7, 47, 1, 7, 48, 1, 7, 49, 1,
                    7, 50, 1, 7, 51, 1, 7, 52, 1, 7, 53, 1, 7, 54, 1, 7, 55, 1, 7, 68, 1,
                    152, 69, 1, 152, 70, 1, 152, 71, 1, 152, 72, 1, 152, 73, 1, 152, 74,
                    1, 152, 75, 1, 152, 76, 1, 152, 77, 1, 152, 78, 1, 152, 79, 1, 152,
                    80, 1, 152, 81, 1, 152, 82, 1, 152, 83, 1, 152, 84, 1, 152, 85, 1,
                    152, 86, 1, 152, 1, 1, 9, 2, 1, 9, 11, 1, 9, 12, 1, 9, 14, 1, 9, 15,
                    1, 9, 16, 1, 9, 18, 1, 9, 19, 1, 9, 20, 1, 9, 21, 1, 9, 22, 1, 9, 23,
                    1, 9, 25, 1, 9, 28, 1, 9, 29, 1, 9, 30, 1, 9, 31, 1, 9, 32, 1, 9, 33,
                    1, 9, 34, 1, 9, 35, 1, 9, 36, 1, 9, 37, 1, 9, 38, 1, 9, 39, 1, 9, 40,
                    1, 9, 41, 1, 9, 42, 1, 9, 43, 1, 9, 44, 1, 9, 45, 1, 9, 46, 1, 9, 47,
                    1, 9, 48, 1, 9, 49, 1, 9, 50, 1, 9, 51, 1, 9, 52, 1, 9, 53, 1, 9, 54,
                    1, 9, 55, 1, 9, 1, 1, 0, 2, 1, 0, 11, 1, 0, 12, 1, 0, 14, 1, 0, 15,
                    1, 0, 16, 1, 0, 18, 1, 0, 19, 1, 0, 20, 1, 0, 21, 1, 0, 22, 1, 0, 23,
                    1, 0, 25, 1, 0, 28, 1, 0, 29, 1, 0, 30, 1, 0, 31, 1, 0, 32, 1, 0, 33,
                    1, 0, 34, 1, 0, 35, 1, 0, 36, 1, 0, 37, 1, 0, 38, 1, 0, 39, 1, 0, 40,
                    1, 0, 41, 1, 0, 42, 1, 0, 43, 1, 0, 44, 1, 0, 45, 1, 0, 46, 1, 0, 47,
                    1, 0, 48, 1, 0, 49, 1, 0, 50, 1, 0, 51, 1, 0, 52, 1, 0, 53, 1, 0, 54,
                    1, 0, 55, 1, 0, 1, 1, 1, 2, 1, 1, 11, 1, 1, 12, 1, 1, 14, 1, 1, 15,
                    1, 1, 16, 1, 1, 18, 1, 1, 19, 1, 1, 20, 1, 1, 21, 1, 1, 22, 1, 1, 23,
                    1, 1, 25, 1, 1, 28, 1, 1, 29, 1, 1, 30, 1, 1, 31, 1, 1, 32, 1, 1, 33,
                    1, 1, 34, 1, 1, 35, 1, 1, 36, 1, 1, 37, 1, 1, 38, 1, 1, 39, 1, 1, 40,
                    1, 1, 41, 1, 1, 42, 1, 1, 43, 1, 1, 44, 1, 1, 45, 1, 1, 46, 1, 1, 47,
                    1, 1, 48, 1, 1, 49, 1, 1, 50, 1, 1, 51, 1, 1, 52, 1, 1, 53, 1, 1, 54,
                    1, 1, 55, 1, 1, 1, 1, 2, 2, 1, 2, 11, 1, 2, 12, 1, 2, 14, 1, 2, 15,
                    1, 2, 16, 1, 2, 18, 1, 2, 19, 1, 2, 20, 1, 2, 21, 1, 2, 22, 1, 2, 23,
                    1, 2, 25, 1, 2, 28, 1, 2, 29, 1, 2, 30, 1, 2, 31, 1, 2, 32, 1, 2, 33,
                    1, 2, 34, 1, 2, 35, 1, 2, 36, 1, 2, 37, 1, 2, 38, 1, 2, 39, 1, 2, 40,
                    1, 2, 41, 1, 2, 42, 1, 2, 43, 1, 2, 44, 1, 2, 45, 1, 2, 46, 1, 2, 47,
                    1, 2, 48, 1, 2, 49, 1, 2, 50, 1, 2, 51, 1, 2, 52, 1, 2, 53, 1, 2, 54,
                    1, 2, 55, 1, 2, 1, 1, 3, 2, 1, 3, 11, 1, 3, 12, 1, 3, 14, 1, 3, 15,
                    1, 3, 16, 1, 3, 18, 1, 3, 19, 1, 3, 20, 1, 3, 21, 1, 3, 22, 1, 3, 23,
                    1, 3, 25, 1, 3, 28, 1, 3, 29, 1, 3, 30, 1, 3, 31, 1, 3, 32, 1, 3, 33,
                    1, 3, 34, 1, 3, 35, 1, 3, 36, 1, 3, 37, 1, 3, 38, 1, 3, 39, 1, 3, 40,
                    1, 3, 41, 1, 3, 42, 1, 3, 43, 1, 3, 44, 1, 3, 45, 1, 3, 46, 1, 3, 47,
                    1, 3, 48, 1, 3, 49, 1, 3, 50, 1, 3, 51, 1, 3, 52, 1, 3, 53, 1, 3, 54,
                    1, 3, 55, 1, 3, 1, 1, 4, 2, 1, 4, 11, 1, 4, 12, 1, 4, 14, 1, 4, 15,
                    1, 4, 16, 1, 4, 18, 1, 4, 19, 1, 4, 20, 1, 4, 21, 1, 4, 22, 1, 4, 23,
                    1, 4, 25, 1, 4, 28, 1, 4, 29, 1, 4, 30, 1, 4, 31, 1, 4, 32, 1, 4, 33,
                    1, 4, 34, 1, 4, 35, 1, 4, 36, 1, 4, 37, 1, 4, 38, 1, 4, 39, 1, 4, 40,
                    1, 4, 41, 1, 4, 42, 1, 4, 43, 1, 4, 44, 1, 4, 45, 1, 4, 46, 1, 4, 47,
                    1, 4, 48, 1, 4, 49, 1, 4, 50, 1, 4, 51, 1, 4, 52, 1, 4, 53, 1, 4, 54,
                    1, 4, 55, 1, 4, 1, 1, 5, 2, 1, 5, 11, 1, 5, 12, 1, 5, 14, 1, 5, 15,
                    1, 5, 16, 1, 5, 18, 1, 5, 19, 1, 5, 20, 1, 5, 21, 1, 5, 22, 1, 5, 23,
                    1, 5, 25, 1, 5, 28, 1, 5, 29, 1, 5, 30, 1, 5, 31, 1, 5, 32, 1, 5, 33,
                    1, 5, 34, 1, 5, 35, 1, 5, 36, 1, 5, 37, 1, 5, 38, 1, 5, 39, 1, 5, 40,
                    1, 5, 41, 1, 5, 42, 1, 5, 43, 1, 5, 44, 1, 5, 45, 1, 5, 46, 1, 5, 47,
                    1, 5, 48, 1, 5, 49, 1, 5, 50, 1, 5, 51, 1, 5, 52, 1, 5, 53, 1, 5, 54,
                    1, 5, 55, 1, 5, 1, 1, 6, 2, 1, 6, 11, 1, 6, 12, 1, 6, 14, 1, 6, 15,
                    1, 6, 16, 1, 6, 18, 1, 6, 19, 1, 6, 20, 1, 6, 21, 1, 6, 22, 1, 6, 23,
                    1, 6, 25, 1, 6, 28, 1, 6, 29, 1, 6, 30, 1, 6, 31, 1, 6, 32, 1, 6, 33,
                    1, 6, 34, 1, 6, 35, 1, 6, 36, 1, 6, 37, 1, 6, 38, 1, 6, 39, 1, 6, 40,
                    1, 6, 41, 1, 6, 42, 1, 6, 43, 1, 6, 44, 1, 6, 45, 1, 6, 46, 1, 6, 47,
                    1, 6, 48, 1, 6, 49, 1, 6, 50, 1, 6, 51, 1, 6, 52, 1, 6, 53, 1, 6, 54,
                    1, 6, 55, 1, 6, 2, 1, 21, 12, 1, 21, 14, 1, 21, 15, 1, 21, 16, 1, 21,
                    18, 1, 21, 19, 1, 21, 25, 1, 21, 28, 1, 21, 29, 1, 21, 30, 1, 21, 31,
                    1, 21, 32, 1, 21, 33, 1, 21, 34, 1, 21, 35, 1, 21, 36, 1, 21, 37, 1,
                    21, 38, 1, 21, 39, 1, 21, 40, 1, 21, 41, 1, 21, 42, 1, 21, 43, 1, 21,
                    44, 1, 21, 45, 1, 21, 46, 1, 21, 47, 1, 21, 48, 1, 21, 49, 1, 21, 50,
                    1, 21, 51, 1, 21, 52, 1, 21, 53, 1, 21, 54, 1, 21, 55, 1, 21, 1, 1,
                    13, 2, 1, 13, 11, 1, 13, 12, 1, 13, 14, 1, 13, 15, 1, 13, 16, 1, 13,
                    18, 1, 13, 19, 1, 13, 20, 1, 13, 21, 1, 13, 22, 1, 13, 23, 1, 13, 25,
                    1, 13, 28, 1, 13, 29, 1, 13, 30, 1, 13, 31, 1, 13, 32, 1, 13, 33, 1,
                    13, 34, 1, 13, 35, 1, 13, 36, 1, 13, 37, 1, 13, 38, 1, 13, 39, 1, 13,
                    40, 1, 13, 41, 1, 13, 42, 1, 13, 43, 1, 13, 44, 1, 13, 45, 1, 13, 46,
                    1, 13, 47, 1, 13, 48, 1, 13, 49, 1, 13, 50, 1, 13, 51, 1, 13, 52, 1,
                    13, 53, 1, 13, 54, 1, 13, 55, 1, 13, 1, 1, 14, 2, 1, 14, 11, 1, 14,
                    12, 1, 14, 14, 1, 14, 15, 1, 14, 16, 1, 14, 18, 1, 14, 19, 1, 14, 20,
                    1, 14, 21, 1, 14, 22, 1, 14, 23, 1, 14, 25, 1, 14, 28, 1, 14, 29, 1,
                    14, 30, 1, 14, 31, 1, 14, 32, 1, 14, 33, 1, 14, 34, 1, 14, 35, 1, 14,
                    36, 1, 14, 37, 1, 14, 38, 1, 14, 39, 1, 14, 40, 1, 14, 41, 1, 14, 42,
                    1, 14, 43, 1, 14, 44, 1, 14, 45, 1, 14, 46, 1, 14, 47, 1, 14, 48, 1,
                    14, 49, 1, 14, 50, 1, 14, 51, 1, 14, 52, 1, 14, 53, 1, 14, 54, 1, 14,
                    55, 1, 14, 2, 1, 32, 12, 1, 32, 14, 1, 32, 15, 1, 32, 16, 1, 32, 18,
                    1, 32, 19, 1, 32, 25, 1, 32, 28, 1, 32, 29, 1, 32, 30, 1, 32, 31, 1,
                    32, 32, 1, 32, 33, 1, 32, 34, 1, 32, 35, 1, 32, 36, 1, 32, 37, 1, 32,
                    38, 1, 32, 39, 1, 32, 40, 1, 32, 41, 1, 32, 42, 1, 32, 43, 1, 32, 44,
                    1, 32, 2, 1, 38, 12, 1, 38, 14, 1, 38, 15, 1, 38, 16, 1, 38, 18, 1,
                    38, 19, 1, 38, 25, 1, 38, 28, 1, 38, 29, 1, 38, 33, 1, 38, 34, 1, 38,
                    35, 1, 38, 36, 1, 38, 37, 1, 38, 38, 1, 38, 39, 1, 38, 40, 1, 38, 41,
                    1, 38, 42, 1, 38, 43, 1, 38, 44, 1, 38, 0, 1, 179, 1, 1, 179, 2, 1,
                    179, 11, 1, 179, 15, 1, 179, 16, 1, 179, 19, 1, 179, 30, 1, 179, 68,
                    1, 179, 69, 1, 179, 70, 1, 179, 71, 1, 179, 72, 1, 179, 73, 1, 179,
                    74, 1, 179, 75, 1, 179, 76, 1, 179, 77, 1, 179, 78, 1, 179, 79, 1,
                    179, 80, 1, 179, 81, 1, 179, 82, 1, 179, 83, 1, 179, 84, 1, 179, 85,
                    1, 179, 86, 1, 179, 14, 1, 182, 15, 1, 182, 14, 1, 181, 15, 1, 181,
                    2, 1, 41, 12, 1, 41, 14, 1, 41, 15, 1, 41, 16, 1, 41, 18, 1, 41, 19,
                    1, 41, 25, 1, 41, 33, 1, 41, 34, 1, 41, 35, 1, 41, 36, 1, 41, 37, 1,
                    41, 38, 1, 41, 39, 1, 41, 40, 1, 41, 41, 1, 41, 42, 1, 41, 43, 1, 41,
                    44, 1, 41, 2, 1, 44, 12, 1, 44, 14, 1, 44, 15, 1, 44, 16, 1, 44, 18,
                    1, 44, 19, 1, 44, 25, 1, 44, 35, 1, 44, 36, 1, 44, 37, 1, 44, 38, 1,
                    44, 39, 1, 44, 40, 1, 44, 41, 1, 44, 42, 1, 44, 43, 1, 44, 44, 1, 44,
                    2, 1, 49, 12, 1, 49, 14, 1, 49, 15, 1, 49, 16, 1, 49, 18, 1, 49, 19,
                    1, 49, 25, 1, 49, 37, 1, 49, 38, 1, 49, 41, 1, 49, 42, 1, 49, 43, 1,
                    49, 44, 1, 49, 2, 1, 52, 12, 1, 52, 14, 1, 52, 15, 1, 52, 16, 1, 52,
                    18, 1, 52, 19, 1, 52, 25, 1, 52, 37, 1, 52, 38, 1, 52, 43, 1, 52, 44,
                    1, 52, 2, 1, 50, 12, 1, 50, 14, 1, 50, 15, 1, 50, 16, 1, 50, 18, 1,
                    50, 19, 1, 50, 25, 1, 50, 37, 1, 50, 38, 1, 50, 41, 1, 50, 42, 1, 50,
                    43, 1, 50, 44, 1, 50, 2, 1, 46, 12, 1, 46, 14, 1, 46, 15, 1, 46, 16,
                    1, 46, 18, 1, 46, 19, 1, 46, 25, 1, 46, 35, 1, 46, 36, 1, 46, 37, 1,
                    46, 38, 1, 46, 39, 1, 46, 40, 1, 46, 41, 1, 46, 42, 1, 46, 43, 1, 46,
                    44, 1, 46, 2, 1, 43, 12, 1, 43, 14, 1, 43, 15, 1, 43, 16, 1, 43, 18,
                    1, 43, 19, 1, 43, 25, 1, 43, 33, 1, 43, 34, 1, 43, 35, 1, 43, 36, 1,
                    43, 37, 1, 43, 38, 1, 43, 39, 1, 43, 40, 1, 43, 41, 1, 43, 42, 1, 43,
                    43, 1, 43, 44, 1, 43, 2, 1, 39, 12, 1, 39, 14, 1, 39, 15, 1, 39, 16,
                    1, 39, 18, 1, 39, 19, 1, 39, 25, 1, 39, 28, 1, 39, 29, 1, 39, 33, 1,
                    39, 34, 1, 39, 35, 1, 39, 36, 1, 39, 37, 1, 39, 38, 1, 39, 39, 1, 39,
                    40, 1, 39, 41, 1, 39, 42, 1, 39, 43, 1, 39, 44, 1, 39, 2, 1, 36, 12,
                    1, 36, 14, 1, 36, 15, 1, 36, 16, 1, 36, 18, 1, 36, 19, 1, 36, 25, 1,
                    36, 28, 1, 36, 29, 1, 36, 30, 1, 36, 31, 1, 36, 32, 1, 36, 33, 1, 36,
                    34, 1, 36, 35, 1, 36, 36, 1, 36, 37, 1, 36, 38, 1, 36, 39, 1, 36, 40,
                    1, 36, 41, 1, 36, 42, 1, 36, 43, 1, 36, 44, 1, 36, 2, 1, 37, 12, 1,
                    37, 14, 1, 37, 15, 1, 37, 16, 1, 37, 18, 1, 37, 19, 1, 37, 25, 1, 37,
                    28, 1, 37, 29, 1, 37, 30, 1, 37, 31, 1, 37, 32, 1, 37, 33, 1, 37, 34,
                    1, 37, 35, 1, 37, 36, 1, 37, 37, 1, 37, 38, 1, 37, 39, 1, 37, 40, 1,
                    37, 41, 1, 37, 42, 1, 37, 43, 1, 37, 44, 1, 37, 2, 1, 47, 12, 1, 47,
                    14, 1, 47, 15, 1, 47, 16, 1, 47, 18, 1, 47, 19, 1, 47, 25, 1, 47, 35,
                    1, 47, 36, 1, 47, 37, 1, 47, 38, 1, 47, 39, 1, 47, 40, 1, 47, 41, 1,
                    47, 42, 1, 47, 43, 1, 47, 44, 1, 47, 2, 1, 48, 12, 1, 48, 14, 1, 48,
                    15, 1, 48, 16, 1, 48, 18, 1, 48, 19, 1, 48, 25, 1, 48, 35, 1, 48, 36,
                    1, 48, 37, 1, 48, 38, 1, 48, 39, 1, 48, 40, 1, 48, 41, 1, 48, 42, 1,
                    48, 43, 1, 48, 44, 1, 48, 2, 1, 51, 12, 1, 51, 14, 1, 51, 15, 1, 51,
                    16, 1, 51, 18, 1, 51, 19, 1, 51, 25, 1, 51, 37, 1, 51, 38, 1, 51, 41,
                    1, 51, 42, 1, 51, 43, 1, 51, 44, 1, 51, 2, 1, 54, 12, 1, 54, 14, 1,
                    54, 15, 1, 54, 16, 1, 54, 18, 1, 54, 19, 1, 54, 37, 1, 54, 38, 1, 54,
                    43, 1, 54, 44, 1, 54, 2, 1, 53, 12, 1, 53, 14, 1, 53, 15, 1, 53, 16,
                    1, 53, 18, 1, 53, 19, 1, 53, 25, 1, 53, 37, 1, 53, 38, 1, 53, 43, 1,
                    53, 44, 1, 53, 2, 1, 56, 12, 1, 56, 14, 1, 56, 15, 1, 56, 16, 1, 56,
                    18, 1, 56, 19, 1, 56, 38, 1, 56, 43, 1, 56, 44, 1, 56, 2, 1, 55, 12,
                    1, 55, 14, 1, 55, 15, 1, 55, 16, 1, 55, 18, 1, 55, 19, 1, 55, 37, 1,
                    55, 38, 1, 55, 43, 1, 55, 44, 1, 55, 2, 1, 58, 12, 1, 58, 14, 1, 58,
                    15, 1, 58, 16, 1, 58, 18, 1, 58, 19, 1, 58, 43, 1, 58, 44, 1, 58, 2,
                    1, 57, 12, 1, 57, 14, 1, 57, 15, 1, 57, 16, 1, 57, 18, 1, 57, 19, 1,
                    57, 38, 1, 57, 43, 1, 57, 44, 1, 57, 2, 1, 60, 12, 1, 60, 14, 1, 60,
                    15, 1, 60, 16, 1, 60, 18, 1, 60, 19, 1, 60, 44, 1, 60, 2, 1, 59, 12,
                    1, 59, 14, 1, 59, 15, 1, 59, 16, 1, 59, 18, 1, 59, 19, 1, 59, 43, 1,
                    59, 44, 1, 59, 2, 1, 62, 12, 1, 62, 14, 1, 62, 15, 1, 62, 16, 1, 62,
                    19, 1, 62, 1, 1, 10, 2, 1, 10, 11, 1, 10, 12, 1, 10, 14, 1, 10, 15,
                    1, 10, 16, 1, 10, 18, 1, 10, 19, 1, 10, 20, 1, 10, 21, 1, 10, 22, 1,
                    10, 23, 1, 10, 25, 1, 10, 28, 1, 10, 29, 1, 10, 30, 1, 10, 31, 1, 10,
                    32, 1, 10, 33, 1, 10, 34, 1, 10, 35, 1, 10, 36, 1, 10, 37, 1, 10, 38,
                    1, 10, 39, 1, 10, 40, 1, 10, 41, 1, 10, 42, 1, 10, 43, 1, 10, 44, 1,
                    10, 45, 1, 10, 46, 1, 10, 47, 1, 10, 48, 1, 10, 49, 1, 10, 50, 1, 10,
                    51, 1, 10, 52, 1, 10, 53, 1, 10, 54, 1, 10, 55, 1, 10, 2, 1, 77, 12,
                    1, 77, 15, 1, 77, 16, 1, 77, 19, 1, 77, 0, 1, 140, 1, 1, 140, 2, 1,
                    140, 11, 1, 140, 19, 1, 140, 30, 1, 140, 73, 1, 140, 74, 1, 140, 75,
                    1, 140, 76, 1, 140, 77, 1, 140, 78, 1, 140, 79, 1, 140, 80, 1, 140,
                    81, 1, 140, 82, 1, 140, 83, 1, 140, 84, 1, 140, 85, 1, 140, 86, 1,
                    140, 0, 1, 203, 1, 1, 203, 2, 1, 203, 11, 1, 203, 19, 1, 203, 30, 1,
                    203, 73, 1, 203, 74, 1, 203, 75, 1, 203, 76, 1, 203, 77, 1, 203, 78,
                    1, 203, 79, 1, 203, 80, 1, 203, 81, 1, 203, 82, 1, 203, 83, 1, 203,
                    84, 1, 203, 85, 1, 203, 86, 1, 203, 0, 1, 141, 1, 1, 141, 2, 1, 141,
                    11, 1, 141, 19, 1, 141, 30, 1, 141, 73, 1, 141, 74, 1, 141, 75, 1,
                    141, 76, 1, 141, 77, 1, 141, 78, 1, 141, 79, 1, 141, 80, 1, 141, 81,
                    1, 141, 82, 1, 141, 83, 1, 141, 84, 1, 141, 85, 1, 141, 86, 1, 141,
                    13, 1, 210, 0, 1, 178, 1, 1, 178, 2, 1, 178, 11, 1, 178, 13, 1, 209,
                    15, 1, 178, 16, 1, 178, 19, 1, 178, 30, 1, 178, 68, 1, 178, 69, 1,
                    178, 70, 1, 178, 71, 1, 178, 72, 1, 178, 73, 1, 178, 74, 1, 178, 75,
                    1, 178, 76, 1, 178, 77, 1, 178, 78, 1, 178, 79, 1, 178, 80, 1, 178,
                    81, 1, 178, 82, 1, 178, 83, 1, 178, 84, 1, 178, 85, 1, 178, 86, 1,
                    178, 14, 1, 214, 0, 1, 211, 14, 1, 211, 73, 1, 211, 74, 1, 211, 75,
                    1, 211, 76, 1, 211, 77, 1, 211, 78, 1, 211, 79, 1, 211, 80, 1, 211,
                    81, 1, 211, 82, 1, 211, 83, 1, 211, 84, 1, 211, 85, 1, 211, 86, 1,
                    211, 0, 1, 153, 1, 1, 153, 2, 1, 153, 11, 1, 153, 15, 1, 153, 16, 1,
                    153, 19, 1, 153, 30, 1, 153, 68, 1, 153, 69, 1, 153, 70, 1, 153, 71,
                    1, 153, 72, 1, 153, 73, 1, 153, 74, 1, 153, 75, 1, 153, 76, 1, 153,
                    77, 1, 153, 78, 1, 153, 79, 1, 153, 80, 1, 153, 81, 1, 153, 82, 1,
                    153, 83, 1, 153, 84, 1, 153, 85, 1, 153, 86, 1, 153, 0, 1, 154, 1, 1,
                    154, 2, 1, 154, 11, 1, 154, 15, 1, 154, 16, 1, 154, 19, 1, 154, 30,
                    1, 154, 68, 1, 154, 69, 1, 154, 70, 1, 154, 71, 1, 154, 72, 1, 154,
                    73, 1, 154, 74, 1, 154, 75, 1, 154, 76, 1, 154, 77, 1, 154, 78, 1,
                    154, 79, 1, 154, 80, 1, 154, 81, 1, 154, 82, 1, 154, 83, 1, 154, 84,
                    1, 154, 85, 1, 154, 86, 1, 154, 19, 1, 208, 1, 1, 123, 2, 1, 123, 11,
                    1, 123, 13, 1, 123, 15, 1, 123, 16, 1, 123, 19, 1, 123, 45, 1, 123,
                    0, 1, 162, 1, 1, 162, 2, 1, 162, 11, 1, 162, 15, 1, 162, 16, 1, 162,
                    30, 1, 162, 68, 1, 162, 69, 1, 162, 70, 1, 162, 71, 1, 162, 72, 1,
                    162, 73, 1, 162, 74, 1, 162, 75, 1, 162, 76, 1, 162, 77, 1, 162, 78,
                    1, 162, 79, 1, 162, 80, 1, 162, 81, 1, 162, 82, 1, 162, 83, 1, 162,
                    84, 1, 162, 85, 1, 162, 86, 1, 162, 0, 1, 161, 1, 1, 161, 2, 1, 161,
                    11, 1, 161, 15, 1, 161, 16, 1, 161, 30, 1, 161, 68, 1, 161, 69, 1,
                    161, 70, 1, 161, 71, 1, 161, 72, 1, 161, 73, 1, 161, 74, 1, 161, 75,
                    1, 161, 76, 1, 161, 77, 1, 161, 78, 1, 161, 79, 1, 161, 80, 1, 161,
                    81, 1, 161, 82, 1, 161, 83, 1, 161, 84, 1, 161, 85, 1, 161, 86, 1,
                    161, 0, 1, 160, 1, 1, 160, 2, 1, 160, 11, 1, 160, 15, 1, 160, 16, 1,
                    160, 30, 1, 160, 68, 1, 160, 69, 1, 160, 70, 1, 160, 71, 1, 160, 72,
                    1, 160, 73, 1, 160, 74, 1, 160, 75, 1, 160, 76, 1, 160, 77, 1, 160,
                    78, 1, 160, 79, 1, 160, 80, 1, 160, 81, 1, 160, 82, 1, 160, 83, 1,
                    160, 84, 1, 160, 85, 1, 160, 86, 1, 160, 0, 1, 193, 1, 1, 193, 2, 1,
                    193, 11, 1, 193, 15, 1, 193, 16, 1, 193, 30, 1, 193, 68, 1, 193, 69,
                    1, 193, 70, 1, 193, 71, 1, 193, 72, 1, 193, 73, 1, 193, 74, 1, 193,
                    75, 1, 193, 76, 1, 193, 77, 1, 193, 78, 1, 193, 79, 1, 193, 80, 1,
                    193, 81, 1, 193, 82, 1, 193, 83, 1, 193, 84, 1, 193, 85, 1, 193, 86,
                    1, 193, 2, 1, 166, 15, 1, 166, 2, 1, 168, 2, 1, 169, 2, 1, 167, 15,
                    1, 167, 14, 1, 213, 0, 1, 212, 14, 1, 212, 73, 1, 212, 74, 1, 212,
                    75, 1, 212, 76, 1, 212, 77, 1, 212, 78, 1, 212, 79, 1, 212, 80, 1,
                    212, 81, 1, 212, 82, 1, 212, 83, 1, 212, 84, 1, 212, 85, 1, 212, 86,
                    1, 212, 0, 1, 177, 1, 1, 177, 2, 1, 177, 11, 1, 177, 15, 1, 177, 16,
                    1, 177, 19, 1, 177, 30, 1, 177, 68, 1, 177, 69, 1, 177, 70, 1, 177,
                    71, 1, 177, 72, 1, 177, 73, 1, 177, 74, 1, 177, 75, 1, 177, 76, 1,
                    177, 77, 1, 177, 78, 1, 177, 79, 1, 177, 80, 1, 177, 81, 1, 177, 82,
                    1, 177, 83, 1, 177, 84, 1, 177, 85, 1, 177, 86, 1, 177, 2, 1, 165,
                    15, 1, 165, 1, 1, 136, 2, 1, 136, 11, 1, 136, 15, 1, 136, 2, 1, 31,
                    12, 1, 31, 14, 1, 31, 15, 1, 31, 16, 1, 31, 18, 1, 31, 19, 1, 31, 25,
                    1, 31, 28, 1, 31, 29, 1, 31, 30, 1, 31, 31, 1, 31, 32, 1, 31, 33, 1,
                    31, 34, 1, 31, 35, 1, 31, 36, 1, 31, 37, 1, 31, 38, 1, 31, 39, 1, 31,
                    40, 1, 31, 41, 1, 31, 42, 1, 31, 43, 1, 31, 44, 1, 31, 45, 1, 31, 46,
                    1, 31, 47, 1, 31, 48, 1, 31, 49, 1, 31, 50, 1, 31, 51, 1, 31, 52, 1,
                    31, 53, 1, 31, 54, 1, 31, 55, 1, 31, 2, 1, 206, 1, 1, 202, 2, 1, 202,
                    11, 1, 202, 30, 1, 202, 2, 1, 129, 2, 1, 128, 15, 1, 128, 2, 1, 130,
                    15, 1, 130, 1, 1, 138, 2, 1, 138, 11, 1, 138, 15, 1, 138, 1, 1, 139,
                    2, 1, 139, 11, 1, 139, 15, 1, 139, 1, 1, 134, 2, 1, 134, 11, 1, 134,
                    15, 1, 134, 2, 1, 19, 15, 1, 19, 2, 1, 35, 12, 1, 35, 14, 1, 35, 15,
                    1, 35, 16, 1, 35, 18, 1, 35, 19, 1, 35, 25, 1, 35, 28, 1, 35, 29, 1,
                    35, 30, 1, 35, 31, 1, 35, 32, 1, 35, 33, 1, 35, 34, 1, 35, 35, 1, 35,
                    36, 1, 35, 37, 1, 35, 38, 1, 35, 39, 1, 35, 40, 1, 35, 41, 1, 35, 42,
                    1, 35, 43, 1, 35, 44, 1, 35, 2, 1, 40, 12, 1, 40, 14, 1, 40, 15, 1,
                    40, 16, 1, 40, 18, 1, 40, 19, 1, 40, 25, 1, 40, 28, 1, 40, 29, 1, 40,
                    33, 1, 40, 34, 1, 40, 35, 1, 40, 36, 1, 40, 37, 1, 40, 38, 1, 40, 39,
                    1, 40, 40, 1, 40, 41, 1, 40, 42, 1, 40, 43, 1, 40, 44, 1, 40, 2, 1,
                    42, 12, 1, 42, 14, 1, 42, 15, 1, 42, 16, 1, 42, 18, 1, 42, 19, 1, 42,
                    25, 1, 42, 33, 1, 42, 34, 1, 42, 35, 1, 42, 36, 1, 42, 37, 1, 42, 38,
                    1, 42, 39, 1, 42, 40, 1, 42, 41, 1, 42, 42, 1, 42, 43, 1, 42, 44, 1,
                    42, 2, 1, 45, 12, 1, 45, 14, 1, 45, 15, 1, 45, 16, 1, 45, 18, 1, 45,
                    19, 1, 45, 25, 1, 45, 35, 1, 45, 36, 1, 45, 37, 1, 45, 38, 1, 45, 39,
                    1, 45, 40, 1, 45, 41, 1, 45, 42, 1, 45, 43, 1, 45, 44, 1, 45, 1, 1,
                    12, 2, 1, 12, 11, 1, 12, 12, 1, 12, 14, 1, 12, 15, 1, 12, 16, 1, 12,
                    18, 1, 12, 19, 1, 12, 20, 1, 12, 21, 1, 12, 22, 1, 12, 23, 1, 12, 25,
                    1, 12, 28, 1, 12, 29, 1, 12, 30, 1, 12, 31, 1, 12, 32, 1, 12, 33, 1,
                    12, 34, 1, 12, 35, 1, 12, 36, 1, 12, 37, 1, 12, 38, 1, 12, 39, 1, 12,
                    40, 1, 12, 41, 1, 12, 42, 1, 12, 43, 1, 12, 44, 1, 12, 45, 1, 12, 46,
                    1, 12, 47, 1, 12, 48, 1, 12, 49, 1, 12, 50, 1, 12, 51, 1, 12, 52, 1,
                    12, 53, 1, 12, 54, 1, 12, 55, 1, 12, 1, 1, 15, 2, 1, 15, 11, 1, 15,
                    12, 1, 15, 14, 1, 15, 15, 1, 15, 16, 1, 15, 18, 1, 15, 19, 1, 15, 20,
                    1, 15, 21, 1, 15, 22, 1, 15, 23, 1, 15, 25, 1, 15, 28, 1, 15, 29, 1,
                    15, 30, 1, 15, 31, 1, 15, 32, 1, 15, 33, 1, 15, 34, 1, 15, 35, 1, 15,
                    36, 1, 15, 37, 1, 15, 38, 1, 15, 39, 1, 15, 40, 1, 15, 41, 1, 15, 42,
                    1, 15, 43, 1, 15, 44, 1, 15, 45, 1, 15, 46, 1, 15, 47, 1, 15, 48, 1,
                    15, 49, 1, 15, 50, 1, 15, 51, 1, 15, 52, 1, 15, 53, 1, 15, 54, 1, 15,
                    55, 1, 15, 1, 1, 16, 2, 1, 16, 11, 1, 16, 12, 1, 16, 14, 1, 16, 15,
                    1, 16, 16, 1, 16, 18, 1, 16, 19, 1, 16, 20, 1, 16, 21, 1, 16, 22, 1,
                    16, 23, 1, 16, 25, 1, 16, 28, 1, 16, 29, 1, 16, 30, 1, 16, 31, 1, 16,
                    32, 1, 16, 33, 1, 16, 34, 1, 16, 35, 1, 16, 36, 1, 16, 37, 1, 16, 38,
                    1, 16, 39, 1, 16, 40, 1, 16, 41, 1, 16, 42, 1, 16, 43, 1, 16, 44, 1,
                    16, 45, 1, 16, 46, 1, 16, 47, 1, 16, 48, 1, 16, 49, 1, 16, 50, 1, 16,
                    51, 1, 16, 52, 1, 16, 53, 1, 16, 54, 1, 16, 55, 1, 16, 1, 1, 17, 2,
                    1, 17, 11, 1, 17, 12, 1, 17, 14, 1, 17, 15, 1, 17, 16, 1, 17, 18, 1,
                    17, 19, 1, 17, 20, 1, 17, 21, 1, 17, 22, 1, 17, 23, 1, 17, 25, 1, 17,
                    28, 1, 17, 29, 1, 17, 30, 1, 17, 31, 1, 17, 32, 1, 17, 33, 1, 17, 34,
                    1, 17, 35, 1, 17, 36, 1, 17, 37, 1, 17, 38, 1, 17, 39, 1, 17, 40, 1,
                    17, 41, 1, 17, 42, 1, 17, 43, 1, 17, 44, 1, 17, 45, 1, 17, 46, 1, 17,
                    47, 1, 17, 48, 1, 17, 49, 1, 17, 50, 1, 17, 51, 1, 17, 52, 1, 17, 53,
                    1, 17, 54, 1, 17, 55, 1, 17, 1, 1, 18, 2, 1, 18, 11, 1, 18, 12, 1,
                    18, 14, 1, 18, 15, 1, 18, 16, 1, 18, 18, 1, 18, 19, 1, 18, 20, 1, 18,
                    21, 1, 18, 22, 1, 18, 23, 1, 18, 25, 1, 18, 28, 1, 18, 29, 1, 18, 30,
                    1, 18, 31, 1, 18, 32, 1, 18, 33, 1, 18, 34, 1, 18, 35, 1, 18, 36, 1,
                    18, 37, 1, 18, 38, 1, 18, 39, 1, 18, 40, 1, 18, 41, 1, 18, 42, 1, 18,
                    43, 1, 18, 44, 1, 18, 45, 1, 18, 46, 1, 18, 47, 1, 18, 48, 1, 18, 49,
                    1, 18, 50, 1, 18, 51, 1, 18, 52, 1, 18, 53, 1, 18, 54, 1, 18, 55, 1,
                    18, 2, 1, 25, 12, 1, 25, 14, 1, 25, 15, 1, 25, 16, 1, 25, 18, 1, 25,
                    19, 1, 25, 25, 1, 25, 28, 1, 25, 29, 1, 25, 30, 1, 25, 31, 1, 25, 32,
                    1, 25, 33, 1, 25, 34, 1, 25, 35, 1, 25, 36, 1, 25, 37, 1, 25, 38, 1,
                    25, 39, 1, 25, 40, 1, 25, 41, 1, 25, 42, 1, 25, 43, 1, 25, 44, 1, 25,
                    45, 1, 25, 46, 1, 25, 47, 1, 25, 48, 1, 25, 49, 1, 25, 50, 1, 25, 51,
                    1, 25, 52, 1, 25, 53, 1, 25, 54, 1, 25, 55, 1, 25, 2, 1, 26, 12, 1,
                    26, 14, 1, 26, 15, 1, 26, 16, 1, 26, 18, 1, 26, 19, 1, 26, 25, 1, 26,
                    28, 1, 26, 29, 1, 26, 30, 1, 26, 31, 1, 26, 32, 1, 26, 33, 1, 26, 34,
                    1, 26, 35, 1, 26, 36, 1, 26, 37, 1, 26, 38, 1, 26, 39, 1, 26, 40, 1,
                    26, 41, 1, 26, 42, 1, 26, 43, 1, 26, 44, 1, 26, 45, 1, 26, 46, 1, 26,
                    47, 1, 26, 48, 1, 26, 49, 1, 26, 50, 1, 26, 51, 1, 26, 52, 1, 26, 53,
                    1, 26, 54, 1, 26, 55, 1, 26, 2, 1, 27, 12, 1, 27, 14, 1, 27, 15, 1,
                    27, 16, 1, 27, 18, 1, 27, 19, 1, 27, 25, 1, 27, 28, 1, 27, 29, 1, 27,
                    30, 1, 27, 31, 1, 27, 32, 1, 27, 33, 1, 27, 34, 1, 27, 35, 1, 27, 36,
                    1, 27, 37, 1, 27, 38, 1, 27, 39, 1, 27, 40, 1, 27, 41, 1, 27, 42, 1,
                    27, 43, 1, 27, 44, 1, 27, 45, 1, 27, 46, 1, 27, 47, 1, 27, 48, 1, 27,
                    49, 1, 27, 50, 1, 27, 51, 1, 27, 52, 1, 27, 53, 1, 27, 54, 1, 27, 55,
                    1, 27, 2, 1, 28, 12, 1, 28, 14, 1, 28, 15, 1, 28, 16, 1, 28, 18, 1,
                    28, 19, 1, 28, 25, 1, 28, 28, 1, 28, 29, 1, 28, 30, 1, 28, 31, 1, 28,
                    32, 1, 28, 33, 1, 28, 34, 1, 28, 35, 1, 28, 36, 1, 28, 37, 1, 28, 38,
                    1, 28, 39, 1, 28, 40, 1, 28, 41, 1, 28, 42, 1, 28, 43, 1, 28, 44, 1,
                    28, 45, 1, 28, 46, 1, 28, 47, 1, 28, 48, 1, 28, 49, 1, 28, 50, 1, 28,
                    51, 1, 28, 52, 1, 28, 53, 1, 28, 54, 1, 28, 55, 1, 28, 2, 1, 29, 12,
                    1, 29, 14, 1, 29, 15, 1, 29, 16, 1, 29, 18, 1, 29, 19, 1, 29, 25, 1,
                    29, 28, 1, 29, 29, 1, 29, 30, 1, 29, 31, 1, 29, 32, 1, 29, 33, 1, 29,
                    34, 1, 29, 35, 1, 29, 36, 1, 29, 37, 1, 29, 38, 1, 29, 39, 1, 29, 40,
                    1, 29, 41, 1, 29, 42, 1, 29, 43, 1, 29, 44, 1, 29, 45, 1, 29, 46, 1,
                    29, 47, 1, 29, 48, 1, 29, 49, 1, 29, 50, 1, 29, 51, 1, 29, 52, 1, 29,
                    53, 1, 29, 54, 1, 29, 55, 1, 29, 2, 1, 33, 12, 1, 33, 14, 1, 33, 15,
                    1, 33, 16, 1, 33, 18, 1, 33, 19, 1, 33, 25, 1, 33, 28, 1, 33, 29, 1,
                    33, 30, 1, 33, 31, 1, 33, 32, 1, 33, 33, 1, 33, 34, 1, 33, 35, 1, 33,
                    36, 1, 33, 37, 1, 33, 38, 1, 33, 39, 1, 33, 40, 1, 33, 41, 1, 33, 42,
                    1, 33, 43, 1, 33, 44, 1, 33, 45, 1, 33, 46, 1, 33, 47, 1, 33, 48, 1,
                    33, 49, 1, 33, 50, 1, 33, 51, 1, 33, 52, 1, 33, 53, 1, 33, 54, 1, 33,
                    55, 1, 33, 2, 1, 24, 12, 1, 24, 14, 1, 24, 15, 1, 24, 16, 1, 24, 18,
                    1, 24, 19, 1, 24, 25, 1, 24, 28, 1, 24, 29, 1, 24, 30, 1, 24, 31, 1,
                    24, 32, 1, 24, 33, 1, 24, 34, 1, 24, 35, 1, 24, 36, 1, 24, 37, 1, 24,
                    38, 1, 24, 39, 1, 24, 40, 1, 24, 41, 1, 24, 42, 1, 24, 43, 1, 24, 44,
                    1, 24, 45, 1, 24, 46, 1, 24, 47, 1, 24, 48, 1, 24, 49, 1, 24, 50, 1,
                    24, 51, 1, 24, 52, 1, 24, 53, 1, 24, 54, 1, 24, 55, 1, 24, 2, 1, 30,
                    12, 1, 30, 14, 1, 30, 15, 1, 30, 16, 1, 30, 18, 1, 30, 19, 1, 30, 25,
                    1, 30, 28, 1, 30, 29, 1, 30, 30, 1, 30, 31, 1, 30, 32, 1, 30, 33, 1,
                    30, 34, 1, 30, 35, 1, 30, 36, 1, 30, 37, 1, 30, 38, 1, 30, 39, 1, 30,
                    40, 1, 30, 41, 1, 30, 42, 1, 30, 43, 1, 30, 44, 1, 30, 45, 1, 30, 46,
                    1, 30, 47, 1, 30, 48, 1, 30, 49, 1, 30, 50, 1, 30, 51, 1, 30, 52, 1,
                    30, 53, 1, 30, 54, 1, 30, 55, 1, 30, 2, 1, 23, 12, 1, 23, 14, 1, 23,
                    15, 1, 23, 16, 1, 23, 18, 1, 23, 19, 1, 23, 25, 1, 23, 28, 1, 23, 29,
                    1, 23, 30, 1, 23, 31, 1, 23, 32, 1, 23, 33, 1, 23, 34, 1, 23, 35, 1,
                    23, 36, 1, 23, 37, 1, 23, 38, 1, 23, 39, 1, 23, 40, 1, 23, 41, 1, 23,
                    42, 1, 23, 43, 1, 23, 44, 1, 23, 45, 1, 23, 46, 1, 23, 47, 1, 23, 48,
                    1, 23, 49, 1, 23, 50, 1, 23, 51, 1, 23, 52, 1, 23, 53, 1, 23, 54, 1,
                    23, 55, 1, 23, 2, 1, 22, 12, 1, 22, 14, 1, 22, 15, 1, 22, 16, 1, 22,
                    18, 1, 22, 19, 1, 22, 25, 1, 22, 28, 1, 22, 29, 1, 22, 30, 1, 22, 31,
                    1, 22, 32, 1, 22, 33, 1, 22, 34, 1, 22, 35, 1, 22, 36, 1, 22, 37, 1,
                    22, 38, 1, 22, 39, 1, 22, 40, 1, 22, 41, 1, 22, 42, 1, 22, 43, 1, 22,
                    44, 1, 22, 45, 1, 22, 46, 1, 22, 47, 1, 22, 48, 1, 22, 49, 1, 22, 50,
                    1, 22, 51, 1, 22, 52, 1, 22, 53, 1, 22, 54, 1, 22, 55, 1, 22, 2, 1,
                    63, 12, 1, 63, 14, 1, 63, 15, 1, 63, 16, 1, 63, 19, 1, 63, 2, 1, 61,
                    12, 1, 61, 14, 1, 61, 15, 1, 61, 16, 1, 61, 18, 1, 61, 19, 1, 61, 44,
                    1, 61, 2, 1, 65, 12, 1, 65, 14, 1, 65, 15, 1, 65, 16, 1, 65, 19, 1,
                    65, 2, 1, 66, 12, 1, 66, 14, 1, 66, 15, 1, 66, 16, 1, 66, 19, 1, 66,
                    2, 1, 67, 12, 1, 67, 14, 1, 67, 15, 1, 67, 16, 1, 67, 19, 1, 67, 2,
                    1, 68, 12, 1, 68, 14, 1, 68, 15, 1, 68, 16, 1, 68, 19, 1, 68, 2, 1,
                    69, 12, 1, 69, 14, 1, 69, 15, 1, 69, 16, 1, 69, 19, 1, 69, 2, 1, 70,
                    12, 1, 70, 14, 1, 70, 15, 1, 70, 16, 1, 70, 19, 1, 70, 2, 1, 71, 12,
                    1, 71, 14, 1, 71, 15, 1, 71, 16, 1, 71, 19, 1, 71, 2, 1, 72, 12, 1,
                    72, 14, 1, 72, 15, 1, 72, 16, 1, 72, 19, 1, 72, 2, 1, 73, 12, 1, 73,
                    14, 1, 73, 15, 1, 73, 16, 1, 73, 19, 1, 73, 2, 1, 74, 12, 1, 74, 14,
                    1, 74, 15, 1, 74, 16, 1, 74, 19, 1, 74, 2, 1, 75, 12, 1, 75, 14, 1,
                    75, 15, 1, 75, 16, 1, 75, 19, 1, 75, 1, 1, 135, 2, 1, 135, 11, 1,
                    135, 15, 1, 135, 1, 1, 131, 2, 1, 131, 11, 1, 131, 15, 1, 131, 1, 1,
                    137, 2, 1, 137, 11, 1, 137, 15, 1, 137, 2, 1, 205, 0, 1, 204, 1, 1,
                    204, 2, 1, 204, 11, 1, 204, 19, 1, 204, 30, 1, 204, 73, 1, 204, 74,
                    1, 204, 75, 1, 204, 76, 1, 204, 77, 1, 204, 78, 1, 204, 79, 1, 204,
                    80, 1, 204, 81, 1, 204, 82, 1, 204, 83, 1, 204, 84, 1, 204, 85, 1,
                    204, 86, 1, 204, 2, 1, 142, 1, 1, 132, 2, 1, 132, 11, 1, 132, 15, 1,
                    132, 1, 1, 133, 2, 1, 133, 11, 1, 133, 15, 1, 133, 0, 1, 202, 1, 1,
                    202, 2, 1, 202, 11, 1, 202, 15, 1, 202, 30, 1, 202, 2, 1, 129, 15, 1,
                    129, 1, 1, 126, 2, 1, 126, 11, 1, 126, 13, 1, 126, 15, 1, 126, 16, 1,
                    126, 19, 1, 126, 45, 1, 126, 1, 1, 125, 2, 1, 125, 11, 1, 125, 13, 1,
                    125, 15, 1, 125, 16, 1, 125, 19, 1, 125, 45, 1, 125, 1, 1, 124, 2, 1,
                    124, 11, 1, 124, 13, 1, 124, 15, 1, 124, 16, 1, 124, 19, 1, 124, 45,
                    1, 124, 2, 1, 163, 15, 1, 163, 2, 1, 164, 15, 1, 164, 0, 1, 194, 1,
                    1, 194, 2, 1, 194, 11, 1, 194, 15, 1, 194, 16, 1, 194, 30, 1, 194,
                    68, 1, 194, 69, 1, 194, 70, 1, 194, 71, 1, 194, 72, 1, 194, 73, 1,
                    194, 74, 1, 194, 75, 1, 194, 76, 1, 194, 77, 1, 194, 78, 1, 194, 79,
                    1, 194, 80, 1, 194, 81, 1, 194, 82, 1, 194, 83, 1, 194, 84, 1, 194,
                    85, 1, 194, 86, 1, 194, 15, 1, 172, 16, 1, 172, 19, 1, 207, 15, 1,
                    174, 16, 1, 174, 19, 1, 208, 15, 1, 175, 16, 1, 175, 15, 1, 173, 16,
                    1, 173, 0, 1, 176, 14, 1, 176, 73, 1, 176, 74, 1, 176, 75, 1, 176,
                    76, 1, 176, 77, 1, 176, 78, 1, 176, 79, 1, 176, 80, 1, 176, 81, 1,
                    176, 82, 1, 176, 83, 1, 176, 84, 1, 176, 85, 1, 176, 86, 1, 176, 2,
                    1, 20, 15, 1, 20, 14, 1, 184, 15, 1, 184, 0, 1, 195, 1, 1, 195, 30,
                    1, 195, 68, 1, 195, 69, 1, 195, 70, 1, 195, 71, 1, 195, 72, 1, 195,
                    73, 1, 195, 74, 1, 195, 75, 1, 195, 76, 1, 195, 77, 1, 195, 78, 1,
                    195, 79, 1, 195, 80, 1, 195, 81, 1, 195, 82, 1, 195, 83, 1, 195, 84,
                    1, 195, 85, 1, 195, 86, 1, 195, 89, 1, 195, 14, 1, 192, 0, 1, 152, 1,
                    2, 7, 152, 11, 1, 7, 15, 1, 7, 16, 2, 7, 152, 18, 1, 7, 20, 1, 7, 21,
                    1, 7, 22, 1, 7, 23, 1, 7, 25, 1, 7, 28, 1, 7, 29, 1, 7, 30, 2, 7,
                    152, 31, 1, 7, 32, 1, 7, 33, 1, 7, 34, 1, 7, 35, 1, 7, 36, 1, 7, 37,
                    1, 7, 38, 1, 7, 39, 1, 7, 40, 1, 7, 41, 1, 7, 42, 1, 7, 43, 1, 7, 44,
                    1, 7, 45, 1, 7, 46, 1, 7, 47, 1, 7, 48, 1, 7, 49, 1, 7, 50, 1, 7, 51,
                    1, 7, 52, 1, 7, 53, 1, 7, 54, 1, 7, 55, 1, 7, 68, 1, 152, 69, 1, 152,
                    70, 1, 152, 71, 1, 152, 72, 1, 152, 73, 1, 152, 74, 1, 152, 75, 1,
                    152, 76, 1, 152, 77, 1, 152, 78, 1, 152, 79, 1, 152, 80, 1, 152, 81,
                    1, 152, 82, 1, 152, 83, 1, 152, 84, 1, 152, 85, 1, 152, 86, 1, 152,
                    1, 1, 7, 11, 1, 7, 15, 1, 7, 16, 1, 7, 18, 1, 7, 20, 1, 7, 21, 1, 7,
                    22, 1, 7, 23, 1, 7, 25, 1, 7, 28, 1, 7, 29, 1, 7, 30, 1, 7, 31, 1, 7,
                    32, 1, 7, 33, 1, 7, 34, 1, 7, 35, 1, 7, 36, 1, 7, 37, 1, 7, 38, 1, 7,
                    39, 1, 7, 40, 1, 7, 41, 1, 7, 42, 1, 7, 43, 1, 7, 44, 1, 7, 45, 1, 7,
                    46, 1, 7, 47, 1, 7, 48, 1, 7, 49, 1, 7, 50, 1, 7, 51, 1, 7, 52, 1, 7,
                    53, 1, 7, 54, 1, 7, 55, 1, 7, 0, 1, 89, 1, 1, 89, 2, 1, 89, 3, 1, 89,
                    4, 1, 89, 5, 1, 89, 6, 1, 89, 7, 1, 89, 8, 1, 89, 9, 1, 89, 10, 1,
                    89, 13, 1, 89, 14, 1, 89, 16, 1, 89, 22, 1, 89, 23, 1, 89, 24, 1, 89,
                    25, 1, 89, 26, 1, 89, 27, 1, 89, 28, 1, 89, 29, 1, 89, 30, 1, 89, 56,
                    1, 89, 57, 1, 89, 58, 1, 89, 59, 1, 89, 60, 1, 89, 61, 1, 89, 62, 1,
                    89, 63, 1, 89, 64, 1, 89, 65, 1, 89, 66, 1, 89, 67, 1, 89, 68, 1, 89,
                    69, 1, 89, 70, 1, 89, 71, 1, 89, 72, 1, 89, 73, 1, 89, 74, 1, 89, 75,
                    1, 89, 76, 1, 89, 77, 1, 89, 78, 1, 89, 79, 1, 89, 80, 1, 89, 81, 1,
                    89, 82, 1, 89, 83, 1, 89, 84, 1, 89, 85, 1, 89, 86, 1, 89, 0, 1, 90,
                    1, 1, 90, 2, 1, 90, 3, 1, 90, 4, 1, 90, 5, 1, 90, 6, 1, 90, 7, 1, 90,
                    8, 1, 90, 9, 1, 90, 10, 1, 90, 13, 1, 90, 14, 1, 90, 16, 1, 90, 22,
                    1, 90, 23, 1, 90, 24, 1, 90, 25, 1, 90, 26, 1, 90, 27, 1, 90, 28, 1,
                    90, 29, 1, 90, 30, 1, 90, 56, 1, 90, 57, 1, 90, 58, 1, 90, 59, 1, 90,
                    60, 1, 90, 61, 1, 90, 62, 1, 90, 63, 1, 90, 64, 1, 90, 65, 1, 90, 66,
                    1, 90, 67, 1, 90, 68, 1, 90, 69, 1, 90, 70, 1, 90, 71, 1, 90, 72, 1,
                    90, 73, 1, 90, 74, 1, 90, 75, 1, 90, 76, 1, 90, 77, 1, 90, 78, 1, 90,
                    79, 1, 90, 80, 1, 90, 81, 1, 90, 82, 1, 90, 83, 1, 90, 84, 1, 90, 85,
                    1, 90, 86, 1, 90, 0, 1, 100, 1, 1, 100, 3, 1, 100, 4, 1, 100, 5, 1,
                    100, 6, 1, 100, 7, 1, 100, 8, 1, 100, 9, 1, 100, 10, 1, 100, 13, 1,
                    100, 14, 1, 100, 16, 1, 100, 22, 1, 100, 23, 1, 100, 24, 1, 100, 25,
                    1, 100, 26, 1, 100, 27, 1, 100, 28, 1, 100, 29, 1, 100, 30, 1, 100,
                    56, 1, 100, 57, 1, 100, 58, 1, 100, 59, 1, 100, 60, 1, 100, 61, 1,
                    100, 62, 1, 100, 63, 1, 100, 64, 1, 100, 65, 1, 100, 66, 1, 100, 67,
                    1, 100, 68, 1, 100, 69, 1, 100, 70, 1, 100, 71, 1, 100, 72, 1, 100,
                    73, 1, 100, 74, 1, 100, 75, 1, 100, 76, 1, 100, 77, 1, 100, 78, 1,
                    100, 79, 1, 100, 80, 1, 100, 81, 1, 100, 82, 1, 100, 83, 1, 100, 84,
                    1, 100, 85, 1, 100, 86, 1, 100, 0, 1, 101, 1, 1, 101, 3, 1, 101, 4,
                    1, 101, 5, 1, 101, 6, 1, 101, 7, 1, 101, 8, 1, 101, 9, 1, 101, 10, 1,
                    101, 13, 1, 101, 14, 1, 101, 16, 1, 101, 22, 1, 101, 23, 1, 101, 24,
                    1, 101, 25, 1, 101, 26, 1, 101, 27, 1, 101, 28, 1, 101, 29, 1, 101,
                    30, 1, 101, 56, 1, 101, 57, 1, 101, 58, 1, 101, 59, 1, 101, 60, 1,
                    101, 61, 1, 101, 62, 1, 101, 63, 1, 101, 64, 1, 101, 65, 1, 101, 66,
                    1, 101, 67, 1, 101, 68, 1, 101, 69, 1, 101, 70, 1, 101, 71, 1, 101,
                    72, 1, 101, 73, 1, 101, 74, 1, 101, 75, 1, 101, 76, 1, 101, 77, 1,
                    101, 78, 1, 101, 79, 1, 101, 80, 1, 101, 81, 1, 101, 82, 1, 101, 83,
                    1, 101, 84, 1, 101, 85, 1, 101, 86, 1, 101, 0, 1, 102, 1, 1, 102, 3,
                    1, 102, 4, 1, 102, 5, 1, 102, 6, 1, 102, 7, 1, 102, 8, 1, 102, 9, 1,
                    102, 10, 1, 102, 13, 1, 102, 14, 1, 102, 16, 1, 102, 22, 1, 102, 23,
                    1, 102, 24, 1, 102, 25, 1, 102, 26, 1, 102, 27, 1, 102, 28, 1, 102,
                    29, 1, 102, 30, 1, 102, 56, 1, 102, 57, 1, 102, 58, 1, 102, 59, 1,
                    102, 60, 1, 102, 61, 1, 102, 62, 1, 102, 63, 1, 102, 64, 1, 102, 65,
                    1, 102, 66, 1, 102, 67, 1, 102, 68, 1, 102, 69, 1, 102, 70, 1, 102,
                    71, 1, 102, 72, 1, 102, 73, 1, 102, 74, 1, 102, 75, 1, 102, 76, 1,
                    102, 77, 1, 102, 78, 1, 102, 79, 1, 102, 80, 1, 102, 81, 1, 102, 82,
                    1, 102, 83, 1, 102, 84, 1, 102, 85, 1, 102, 86, 1, 102, 0, 1, 103, 1,
                    1, 103, 3, 1, 103, 4, 1, 103, 5, 1, 103, 6, 1, 103, 7, 1, 103, 8, 1,
                    103, 9, 1, 103, 10, 1, 103, 13, 1, 103, 14, 1, 103, 16, 1, 103, 22,
                    1, 103, 23, 1, 103, 24, 1, 103, 25, 1, 103, 26, 1, 103, 27, 1, 103,
                    28, 1, 103, 29, 1, 103, 30, 1, 103, 56, 1, 103, 57, 1, 103, 58, 1,
                    103, 59, 1, 103, 60, 1, 103, 61, 1, 103, 62, 1, 103, 63, 1, 103, 64,
                    1, 103, 65, 1, 103, 66, 1, 103, 67, 1, 103, 68, 1, 103, 69, 1, 103,
                    70, 1, 103, 71, 1, 103, 72, 1, 103, 73, 1, 103, 74, 1, 103, 75, 1,
                    103, 76, 1, 103, 77, 1, 103, 78, 1, 103, 79, 1, 103, 80, 1, 103, 81,
                    1, 103, 82, 1, 103, 83, 1, 103, 84, 1, 103, 85, 1, 103, 86, 1, 103,
                    0, 1, 104, 1, 1, 104, 3, 1, 104, 4, 1, 104, 5, 1, 104, 6, 1, 104, 7,
                    1, 104, 8, 1, 104, 9, 1, 104, 10, 1, 104, 13, 1, 104, 14, 1, 104, 16,
                    1, 104, 22, 1, 104, 23, 1, 104, 24, 1, 104, 25, 1, 104, 26, 1, 104,
                    27, 1, 104, 28, 1, 104, 29, 1, 104, 30, 1, 104, 56, 1, 104, 57, 1,
                    104, 58, 1, 104, 59, 1, 104, 60, 1, 104, 61, 1, 104, 62, 1, 104, 63,
                    1, 104, 64, 1, 104, 65, 1, 104, 66, 1, 104, 67, 1, 104, 68, 1, 104,
                    69, 1, 104, 70, 1, 104, 71, 1, 104, 72, 1, 104, 73, 1, 104, 74, 1,
                    104, 75, 1, 104, 76, 1, 104, 77, 1, 104, 78, 1, 104, 79, 1, 104, 80,
                    1, 104, 81, 1, 104, 82, 1, 104, 83, 1, 104, 84, 1, 104, 85, 1, 104,
                    86, 1, 104, 0, 1, 98, 1, 1, 98, 3, 1, 98, 4, 1, 98, 5, 1, 98, 6, 1,
                    98, 7, 1, 98, 8, 1, 98, 9, 1, 98, 10, 1, 98, 13, 1, 98, 14, 1, 98,
                    16, 1, 98, 22, 1, 98, 23, 1, 98, 24, 1, 98, 25, 1, 98, 26, 1, 98, 27,
                    1, 98, 28, 1, 98, 29, 1, 98, 30, 1, 98, 56, 1, 98, 57, 1, 98, 58, 1,
                    98, 59, 1, 98, 60, 1, 98, 61, 1, 98, 62, 1, 98, 63, 1, 98, 64, 1, 98,
                    65, 1, 98, 66, 1, 98, 67, 1, 98, 68, 1, 98, 69, 1, 98, 70, 1, 98, 71,
                    1, 98, 72, 1, 98, 73, 1, 98, 74, 1, 98, 75, 1, 98, 76, 1, 98, 77, 1,
                    98, 78, 1, 98, 79, 1, 98, 80, 1, 98, 81, 1, 98, 82, 1, 98, 83, 1, 98,
                    84, 1, 98, 85, 1, 98, 86, 1, 98, 0, 1, 99, 1, 1, 99, 3, 1, 99, 4, 1,
                    99, 5, 1, 99, 6, 1, 99, 7, 1, 99, 8, 1, 99, 9, 1, 99, 10, 1, 99, 13,
                    1, 99, 14, 1, 99, 16, 1, 99, 22, 1, 99, 23, 1, 99, 24, 1, 99, 25, 1,
                    99, 26, 1, 99, 27, 1, 99, 28, 1, 99, 29, 1, 99, 30, 1, 99, 56, 1, 99,
                    57, 1, 99, 58, 1, 99, 59, 1, 99, 60, 1, 99, 61, 1, 99, 62, 1, 99, 63,
                    1, 99, 64, 1, 99, 65, 1, 99, 66, 1, 99, 67, 1, 99, 68, 1, 99, 69, 1,
                    99, 70, 1, 99, 71, 1, 99, 72, 1, 99, 73, 1, 99, 74, 1, 99, 75, 1, 99,
                    76, 1, 99, 77, 1, 99, 78, 1, 99, 79, 1, 99, 80, 1, 99, 81, 1, 99, 82,
                    1, 99, 83, 1, 99, 84, 1, 99, 85, 1, 99, 86, 1, 99, 0, 1, 105, 1, 1,
                    105, 3, 1, 105, 4, 1, 105, 5, 1, 105, 6, 1, 105, 7, 1, 105, 8, 1,
                    105, 9, 1, 105, 10, 1, 105, 13, 1, 105, 14, 1, 105, 16, 1, 105, 22,
                    1, 105, 23, 1, 105, 24, 1, 105, 25, 1, 105, 26, 1, 105, 27, 1, 105,
                    28, 1, 105, 29, 1, 105, 30, 1, 105, 56, 1, 105, 57, 1, 105, 58, 1,
                    105, 60, 1, 105, 61, 1, 105, 62, 1, 105, 63, 1, 105, 64, 1, 105, 65,
                    1, 105, 66, 1, 105, 67, 1, 105, 68, 1, 105, 69, 1, 105, 70, 1, 105,
                    71, 1, 105, 72, 1, 105, 73, 1, 105, 74, 1, 105, 75, 1, 105, 76, 1,
                    105, 77, 1, 105, 78, 1, 105, 79, 1, 105, 80, 1, 105, 81, 1, 105, 82,
                    1, 105, 83, 1, 105, 84, 1, 105, 85, 1, 105, 86, 1, 105, 89, 1, 105,
                    15, 1, 185, 16, 1, 185, 14, 1, 81, 15, 1, 81, 14, 1, 79, 15, 1, 79,
                    16, 1, 79, 14, 1, 80, 15, 1, 80, 16, 1, 80, 14, 1, 82, 15, 1, 82, 15,
                    1, 186, 16, 1, 186, 15, 1, 187, 16, 1, 187, 15, 1, 188, 16, 1, 188,
                    0, 1, 106, 1, 1, 106, 3, 1, 106, 4, 1, 106, 5, 1, 106, 6, 1, 106, 7,
                    1, 106, 8, 1, 106, 9, 1, 106, 10, 1, 106, 13, 1, 106, 14, 1, 106, 16,
                    1, 106, 22, 1, 106, 23, 1, 106, 24, 1, 106, 25, 1, 106, 26, 1, 106,
                    27, 1, 106, 28, 1, 106, 29, 1, 106, 30, 1, 106, 56, 1, 106, 57, 1,
                    106, 58, 1, 106, 60, 1, 106, 61, 1, 106, 62, 1, 106, 63, 1, 106, 64,
                    1, 106, 65, 1, 106, 66, 1, 106, 67, 1, 106, 68, 1, 106, 69, 1, 106,
                    70, 1, 106, 71, 1, 106, 72, 1, 106, 73, 1, 106, 74, 1, 106, 75, 1,
                    106, 76, 1, 106, 77, 1, 106, 78, 1, 106, 79, 1, 106, 80, 1, 106, 81,
                    1, 106, 82, 1, 106, 83, 1, 106, 84, 1, 106, 85, 1, 106, 86, 1, 106,
                    89, 1, 106, 0, 1, 189, 1, 1, 189, 3, 1, 189, 4, 1, 189, 5, 1, 189, 6,
                    1, 189, 7, 1, 189, 8, 1, 189, 9, 1, 189, 10, 1, 189, 13, 1, 189, 14,
                    1, 189, 16, 1, 189, 22, 1, 189, 23, 1, 189, 24, 1, 189, 25, 1, 189,
                    26, 1, 189, 27, 1, 189, 28, 1, 189, 29, 1, 189, 30, 1, 189, 56, 1,
                    189, 57, 1, 189, 58, 1, 189, 60, 1, 189, 61, 1, 189, 62, 1, 189, 63,
                    1, 189, 64, 1, 189, 65, 1, 189, 66, 1, 189, 67, 1, 189, 68, 1, 189,
                    69, 1, 189, 70, 1, 189, 71, 1, 189, 72, 1, 189, 73, 1, 189, 74, 1,
                    189, 75, 1, 189, 76, 1, 189, 77, 1, 189, 78, 1, 189, 79, 1, 189, 80,
                    1, 189, 81, 1, 189, 82, 1, 189, 83, 1, 189, 84, 1, 189, 85, 1, 189,
                    86, 1, 189, 14, 1, 191, 0, 1, 190, 1, 1, 190, 3, 1, 190, 4, 1, 190,
                    5, 1, 190, 6, 1, 190, 7, 1, 190, 8, 1, 190, 9, 1, 190, 10, 1, 190,
                    13, 1, 190, 14, 1, 190, 16, 1, 190, 22, 1, 190, 23, 1, 190, 24, 1,
                    190, 25, 1, 190, 26, 1, 190, 27, 1, 190, 28, 1, 190, 29, 1, 190, 30,
                    1, 190, 56, 1, 190, 57, 1, 190, 58, 1, 190, 60, 1, 190, 61, 1, 190,
                    62, 1, 190, 63, 1, 190, 64, 1, 190, 65, 1, 190, 66, 1, 190, 67, 1,
                    190, 68, 1, 190, 69, 1, 190, 70, 1, 190, 71, 1, 190, 72, 1, 190, 73,
                    1, 190, 74, 1, 190, 75, 1, 190, 76, 1, 190, 77, 1, 190, 78, 1, 190,
                    79, 1, 190, 80, 1, 190, 81, 1, 190, 82, 1, 190, 83, 1, 190, 84, 1,
                    190, 85, 1, 190, 86, 1, 190, 0, 1, 88, 1, 1, 88, 3, 1, 88, 4, 1, 88,
                    5, 1, 88, 6, 1, 88, 7, 1, 88, 8, 1, 88, 9, 1, 88, 10, 1, 88, 13, 1,
                    88, 14, 1, 88, 16, 1, 88, 22, 1, 88, 23, 1, 88, 24, 1, 88, 25, 1, 88,
                    26, 1, 88, 27, 1, 88, 28, 1, 88, 29, 1, 88, 30, 1, 88, 56, 1, 88, 57,
                    1, 88, 58, 1, 88, 59, 1, 88, 60, 1, 88, 61, 1, 88, 62, 1, 88, 63, 1,
                    88, 64, 1, 88, 65, 1, 88, 66, 1, 88, 67, 1, 88, 68, 1, 88, 69, 1, 88,
                    70, 1, 88, 71, 1, 88, 72, 1, 88, 73, 1, 88, 74, 1, 88, 75, 1, 88, 76,
                    1, 88, 77, 1, 88, 78, 1, 88, 79, 1, 88, 80, 1, 88, 81, 1, 88, 82, 1,
                    88, 83, 1, 88, 84, 1, 88, 85, 1, 88, 86, 1, 88, 89, 1, 88, 0, 1, 97,
                    1, 1, 97, 3, 1, 97, 4, 1, 97, 5, 1, 97, 6, 1, 97, 7, 1, 97, 8, 1, 97,
                    9, 1, 97, 10, 1, 97, 13, 1, 97, 14, 1, 97, 16, 1, 97, 22, 1, 97, 23,
                    1, 97, 24, 1, 97, 25, 1, 97, 26, 1, 97, 27, 1, 97, 28, 1, 97, 29, 1,
                    97, 30, 1, 97, 56, 1, 97, 57, 1, 97, 58, 1, 97, 59, 1, 97, 60, 1, 97,
                    61, 1, 97, 62, 1, 97, 63, 1, 97, 64, 1, 97, 65, 1, 97, 66, 1, 97, 67,
                    1, 97, 68, 1, 97, 69, 1, 97, 70, 1, 97, 71, 1, 97, 72, 1, 97, 73, 1,
                    97, 74, 1, 97, 75, 1, 97, 76, 1, 97, 77, 1, 97, 78, 1, 97, 79, 1, 97,
                    80, 1, 97, 81, 1, 97, 82, 1, 97, 83, 1, 97, 84, 1, 97, 85, 1, 97, 86,
                    1, 97, 0, 1, 96, 1, 1, 96, 3, 1, 96, 4, 1, 96, 5, 1, 96, 6, 1, 96, 7,
                    1, 96, 8, 1, 96, 9, 1, 96, 10, 1, 96, 13, 1, 96, 14, 1, 96, 16, 1,
                    96, 22, 1, 96, 23, 1, 96, 24, 1, 96, 25, 1, 96, 26, 1, 96, 27, 1, 96,
                    28, 1, 96, 29, 1, 96, 30, 1, 96, 56, 1, 96, 57, 1, 96, 58, 1, 96, 59,
                    1, 96, 60, 1, 96, 61, 1, 96, 62, 1, 96, 63, 1, 96, 64, 1, 96, 65, 1,
                    96, 66, 1, 96, 67, 1, 96, 68, 1, 96, 69, 1, 96, 70, 1, 96, 71, 1, 96,
                    72, 1, 96, 73, 1, 96, 74, 1, 96, 75, 1, 96, 76, 1, 96, 77, 1, 96, 78,
                    1, 96, 79, 1, 96, 80, 1, 96, 81, 1, 96, 82, 1, 96, 83, 1, 96, 84, 1,
                    96, 85, 1, 96, 86, 1, 96, 0, 1, 93, 1, 1, 93, 3, 1, 93, 4, 1, 93, 5,
                    1, 93, 6, 1, 93, 7, 1, 93, 8, 1, 93, 9, 1, 93, 10, 1, 93, 13, 1, 93,
                    14, 1, 93, 16, 1, 93, 22, 1, 93, 23, 1, 93, 24, 1, 93, 25, 1, 93, 26,
                    1, 93, 27, 1, 93, 28, 1, 93, 29, 1, 93, 30, 1, 93, 56, 1, 93, 57, 1,
                    93, 58, 1, 93, 59, 1, 93, 60, 1, 93, 61, 1, 93, 62, 1, 93, 63, 1, 93,
                    64, 1, 93, 65, 1, 93, 66, 1, 93, 67, 1, 93, 68, 1, 93, 69, 1, 93, 70,
                    1, 93, 71, 1, 93, 72, 1, 93, 73, 1, 93, 74, 1, 93, 75, 1, 93, 76, 1,
                    93, 77, 1, 93, 78, 1, 93, 79, 1, 93, 80, 1, 93, 81, 1, 93, 82, 1, 93,
                    83, 1, 93, 84, 1, 93, 85, 1, 93, 86, 1, 93, 0, 1, 91, 1, 1, 91, 3, 1,
                    91, 4, 1, 91, 5, 1, 91, 6, 1, 91, 7, 1, 91, 8, 1, 91, 9, 1, 91, 10,
                    1, 91, 13, 1, 91, 14, 1, 91, 16, 1, 91, 22, 1, 91, 23, 1, 91, 24, 1,
                    91, 25, 1, 91, 26, 1, 91, 27, 1, 91, 28, 1, 91, 29, 1, 91, 30, 1, 91,
                    56, 1, 91, 57, 1, 91, 58, 1, 91, 60, 1, 91, 61, 1, 91, 62, 1, 91, 63,
                    1, 91, 64, 1, 91, 65, 1, 91, 66, 1, 91, 67, 1, 91, 68, 1, 91, 69, 1,
                    91, 70, 1, 91, 71, 1, 91, 72, 1, 91, 73, 1, 91, 74, 1, 91, 75, 1, 91,
                    76, 1, 91, 77, 1, 91, 78, 1, 91, 79, 1, 91, 80, 1, 91, 81, 1, 91, 82,
                    1, 91, 83, 1, 91, 84, 1, 91, 85, 1, 91, 86, 1, 91, 0, 1, 92, 1, 1,
                    92, 3, 1, 92, 4, 1, 92, 5, 1, 92, 6, 1, 92, 7, 1, 92, 8, 1, 92, 9, 1,
                    92, 10, 1, 92, 13, 1, 92, 14, 1, 92, 16, 1, 92, 22, 1, 92, 23, 1, 92,
                    24, 1, 92, 25, 1, 92, 26, 1, 92, 27, 1, 92, 28, 1, 92, 29, 1, 92, 30,
                    1, 92, 56, 1, 92, 57, 1, 92, 58, 1, 92, 59, 1, 92, 60, 1, 92, 61, 1,
                    92, 62, 1, 92, 63, 1, 92, 64, 1, 92, 65, 1, 92, 66, 1, 92, 67, 1, 92,
                    68, 1, 92, 69, 1, 92, 70, 1, 92, 71, 1, 92, 72, 1, 92, 73, 1, 92, 74,
                    1, 92, 75, 1, 92, 76, 1, 92, 77, 1, 92, 78, 1, 92, 79, 1, 92, 80, 1,
                    92, 81, 1, 92, 82, 1, 92, 83, 1, 92, 84, 1, 92, 85, 1, 92, 86, 1, 92,
                    0, 1, 85, 1, 1, 85, 3, 1, 85, 4, 1, 85, 5, 1, 85, 6, 1, 85, 7, 1, 85,
                    8, 1, 85, 9, 1, 85, 10, 1, 85, 13, 1, 85, 14, 1, 85, 16, 1, 85, 22,
                    1, 85, 23, 1, 85, 24, 1, 85, 25, 1, 85, 26, 1, 85, 27, 1, 85, 28, 1,
                    85, 29, 1, 85, 30, 1, 85, 56, 1, 85, 57, 1, 85, 58, 1, 85, 59, 1, 85,
                    60, 1, 85, 61, 1, 85, 62, 1, 85, 63, 1, 85, 64, 1, 85, 65, 1, 85, 66,
                    1, 85, 67, 1, 85, 68, 1, 85, 69, 1, 85, 70, 1, 85, 71, 1, 85, 72, 1,
                    85, 73, 1, 85, 74, 1, 85, 75, 1, 85, 76, 1, 85, 77, 1, 85, 78, 1, 85,
                    79, 1, 85, 80, 1, 85, 81, 1, 85, 82, 1, 85, 83, 1, 85, 84, 1, 85, 85,
                    1, 85, 86, 1, 85, 0, 1, 84, 1, 1, 84, 3, 1, 84, 4, 1, 84, 5, 1, 84,
                    6, 1, 84, 7, 1, 84, 8, 1, 84, 9, 1, 84, 10, 1, 84, 13, 1, 84, 14, 1,
                    84, 16, 1, 84, 22, 1, 84, 23, 1, 84, 24, 1, 84, 25, 1, 84, 26, 1, 84,
                    27, 1, 84, 28, 1, 84, 29, 1, 84, 30, 1, 84, 56, 1, 84, 57, 1, 84, 58,
                    1, 84, 59, 1, 84, 60, 1, 84, 61, 1, 84, 62, 1, 84, 63, 1, 84, 64, 1,
                    84, 65, 1, 84, 66, 1, 84, 67, 1, 84, 68, 1, 84, 69, 1, 84, 70, 1, 84,
                    71, 1, 84, 72, 1, 84, 73, 1, 84, 74, 1, 84, 75, 1, 84, 76, 1, 84, 77,
                    1, 84, 78, 1, 84, 79, 1, 84, 80, 1, 84, 81, 1, 84, 82, 1, 84, 83, 1,
                    84, 84, 1, 84, 85, 1, 84, 86, 1, 84, 0, 1, 83, 1, 1, 83, 3, 1, 83, 4,
                    1, 83, 5, 1, 83, 6, 1, 83, 7, 1, 83, 8, 1, 83, 9, 1, 83, 10, 1, 83,
                    13, 1, 83, 14, 1, 83, 16, 1, 83, 22, 1, 83, 23, 1, 83, 24, 1, 83, 25,
                    1, 83, 26, 1, 83, 27, 1, 83, 28, 1, 83, 29, 1, 83, 30, 1, 83, 56, 1,
                    83, 57, 1, 83, 58, 1, 83, 59, 1, 83, 60, 1, 83, 61, 1, 83, 62, 1, 83,
                    63, 1, 83, 64, 1, 83, 65, 1, 83, 66, 1, 83, 67, 1, 83, 68, 1, 83, 69,
                    1, 83, 70, 1, 83, 71, 1, 83, 72, 1, 83, 73, 1, 83, 74, 1, 83, 75, 1,
                    83, 76, 1, 83, 77, 1, 83, 78, 1, 83, 79, 1, 83, 80, 1, 83, 81, 1, 83,
                    82, 1, 83, 83, 1, 83, 84, 1, 83, 85, 1, 83, 86, 1, 83, 0, 1, 108, 1,
                    1, 108, 30, 1, 108, 68, 1, 108, 69, 1, 108, 70, 1, 108, 71, 1, 108,
                    72, 1, 108, 73, 1, 108, 74, 1, 108, 75, 1, 108, 76, 1, 108, 77, 1,
                    108, 78, 1, 108, 79, 1, 108, 80, 1, 108, 81, 1, 108, 82, 1, 108, 83,
                    1, 108, 84, 1, 108, 85, 1, 108, 86, 1, 108, 89, 1, 108, 15, 1, 185,
                    16, 1, 185, 0, 1, 107, 1, 1, 107, 30, 1, 107, 68, 1, 107, 69, 1, 107,
                    70, 1, 107, 71, 1, 107, 72, 1, 107, 73, 1, 107, 74, 1, 107, 75, 1,
                    107, 76, 1, 107, 77, 1, 107, 78, 1, 107, 79, 1, 107, 80, 1, 107, 81,
                    1, 107, 82, 1, 107, 83, 1, 107, 84, 1, 107, 85, 1, 107, 86, 1, 107,
                    89, 1, 107, 89, 1, 197, 0, 1, 196, 1, 1, 196, 30, 1, 196, 68, 1, 196,
                    69, 1, 196, 70, 1, 196, 71, 1, 196, 72, 1, 196, 73, 1, 196, 74, 1,
                    196, 75, 1, 196, 76, 1, 196, 77, 1, 196, 78, 1, 196, 79, 1, 196, 80,
                    1, 196, 81, 1, 196, 82, 1, 196, 83, 1, 196, 84, 1, 196, 85, 1, 196,
                    86, 1, 196, 89, 1, 196, 89, 1, 117,
                ];
                static REDUCE_OFFSETS: &[u32] = &[
                    0, 0, 3, 96, 96, 120, 129, 210, 291, 315, 333, 357, 357, 375, 393,
                    393, 471, 495, 573, 651, 729, 807, 885, 966, 1047, 1128, 1209, 1290,
                    1371, 1452, 1533, 1614, 1620, 1626, 1629, 1713, 1713, 1713, 1719,
                    1719, 1845, 1845, 2027, 2153, 2279, 2405, 2531, 2657, 2783, 2909,
                    3035, 3035, 3035, 3035, 3035, 3035, 3035, 3035, 3035, 3035, 3035,
                    3035, 3143, 3143, 3269, 3269, 3395, 3395, 3470, 3470, 3536, 3536,
                    3536, 3617, 3617, 3623, 3629, 3689, 3689, 3743, 3743, 3785, 3785,
                    3821, 3821, 3863, 3863, 3917, 3917, 3977, 3977, 4043, 4043, 4118,
                    4118, 4193, 4193, 4247, 4247, 4301, 4301, 4343, 4376, 4376, 4412,
                    4442, 4442, 4475, 4502, 4502, 4532, 4556, 4556, 4583, 4601, 4601,
                    4601, 4727, 4727, 4742, 4802, 4862, 4862, 4862, 4922, 4925, 5009,
                    5009, 5012, 5060, 5141, 5222, 5225, 5225, 5249, 5327, 5405, 5483,
                    5561, 5567, 5570, 5570, 5573, 5579, 5582, 5630, 5630, 5711, 5717,
                    5717, 5729, 5729, 5729, 5837, 5840, 5840, 5852, 5855, 5861, 5867,
                    5867, 5879, 5879, 5891, 5891, 5903, 5909, 5909, 5984, 6050, 6110,
                    6164, 6164, 6290, 6290, 6416, 6416, 6542, 6668, 6794, 6902, 7010,
                    7118, 7226, 7334, 7442, 7550, 7658, 7766, 7874, 7874, 7874, 7892,
                    7892, 7916, 7934, 7934, 7952, 7952, 7970, 7970, 7988, 7988, 8006,
                    8006, 8024, 8024, 8042, 8042, 8060, 8060, 8078, 8078, 8096, 8096,
                    8114, 8114, 8126, 8126, 8138, 8138, 8150, 8153, 8213, 8216, 8228,
                    8228, 8240, 8258, 8264, 8264, 8288, 8288, 8312, 8312, 8336, 8342,
                    8348, 8426, 8435, 8441, 8441, 8444, 8450, 8450, 8450, 8456, 8504,
                    8510, 8516, 8585, 8585, 8585, 8585, 8588, 8765, 8765, 8879, 9041,
                    9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041,
                    9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9041, 9203,
                    9203, 9203, 9203, 9203, 9203, 9362, 9362, 9521, 9521, 9680, 9680,
                    9839, 9839, 9998, 10157, 10157, 10157, 10316, 10316, 10475, 10481,
                    10481, 10481, 10487, 10487, 10496, 10496, 10505, 10511, 10517, 10523,
                    10523, 10523, 10529, 10688, 10844, 10847, 11003, 11003, 11165, 11165,
                    11165, 11165, 11165, 11165, 11324, 11483, 11642, 11798, 11798, 11957,
                    12116, 12275, 12434, 12503, 12503, 12509, 12578, 12581, 12650, 12653,
                ];
                static CAN_ACCEPT_ERROR: &[u8] = &[
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ];
                let num_rules = 216usize;
                let mut rules = Vec::with_capacity(num_rules);
                for i in 0..num_rules {
                    let lhs = NonTerminals::from_usize(RULE_NAMES[i] as usize);
                    rules
                        .push(::rusty_lr::parser::table::RuleInfo {
                            lhs,
                            len: RULE_LENGTHS[i] as usize,
                        });
                }
                let num_states = 341usize;
                let mut state_rows = Vec::with_capacity(num_states);
                for i in 0..num_states {
                    let term_start = SHIFT_TERM_OFFSETS[i] as usize;
                    let term_end = SHIFT_TERM_OFFSETS[i + 1] as usize;
                    let mut shift_goto_map_term = Vec::with_capacity(
                        term_end - term_start,
                    );
                    for idx in term_start..term_end {
                        let val = SHIFT_TERM_DATA[idx];
                        let term_class = TerminalClasses::from_usize(
                            (val & 0x7fff) as usize,
                        );
                        let state = ((val >> 15) & 0xffff) as usize;
                        let push = (val >> 31) != 0;
                        shift_goto_map_term
                            .push((
                                term_class,
                                ::rusty_lr::parser::table::ShiftTarget::new(state, push),
                            ));
                    }
                    let nonterm_start = SHIFT_NONTERM_OFFSETS[i] as usize;
                    let nonterm_end = SHIFT_NONTERM_OFFSETS[i + 1] as usize;
                    let mut shift_goto_map_nonterm = Vec::with_capacity(
                        nonterm_end - nonterm_start,
                    );
                    for idx in nonterm_start..nonterm_end {
                        let val = SHIFT_NONTERM_DATA[idx];
                        let nonterm = NonTerminals::from_usize((val & 0x7fff) as usize);
                        let state = ((val >> 15) & 0xffff) as usize;
                        let push = (val >> 31) != 0;
                        shift_goto_map_nonterm
                            .push((
                                nonterm,
                                ::rusty_lr::parser::table::ShiftTarget::new(state, push),
                            ));
                    }
                    let reduce_start = REDUCE_OFFSETS[i] as usize;
                    let reduce_end = REDUCE_OFFSETS[i + 1] as usize;
                    let mut reduce_map = Vec::new();
                    let mut idx = reduce_start;
                    while idx < reduce_end {
                        let term_val = REDUCE_DATA[idx];
                        let term_class = TerminalClasses::from_usize(term_val as usize);
                        let len = REDUCE_DATA[idx + 1] as usize;
                        let mut rules = Vec::with_capacity(len);
                        for r_idx in 0..len {
                            rules.push(REDUCE_DATA[idx + 2 + r_idx] as usize);
                        }
                        reduce_map.push((term_class, rules));
                        idx += 2 + len;
                    }
                    let can_accept_error = match CAN_ACCEPT_ERROR[i] {
                        0 => ::rusty_lr::TriState::False,
                        1 => ::rusty_lr::TriState::True,
                        2 => ::rusty_lr::TriState::Maybe,
                        _ => unreachable!(),
                    };
                    let intermediate = ::rusty_lr::parser::state::IntermediateState {
                        shift_goto_map_term,
                        shift_goto_map_nonterm,
                        reduce_map,
                        ruleset: Vec::new(),
                        can_accept_error,
                    };
                    state_rows.push(intermediate);
                }
                ::rusty_lr::parser::table::IntermediateTables {
                    state_rows,
                    rules,
                }
                    .into()
            })
    }
    #[doc(hidden)]
    fn __rusty_lr_parser_version() -> (usize, usize, usize) {
        (4, 4, 0)
    }
    #[doc(hidden)]
    fn __rustylr_version() -> (usize, usize, usize) {
        (1, 35, 0)
    }
    #[doc(hidden)]
    fn __rusty_lr_version() -> (usize, usize, usize) {
        (4, 4, 0)
    }
}

// ==============================Generated Codes End===============================
        