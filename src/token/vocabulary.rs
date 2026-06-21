use std::collections::HashMap;

use super::token::Token;

// This module defines lexical vocabularies rather than parser-combinator tries.

const PUNCTUATOR_AND_DIRECTIVE_TABLE: &[(&str, Token)] = &[
    ("#include", Token::PreprocessorInclude),
    ("#ifndef", Token::PreprocessorIfNDef),
    ("#define", Token::PreprocessorDefine),
    ("#ifdef", Token::PreprocessorIfDef),
    ("#undef", Token::PreprocessorUndef),
    ("#endif", Token::PreprocessorEndIf),
    ("#elif", Token::PreprocessorElIf),
    ("#else", Token::PreprocessorElse),
    ("#if", Token::PreprocessorIf),
    ("...", Token::Ellipsis),
    (">>=", Token::RightAssign),
    ("<<=", Token::LeftAssign),
    ("+=", Token::AddAssign),
    ("-=", Token::SubAssign),
    ("*=", Token::MulAssign),
    ("/=", Token::DivAssign),
    ("%=", Token::ModAssign),
    ("&=", Token::AndAssign),
    ("^=", Token::XorAssign),
    ("|=", Token::OrAssign),
    (">>", Token::RightOp),
    ("<<", Token::LeftOp),
    ("++", Token::IncOp),
    ("--", Token::DecOp),
    ("->", Token::PtrOp),
    ("&&", Token::AndOp),
    ("||", Token::OrOp),
    ("<=", Token::LeOp),
    (">=", Token::GeOp),
    ("==", Token::EqOp),
    ("!=", Token::NeOp),
    ("<%", Token::LeftBrace),
    ("%>", Token::RightBrace),
    ("<:", Token::LeftBracket),
    (":>", Token::RightBracket),
    (";", Token::SemiColon),
    ("{", Token::LeftBrace),
    ("}", Token::RightBrace),
    (",", Token::Comma),
    (":", Token::Colon),
    ("=", Token::Equal),
    ("(", Token::LeftParen),
    (")", Token::RightParen),
    ("[", Token::LeftBracket),
    ("]", Token::RightBracket),
    (".", Token::Dot),
    ("&", Token::Ampersand),
    ("!", Token::Exclamation),
    ("~", Token::Tilde),
    ("-", Token::Minus),
    ("+", Token::Plus),
    ("*", Token::Star),
    ("/", Token::Slash),
    ("%", Token::Percent),
    ("<", Token::LessThan),
    (">", Token::GreaterThan),
    ("^", Token::Caret),
    ("|", Token::Pipe),
    ("?", Token::Question),
    ("\n", Token::NewLine),
];

/// Return the complete punctuator/directive lexical vocabulary used by token recognition.
pub fn punctuator_and_directive_table() -> &'static [(&'static str, Token)] {
    // Return the lexical vocabulary in maximal-munch order, so longer lexemes win before prefixes.
    PUNCTUATOR_AND_DIRECTIVE_TABLE
}

/// Build the reserved-word environment used after preprocessing preserves macro semantics.
pub fn ident_to_keyword_map() -> HashMap<String, Token> {
    // Build the semantic keyword environment used after macro expansion preserves identifiers.
    let mut dict = HashMap::new();

    // Insert each reserved word as an identifier-to-token denotation.
    dict.insert("auto".to_string(), Token::Auto);
    dict.insert("break".to_string(), Token::Break);
    dict.insert("case".to_string(), Token::Case);
    dict.insert("char".to_string(), Token::Char);
    dict.insert("const".to_string(), Token::Const);
    dict.insert("continue".to_string(), Token::Continue);
    dict.insert("default".to_string(), Token::Default);
    dict.insert("do".to_string(), Token::Do);
    dict.insert("double".to_string(), Token::Double);
    dict.insert("else".to_string(), Token::Else);
    dict.insert("enum".to_string(), Token::Enum);
    dict.insert("extern".to_string(), Token::Extern);
    dict.insert("float".to_string(), Token::Float);
    dict.insert("for".to_string(), Token::For);
    dict.insert("goto".to_string(), Token::Goto);
    dict.insert("if".to_string(), Token::If);
    dict.insert("int".to_string(), Token::Int);
    dict.insert("long".to_string(), Token::Long);
    dict.insert("register".to_string(), Token::Register);
    dict.insert("return".to_string(), Token::Return);
    dict.insert("short".to_string(), Token::Short);
    dict.insert("signed".to_string(), Token::Signed);
    dict.insert("sizeof".to_string(), Token::Sizeof);
    dict.insert("static".to_string(), Token::Static);
    dict.insert("struct".to_string(), Token::Struct);
    dict.insert("switch".to_string(), Token::Switch);
    dict.insert("typedef".to_string(), Token::Typedef);
    dict.insert("union".to_string(), Token::Union);
    dict.insert("unsigned".to_string(), Token::Unsigned);
    dict.insert("void".to_string(), Token::Void);
    dict.insert("volatile".to_string(), Token::Volatile);
    dict.insert("while".to_string(), Token::While);

    dict
}
