use std::collections::HashMap;

use super::token::Token;
use rusty_parser as rp;

pub fn keyword_parser() -> rp::DictHashMap<(Token,), char> {
    let mut dict = rp::DictHashMap::new();

    dict.insert("...".chars(), (Token::Ellipsis,));
    dict.insert(">>=".chars(), (Token::RightAssign,));
    dict.insert("<<=".chars(), (Token::LeftAssign,));
    dict.insert("+=".chars(), (Token::AddAssign,));
    dict.insert("-=".chars(), (Token::SubAssign,));
    dict.insert("*=".chars(), (Token::MulAssign,));
    dict.insert("/=".chars(), (Token::DivAssign,));
    dict.insert("%=".chars(), (Token::ModAssign,));
    dict.insert("&=".chars(), (Token::AndAssign,));
    dict.insert("^=".chars(), (Token::XorAssign,));
    dict.insert("|=".chars(), (Token::OrAssign,));
    dict.insert(">>".chars(), (Token::RightOp,));
    dict.insert("<<".chars(), (Token::LeftOp,));
    dict.insert("++".chars(), (Token::IncOp,));
    dict.insert("--".chars(), (Token::DecOp,));
    dict.insert("->".chars(), (Token::PtrOp,));
    dict.insert("&&".chars(), (Token::AndOp,));
    dict.insert("||".chars(), (Token::OrOp,));
    dict.insert("<=".chars(), (Token::LeOp,));
    dict.insert(">=".chars(), (Token::GeOp,));
    dict.insert("==".chars(), (Token::EqOp,));
    dict.insert("!=".chars(), (Token::NeOp,));

    dict.insert(";".chars(), (Token::SemiColon,));
    dict.insert("{".chars(), (Token::LeftBrace,));
    dict.insert("<%".chars(), (Token::LeftBrace,));
    dict.insert("}".chars(), (Token::RightBrace,));
    dict.insert("%>".chars(), (Token::RightBrace,));
    dict.insert(",".chars(), (Token::Comma,));
    dict.insert(":".chars(), (Token::Colon,));
    dict.insert("=".chars(), (Token::Equal,));
    dict.insert("(".chars(), (Token::LeftParen,));
    dict.insert(")".chars(), (Token::RightParen,));
    dict.insert("[".chars(), (Token::LeftBracket,));
    dict.insert("<:".chars(), (Token::LeftBracket,));
    dict.insert("]".chars(), (Token::RightBracket,));
    dict.insert(":>".chars(), (Token::RightBracket,));
    dict.insert(".".chars(), (Token::Dot,));
    dict.insert("&".chars(), (Token::Ampersand,));
    dict.insert("!".chars(), (Token::Exclamation,));
    dict.insert("~".chars(), (Token::Tilde,));
    dict.insert("-".chars(), (Token::Minus,));
    dict.insert("+".chars(), (Token::Plus,));
    dict.insert("*".chars(), (Token::Star,));
    dict.insert("/".chars(), (Token::Slash,));
    dict.insert("%".chars(), (Token::Percent,));
    dict.insert("<".chars(), (Token::LessThan,));
    dict.insert(">".chars(), (Token::GreaterThan,));
    dict.insert("^".chars(), (Token::Caret,));
    dict.insert("|".chars(), (Token::Pipe,));
    dict.insert("?".chars(), (Token::Question,));
    dict.insert("#define".chars(), (Token::PreprocessorDefine,));
    dict.insert("#ifdef".chars(), (Token::PreprocessorIfDef,));
    dict.insert("#ifndef".chars(), (Token::PreprocessorIfNDef,));
    dict.insert("#undef".chars(), (Token::PreprocessorUndef,));
    dict.insert("#elif".chars(), (Token::PreprocessorElIf,));
    dict.insert("#endif".chars(), (Token::PreprocessorEndIf,));
    dict.insert("#else".chars(), (Token::PreprocessorElse,));
    dict.insert("#if".chars(), (Token::PreprocessorIf,));
    dict.insert("#include".chars(), (Token::PreprocessorInclude,));
    dict.insert("\n".chars(), (Token::NewLine,));

    dict
}

pub fn ident_to_keyword_map() -> HashMap<String, Token> {
    let mut dict = HashMap::new();

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
