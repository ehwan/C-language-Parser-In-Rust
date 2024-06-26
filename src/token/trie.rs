use super::token::Token;
use rusty_parser as rp;

pub fn build_trie() -> rp::DictHashMap<(Token,), char> {
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

    dict
}
pub fn build_keyword_trie() -> rp::DictHashMap<(Token,), char> {
    let mut dict = rp::DictHashMap::new();

    dict.insert("auto".chars(), (Token::Auto,));
    dict.insert("break".chars(), (Token::Break,));
    dict.insert("case".chars(), (Token::Case,));
    dict.insert("char".chars(), (Token::Char,));
    dict.insert("const".chars(), (Token::Const,));
    dict.insert("continue".chars(), (Token::Continue,));
    dict.insert("default".chars(), (Token::Default,));
    dict.insert("do".chars(), (Token::Do,));
    dict.insert("double".chars(), (Token::Double,));
    dict.insert("else".chars(), (Token::Else,));
    dict.insert("enum".chars(), (Token::Enum,));
    dict.insert("extern".chars(), (Token::Extern,));
    dict.insert("float".chars(), (Token::Float,));
    dict.insert("for".chars(), (Token::For,));
    dict.insert("goto".chars(), (Token::Goto,));
    dict.insert("if".chars(), (Token::If,));
    dict.insert("int".chars(), (Token::Int,));
    dict.insert("long".chars(), (Token::Long,));
    dict.insert("register".chars(), (Token::Register,));
    dict.insert("return".chars(), (Token::Return,));
    dict.insert("short".chars(), (Token::Short,));
    dict.insert("signed".chars(), (Token::Signed,));
    dict.insert("sizeof".chars(), (Token::Sizeof,));
    dict.insert("static".chars(), (Token::Static,));
    dict.insert("struct".chars(), (Token::Struct,));
    dict.insert("switch".chars(), (Token::Switch,));
    dict.insert("typedef".chars(), (Token::Typedef,));
    dict.insert("union".chars(), (Token::Union,));
    dict.insert("unsigned".chars(), (Token::Unsigned,));
    dict.insert("void".chars(), (Token::Void,));
    dict.insert("volatile".chars(), (Token::Volatile,));
    dict.insert("while".chars(), (Token::While,));

    dict
}
