#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Other(char), // Other single character
    Identifier(String),
    ConstantInteger(u64),
    ConstantFloat(f64),
    ConstantCharacter(char),
    StringLiteral(String),
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Int,
    Long,
    Register,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    Ellipsis, // ...
    RightAssign,
    LeftAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    XorAssign,
    OrAssign,
    RightOp,
    LeftOp,
    IncOp,
    DecOp,
    PtrOp,
    AndOp,
    OrOp,
    LeOp,
    GeOp,
    EqOp,
    NeOp,
}
use rusty_parser as rp;

use rp::IntoParser;
pub type DynParser = rp::DynBoxChars<(Token,)>;

use std::char;
