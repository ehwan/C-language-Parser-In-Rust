use super::token::DynParser;
use super::token::Token;
use rusty_parser as rp;

use rp::IntoParser;

pub fn identifier_parser() -> DynParser {
    let digit = ('0'..='9').void();
    let alpha = rp::or!('a'..='z', 'A'..='Z').void();
    let identifier = rp::seq!(
        rp::or!(alpha, '_'.void()),
        rp::or!(alpha, digit, '_'.void()).repeat(0..)
    )
    .string();

    DynParser::new(identifier.map(|s: String| Token::Identifier(s)))
}
