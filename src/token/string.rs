use super::token::DynParser;
use super::token::Token;
use rusty_parser as rp;

use rp::IntoParser;

pub fn string_literal() -> DynParser {
    // char literal enclosed with " "
    // this is a bit different from the char literal
    let escape = rp::or!(
        'n'.output('\n'),
        't'.output('\t'),
        '\\'.output('\\'),
        '\''.output('\''),
        '"'.output('"')
    );
    let escape = rp::seq!('\\'.void(), escape);
    let one_char = rp::or!(escape, rp::any().not('"'));

    let string_literal = rp::seq!(
        '"'.void(),
        one_char
            .repeat(0..)
            .map(|v: Vec<char>| v.into_iter().collect::<String>()),
        '"'.void()
    );

    string_literal
        .map(|s: String| Token::StringLiteral(s))
        .box_chars()
}
