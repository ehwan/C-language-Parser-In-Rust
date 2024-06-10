use super::token::DynParser;
use super::token::Token;
use rusty_parser as rp;

use rp::IntoParser;

pub fn char_literal() -> DynParser {
    // char literal enclosed with ' '
    let escape = rp::or!(
        'n'.output('\n'),
        't'.output('\t'),
        '\\'.output('\\'),
        '\''.output('\''),
        '"'.output('"')
    );
    let escape = rp::seq!('\\'.void(), escape);
    let char_literal = rp::seq!(
        '\''.void(),
        rp::or!(escape, rp::any().not('\'')),
        '\''.void()
    );
    DynParser::new(char_literal.map(|c: char| Token::ConstantCharacter((c as u8) as i8)))
}
