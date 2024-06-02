use super::character;
use super::identifier;
use super::numeric;
use super::string;
use super::token::DynParser;
use super::token::Token;
use super::trie;
use rusty_parser as rp;

use rp::IntoParser;

pub fn tokenize(source: String) -> Vec<Token> {
    let cpp_comment = rp::seq!("//", rp::any().not('\n').repeat(0..), '\n'.not_consume()).void(); // c++ comment
    let c_comment = rp::seq!("/*", rp::any().not("*/").repeat(0..), "*/").void();
    // c comment

    let whitespace = rp::or!(' ', '\t', '\n', '\r').void();
    let whitespaces0 = whitespace.repeat(0..);
    let whitespaces1 = whitespace.repeat(1..);

    let trie = trie::build_trie();
    let char_literal = character::char_literal();
    let string_literal = string::string_literal();
    let identifier = identifier::identifier();
    let integer_numeric = numeric::integer_numeric();
    let float_numeric = numeric::float_numeric();

    let one_token = rp::seq!(
        rp::or!(cpp_comment, c_comment).optional().void(),
        whitespaces0,
        rp::or!(
            string_literal,
            char_literal,
            float_numeric,
            integer_numeric,
            identifier,
            trie
        )
    );

    let multiple_tokens = rp::seq!(whitespaces0, rp::seq!(one_token, whitespaces0).repeat(0..));

    let file = rp::seq!(multiple_tokens, rp::end());

    let res = rp::parse(&file, source.chars());
    res.output.expect("Failed to Tokenize").0
}
