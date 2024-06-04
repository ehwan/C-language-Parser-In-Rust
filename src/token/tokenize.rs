use super::character;
use super::identifier;
use super::numeric;
use super::string;
use super::token::Token;
use super::trie;
use rusty_parser as rp;

use rp::IntoParser;

pub fn tokenize(source: String) -> Vec<Token> {
    let cpp_comment = rp::seq!("//", rp::any().not('\n').repeat(0..), '\n'.not_consume()).void(); // c++ comment
    let c_comment = rp::seq!("/*", rp::any().not("*/").repeat(0..), "*/").void();
    // c comment

    let whitespace = rp::or!(' ', '\t', '\n', '\r').void();

    let ignore_all = rp::or!(cpp_comment, c_comment, whitespace).repeat(0..);

    let trie = trie::build_trie();
    let char_literal = character::char_literal();
    let string_literal = string::string_literal();
    let identifier = identifier::identifier();
    let integer_numeric = numeric::integer_numeric();
    let float_numeric = numeric::float_numeric();

    let one_token = rp::seq!(rp::or!(
        string_literal,
        char_literal,
        float_numeric,
        integer_numeric,
        trie,
        identifier
    ));

    let multiple_tokens = rp::seq!(ignore_all, rp::seq!(one_token, ignore_all).repeat(0..));

    let file = rp::seq!(multiple_tokens, rp::end());

    let res = rp::parse(&file, source.chars());
    res.output.expect("Failed to Tokenize").0
}
