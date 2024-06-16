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

    let char_literal = character::char_literal();
    let string_literal = string::string_literal();
    let identifier = identifier::identifier();
    let integer_numeric = numeric::integer_numeric();
    let float_numeric = numeric::float_numeric();
    let trie = trie::build_trie();

    // parse keyword as identifier first
    let one_token = rp::seq!(rp::or!(
        trie,
        identifier,
        string_literal,
        char_literal,
        float_numeric,
        integer_numeric
    ));

    let multiple_tokens = rp::seq!(ignore_all, rp::seq!(one_token, ignore_all).repeat(0..));

    let file = rp::seq!(multiple_tokens, rp::end());

    let res = rp::parse(&file, source.chars());
    if res.output.is_none() {
        panic!("Failed to Tokenize");
    }

    let mut tokens = res.output.unwrap().0;
    let keyword_trie = rp::seq!(trie::build_keyword_trie(), rp::end());

    for token in &mut tokens {
        if let Token::Identifier(ref mut s) = token {
            let keyword_matched = rp::parse(&keyword_trie, s.chars());
            // if it was keyword, change it
            if let Some((t,)) = keyword_matched.output {
                *token = t;
            }
        }
    }

    // tokens

    // ignoring unsupported tokens
    tokens
        .iter()
        .cloned()
        .filter(move |t| {
            *t != Token::Static
                && *t != Token::Volatile
                && *t != Token::Auto
                && *t != Token::Register
        })
        .collect()
}
