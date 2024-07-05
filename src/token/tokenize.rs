use super::character;
use super::identifier;
use super::numeric;
use super::string;
use super::token::Token;
use super::trie;
use rusty_parser as rp;

use rp::IntoParser;

/// Tokenize the raw source code into a list of tokens
/// This phase will remove c/cpp comments and whitespace
/// the newline '\n' will be kept as a token Token::NewLine for later phase
pub fn tokenize(source: &str) -> Vec<Token> {
    let cpp_comment = rp::seq!(
        "//",
        rp::any().not('\n').repeat(0..),
        rp::or!(rp::end(), '\n'.void()).not_consume()
    )
    .void(); // c++ comment
    let c_comment = rp::seq!("/*", rp::any().not("*/").repeat(0..), "*/").void();
    // c comment

    let whitespace = rp::or!(' '.void(), '\t'.void(), '\r'.void(), "\\\n".void());

    let ignore_all = rp::or!(cpp_comment, c_comment, whitespace).repeat(0..);

    let char_literal = character::char_literal();
    let string_literal = string::string_literal();
    let identifier = identifier::identifier();
    let integer_numeric = numeric::integer_numeric();
    let float_numeric = numeric::float_numeric();
    let trie = trie::build_trie();

    // parse keyword as identifier first
    let one_token = rp::or!(
        trie,
        identifier,
        string_literal,
        char_literal,
        float_numeric,
        integer_numeric
    );

    let multiple_tokens = rp::seq!(ignore_all, rp::seq!(one_token, ignore_all).repeat(0..));

    let file_parser = rp::seq!(multiple_tokens, rp::end());
    let res = rp::parse(&file_parser, source.chars());
    res.output.expect("Failed to Tokenize").0

    /*





    // flatten into one lines
    let tokens: Vec<_> = token_lines.iter().flatten().cloned().collect();

    // ignoring unsupported tokens
    tokens
        .iter()
        .cloned()
        .filter(move |t| {
            *t != Token::Static
                && *t != Token::Volatile
                && *t != Token::Auto
                && *t != Token::Register
                && *t != Token::Extern
        })
        .collect()
        */
}
