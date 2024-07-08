use super::character;
use super::identifier;
use super::numeric;
use super::string;
use super::token::Token;
use super::trie;
use rusty_parser as rp;

use rp::IntoParser;

/// Tokenize the raw source code into a list of tokens
/// This phase will remove c/cpp comments
/// sequence of whitespaces will be combined into one Token::Whitespace
/// the newline '\n' will be kept as a Token::NewLine for later phase
/// If the source is not end with '\n', it will be added automatically
pub fn tokenize(source: &str) -> Vec<Token> {
    // c++ comment
    let cpp_comment = rp::seq!(
        "//",
        rp::any().not('\n').repeat(0..),
        rp::or!(rp::end(), '\n'.void()).not_consume()
    )
    .void();

    // c comment
    let c_comment = rp::seq!("/*", rp::any().not("*/").repeat(0..), "*/").void();

    let ignore_all = rp::or!(cpp_comment, c_comment).void().repeat(0..);
    let whitespace = rp::or!(' '.void(), '\t'.void(), '\r'.void(), "\\\n".void());

    let char_literal = character::char_literal_parser();
    let string_literal = string::string_literal_parser();
    let identifier = identifier::identifier_parser();
    let integer_numeric = numeric::integer_numeric_parser();
    let float_numeric = numeric::float_numeric_parser();
    let trie = trie::keyword_parser();

    // parse keyword as identifier first
    let one_token = rp::or!(
        trie,
        identifier,
        string_literal,
        char_literal,
        float_numeric,
        integer_numeric,
        whitespace.repeat(1..).void().map(|| Token::Whitespace),
        rp::any().map(|c| Token::Others(c))
    );

    let multiple_tokens = rp::seq!(ignore_all, rp::seq!(one_token, ignore_all).repeat(0..));

    let file_parser = rp::seq!(multiple_tokens, rp::end());
    let res = rp::parse(&file_parser, source.chars());
    let mut res = res.output.expect("Failed to Tokenize").0;
    if res.last() != Some(&Token::NewLine) {
        res.push(Token::NewLine);
    }

    res
}
