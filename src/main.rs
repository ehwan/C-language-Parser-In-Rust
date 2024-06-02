use rusty_parser as rp;
use std::io::{stdin, Read};

mod ast;
mod token;

fn main() {
    let mut source: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut source)
        .expect("Failed to read from stdin");

    let source = String::from_utf8(source).expect("Invalid UTF-8");

    let tokens = token::tokenize::tokenize(source);

    for token in tokens {
        println!("{:?}", token);
    }
}
