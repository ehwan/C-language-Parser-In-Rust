use std::io::{stdin, stdout, Read, Write};

mod ast;
mod program;
mod token;

fn main() {
    println!("Enter your code:");
    stdout().flush().expect("Failed to flush stdout");

    let mut source: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut source)
        .expect("Failed to read from stdin");

    let source = String::from_utf8(source).expect("Invalid UTF-8");

    println!("Tokenizing...");
    let tokens = token::tokenize::tokenize(source);
    println!("Tokens: ");

    for token in tokens.iter() {
        println!("{:?}", *token);
    }

    println!("-----------------------------------");
    println!("Parsing...");

    println!("ASTs: ");
    let parser = ast::parser::ASTParser::new();
    let ast = parser.parse(tokens);
    println!("{:?}", ast);
}
