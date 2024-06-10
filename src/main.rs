use std::io::{stdin, stdout, Read, Write};

use program::program::Program;

mod ast;
mod program;
mod token;

use ast::statement::Statement;

fn main() {
    println!("Enter your code:");
    stdout().flush().expect("Failed to flush stdout");

    let mut source: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut source)
        .expect("Failed to read from stdin");

    let source = String::from_utf8(source).expect("Invalid UTF-8");

    println!("============================ Tokenizing ============================");

    let tokens = token::tokenize::tokenize(source);
    println!("Tokens: ");

    for token in tokens.iter() {
        println!("{:?}", *token);
    }

    println!("============================ Building AST ============================");

    println!("ASTs: ");
    let parser = ast::parser::ASTParser::new();
    let translation_unit = parser.parse(tokens);
    println!("{:?}", translation_unit);

    println!("============================ Generating Instructions ============================");

    let mut program: Program = Program::new();
    let mut instructions: Vec<_> = Vec::new();
    translation_unit.emit(&mut program, &mut instructions);

    println!("Instructions: ");
    for instruction in instructions.iter() {
        println!("{:?}", instruction);
    }

    println!("============================ Executing Instructions ============================");
    program.execute(&mut instructions);

    stdout().flush().expect("Failed to flush stdout");
}
