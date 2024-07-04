use std::io::{stdin, stdout, Read, Write};

use virtualmachine::instruction::generation::InstructionGenerator;
use virtualmachine::program::VirtualProgram;

mod ast;
mod token;
mod virtualmachine;

use ast::statement::Statement;

fn main() {
    println!("Enter your code (and ^D for EOF):");
    stdout().flush().expect("Failed to flush stdout");

    let mut source: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut source)
        .expect("Failed to read from stdin");

    let source = String::from_utf8(source).expect("Invalid UTF-8");

    println!("{:=^80}", "Tokenizing");

    let tokens = token::tokenize::tokenize(&source);
    let tokens = token::preprocessor::preprocess_phase1(&tokens);

    println!("Tokens: ");

    for (id, token) in tokens.iter().enumerate() {
        println!("{:4}: {:?}", id, *token);
    }

    println!("{:=^80}", "Building AbstractSyntaxTree");

    println!("ASTs: ");
    let parser = ast::parser::ASTParser::new();
    let translation_unit = parser.parse(tokens);
    println!("{:#?}", translation_unit);

    println!("{:=^80}", "Generating Instructions");

    let mut program: VirtualProgram = VirtualProgram::new();
    let mut instructions: InstructionGenerator = InstructionGenerator::new();
    translation_unit.emit(&mut instructions);

    println!("Instructions: ");
    for (id, instruction) in instructions.instructions.iter().enumerate() {
        if id == instructions.start_address {
            println!("      -------------------- Start Address ---------------------");
        }
        println!("{:4}: {:?}", id, instruction);
    }

    println!("============================ Executing Instructions ============================");
    program.execute(&mut instructions);

    stdout().flush().expect("Failed to flush stdout");
}
