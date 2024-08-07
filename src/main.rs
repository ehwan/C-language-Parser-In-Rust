use std::io::{stdin, stdout, Read, Write};

use virtualmachine::instruction::generation::InstructionGenerator;
use virtualmachine::program::VirtualProgram;

mod ast;
mod preprocess;
mod token;
mod virtualmachine;

use ast::statement::Statement;

fn main() {
    println!("Enter your code (and ^D for EOF):");
    stdout().flush().expect("Failed to flush stdout");

    // Read from stdin
    let mut source: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut source)
        .expect("Failed to read from stdin");

    let source = String::from_utf8(source).expect("Invalid UTF-8");
    let preprocessor = preprocess::parser::PreprocessorParser::new();

    // tokenize
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase1: Tokenizing");
    println!("{:=^80}", "");
    println!("LINE | {:-^73}", "Result");
    let tokens = preprocessor.tokenize(&source);
    let mut linestart = true;
    let mut lineid = 0;
    for token in tokens.iter() {
        if linestart {
            print!("{:4}: ", lineid);
        }
        if token == &token::Token::NewLine {
            println!();
            linestart = true;
            lineid += 1;
        } else {
            print!("{:?} ", token);
            linestart = false;
        }
    }

    // line analysis
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase2: Line Analysis");
    println!("{:=^80}", "");
    println!("LINE | {:-^73}", "Result");
    let lines = preprocessor.parse_lines(&tokens);
    for (lineid, line) in lines.iter().enumerate() {
        println!("{:4}: {:?}", lineid, line);
    }

    // preprocess
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase3: Preprocessing");
    println!("{:=^80}", "");
    println!("LINE | {:-^73}", "Result");
    let line_tokenized = preprocessor.preprocess(&lines);

    for (lineid, line) in line_tokenized.iter().enumerate() {
        println!("{:4}: {:?}", lineid, line);
    }

    // flatten the line_tokenized
    let tokens: Vec<token::Token> = line_tokenized.into_iter().flatten().collect();

    println!("{:=^80}", "");
    println!("{:=^80}", "Phase4: Building AbstractSyntaxTree");
    println!("{:=^80}", "");

    // parse the tokens into AST
    println!("ASTs: ");
    let parser = ast::parser::ASTParser::new();
    let translation_unit = parser.parse(tokens);
    println!("{:#?}", translation_unit);

    // generate instructions
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase5: Generating Instructions");
    println!("{:=^80}", "");
    println!("ADDR | {:-^73}", "Result");

    let mut instructions: InstructionGenerator = InstructionGenerator::new();
    translation_unit.emit(&mut instructions);

    println!("Instructions: ");
    for (id, instruction) in instructions.instructions.iter().enumerate() {
        if id == instructions.start_address {
            println!("{: ^2}{:-^78}", "", "Start Address");
        }
        println!("{:4}: {:?}", id, instruction);
    }

    // execute instructions
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase6: Executing Instructions");
    println!("{:=^80}", "");
    let mut program: VirtualProgram = VirtualProgram::new();
    program.execute(&mut instructions);

    stdout().flush().expect("Failed to flush stdout");
}
