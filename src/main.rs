use std::io::{stdin, stdout, Read, Write};

mod ast;
mod llvm;
mod preprocess;
mod semantic;
mod token;

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

    // let mut context = inkwell::context::Context::create();
    // let mut module = context.create_module("m1");
    // let mut builder = context.create_builder();

    // parse the tokens into AST
    println!("ASTs: ");
    let parser = ast::translation_unitParser::new();
    let mut context = ast::translation_unitContext::new();
    for token in tokens.into_iter() {
        println!("=====");
        if !context.can_feed(&parser, &token) {
            println!("Error: Unexpected token: {:?}", token);
            println!("Backtrace: {:?}", context.backtrace(&parser));
            println!("State: {}", context.state());
            break;
        }
        match context.feed(&parser, token, &mut ()) {
            Ok(_) => {}
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    let ast = match context.accept(&parser, &mut ()) {
        Ok(tu) => tu,
        Err(err) => {
            println!("Error: {:?}", err);
            return;
        }
    };
    println!("{:#?}", ast);

    println!("{:=^80}", "");
    println!("{:=^80}", "Phase5: Semantic Analysis");
    println!("{:=^80}", "");

    let mut context = semantic::Context::new();
    let ast = match context.process(ast) {
        Ok(ast) => ast,
        Err(err) => {
            println!("Error: {:?}", err);
            return;
        }
    };
    println!("{:#?}", ast);

    // generate instructions
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase6: Generating Instructions");
    println!("{:=^80}", "");

    let mut context = llvm::Context::new();
    context.compile(ast);

    // let mut context = virtualmachine::InstructionGenerator::new();
    // let vm = match context.emit(translation_unit) {
    //     Ok(vm) => vm,
    //     Err(err) => {
    //         println!("Error: {:?}", err);
    //         return;
    //     }
    // };

    // println!("ADDR | {:-^73}", "Result");

    // for (id, instruction) in vm.instructions.iter().enumerate() {
    //     println!("{:4}: {:?}", id, instruction);
    // }

    /*
    // execute instructions
    println!("{:=^80}", "");
    println!("{:=^80}", "Phase6: Executing Instructions");
    println!("{:=^80}", "");
    let mut vm: VirtualMachine = VirtualMachine::new();
    vm.execute(&mut instructions);

    stdout().flush().expect("Failed to flush stdout");
    */
}
