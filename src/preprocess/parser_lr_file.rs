#![allow(non_camel_case_types)]

use super::preprocessor::*;
use crate::token::Token;

fn discard_layout_tokens(tokens: Vec<Token>) -> Vec<Token> {
    tokens
        .into_iter()
        .filter(|token| token != &Token::Whitespace)
        .collect()
}

fn build_function_like_macro(
    name: String,
    parameter_tokens: Vec<Token>,
    mut replacement: Vec<Token>,
) -> Box<dyn PreprocessedTokenLine> {
    let parameters = parse_formal_parameter_list(parameter_tokens);
    for (parameter_index, parameter_name) in parameters.iter().enumerate() {
        for token in replacement.iter_mut() {
            if let Token::Identifier(identifier) = token {
                if identifier == parameter_name {
                    *token = Token::PreprocessorPlaceholder(parameter_index);
                }
            }
        }
    }

    Box::new(DefineFunction {
        name,
        param_count: parameters.len(),
        replacement,
    })
}

fn parse_formal_parameter_list(parameter_tokens: Vec<Token>) -> Vec<String> {
    let parameter_tokens = discard_layout_tokens(parameter_tokens);
    if parameter_tokens.is_empty() {
        return Vec::new();
    }

    let mut parameters = Vec::new();
    let mut expect_parameter = true;

    for token in parameter_tokens {
        match token {
            Token::Identifier(name) if expect_parameter => {
                parameters.push(name);
                expect_parameter = false;
            }
            Token::Comma if !expect_parameter => {
                expect_parameter = true;
            }
            _ => {
                panic!("Invalid macro parameters; must be comma-separated identifiers");
            }
        }
    }

    if expect_parameter {
        panic!("Invalid macro parameters; must be comma-separated identifiers");
    }

    parameters
}

%%

%lalr;
%tokentype Token;

%token other Token::Others(_);
%token ident Token::Identifier(_);
%token constant_character Token::ConstantCharacter(_);
%token constant_integer Token::ConstantInteger(_);
%token constant_long Token::ConstantLong(_);
%token constant_unsigned_integer Token::ConstantUnsignedInteger(_);
%token constant_unsigned_long Token::ConstantUnsignedLong(_);
%token constant_float Token::ConstantFloat(_);
%token constant_double Token::ConstantDouble(_);
%token string_literal Token::StringLiteral(_);
%token pp_define Token::PreprocessorDefine;
%token pp_ifdef Token::PreprocessorIfDef;
%token pp_ifndef Token::PreprocessorIfNDef;
%token pp_endif Token::PreprocessorEndIf;
%token pp_elif Token::PreprocessorElIf;
%token pp_undef Token::PreprocessorUndef;
%token pp_else Token::PreprocessorElse;
%token pp_placeholder Token::PreprocessorPlaceholder(_);
%token pp_if Token::PreprocessorIf;
%token pp_include Token::PreprocessorInclude;
%token newline Token::NewLine;
%token auto Token::Auto;
%token break_ Token::Break;
%token case Token::Case;
%token char_ Token::Char;
%token const_ Token::Const;
%token continue_ Token::Continue;
%token default Token::Default;
%token do_ Token::Do;
%token double_ Token::Double;
%token else_ Token::Else;
%token enum_ Token::Enum;
%token extern_ Token::Extern;
%token float_ Token::Float;
%token for_ Token::For;
%token goto_ Token::Goto;
%token if_ Token::If;
%token int_ Token::Int;
%token long_ Token::Long;
%token register Token::Register;
%token return_ Token::Return;
%token short_ Token::Short;
%token signed Token::Signed;
%token sizeof Token::Sizeof;
%token static_ Token::Static;
%token struct_ Token::Struct;
%token switch Token::Switch;
%token typedef Token::Typedef;
%token union_ Token::Union;
%token unsigned Token::Unsigned;
%token void_ Token::Void;
%token volatile Token::Volatile;
%token while_ Token::While;
%token ellipsis Token::Ellipsis;
%token right_assign Token::RightAssign;
%token left_assign Token::LeftAssign;
%token add_assign Token::AddAssign;
%token sub_assign Token::SubAssign;
%token mul_assign Token::MulAssign;
%token div_assign Token::DivAssign;
%token mod_assign Token::ModAssign;
%token and_assign Token::AndAssign;
%token xor_assign Token::XorAssign;
%token or_assign Token::OrAssign;
%token right_op Token::RightOp;
%token left_op Token::LeftOp;
%token inc_op Token::IncOp;
%token dec_op Token::DecOp;
%token ptr_op Token::PtrOp;
%token and_op Token::AndOp;
%token or_op Token::OrOp;
%token le Token::LeOp;
%token ge Token::GeOp;
%token eq Token::EqOp;
%token ne Token::NeOp;
%token semicolon Token::SemiColon;
%token lbrace Token::LeftBrace;
%token rbrace Token::RightBrace;
%token comma Token::Comma;
%token colon Token::Colon;
%token assign Token::Equal;
%token lparen Token::LeftParen;
%token rparen Token::RightParen;
%token lbracket Token::LeftBracket;
%token rbracket Token::RightBracket;
%token dot Token::Dot;
%token ampersand Token::Ampersand;
%token exclamation Token::Exclamation;
%token tilde Token::Tilde;
%token minus Token::Minus;
%token plus Token::Plus;
%token star Token::Star;
%token slash Token::Slash;
%token percent Token::Percent;
%token less Token::LessThan;
%token greater Token::GreaterThan;
%token caret Token::Caret;
%token pipe Token::Pipe;
%token question Token::Question;
%token whitespace Token::Whitespace;

%start PreprocessingFile;

PreprocessingFile(Vec<Box<dyn PreprocessedTokenLine>>)
    : lines=LogicalLine* {
        lines
            .into_iter()
            .filter(|line| line.is_empty() == false)
            .collect()
    }
    ;

LogicalLine(Box<dyn PreprocessedTokenLine>)
    : DirectiveLine
    | whitespace! DirectiveLine
    | RawLine
    ;

DirectiveLine(Box<dyn PreprocessedTokenLine>)
    : DefineLine
    | UndefLine
    | IfDefLine
    | IfNDefLine
    | IfLine
    | ElIfLine
    | ElseLine
    | EndIfLine
    ;

DefineLine(Box<dyn PreprocessedTokenLine>)
    : pp_define! whitespace! name=Identifier lparen! parameter_tokens=[^rparen]* rparen! replacement=ReplacementList {
        build_function_like_macro(name, parameter_tokens, replacement)
    }
    | pp_define! whitespace! name=Identifier replacement=ReplacementList {
        Box::new(Define { name, replacement })
    }
    ;

UndefLine(Box<dyn PreprocessedTokenLine>)
    : pp_undef! whitespace! name=Identifier newline! {
        Box::new(Undef { name })
    }
    | pp_undef! whitespace! name=Identifier whitespace! newline! {
        Box::new(Undef { name })
    }
    ;

IfDefLine(Box<dyn PreprocessedTokenLine>)
    : pp_ifdef! whitespace! name=Identifier newline! {
        Box::new(IfDef { name })
    }
    | pp_ifdef! whitespace! name=Identifier whitespace! newline! {
        Box::new(IfDef { name })
    }
    ;

IfNDefLine(Box<dyn PreprocessedTokenLine>)
    : pp_ifndef! whitespace! name=Identifier newline! {
        Box::new(IfNDef { name })
    }
    | pp_ifndef! whitespace! name=Identifier whitespace! newline! {
        Box::new(IfNDef { name })
    }
    ;

IfLine(Box<dyn PreprocessedTokenLine>)
    : pp_if! whitespace! expression_tokens=ReplacementTokens newline! {
        Box::new(If { expression_tokens })
    }
    ;

ElIfLine(Box<dyn PreprocessedTokenLine>)
    : pp_elif! whitespace! expression_tokens=ReplacementTokens newline! {
        Box::new(ElIf { expression_tokens })
    }
    ;

ElseLine(Box<dyn PreprocessedTokenLine>)
    : pp_else! newline! {
        Box::new(Else {})
    }
    | pp_else! whitespace! newline! {
        Box::new(Else {})
    }
    ;

EndIfLine(Box<dyn PreprocessedTokenLine>)
    : pp_endif! newline! {
        Box::new(EndIf {})
    }
    | pp_endif! whitespace! newline! {
        Box::new(EndIf {})
    }
    ;

RawLine(Box<dyn PreprocessedTokenLine>)
    : newline! {
        Box::new(RawTokens { tokens: Vec::new() })
    }
    | whitespace! newline! {
        Box::new(RawTokens { tokens: Vec::new() })
    }
    | first=[^newline whitespace pp_define pp_undef pp_ifdef pp_ifndef pp_if pp_elif pp_else pp_endif] rest=ReplacementTokens newline! {
        let mut tokens = Vec::new();
        tokens.push(first);
        tokens.append(&mut rest);
        Box::new(RawTokens {
            tokens: discard_layout_tokens(tokens),
        })
    }
    | whitespace! first=[^newline pp_define pp_undef pp_ifdef pp_ifndef pp_if pp_elif pp_else pp_endif] rest=ReplacementTokens newline! {
        let mut tokens = Vec::new();
        tokens.push(first);
        tokens.append(&mut rest);
        Box::new(RawTokens {
            tokens: discard_layout_tokens(tokens),
        })
    }
    ;

ReplacementList(Vec<Token>)
    : whitespace! replacement_tokens=ReplacementTokens newline! {
        discard_layout_tokens(replacement_tokens)
    }
    | newline! {
        Vec::new()
    }
    ;

ReplacementTokens(Vec<Token>)
    : tokens=[^newline]* {
        tokens
    }
    ;

Identifier(String)
    : ident {
        if let Token::Identifier(name) = ident {
            name
        } else {
            unreachable!()
        }
    }
    ;
