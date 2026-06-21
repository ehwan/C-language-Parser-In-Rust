#![allow(non_camel_case_types)]

use crate::token::Token;

fn materialize_parenthesized_argument_list(mut arguments: Vec<Vec<Token>>) -> Vec<Token> {
    let mut tokens = Vec::new();
    tokens.push(Token::LeftParen);
    for (argument_index, argument) in arguments.iter_mut().enumerate() {
        if argument_index > 0 {
            tokens.push(Token::Comma);
        }
        tokens.append(argument);
    }
    tokens.push(Token::RightParen);
    tokens
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

%start MacroInvocationTail;

MacroInvocationTail((Vec<Vec<Token>>, Vec<Token>))
    : lparen! rparen! tail=TokenSequence {
        (Vec::new(), tail)
    }
    | lparen! arguments=NonEmptyMacroArgumentList rparen! tail=TokenSequence {
        (arguments, tail)
    }
    ;

NonEmptyMacroArgumentList(Vec<Vec<Token>>)
    : argument=NonEmptyMacroArgument rest=(comma! MacroArgument)* {
        let mut arguments = vec![argument];
        arguments.extend(rest);
        arguments
    }
    | comma! rest=MacroArgumentListAfterLeadingComma {
        let mut arguments = vec![Vec::new()];
        arguments.extend(rest);
        arguments
    }
    ;

MacroArgumentListAfterLeadingComma(Vec<Vec<Token>>)
    : argument=MacroArgument rest=(comma! MacroArgument)* {
        let mut arguments = vec![argument];
        arguments.extend(rest);
        arguments
    }
    ;

MacroArgument(Vec<Token>)
    : lexemes=MacroArgumentLexeme* {
        lexemes.into_iter().flatten().collect()
    }
    ;

NonEmptyMacroArgument(Vec<Token>)
    : lexemes=MacroArgumentLexeme+ {
        lexemes.into_iter().flatten().collect()
    }
    ;

MacroArgumentLexeme(Vec<Token>)
    : lparen! rparen! {
        vec![Token::LeftParen, Token::RightParen]
    }
    | lparen! arguments=NonEmptyMacroArgumentList rparen! {
        materialize_parenthesized_argument_list(arguments)
    }
    | token=[^comma lparen rparen] {
        vec![token]
    }
    ;

TokenSequence(Vec<Token>)
    : tokens=LexicalToken* {
        tokens
    }
    ;

LexicalToken(Token)
    : token=. {
        token
    }
    ;
