#![allow(non_camel_case_types)]

use super::expression::*;
use crate::token::Token;

fn reject_floating_constant() -> Box<dyn PreprocessorExpression> {
    panic!("Float constant is not supported in preprocessing stage");
}

%%

%lalr;
%tokentype Token;

%token ident Token::Identifier(_);
%token constant_character Token::ConstantCharacter(_);
%token constant_integer Token::ConstantInteger(_);
%token constant_long Token::ConstantLong(_);
%token constant_unsigned_integer Token::ConstantUnsignedInteger(_);
%token constant_unsigned_long Token::ConstantUnsignedLong(_);
%token constant_float Token::ConstantFloat(_);
%token constant_double Token::ConstantDouble(_);
%token lparen Token::LeftParen;
%token rparen Token::RightParen;
%token question Token::Question;
%token colon Token::Colon;
%token plus Token::Plus;
%token minus Token::Minus;
%token tilde Token::Tilde;
%token exclamation Token::Exclamation;
%token star Token::Star;
%token slash Token::Slash;
%token percent Token::Percent;
%token left_op Token::LeftOp;
%token right_op Token::RightOp;
%token less Token::LessThan;
%token greater Token::GreaterThan;
%token le Token::LeOp;
%token ge Token::GeOp;
%token eq Token::EqOp;
%token ne Token::NeOp;
%token ampersand Token::Ampersand;
%token caret Token::Caret;
%token pipe Token::Pipe;
%token and_op Token::AndOp;
%token or_op Token::OrOp;

%start PreprocessingExpression;

PreprocessingExpression(Box<dyn PreprocessorExpression>)
    : conditional_expression
    ;

primary_expression(Box<dyn PreprocessorExpression>)
    : constant_character {
        if let Token::ConstantCharacter(value) = constant_character {
            Box::new(Constant { value: value as i64 })
        } else {
            unreachable!()
        }
    }
    | constant_integer {
        if let Token::ConstantInteger(value) = constant_integer {
            Box::new(Constant { value: value as i64 })
        } else {
            unreachable!()
        }
    }
    | constant_long {
        if let Token::ConstantLong(value) = constant_long {
            Box::new(Constant { value })
        } else {
            unreachable!()
        }
    }
    | constant_unsigned_integer {
        if let Token::ConstantUnsignedInteger(value) = constant_unsigned_integer {
            Box::new(Constant { value: value as i64 })
        } else {
            unreachable!()
        }
    }
    | constant_unsigned_long {
        if let Token::ConstantUnsignedLong(value) = constant_unsigned_long {
            Box::new(Constant { value: value as i64 })
        } else {
            unreachable!()
        }
    }
    | constant_float {
        let _ = constant_float;
        reject_floating_constant()
    }
    | constant_double {
        let _ = constant_double;
        reject_floating_constant()
    }
    | ident lparen! macro_name=ident rparen! {
        if let Token::Identifier(operator_name) = ident {
            if operator_name != "defined" {
                panic!("Only defined(NAME) is supported as an identifier expression in preprocessing");
            }
        } else {
            unreachable!()
        }

        if let Token::Identifier(name) = macro_name {
            Box::new(Defined { name })
        } else {
            unreachable!()
        }
    }
    | lparen! PreprocessingExpression rparen!
    ;

unary_expression(Box<dyn PreprocessorExpression>)
    : primary_expression
    | plus! src=unary_expression {
        Box::new(UnaryExpression {
            op: UnaryOperator::Plus,
            src,
        })
    }
    | minus! src=unary_expression {
        Box::new(UnaryExpression {
            op: UnaryOperator::Minus,
            src,
        })
    }
    | tilde! src=unary_expression {
        Box::new(UnaryExpression {
            op: UnaryOperator::BitwiseNot,
            src,
        })
    }
    | exclamation! src=unary_expression {
        Box::new(UnaryExpression {
            op: UnaryOperator::LogicalNot,
            src,
        })
    }
    ;

multiplicative_expression(Box<dyn PreprocessorExpression>)
    : unary_expression
    | lhs=multiplicative_expression star! rhs=unary_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Mul,
            lhs,
            rhs,
        })
    }
    | lhs=multiplicative_expression slash! rhs=unary_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Div,
            lhs,
            rhs,
        })
    }
    | lhs=multiplicative_expression percent! rhs=unary_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Mod,
            lhs,
            rhs,
        })
    }
    ;

additive_expression(Box<dyn PreprocessorExpression>)
    : multiplicative_expression
    | lhs=additive_expression plus! rhs=multiplicative_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Add,
            lhs,
            rhs,
        })
    }
    | lhs=additive_expression minus! rhs=multiplicative_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Sub,
            lhs,
            rhs,
        })
    }
    ;

shift_expression(Box<dyn PreprocessorExpression>)
    : additive_expression
    | lhs=shift_expression left_op! rhs=additive_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::ShiftLeft,
            lhs,
            rhs,
        })
    }
    | lhs=shift_expression right_op! rhs=additive_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::ShiftRight,
            lhs,
            rhs,
        })
    }
    ;

relational_expression(Box<dyn PreprocessorExpression>)
    : shift_expression
    | lhs=relational_expression less! rhs=shift_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::LessThan,
            lhs,
            rhs,
        })
    }
    | lhs=relational_expression greater! rhs=shift_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::GreaterThan,
            lhs,
            rhs,
        })
    }
    | lhs=relational_expression le! rhs=shift_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::LessThanOrEqual,
            lhs,
            rhs,
        })
    }
    | lhs=relational_expression ge! rhs=shift_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::GreaterThanOrEqual,
            lhs,
            rhs,
        })
    }
    ;

equality_expression(Box<dyn PreprocessorExpression>)
    : relational_expression
    | lhs=equality_expression eq! rhs=relational_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::Equal,
            lhs,
            rhs,
        })
    }
    | lhs=equality_expression ne! rhs=relational_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::NotEqual,
            lhs,
            rhs,
        })
    }
    ;

and_expression(Box<dyn PreprocessorExpression>)
    : equality_expression
    | lhs=and_expression ampersand! rhs=equality_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::BitwiseAnd,
            lhs,
            rhs,
        })
    }
    ;

exclusive_or_expression(Box<dyn PreprocessorExpression>)
    : and_expression
    | lhs=exclusive_or_expression caret! rhs=and_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::BitwiseXor,
            lhs,
            rhs,
        })
    }
    ;

inclusive_or_expression(Box<dyn PreprocessorExpression>)
    : exclusive_or_expression
    | lhs=inclusive_or_expression pipe! rhs=exclusive_or_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::BitwiseOr,
            lhs,
            rhs,
        })
    }
    ;

logical_and_expression(Box<dyn PreprocessorExpression>)
    : inclusive_or_expression
    | lhs=logical_and_expression and_op! rhs=inclusive_or_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::LogicalAnd,
            lhs,
            rhs,
        })
    }
    ;

logical_or_expression(Box<dyn PreprocessorExpression>)
    : logical_and_expression
    | lhs=logical_or_expression or_op! rhs=logical_and_expression {
        Box::new(BinaryExpression {
            op: BinaryOperator::LogicalOr,
            lhs,
            rhs,
        })
    }
    ;

conditional_expression(Box<dyn PreprocessorExpression>)
    : logical_or_expression
    | cond=logical_or_expression question! then_expr=PreprocessingExpression colon! else_expr=conditional_expression {
        Box::new(ConditionalExpression {
            cond,
            then_expr,
            else_expr,
        })
    }
    ;
