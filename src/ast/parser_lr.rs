use rusty_lr::lr1;

use super::declarator;
use super::declarator::Declarator;
use super::expression;
use super::expression::Expression;
use super::statement;
use super::statement::Statement;

use crate::token::Token;

%%
// lr1! {

// %lalr;
%glr;

%tokentype Token;

%token ident Token::Identifier(_);
%token lparen Token::LeftParen;
%token rparen Token::RightParen;
%token string_literal Token::StringLiteral(_);
%token constant_character Token::ConstantCharacter(_);
%token constant_integer Token::ConstantInteger(_);
%token constant_long Token::ConstantLong(_);
%token constant_unsigned_integer Token::ConstantUnsignedInteger(_);
%token constant_unsigned_long Token::ConstantUnsignedLong(_);
%token constant_float Token::ConstantFloat(_);
%token constant_double Token::ConstantDouble(_);
%token lbracket Token::LeftBracket;
%token rbracket Token::RightBracket;
%token lbrace Token::LeftBrace;
%token rbrace Token::RightBrace;
%token comma Token::Comma;
%token semicolon Token::SemiColon;
%token ellipsis Token::Ellipsis;
%token question Token::Question;
%token colon Token::Colon;
%token dot Token::Dot;
%token ptr_op Token::PtrOp;
%token inc_op Token::IncOp;
%token dec_op Token::DecOp;
%token sizeof Token::Sizeof;
%token ampersand Token::Ampersand;
%token exclamation Token::Exclamation;
%token tilde Token::Tilde;
%token minus Token::Minus;
%token plus Token::Plus;
%token star Token::Star;
%token slash Token::Slash;
%token percent Token::Percent;
%token left_op Token::LeftOp;
%token right_op Token::RightOp;
%token less Token::LessThan;
%token greater Token::GreaterThan;
%token caret Token::Caret;
%token pipe Token::Pipe;
%token le Token::LeOp;
%token ge Token::GeOp;
%token eq Token::EqOp;
%token ne Token::NeOp;
%token and_op Token::AndOp;
%token or_op Token::OrOp;
%token assign Token::Equal;
%token mul_assign Token::MulAssign;
%token div_assign Token::DivAssign;
%token mod_assign Token::ModAssign;
%token add_assign Token::AddAssign;
%token sub_assign Token::SubAssign;
%token left_assign Token::LeftAssign;
%token right_assign Token::RightAssign;
%token and_assign Token::AndAssign;
%token xor_assign Token::XorAssign;
%token or_assign Token::OrAssign;
%token case Token::Case;
%token default Token::Default;
%token if_ Token::If;
%token else_ Token::Else;
%token switch Token::Switch;
%token while_ Token::While;
%token do_ Token::Do;
%token for_ Token::For;
%token goto_ Token::Goto;
%token continue_ Token::Continue;
%token break_ Token::Break;
%token return_ Token::Return;
%token typedef Token::Typedef;
%token extern_ Token::Extern;
%token static_ Token::Static;
%token auto Token::Auto;
%token register Token::Register;
%token const_ Token::Const;
%token volatile Token::Volatile;
%token void_ Token::Void;
%token char_ Token::Char;
%token short_ Token::Short;
%token int_ Token::Int;
%token long_ Token::Long;
%token float_ Token::Float;
%token double_ Token::Double;
%token signed Token::Signed;
%token unsigned Token::Unsigned;
%token struct_ Token::Struct;
%token union_ Token::Union;
%token enum_ Token::Enum;

%left else_;
%precedence IFSTMT;
%nooptim;

%start translation_unit;


Constant(Expression)
    : constant_character {
        if let Token::ConstantCharacter(value) = constant_character {
            Expression::ConstantCharacter(expression::ExprConstantCharacter{ value })
        } else {
            unreachable!()
        }
    }
    | constant_integer {
        if let Token::ConstantInteger(value) = constant_integer {
            Expression::ConstantInteger(expression::ExprConstantInteger{ value })
        } else {
            unreachable!()
        }
    }
    | constant_long {
        if let Token::ConstantLong(value) = constant_long {
            Expression::ConstantLong(expression::ExprConstantLong{ value })
        } else {
            unreachable!()
        }
    }
    | constant_unsigned_integer {
        if let Token::ConstantUnsignedInteger(value) = constant_unsigned_integer {
            Expression::ConstantUnsignedInteger(expression::ExprConstantUnsignedInteger{ value })
        } else {
            unreachable!()
        }
    }
    | constant_unsigned_long {
        if let Token::ConstantUnsignedLong(value) = constant_unsigned_long {
            Expression::ConstantUnsignedLong(expression::ExprConstantUnsignedLong{ value })
        } else {
            unreachable!()
        }
    }
    | constant_float {
        if let Token::ConstantFloat(value) = constant_float {
            Expression::ConstantFloat(expression::ExprConstantFloat{ value })
        } else {
            unreachable!()
        }
    }
    | constant_double {
        if let Token::ConstantDouble(value) = constant_double {
            Expression::ConstantDouble(expression::ExprConstantDouble{ value })
        } else {
            unreachable!()
        }
    }
    ;


primary_expression(Expression)
    : ident {
        if let Token::Identifier(name) = ident {
            Expression::Identifier(expression::ExprIdentifier{ name })
        } else {
            unreachable!()
        }
    }
    | Constant
    | string_literal {
        if let Token::StringLiteral(value) = string_literal {
            Expression::String(expression::ExprString { value })
        } else {
            unreachable!()
        }
    }
    | lparen! expression rparen!
    ;


postfix_expression(Expression)
    : primary_expression
    | postfix_expression lbracket! expression rbracket! {
        Expression::Bracket(expression::ExprBracket{
            src: Box::new(postfix_expression),
            index: Box::new(expression),
        })
    }
    | postfix_expression lparen rparen {
        Expression::Paren(expression::ExprParen{
            src: Box::new(postfix_expression),
            args: Vec::new(),
        })
    }
    | postfix_expression lparen! argument_expression_list rparen! {
        Expression::Paren(expression::ExprParen{
            src: Box::new(postfix_expression),
            args: argument_expression_list,
        })
    }
    | postfix_expression dot! ident {
        if let Token::Identifier(name) = ident {
            Expression::Member(expression::ExprMember{
                src: Box::new(postfix_expression),
                member: name,
            })
        } else {
            unreachable!()
        }
    }
    | postfix_expression ptr_op ident {
        if let Token::Identifier(name) = ident {
            Expression::Arrow(expression::ExprArrow{
                src: Box::new(postfix_expression),
                member: name,
            })
        } else {
            unreachable!()
        }
    }
    | postfix_expression inc_op {
        Expression::Unary(expression::ExprUnary {
            op: expression::ExprUnaryOperator::IncrementPost,
            src: Box::new(postfix_expression),
        })
    }
    | postfix_expression dec_op {
        Expression::Unary(expression::ExprUnary{
            op: expression::ExprUnaryOperator::DecrementPost,
            src: Box::new(postfix_expression),
        })
    }
    ;


argument_expression_list(Vec<Expression>)
    : assignment_expression {
        vec![assignment_expression]
    }
    | argument_expression_list comma! assignment_expression {
        argument_expression_list.push(assignment_expression);
        argument_expression_list
    }
    ;


unary_expression(Expression)
    : postfix_expression
    | inc_op! unary_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::IncrementPre,
            src: Box::new(unary_expression),
        })
    }
    | dec_op! unary_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::DecrementPre,
            src: Box::new(unary_expression),
        })
    }
    | ampersand! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::AddressOf,
            src: Box::new(cast_expression),
        })
    }
    | star! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::Dereference,
            src: Box::new(cast_expression),
        })
    }
    | plus! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::Plus,
            src: Box::new(cast_expression),
        })
    }
    | minus! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::Minus,
            src: Box::new(cast_expression),
        })
    }
    | tilde! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::BitwiseNot,
            src: Box::new(cast_expression),
        })
    }
    | exclamation! cast_expression {
        Expression::Unary( expression::ExprUnary{
            op: expression::ExprUnaryOperator::LogicalNot,
            src: Box::new(cast_expression),
        })
    }
    | sizeof! unary_expression {
        Expression::SizeofExpr(expression::ExprSizeOfExpr{
            expr: Box::new(unary_expression),
        })
    }
    | sizeof! lparen! type_name rparen! {
        Expression::SizeofType( expression::ExprSizeOfType{
            typename: type_name,
        })
    }
    ;

cast_expression(Expression)
    : unary_expression
    | lparen! type_name rparen! cast_expression {
        Expression::Cast( expression::ExprCast{
            src: Box::new(cast_expression),
            typename: type_name,
        })
    }
    ;


multiplicative_expression( Expression )
    : cast_expression
    | multiplicative_expression star cast_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Mul,
            lhs: Box::new(multiplicative_expression),
            rhs: Box::new(cast_expression),
        })
    }
    | multiplicative_expression slash cast_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Div,
            lhs: Box::new(multiplicative_expression),
            rhs: Box::new(cast_expression),
        })
    }
    | multiplicative_expression percent cast_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Mod,
            lhs: Box::new(multiplicative_expression),
            rhs: Box::new(cast_expression),
        })
    }
    ;


additive_expression( Expression )
    : multiplicative_expression
    | additive_expression plus multiplicative_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Add,
            lhs: Box::new(additive_expression),
            rhs: Box::new(multiplicative_expression),
        })
    }
    | additive_expression minus multiplicative_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Sub,
            lhs: Box::new(additive_expression),
            rhs: Box::new(multiplicative_expression),
        })
    }
    ;

shift_expression( Expression )
    : additive_expression
    | shift_expression left_op additive_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::ShiftLeft,
            lhs: Box::new(shift_expression),
            rhs: Box::new(additive_expression),
        })
    }
    | shift_expression right_op additive_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::ShiftRight,
            lhs: Box::new(shift_expression),
            rhs: Box::new(additive_expression),
        })
    }
    ;


relational_expression(Expression)
    : shift_expression
    | relational_expression less shift_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::LessThan,
            lhs: Box::new(relational_expression),
            rhs: Box::new(shift_expression),
        })
    }
    | relational_expression greater shift_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::GreaterThan,
            lhs: Box::new(relational_expression),
            rhs: Box::new(shift_expression),
        })
    }
    | relational_expression le shift_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::LessThanOrEqual,
            lhs: Box::new(relational_expression),
            rhs: Box::new(shift_expression),
        })
    }
    | relational_expression ge shift_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::GreaterThanOrEqual,
            lhs: Box::new(relational_expression),
            rhs: Box::new(shift_expression),
        })
    }
    ;


equality_expression( Expression )
    : relational_expression
    | equality_expression eq relational_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Equal,
            lhs: Box::new(equality_expression),
            rhs: Box::new(relational_expression),
        })
    }
    | equality_expression ne relational_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::NotEqual,
            lhs: Box::new(equality_expression),
            rhs: Box::new(relational_expression),
        })
    }
    ;


and_expression( Expression )
    : equality_expression
    | and_expression ampersand equality_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseAnd,
            lhs: Box::new(and_expression),
            rhs: Box::new(equality_expression),
        })
    }
    ;


exclusive_or_expression ( Expression )
     : and_expression
     | exclusive_or_expression caret and_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseXor,
            lhs: Box::new(exclusive_or_expression),
            rhs: Box::new(and_expression),
        })
    }
     ;

inclusive_or_expression( Expression )
    : exclusive_or_expression
    | inclusive_or_expression pipe exclusive_or_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseOr,
            lhs: Box::new(inclusive_or_expression),
            rhs: Box::new(exclusive_or_expression),
        })
    }
    ;


logical_and_expression( Expression )
    : inclusive_or_expression
    | logical_and_expression and_op inclusive_or_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::LogicalAnd,
            lhs: Box::new(logical_and_expression),
            rhs: Box::new(inclusive_or_expression),
        })
    }
    ;


logical_or_expression( Expression )
    : logical_and_expression
    | logical_or_expression or_op logical_and_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::LogicalOr,
            lhs: Box::new(logical_or_expression),
            rhs: Box::new(logical_and_expression),
        })
    }
    ;

conditional_expression( Expression )
    : logical_or_expression
    | logical_or_expression question! expression colon! conditional_expression {
        Expression::Conditional( expression::ExprConditional{
            cond: Box::new(logical_or_expression),
            then_expr: Box::new(expression),
            else_expr: Box::new(conditional_expression),
        })
    }
    ;

assignment_expression( Expression )
    : conditional_expression
    | unary_expression assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Assign(false),
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression mul_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::MulAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression div_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::DivAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression mod_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::ModAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression add_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::AddAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression sub_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::SubAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression left_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::ShiftLeftAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression right_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::ShiftRightAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression and_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseAndAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression xor_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseXorAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    | unary_expression or_assign assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::BitwiseOrAssign,
            lhs: Box::new(unary_expression),
            rhs: Box::new(assignment_expression),
        })
    }
    ;

expression( Expression )
    : assignment_expression
    | expression comma assignment_expression {
        Expression::Binary( expression::ExprBinary{
            op: expression::ExprBinaryOperator::Comma,
            lhs: Box::new(expression),
            rhs: Box::new(assignment_expression),
        })
    }
    ;

constant_expression( Expression )
    : conditional_expression
    ;



initializer( Expression )
    : assignment_expression
    | lbrace! initializer_list rbrace! {
        Expression::InitializerList( expression::ExprInitializerList{
            initializers: initializer_list,
        })
    }
    | lbrace! initializer_list comma! rbrace! {
        Expression::InitializerList( expression::ExprInitializerList{
            initializers: initializer_list,
        })
    }
    ;

initializer_list( Vec<Expression> )
    : initializer {
        vec![initializer]
    }
    | initializer_list comma! initializer {
        initializer_list.push(initializer);
        initializer_list
    }
    ;


labeled_statement ( Statement )
    : ident colon statement {
        if let Token::Identifier(label) = ident {
            Statement::Labeled( statement::StmtLabeled{
                label,
                statement: Box::new(statement),
            })
        } else {
            unreachable!()
        }
    }
    | case constant_expression colon statement {
        Statement::Case( statement::StmtCase{
            value: constant_expression,
            statement: Box::new(statement),
        })
    }
    | default colon statement {
        Statement::Default( statement::StmtDefault{
            statement: Box::new(statement),
        })
    }
    ;

statement_or_declaration( Statement )
    : statement
    | declaration
    ;

compound_statement( Statement )
    : lbrace! statement_or_declaration* rbrace! {
        Statement::Compound( statement::StmtCompound{
            statements: statement_or_declaration
        })
    }
    ;


expression_statement( Statement )
    : semicolon {
        Statement::Null( statement::StmtNull{} )
    }
    | expression semicolon {
        Statement::Expression( statement::StmtExpression{
            expression: expression,
        })
    }
    ;


selection_statement( Statement )
    : if_ lparen! expression rparen! statement %prec IFSTMT {
        Statement::If( statement::StmtIf{
            cond: expression,
            then_statement: Box::new(statement),
            else_statement: None,
        })
    }
    | if_ lparen expression rparen thenstmt=statement else_ elsestmt=statement {
        Statement::If( statement::StmtIf{
            cond: expression,
            then_statement: Box::new(thenstmt),
            else_statement: Some(Box::new(elsestmt)),
        })
    }
    | switch lparen expression rparen statement {
        Statement::Switch( statement::StmtSwitch{
            target: expression,
            statement: Box::new(statement),
        })
    }
    ;

declaration_or_expression( Statement )
    : declaration
    | expression_statement
    ;


iteration_statement( Statement )
    : while_ lparen expression rparen statement {
        Statement::While( statement::StmtWhile{
            cond: expression,
            statement: Box::new(statement),
        })
    }
    | do_ statement while_ lparen expression rparen semicolon {
        Statement::DoWhile( statement::StmtDoWhile{
            cond: expression,
            statement: Box::new(statement),
        })
    }
    | for_ lparen init=declaration_or_expression cond=expression_statement rparen body=statement {
        let Statement::Expression(cond) = cond else { unreachable!() };
        Statement::For( statement::StmtFor{
            init: Box::new(init),
            cond: cond.expression,
            next: None,
            statement: Box::new(body),
        })
    }
    | for_ lparen init=declaration_or_expression cond=expression_statement next=expression rparen body=statement {
        let Statement::Expression(cond) = cond else { unreachable!() };
        Statement::For( statement::StmtFor{
            init: Box::new(init),
            cond: cond.expression,
            next: Some(next),
            statement: Box::new(body),
        })
    }
    ;


jump_statement( Statement )
    : goto_ ident semicolon {
        let Token::Identifier(ident) = ident else { unreachable!() };
        Statement::Goto( statement::StmtGoto{
            label: ident,
        })
    }
    | continue_ semicolon {
        Statement::Continue( statement::StmtContinue{} )
    }
    | break_ semicolon {
        Statement::Break( statement::StmtBreak{} )
    }
    | return_ semicolon {
        Statement::Return( statement::StmtReturn{
            expr: None,
        })
    }
    | return_ expression semicolon {
        Statement::Return( statement::StmtReturn{
            expr: Some(expression),
        })
    }
    ;

declaration( Statement )
    : declaration_specifier+ semicolon {
        Statement::Declaration( statement::StmtDeclaration{
            specs: declaration_specifier,
            inits: None,
        })
    }
    | declaration_specifier+ inits=init_declarator_list semicolon {
        Statement::Declaration( statement::StmtDeclaration{
            specs: declaration_specifier,
            inits: Some(inits),
        })
    }
    ;

// what is declaration_list for? in function definition?
function_definition( Statement )
    // : declaration_specifier+ declarator declaration+ compound_statement
    : declaration_specifier+ declarator compound_statement {
        Statement::FunctionDefinition( statement::StmtFunctionDefinition{
            specs: Some(declaration_specifier),
            decl: declarator,
            body: Box::new(compound_statement),
        })
    }
    // | declarator declaration+ compound_statement
    | declarator compound_statement {
        Statement::FunctionDefinition( statement::StmtFunctionDefinition {
            specs: None,
            decl: declarator,
            body: Box::new(compound_statement),
        })
    }
    ;

statement( Statement )
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

external_declaration( Statement )
    : function_definition
    | declaration
    ;

translation_unit( statement::TranslationUnit )
    : external_declaration* {
        statement::TranslationUnit {
            statements: external_declaration
        }
    }
    ;

type_qualifier( declarator::TypeQualifier )
    : const_ { declarator::TypeQualifier::Const }
    | volatile { declarator::TypeQualifier::Volatile }
    ;

declarator( Declarator )
    : direct_declarator
    | star! type_qualifier* declarator {
        while let Some(type_qual) = type_qualifier.pop() {
            match type_qual {
                declarator::TypeQualifier::Const => {
                    declarator = Declarator::Const(declarator::DeclConst{
                        declarator: Some(Box::new(declarator)),
                    });
                }
                declarator::TypeQualifier::Volatile => {
                    declarator = Declarator::Volatile(declarator::DeclVolatile{
                        declarator: Some(Box::new(declarator)),
                    });
                }
            }
        }
        declarator = Declarator::Pointer(declarator::DeclPointer{
            declarator: Some(Box::new(declarator)),
        });
        declarator
    }
    ;

direct_declarator( Declarator )
    : ident {
        let Token::Identifier(name) = ident else { unreachable!() };
        Declarator::Identifier(declarator::DeclIdentifier{ name })
    }
    | lparen! declarator rparen!
    | direct_declarator lbracket! constant_expression rbracket! {
        Declarator::ArrayFixed(declarator::DeclArrayFixed{
            declarator: Some(Box::new(direct_declarator)),
            size: constant_expression,
        })
    }
    | direct_declarator lbracket! rbracket! {
        Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded{
            declarator: Some(Box::new(direct_declarator)),
        })
    }
    | direct_declarator lparen! parameter_type_list rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: Some(Box::new(direct_declarator)),
            params: parameter_type_list,
        })
    }
    | direct_declarator lparen! rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: Some(Box::new(direct_declarator)),
            params: declarator::ParameterList {
                params: Vec::new(),
                variadic: false
            }
        })
    }
    ;


abstract_declarator(Declarator)
    : star! type_qualifier* abstract_declarator {
        let mut declarator = abstract_declarator;
        while let Some(type_qual) = type_qualifier.pop() {
            match type_qual {
                declarator::TypeQualifier::Const => {
                    declarator = Declarator::Const(declarator::DeclConst{
                        declarator: Some(Box::new(declarator)),
                    });
                }
                declarator::TypeQualifier::Volatile => {
                    declarator = Declarator::Volatile(declarator::DeclVolatile{
                        declarator: Some(Box::new(declarator)),
                    });
                }
            }
        }
        declarator = Declarator::Pointer(declarator::DeclPointer{
            declarator: Some(Box::new(declarator)),
        });
        declarator
    }
    | star! type_qualifier* {
        let mut declarator = None;
        while let Some(type_qual) = type_qualifier.pop() {
            match type_qual {
                declarator::TypeQualifier::Const => {
                    declarator = Some(Declarator::Const(declarator::DeclConst{
                        declarator: declarator.map( Box::new ),
                    }));
                }
                declarator::TypeQualifier::Volatile => {
                    declarator = Some(Declarator::Volatile(declarator::DeclVolatile{
                        declarator: declarator.map( Box::new ),
                    }));
                }
            }
        }
        Declarator::Pointer(declarator::DeclPointer{
            declarator: declarator.map( Box::new ),
        })
    }
    | direct_abstract_declarator
    ;

direct_abstract_declarator( Declarator )
    : lparen! abstract_declarator rparen!
    | lbracket! rbracket! {
        Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded{
            declarator: None,
        })
    }
    | lbracket! constant_expression rbracket! {
        Declarator::ArrayFixed(declarator::DeclArrayFixed{
            declarator: None,
            size: constant_expression,
        })
    }
    | direct_abstract_declarator lbracket! rbracket! {
        Declarator::ArrayUnbounded(declarator::DeclArrayUnbounded{
            declarator: Some(Box::new(direct_abstract_declarator)),
        })
    }
    | direct_abstract_declarator lbracket! constant_expression rbracket! {
        Declarator::ArrayFixed(declarator::DeclArrayFixed{
            declarator: Some(Box::new(direct_abstract_declarator)),
            size: constant_expression,
        })
    }
    | lparen! rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: None,
            params: declarator::ParameterList {
                params: Vec::new(),
                variadic: false
            }
        })
    }
    | lparen! parameter_type_list rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: None,
            params: parameter_type_list,
        })
    }
    | direct_abstract_declarator lparen! rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: Some(Box::new(direct_abstract_declarator)),
            params: declarator::ParameterList {
                params: Vec::new(),
                variadic: false,
            }
        })
    }
    | direct_abstract_declarator lparen! parameter_type_list rparen! {
        Declarator::Function(declarator::DeclFunction{
            declarator: Some(Box::new(direct_abstract_declarator)),
            params: parameter_type_list,
        })
    }
    ;

specifier_qualifier( declarator::SpecifierQualifier )
    : type_qualifier {
        declarator::SpecifierQualifier::TypeQualifier(type_qualifier)
    }
    | type_specifier {
        declarator::SpecifierQualifier::TypeSpecifier(type_specifier)
    }
    ;

type_name( declarator::Typename )
    : specifier_qualifier+ abstract_declarator? {
        declarator::Typename {
            specs: specifier_qualifier,
            declarator: abstract_declarator.map(Box::new),
        }
    }
    ;

type_specifier( declarator::TypeSpecifier )
    : void_ {
        declarator::TypeSpecifier::Void
    }
    | char_ {
        declarator::TypeSpecifier::Char
    }
    | short_ {
        declarator::TypeSpecifier::Short
    }
    | int_ {
        declarator::TypeSpecifier::Int
    }
    | long_ {
        declarator::TypeSpecifier::Long
    }
    | float_ {
        declarator::TypeSpecifier::Float
    }
    | double_ {
        declarator::TypeSpecifier::Double
    }
    | signed {
        declarator::TypeSpecifier::Signed
    }
    | unsigned {
        declarator::TypeSpecifier::Unsigned
    }
    | ident {
        let Token::Identifier(ident) = ident else { unreachable!() };
        declarator::TypeSpecifier::Typename(ident)
    }
    | struct_or_union_specifier {
        declarator::TypeSpecifier::StructOrUnion(struct_or_union_specifier)
    }
    | enum_specifier {
        declarator::TypeSpecifier::Enum(enum_specifier)
    }
    ;

storage_class_specifier( declarator::StorageClassSpecifier )
    : typedef {
        declarator::StorageClassSpecifier::Typedef
    }
    | extern_ {
        declarator::StorageClassSpecifier::Extern
    }
    | static_ {
        declarator::StorageClassSpecifier::Static
    }
    | auto {
        declarator::StorageClassSpecifier::Auto
    }
    | register {
        declarator::StorageClassSpecifier::Register
    }
    ;
declaration_specifier( declarator::DeclarationSpecifier )
    : storage_class_specifier {
        declarator::DeclarationSpecifier::StorageClassSpecifier(storage_class_specifier)
    }
    | type_specifier {
        declarator::DeclarationSpecifier::TypeSpecifier(type_specifier)
    }
    | type_qualifier {
        declarator::DeclarationSpecifier::TypeQualifier(type_qualifier)
    }
    ;

parameter_declaration( declarator::ParameterDeclaration )
    : declaration_specifier+ declarator {
        declarator::ParameterDeclaration{
            specs: declaration_specifier,
            declarator: Some(Box::new(declarator)),
        }
    }
    | declaration_specifier+ abstract_declarator {
        declarator::ParameterDeclaration{
            specs: declaration_specifier,
            declarator: Some(Box::new(abstract_declarator)),
        }
    }
    | declaration_specifier+ {
        declarator::ParameterDeclaration{
            specs: declaration_specifier,
            declarator: None,
        }
    }
    ;

parameter_list( Vec<declarator::ParameterDeclaration> )
    : parameter_declaration {
        vec![parameter_declaration]
    }
    | parameter_list comma! parameter_declaration {
        parameter_list.push(parameter_declaration);
        parameter_list
    }
    ;

parameter_type_list( declarator::ParameterList )
    : parameter_list {
        declarator::ParameterList {
            params: parameter_list,
            variadic: false,
        }
    }
    | parameter_list comma! ellipsis {
        declarator::ParameterList{
            params: parameter_list,
            variadic: true,
        }
    }
    ;

// returns is_struct
struct_or_union( bool )
    : struct_ {
        true
    }
    | union_ {
        false
    }
    ;

struct_declarator( Declarator )
    : declarator
    // @TODO
    // bit field
    ;

struct_declarator_list( Vec<Declarator> )
    : struct_declarator {
        vec![struct_declarator]
    }
    | struct_declarator_list comma! struct_declarator {
        struct_declarator_list.push(struct_declarator);
        struct_declarator_list
    }
    ;

struct_declaration( declarator::StructDeclaration )
    : specifier_qualifier+ struct_declarator_list semicolon! {
        declarator::StructDeclaration {
            specs: specifier_qualifier,
            declarators: struct_declarator_list,
        }
    }
    ;

struct_or_union_specifier( declarator::StructOrUnionSpecifier )
    : struct_or_union ident? lbrace! struct_declaration* rbrace! {
        let name = ident.map(|name| { let Token::Identifier(name) = name else { unreachable!() }; name });
        declarator::StructOrUnionSpecifier {
            is_struct: struct_or_union,
            name,
            decls: Some(struct_declaration),
        }
    }
    | struct_or_union ident {
        let Token::Identifier(name) = ident else { unreachable!() };
        declarator::StructOrUnionSpecifier {
            is_struct: struct_or_union,
            name: Some(name),
            decls: None,
        }
    }
    ;

enum_specifier( declarator::EnumSpecifier )
    : enum_! ident? lbrace! enumerator_list rbrace! {
        let name = ident.map(|name| { let Token::Identifier(name) = name else { unreachable!() }; name });
        declarator::EnumSpecifier {
            name,
            enumerators: Some(enumerator_list),
        }
    }
    | enum_! ident {
        let Token::Identifier(name) = ident else { unreachable!() };
        declarator::EnumSpecifier {
            name: Some(name),
            enumerators: None,
        }
    }
    ;

enumerator_list( Vec<declarator::Enumerator> )
    : enumerator {
        vec![enumerator]
    }
    | enumerator_list comma! enumerator {
        enumerator_list.push(enumerator);
        enumerator_list
    }
    ;

enumerator( declarator::Enumerator )
    : ident {
        let Token::Identifier(name) = ident else { unreachable!() };
        declarator::Enumerator {
            name,
            value: None,
        }
    }
    | ident assign constant_expression {
        let Token::Identifier(name) = ident else { unreachable!() };
        declarator::Enumerator {
            name,
            value: Some(constant_expression),
        }
    }
    ;


init_declarator( declarator::DeclInit )
    : declarator {
        declarator::DeclInit{
            declarator: Box::new(declarator),
            initializer: None,
        }
    }
    | declarator assign initializer {
        declarator::DeclInit{
            declarator: Box::new(declarator),
            initializer: Some(initializer)
        }
    }
    ;

init_declarator_list( Vec<declarator::DeclInit> )
    : init_declarator {
        vec![init_declarator]
    }
    | init_declarator_list comma! init_declarator {
        init_declarator_list.push(init_declarator);
        init_declarator_list
    }
    ;

// }
