use std::cell::RefCell;
use std::rc::Rc;

use super::expression;
use super::statement;
use super::ArrayType;
use super::CVType;
use super::CombinedDeclarator;
use super::ConversionError;
use super::Expression;
use super::FunctionType;
use super::PrimitiveType;
use super::Statement;
use super::StmtSwitchCase;
use crate::ast;

use super::scope::BlockScope;
use super::scope::FunctionScope;
use super::scope::GlobalScope;
use super::scope::LoopScope;
use super::scope::Scope;
use super::scope::SwitchScope;
use super::scope::VariableScope;

use super::LabelInfo;

pub struct Context {
    /// for unique scope id generation
    pub scope_counter: usize,
    pub global_scope: GlobalScope,
    pub function_scope: Option<FunctionScope>,
    pub scopes: Vec<Scope>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            scope_counter: 0,
            global_scope: GlobalScope::new(),
            function_scope: None,
            scopes: Vec::new(),
        }
    }

    /// generate unique scope id
    fn generate_scope_id(&mut self) -> usize {
        self.scope_counter += 1;
        self.scope_counter
    }
    fn begin_switch_scope(&mut self) -> Result<(), ConversionError> {
        let scope = SwitchScope {
            id: self.generate_scope_id(),
            default: false,
        };
        self.scopes.push(Scope::Switch(scope));
        Ok(())
    }
    fn nearest_switch_scope(&mut self) -> Option<&mut SwitchScope> {
        for scope in self.scopes.iter_mut().rev() {
            match scope {
                Scope::Switch(scope) => return Some(scope),
                _ => {}
            }
        }
        None
    }
    fn end_switch_scope(&mut self) -> Result<SwitchScope, ConversionError> {
        unimplemented!("end_switch_scope")
        // must pop every variable scope until switch scope
    }
    fn begin_loop_scope(&mut self) -> Result<(), ConversionError> {
        let scope = LoopScope {
            id: self.generate_scope_id(),
        };
        self.scopes.push(Scope::Loop(scope));
        Ok(())
    }
    fn end_loop_scope(&mut self) -> Result<LoopScope, ConversionError> {
        unimplemented!("end_loop_scope")
        // must pop every variable scope until loop scope
    }

    fn begin_scope(&mut self) -> Result<(), ConversionError> {
        let scope = BlockScope {
            id: self.generate_scope_id(),
        };
        self.scopes.push(Scope::Block(scope));
        Ok(())
    }
    fn end_scope(&mut self) -> Result<BlockScope, ConversionError> {
        unimplemented!("end_scope")
    }

    fn begin_function_scope(
        &mut self,
        name: String,
        type_: FunctionType,
    ) -> Result<(), ConversionError> {
        unimplemented!("begin_function_scope")
    }
    fn end_function_scope(&mut self) -> Result<FunctionScope, ConversionError> {
        unimplemented!("end_function_scope")
    }

    fn begin_variable_scope(&mut self, name: String) -> Result<(), ConversionError> {
        unimplemented!("begin_variable_scope")
    }

    fn can_continue(&self) -> bool {
        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Loop(_) => return true,
                _ => {}
            }
        }
        false
    }
    fn can_break(&self) -> bool {
        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Loop(_) | Scope::Switch(_) => return true,
                _ => {}
            }
        }
        false
    }
    fn can_return(&self) -> bool {
        self.function_scope.is_some()
    }

    pub fn process_statement(
        &mut self,
        statement: ast::Statement,
    ) -> Result<Statement, ConversionError> {
        match statement {
            ast::Statement::Null(stmt) => self.process_statement_null(stmt),
            ast::Statement::Expression(stmt) => self.process_statement_expression(stmt),
            ast::Statement::Labeled(stmt) => self.process_statement_labeled(stmt),
            ast::Statement::Compound(stmt) => self.process_statement_compound(stmt),
            ast::Statement::If(stmt) => self.process_statement_if(stmt),
            ast::Statement::Switch(stmt) => self.process_statement_switch(stmt),
            ast::Statement::Case(stmt) => self.process_statement_case(stmt),
            ast::Statement::Default(stmt) => self.process_statement_default(stmt),
            ast::Statement::Continue(stmt) => self.process_statement_continue(stmt),
            ast::Statement::Break(stmt) => self.process_statement_break(stmt),
            ast::Statement::While(stmt) => self.process_statement_while(stmt),
            ast::Statement::DoWhile(stmt) => self.process_statement_dowhile(stmt),
            ast::Statement::For(stmt) => self.process_statement_for(stmt),
            ast::Statement::Goto(stmt) => self.process_statement_goto(stmt),
            ast::Statement::Return(stmt) => self.process_statement_return(stmt),
            ast::Statement::Declaration(stmt) => self.process_statement_declaration(stmt),
            ast::Statement::FunctionDefinition(stmt) => {
                self.process_statement_functiondefinition(stmt)
            }
        }
    }

    pub fn process_expression(
        &mut self,
        expression: ast::Expression,
    ) -> Result<Expression, ConversionError> {
        match expression {
            ast::Expression::Identifier(expr) => self.process_expression_identifier(expr),
            ast::Expression::ConstantCharacter(expr) => {
                self.process_expression_constantcharacter(expr)
            }
            ast::Expression::ConstantInteger(expr) => self.process_expression_constantinteger(expr),
            ast::Expression::ConstantUnsignedInteger(expr) => {
                self.process_expression_constantunsignedinteger(expr)
            }
            ast::Expression::ConstantLong(expr) => self.process_expression_constantlong(expr),
            ast::Expression::ConstantUnsignedLong(expr) => {
                self.process_expression_constantunsignedlong(expr)
            }
            ast::Expression::ConstantFloat(expr) => self.process_expression_constantfloat(expr),
            ast::Expression::ConstantDouble(expr) => self.process_expression_constantdouble(expr),
            ast::Expression::String(expr) => self.process_expression_string(expr),
            ast::Expression::Member(expr) => self.process_expression_member(expr),
            ast::Expression::Arrow(expr) => self.process_expression_arrow(expr),
            ast::Expression::Bracket(expr) => self.process_expression_bracket(expr),
            ast::Expression::Paren(expr) => self.process_expression_paren(expr),
            ast::Expression::Cast(expr) => self.process_expression_cast(expr),
            ast::Expression::SizeofType(expr) => self.process_expression_sizeoftype(expr),
            ast::Expression::SizeofExpr(expr) => self.process_expression_sizeofexpr(expr),
            ast::Expression::Conditional(expr) => self.process_expression_conditional(expr),
            ast::Expression::Unary(expr) => self.process_expression_unary(expr),
            ast::Expression::Binary(expr) => self.process_expression_binary(expr),
            ast::Expression::InitializerList(expr) => self.process_expression_initializerlist(expr),
        }
    }

    pub fn process_declarator(
        &mut self,
        declarator: ast::Declarator,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        match declarator {
            ast::Declarator::Identifier(decl) => {
                self.process_declarator_identifier(decl, base_type)
            }
            ast::Declarator::Pointer(decl) => self.process_declarator_pointer(decl, base_type),
            ast::Declarator::ArrayFixed(decl) => {
                self.process_declarator_array_fixed(decl, base_type)
            }
            ast::Declarator::ArrayUnbounded(decl) => {
                self.process_declarator_array_unbounded(decl, base_type)
            }
            ast::Declarator::Function(decl) => self.process_declarator_function(decl, base_type),
            ast::Declarator::Const(decl) => self.process_declarator_const(decl, base_type),
            ast::Declarator::Volatile(decl) => self.process_declarator_volatile(decl, base_type),
        }
    }
}

// for statements
impl Context {
    pub(crate) fn process_statement_null(
        &mut self,
        _stmt: ast::StmtNull,
    ) -> Result<Statement, ConversionError> {
        Ok(Statement::None)
    }
    pub(crate) fn process_statement_expression(
        &mut self,
        stmt: ast::StmtExpression,
    ) -> Result<Statement, ConversionError> {
        Ok(Statement::Expression(statement::StmtExpression {
            expression: self.process_expression(stmt.expression)?,
        }))
    }
    pub(crate) fn process_statement_labeled(
        &mut self,
        stmt: ast::StmtLabeled,
    ) -> Result<Statement, ConversionError> {
        if let Some(function_scope) = &mut self.function_scope {
            let label_info = LabelInfo {
                name: stmt.label.clone(),
            };
            let label_info = Rc::new(RefCell::new(label_info));
            if let Some(old) = function_scope
                .labels
                .insert(stmt.label.clone(), Rc::clone(&label_info))
            {
                let label = old.borrow().name.clone();
                Err(ConversionError::MultipleLabelDefinition(label))
            } else {
                Ok(Statement::Labeled(statement::StmtLabeled {
                    label: label_info,
                    statement: Box::new(self.process_statement(*stmt.statement)?),
                }))
            }
        } else {
            Err(ConversionError::LabelDefinitionOutsideFunction)
        }
    }
    pub(crate) fn process_statement_goto(
        &mut self,
        stmt: ast::StmtGoto,
    ) -> Result<Statement, ConversionError> {
        if let Some(function_scope) = &mut self.function_scope {
            if let Some(label) = function_scope.labels.get(&stmt.label) {
                Ok(Statement::Goto(statement::StmtGoto {
                    label: Rc::clone(label),
                }))
            } else {
                Err(ConversionError::GotoInvalidLabel(stmt.label))
            }
        } else {
            Err(ConversionError::GotoOutsideFunction)
        }
    }
    pub(crate) fn process_statement_compound(
        &mut self,
        stmt: ast::StmtCompound,
    ) -> Result<Statement, ConversionError> {
        self.begin_scope()?;

        let compound = statement::StmtCompound {
            statements: stmt
                .statements
                .into_iter()
                .map(|stmt| self.process_statement(stmt))
                .collect::<Result<Vec<_>, _>>()?,
        };

        self.end_scope()?;
        Ok(Statement::Compound(compound))
    }
    pub(crate) fn process_statement_if(
        &mut self,
        stmt: ast::StmtIf,
    ) -> Result<Statement, ConversionError> {
        let cond = self.process_expression(stmt.cond)?;
        let then = self.process_statement(*stmt.then_statement)?;
        let else_ = if let Some(stmt) = stmt.else_statement {
            Some(Box::new(self.process_statement(*stmt)?))
        } else {
            None
        };

        Ok(Statement::If(statement::StmtIf {
            condition: cond,
            then: Box::new(then),
            else_,
        }))
    }
    pub(crate) fn process_statement_switch(
        &mut self,
        stmt: ast::StmtSwitch,
    ) -> Result<Statement, ConversionError> {
        self.begin_switch_scope()?;
        let value = self.process_expression(stmt.target)?;
        let body = self.process_statement(*stmt.statement)?;
        self.end_switch_scope()?;

        let Statement::Compound(body) = body else {
            return Err(ConversionError::InvalidSwitchBody);
        };

        let mut cases: Vec<StmtSwitchCase> = Vec::new();
        for s in body.statements.into_iter() {
            match s {
                Statement::_Case(c) => {
                    cases.push(StmtSwitchCase {
                        value: Some(c.value),
                        statements: vec![*c.statement],
                    });
                }
                Statement::_Default(d) => {
                    cases.push(StmtSwitchCase {
                        value: None,
                        statements: vec![*d.statement],
                    });
                }
                s => {
                    if let Some(last) = cases.last_mut() {
                        last.statements.push(s);
                    } else {
                        return Err(ConversionError::InvalidSwitchBody);
                    }
                }
            }
        }
        Ok(Statement::Switch(statement::StmtSwitch { value, cases }))
    }
    pub(crate) fn process_statement_case(
        &mut self,
        stmt: ast::StmtCase,
    ) -> Result<Statement, ConversionError> {
        let value = self.process_expression(stmt.value)?;
        let statement = self.process_statement(*stmt.statement)?;
        if self.nearest_switch_scope().is_none() {
            return Err(ConversionError::InvalidCase);
        }

        Ok(Statement::_Case(statement::StmtCase {
            value,
            statement: Box::new(statement),
        }))
    }
    pub(crate) fn process_statement_default(
        &mut self,
        stmt: ast::StmtDefault,
    ) -> Result<Statement, ConversionError> {
        let statement = self.process_statement(*stmt.statement)?;
        if let Some(scope) = self.nearest_switch_scope() {
            if scope.default {
                return Err(ConversionError::MultipleDefault);
            }
        } else {
            return Err(ConversionError::InvalidDefault);
        }

        Ok(Statement::_Default(statement::StmtDefault {
            statement: Box::new(statement),
        }))
    }
    pub(crate) fn process_statement_continue(
        &mut self,
        _stmt: ast::StmtContinue,
    ) -> Result<Statement, ConversionError> {
        if !self.can_continue() {
            return Err(ConversionError::InvalidContinue);
        }

        Ok(Statement::Continue)
    }
    pub(crate) fn process_statement_break(
        &mut self,
        _stmt: ast::StmtBreak,
    ) -> Result<Statement, ConversionError> {
        if !self.can_break() {
            return Err(ConversionError::InvalidBreak);
        }

        Ok(Statement::Break)
    }
    pub(crate) fn process_statement_while(
        &mut self,
        stmt: ast::StmtWhile,
    ) -> Result<Statement, ConversionError> {
        self.begin_loop_scope()?;

        let cond = self.process_expression(stmt.cond)?;
        let body = self.process_statement(*stmt.statement)?;

        self.end_loop_scope()?;

        Ok(Statement::While(statement::StmtWhile {
            condition: cond,
            body: Box::new(body),
        }))
    }
    pub(crate) fn process_statement_dowhile(
        &mut self,
        stmt: ast::StmtDoWhile,
    ) -> Result<Statement, ConversionError> {
        self.begin_loop_scope()?;

        let body = self.process_statement(*stmt.statement)?;
        let cond = self.process_expression(stmt.cond)?;

        self.end_loop_scope()?;

        Ok(Statement::DoWhile(statement::StmtDoWhile {
            body: Box::new(body),
            condition: cond,
        }))
    }
    pub(crate) fn process_statement_for(
        &mut self,
        stmt: ast::StmtFor,
    ) -> Result<Statement, ConversionError> {
        self.begin_loop_scope()?;

        let init = self.process_statement(*stmt.init)?;
        let cond = self.process_expression(stmt.cond)?;
        let next = if let Some(expr) = stmt.next {
            Some(self.process_expression(expr)?)
        } else {
            None
        };
        let body = self.process_statement(*stmt.statement)?;

        self.end_loop_scope()?;

        Ok(Statement::For(statement::StmtFor {
            init: Box::new(init),
            condition: cond,
            next,
            body: Box::new(body),
        }))
    }
    pub(crate) fn process_statement_return(
        &mut self,
        stmt: ast::StmtReturn,
    ) -> Result<Statement, ConversionError> {
        if !self.can_return() {
            return Err(ConversionError::InvalidReturn);
        }
        // @TODO type check

        let expr = if let Some(expr) = stmt.expr {
            Some(self.process_expression(expr)?)
        } else {
            None
        };
        Ok(Statement::Return(statement::StmtReturn {
            expression: expr,
        }))
    }
    pub(crate) fn process_statement_declaration(
        &mut self,
        stmt: ast::StmtDeclaration,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_functiondefinition(
        &mut self,
        stmt: ast::StmtFunctionDefinition,
    ) -> Result<Statement, ConversionError> {
        let base_type = match stmt.specs {
            Some(specs) => self.process_decl_specs(specs.into_iter())?,
            None => CVType::from_primitive(PrimitiveType::Void),
        };
        let decl = self.process_declarator(stmt.decl, base_type)?;
        let name = match decl.name {
            Some(name) => name,
            None => return Err(ConversionError::NoFunctionName),
        };
        match decl.type_.type_ {
            PrimitiveType::Function(func) => {
                self.begin_function_scope(name.clone(), func.clone())?;
                let body = self.process_statement(*stmt.body)?;
                let scope = self.end_function_scope()?;
                let statement = Statement::FunctionDefinition(statement::StmtFunctionDefinition {
                    name,
                    definition: func,
                    body: Box::new(body),
                    stack_size: scope.stack_size,
                });
                Ok(statement)
            }
            _ => return Err(ConversionError::InvalidFunctionDefinition),
        }
    }
}

// for expressions
impl Context {
    pub(crate) fn process_expression_identifier(
        &mut self,
        expr: ast::ExprIdentifier,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantcharacter(
        &mut self,
        expr: ast::ExprConstantCharacter,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantinteger(
        &mut self,
        expr: ast::ExprConstantInteger,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantunsignedinteger(
        &mut self,
        expr: ast::ExprConstantUnsignedInteger,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantlong(
        &mut self,
        expr: ast::ExprConstantLong,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantunsignedlong(
        &mut self,
        expr: ast::ExprConstantUnsignedLong,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantfloat(
        &mut self,
        expr: ast::ExprConstantFloat,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_constantdouble(
        &mut self,
        expr: ast::ExprConstantDouble,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_string(
        &mut self,
        expr: ast::ExprString,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_member(
        &mut self,
        expr: ast::ExprMember,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_arrow(
        &mut self,
        expr: ast::ExprArrow,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_bracket(
        &mut self,
        expr: ast::ExprBracket,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_paren(
        &mut self,
        expr: ast::ExprParen,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_cast(
        &mut self,
        expr: ast::ExprCast,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_sizeoftype(
        &mut self,
        expr: ast::ExprSizeOfType,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_sizeofexpr(
        &mut self,
        expr: ast::ExprSizeOfExpr,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_conditional(
        &mut self,
        expr: ast::ExprConditional,
    ) -> Result<Expression, ConversionError> {
        let cond = self.process_expression(*expr.cond)?;
        let then = self.process_expression(*expr.then_expr)?;
        let else_ = self.process_expression(*expr.else_expr)?;
        Ok(Expression::Conditional(expression::ExprConditional {
            cond: Box::new(cond),
            then_expr: Box::new(then),
            else_expr: Box::new(else_),
        }))
    }
    pub(crate) fn process_expression_unary(
        &mut self,
        expr: ast::ExprUnary,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_binary(
        &mut self,
        expr: ast::ExprBinary,
    ) -> Result<Expression, ConversionError> {
    }
    pub(crate) fn process_expression_initializerlist(
        &mut self,
        expr: ast::ExprInitializerList,
    ) -> Result<Expression, ConversionError> {
        Ok(Expression::InitializerList(
            expression::ExprInitializerList {
                exprs: expr
                    .initializers
                    .into_iter()
                    .map(|expr| self.process_expression(expr))
                    .collect::<Result<Vec<_>, _>>()?,
            },
        ))
    }
}

// for declarators
impl Context {
    pub(crate) fn process_specs(
        &mut self,
        mut specs: impl Iterator<Item = ast::SpecifierQualifier>,
    ) -> Result<CVType, ConversionError> {
        unimplemented!("process_specs")
    }
    pub(crate) fn process_decl_specs(
        &mut self,
        mut specs: impl Iterator<Item = ast::DeclarationSpecifier>,
    ) -> Result<CVType, ConversionError> {
        unimplemented!("process_specs")
    }
    pub(crate) fn process_declarator_identifier(
        &mut self,
        decl: ast::DeclIdentifier,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        Ok(CombinedDeclarator {
            name: Some(decl.name),
            type_: base_type,
        })
    }
    pub(crate) fn process_declarator_pointer(
        &mut self,
        decl: ast::DeclPointer,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.type_ = CVType::from_primitive(PrimitiveType::Pointer(Box::new(res.type_)));
            Ok(res)
        } else {
            Ok(CombinedDeclarator {
                name: None,
                type_: CVType::from_primitive(PrimitiveType::Pointer(Box::new(base_type))),
            })
        }
    }
    pub(crate) fn process_declarator_array_fixed(
        &mut self,
        decl: ast::DeclArrayFixed,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        let size = self.process_expression(decl.size)?;
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.type_ = CVType::from_primitive(PrimitiveType::Array(ArrayType {
                type_: Box::new(res.type_),
                size: size,
            }));
            Ok(res)
        } else {
            Ok(CombinedDeclarator {
                name: None,
                type_: CVType::from_primitive(PrimitiveType::Array(ArrayType {
                    type_: Box::new(base_type),
                    size: size,
                })),
            })
        }
    }
    pub(crate) fn process_declarator_array_unbounded(
        &mut self,
        decl: ast::DeclArrayUnbounded,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
    pub(crate) fn process_declarator_function(
        &mut self,
        decl: ast::DeclFunction,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
    pub(crate) fn process_declarator_const(
        &mut self,
        decl: ast::DeclConst,
        mut base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.type_.const_ = true;
            Ok(res)
        } else {
            base_type.const_ = true;
            Ok(CombinedDeclarator {
                name: None,
                type_: base_type,
            })
        }
    }
    pub(crate) fn process_declarator_volatile(
        &mut self,
        decl: ast::DeclVolatile,
        mut base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.type_.volatile = true;
            Ok(res)
        } else {
            base_type.volatile = true;
            Ok(CombinedDeclarator {
                name: None,
                type_: base_type,
            })
        }
    }
}
