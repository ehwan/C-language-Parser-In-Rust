use std::cell::RefCell;
use std::rc::Rc;

use super::expression;
use super::statement;
use super::Address;
use super::ArrayType;
use super::CVType;
use super::CombinedDeclarator;
use super::CompileError;
use super::Expression;
use super::FunctionType;
use super::PrimitiveType;
use super::Statement;
use super::StmtSwitchCase;
use super::StorageQualifier;
use super::VariableInfo;
use crate::ast;
use crate::ast2::StmtVariableDeclaration;

use super::scope::BlockScope;
use super::scope::FunctionDefinition;
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
    fn begin_switch_scope(&mut self) -> Result<(), CompileError> {
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
    fn end_switch_scope(&mut self) -> Result<SwitchScope, CompileError> {
        // @TODO pop every variable scope until switch scope
        unimplemented!("end_switch_scope")
    }
    fn begin_loop_scope(&mut self) -> Result<(), CompileError> {
        let scope = LoopScope {
            id: self.generate_scope_id(),
        };
        self.scopes.push(Scope::Loop(scope));
        Ok(())
    }
    fn end_loop_scope(&mut self) -> Result<LoopScope, CompileError> {
        // @TODO pop every variable scope until loop scope
        unimplemented!("end_loop_scope")
    }

    fn begin_scope(&mut self) -> Result<(), CompileError> {
        let scope = BlockScope {
            id: self.generate_scope_id(),
        };
        self.scopes.push(Scope::Block(scope));
        Ok(())
    }
    fn end_scope(&mut self) -> Result<BlockScope, CompileError> {
        // @TODO pop every variable scope until block scope
        unimplemented!("end_scope")
    }

    fn begin_function_scope(
        &mut self,
        name: String,
        type_: FunctionType,
    ) -> Result<(), CompileError> {
        unimplemented!("begin_function_scope")
    }
    fn end_function_scope(&mut self) -> Result<FunctionScope, CompileError> {
        // @TODO pop every variable scope until function scope
        unimplemented!("end_function_scope")
    }

    fn begin_variable_scope(
        &mut self,
        name: String,
        type_: CVType,
    ) -> Result<VariableInfo, CompileError> {
        // search for `name` in current scope
        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Variable(scope) => {
                    if scope.name == name {
                        return Err(CompileError::MultipleVariableDefinition(name));
                    }
                }
                _ => break,
            }
        }

        let size = self.type_sizeof(&type_.type_)?;
        let align = self.type_alignof(&type_.type_)?;
        let offset = self.function_scope.as_mut().unwrap().pool.add(size, align);
        let varinfo = VariableInfo {
            name: name.clone(),
            address: Address::Local(offset),
            cv_type: type_,
        };
        let scope = VariableScope {
            name,
            info: varinfo.clone(),
        };
        self.scopes.push(Scope::Variable(scope));
        Ok(varinfo)
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
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Expression, CompileError> {
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
    ) -> Result<CombinedDeclarator, CompileError> {
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
    ) -> Result<Statement, CompileError> {
        Ok(Statement::None)
    }
    pub(crate) fn process_statement_expression(
        &mut self,
        stmt: ast::StmtExpression,
    ) -> Result<Statement, CompileError> {
        Ok(Statement::Expression(statement::StmtExpression {
            expression: self.process_expression(stmt.expression)?,
        }))
    }
    pub(crate) fn process_statement_labeled(
        &mut self,
        stmt: ast::StmtLabeled,
    ) -> Result<Statement, CompileError> {
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
                Err(CompileError::MultipleLabelDefinition(label))
            } else {
                Ok(Statement::Labeled(statement::StmtLabeled {
                    label: label_info,
                    statement: Box::new(self.process_statement(*stmt.statement)?),
                }))
            }
        } else {
            Err(CompileError::LabelDefinitionOutsideFunction)
        }
    }
    pub(crate) fn process_statement_goto(
        &mut self,
        stmt: ast::StmtGoto,
    ) -> Result<Statement, CompileError> {
        if let Some(function_scope) = &mut self.function_scope {
            if let Some(label) = function_scope.labels.get(&stmt.label) {
                Ok(Statement::Goto(statement::StmtGoto {
                    label: Rc::clone(label),
                }))
            } else {
                Err(CompileError::GotoInvalidLabel(stmt.label))
            }
        } else {
            Err(CompileError::GotoOutsideFunction)
        }
    }
    pub(crate) fn process_statement_compound(
        &mut self,
        stmt: ast::StmtCompound,
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Statement, CompileError> {
        self.begin_switch_scope()?;
        let value = self.process_expression(stmt.target)?;
        let body = self.process_statement(*stmt.statement)?;
        self.end_switch_scope()?;

        let Statement::Compound(body) = body else {
            return Err(CompileError::InvalidSwitchBody);
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
                        return Err(CompileError::InvalidSwitchBody);
                    }
                }
            }
        }
        Ok(Statement::Switch(statement::StmtSwitch { value, cases }))
    }
    pub(crate) fn process_statement_case(
        &mut self,
        stmt: ast::StmtCase,
    ) -> Result<Statement, CompileError> {
        let value = self.process_expression(stmt.value)?;
        let statement = self.process_statement(*stmt.statement)?;
        if self.nearest_switch_scope().is_none() {
            return Err(CompileError::InvalidCase);
        }

        Ok(Statement::_Case(statement::StmtCase {
            value,
            statement: Box::new(statement),
        }))
    }
    pub(crate) fn process_statement_default(
        &mut self,
        stmt: ast::StmtDefault,
    ) -> Result<Statement, CompileError> {
        let statement = self.process_statement(*stmt.statement)?;
        if let Some(scope) = self.nearest_switch_scope() {
            if scope.default {
                return Err(CompileError::MultipleDefault);
            }
        } else {
            return Err(CompileError::InvalidDefault);
        }

        Ok(Statement::_Default(statement::StmtDefault {
            statement: Box::new(statement),
        }))
    }
    pub(crate) fn process_statement_continue(
        &mut self,
        _stmt: ast::StmtContinue,
    ) -> Result<Statement, CompileError> {
        if !self.can_continue() {
            return Err(CompileError::InvalidContinue);
        }

        Ok(Statement::Continue)
    }
    pub(crate) fn process_statement_break(
        &mut self,
        _stmt: ast::StmtBreak,
    ) -> Result<Statement, CompileError> {
        if !self.can_break() {
            return Err(CompileError::InvalidBreak);
        }

        Ok(Statement::Break)
    }
    pub(crate) fn process_statement_while(
        &mut self,
        stmt: ast::StmtWhile,
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Statement, CompileError> {
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
    ) -> Result<Statement, CompileError> {
        if !self.can_return() {
            return Err(CompileError::InvalidReturn);
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
    ) -> Result<Statement, CompileError> {
        // @TODO storage_qualifier check
        // currently ignore all storage qualifiers
        let (storage_qualifier, base_type) = self.process_decl_specs(stmt.specs.into_iter())?;

        match stmt.inits {
            Some(decl_inits) => {
                let mut pairs = Vec::new();
                // - variable definition
                // @TODO
                // - function declaration
                // - typedef
                for init in decl_inits.into_iter() {
                    let init_type = self.process_declarator(*init.declarator, base_type.clone())?;
                    let Some(name) = init_type.name else {
                        return Err(CompileError::DeclarationWithoutName);
                    };

                    match &init_type.cv_type.type_ {
                        // this is function declaration without body
                        PrimitiveType::Function(_func) => {
                            if self.function_scope.is_some() {
                                return Err(CompileError::NestedFunctionDefinition);
                            }

                            if let Some(old) = self.global_scope.variables.insert(
                                name.clone(),
                                VariableInfo {
                                    name,
                                    address: Address::Function(self.global_scope.functions.len()),
                                    cv_type: init_type.cv_type.clone(),
                                },
                            ) {
                                return Err(CompileError::MultipleVariableDefinition(old.name));
                            }
                            // body will be `None`, and be `Some` when function definition is found
                            self.global_scope
                                .functions
                                .push(Rc::new(RefCell::new(None)));
                        }
                        _ => {
                            if self.function_scope.is_some() {
                                let varinfo = self.begin_variable_scope(
                                    name.clone(),
                                    init_type.cv_type.clone(),
                                )?;
                                if let Some(init) = init.initializer {
                                    let init = self.process_expression(init)?;
                                    pairs.push((varinfo, init));
                                }
                            } else {
                                if self.global_scope.variables.contains_key(&name) {
                                    return Err(CompileError::MultipleVariableDefinition(name));
                                }

                                let size = self.type_sizeof(&init_type.cv_type.type_)?;
                                let align = self.type_alignof(&init_type.cv_type.type_)?;
                                let offset = self.global_scope.pool.add(size, align);
                                let varinfo = VariableInfo {
                                    name: name.clone(),
                                    address: Address::Global(offset),
                                    cv_type: init_type.cv_type,
                                };
                                self.global_scope.variables.insert(name, varinfo.clone());
                                if let Some(init) = init.initializer {
                                    let init = self.process_expression(init)?;
                                    pairs.push((varinfo, init));
                                } else {
                                    // @TODO
                                    // default value since this variable is in static storage
                                }
                            }
                        }
                    }
                }
                Ok(Statement::VariableDeclaration(StmtVariableDeclaration {
                    pairs,
                }))
            }
            None => {
                // struct, union, enum type definition
                // or just ill-formed declaration (but it's not an error)
                unreachable!("process_statement_declaration: struct, union, enum definition")
            }
        }
    }
    pub(crate) fn process_statement_functiondefinition(
        &mut self,
        stmt: ast::StmtFunctionDefinition,
    ) -> Result<Statement, CompileError> {
        if self.function_scope.is_some() {
            return Err(CompileError::NestedFunctionDefinition);
        }

        let (storage_qualifier, base_type) = match stmt.specs {
            Some(specs) => self.process_decl_specs(specs.into_iter())?,
            None => (
                StorageQualifier::new(),
                CVType::from_primitive(PrimitiveType::Void),
            ),
        };
        // @TODO storage_qualifier check
        let decl = self.process_declarator(stmt.decl, base_type)?;
        let name = match decl.name {
            Some(name) => name,
            None => return Err(CompileError::NoFunctionName),
        };

        let (function_definition, function_type) = match decl.cv_type.type_ {
            PrimitiveType::Function(func) => {
                self.begin_function_scope(name.clone(), func.clone())?;
                let body = self.process_statement(*stmt.body)?;
                let scope = self.end_function_scope()?;
                (
                    FunctionDefinition {
                        body: Box::new(body),
                        stack_size: scope.pool.max_size,
                    },
                    func,
                )
            }
            _ => return Err(CompileError::InvalidFunctionDefinition),
        };

        // check if
        //   - there is other variable with same name
        //   - there is function declaration with same signature

        if let Some(varinfo) = self.global_scope.variables.get(&name) {
            match &varinfo.cv_type.type_ {
                PrimitiveType::Function(func) => {
                    if func != &function_type {
                        return Err(CompileError::FunctionDifferentSignature(name));
                    }
                }
                _ => return Err(CompileError::MultipleVariableDefinition(name)),
            }
            let Address::Function(address) = varinfo.address else {
                unreachable!();
            };

            if self.global_scope.functions[address].borrow().is_some() {
                return Err(CompileError::MultipleFunctionDefinition(name));
            }
            self.global_scope.functions[address].replace(Some(function_definition));
        } else {
            let function_offset = self.global_scope.functions.len();
            self.global_scope
                .functions
                .push(Rc::new(RefCell::new(Some(function_definition))));

            let varinfo = VariableInfo {
                name: name.clone(),
                address: Address::Function(function_offset),
                cv_type: CVType::from_primitive(PrimitiveType::Function(function_type)),
            };
            self.global_scope.variables.insert(name, varinfo);
        }

        Ok(Statement::None)
    }
}

// for expressions
impl Context {
    pub fn expression_type(&self, expr: &Expression) -> CVType {
        match expr {
            Expression::Variable(var) => var.cv_type.clone(),
            Expression::I8(_) => CVType::from_primitive(PrimitiveType::Int8),
            Expression::I16(_) => CVType::from_primitive(PrimitiveType::Int16),
            Expression::I32(_) => CVType::from_primitive(PrimitiveType::Int32),
            Expression::I64(_) => CVType::from_primitive(PrimitiveType::Int64),
            Expression::U8(_) => CVType::from_primitive(PrimitiveType::UInt8),
            Expression::U16(_) => CVType::from_primitive(PrimitiveType::UInt16),
            Expression::U32(_) => CVType::from_primitive(PrimitiveType::UInt32),
            Expression::U64(_) => CVType::from_primitive(PrimitiveType::UInt64),
            Expression::F32(_) => CVType::from_primitive(PrimitiveType::Float32),
            Expression::F64(_) => CVType::from_primitive(PrimitiveType::Float64),
            Expression::String(_) => {
                CVType::from_primitive(PrimitiveType::Pointer(Box::new(CVType {
                    type_: PrimitiveType::Int8,
                    const_: true,
                    volatile: false,
                })))
            }
            Expression::Cast(expr) => expr.type_.clone(),
            Expression::Arrow(expr) => unimplemented!("expression_type Arrow"),
            Expression::Bracket(expr) => unimplemented!("expression_type Bracket"),
            Expression::Conditional(expr) => unimplemented!("expression_type Conditional"),
            Expression::InitializerList(expr) => unimplemented!("expression_type InitializerList"),
            Expression::Paren(expr) => unimplemented!("expression_type Paren"),
            Expression::Binary(expr) => unimplemented!("expression_type Binary"),
            Expression::Unary(expr) => unimplemented!("expression_type Unary"),
        }
    }
    pub(crate) fn process_expression_identifier(
        &mut self,
        expr: ast::ExprIdentifier,
    ) -> Result<Expression, CompileError> {
        if self.function_scope.is_some() {
            for scope in self.scopes.iter().rev() {
                match scope {
                    Scope::Variable(scope) => {
                        if scope.name == expr.name {
                            return Ok(Expression::Variable(scope.info.clone()));
                        }
                    }
                    _ => {}
                }
            }
        }
        if let Some(varinfo) = self.global_scope.variables.get(&expr.name) {
            return Ok(Expression::Variable(varinfo.clone()));
        }
        return Err(CompileError::VariableNotFound(expr.name));
    }
    pub(crate) fn process_expression_constantcharacter(
        &mut self,
        expr: ast::ExprConstantCharacter,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::I8(expr.value))
    }
    pub(crate) fn process_expression_constantinteger(
        &mut self,
        expr: ast::ExprConstantInteger,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::I32(expr.value))
    }
    pub(crate) fn process_expression_constantunsignedinteger(
        &mut self,
        expr: ast::ExprConstantUnsignedInteger,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::U32(expr.value))
    }
    pub(crate) fn process_expression_constantlong(
        &mut self,
        expr: ast::ExprConstantLong,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::I64(expr.value))
    }
    pub(crate) fn process_expression_constantunsignedlong(
        &mut self,
        expr: ast::ExprConstantUnsignedLong,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::U64(expr.value))
    }
    pub(crate) fn process_expression_constantfloat(
        &mut self,
        expr: ast::ExprConstantFloat,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::F32(expr.value))
    }
    pub(crate) fn process_expression_constantdouble(
        &mut self,
        expr: ast::ExprConstantDouble,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::F64(expr.value))
    }
    pub(crate) fn process_expression_string(
        &mut self,
        expr: ast::ExprString,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::String(expr.value))
    }
    pub(crate) fn process_expression_member(
        &mut self,
        expr: ast::ExprMember,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match self.expression_type(&src).type_ {
            PrimitiveType::Struct | PrimitiveType::Union => {}
            _ => return Err(CompileError::MemberAccessOnNonStructOrUnion),
        }
        unreachable!("process_expression_member")
    }
    pub(crate) fn process_expression_arrow(
        &mut self,
        expr: ast::ExprArrow,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match self.expression_type(&src).type_ {
            PrimitiveType::Pointer(_) => {}
            _ => return Err(CompileError::ArrowOnNonPointer),
        }

        Ok(Expression::Arrow(expression::ExprArrow {
            src: Box::new(src),
            member: expr.member,
        }))
    }
    pub(crate) fn process_expression_bracket(
        &mut self,
        expr: ast::ExprBracket,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match self.expression_type(&src).type_ {
            PrimitiveType::Array(_) | PrimitiveType::Pointer(_) => {}
            _ => return Err(CompileError::BracketOnNonArrayOrPointer),
        }
        let index = self.process_expression(*expr.index)?;
        if !self.expression_type(&index).type_.is_integer() {
            return Err(CompileError::BracketIndexNotInteger);
        }

        Ok(Expression::Bracket(expression::ExprBracket {
            src: Box::new(src),
            index: Box::new(index),
        }))
    }
    pub(crate) fn process_expression_paren(
        &mut self,
        expr: ast::ExprParen,
    ) -> Result<Expression, CompileError> {
        // @TODO callable check
        Ok(Expression::Paren(expression::ExprParen {
            src: Box::new(self.process_expression(*expr.src)?),
            args: expr
                .args
                .into_iter()
                .map(|expr| self.process_expression(expr))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
    pub(crate) fn process_expression_cast(
        &mut self,
        expr: ast::ExprCast,
    ) -> Result<Expression, CompileError> {
        let base_type = self.process_specs(expr.typename.specs.into_iter())?;
        let type_to = if let Some(decl) = expr.typename.declarator {
            let decl = self.process_declarator(*decl, base_type)?;
            decl.cv_type
        } else {
            base_type
        };
        // @TODO check castable
        Ok(Expression::Cast(expression::ExprCast {
            expr: Box::new(self.process_expression(*expr.src)?),
            type_: type_to,
        }))
    }
    pub(crate) fn process_expression_sizeoftype(
        &mut self,
        expr: ast::ExprSizeOfType,
    ) -> Result<Expression, CompileError> {
        let base_type = self.process_specs(expr.typename.specs.into_iter())?;
        let typename = if let Some(decl) = expr.typename.declarator {
            self.process_declarator(*decl, base_type)?.cv_type
        } else {
            base_type
        };
        Ok(Expression::U64(self.type_sizeof(&typename.type_)? as u64))
    }
    pub(crate) fn process_expression_sizeofexpr(
        &mut self,
        expr: ast::ExprSizeOfExpr,
    ) -> Result<Expression, CompileError> {
        let expr = self.process_expression(*expr.expr)?;
        let typename = self.expression_type(&expr);
        Ok(Expression::U64(self.type_sizeof(&typename.type_)? as u64))
    }
    pub(crate) fn process_expression_conditional(
        &mut self,
        expr: ast::ExprConditional,
    ) -> Result<Expression, CompileError> {
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
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        Ok(Expression::Unary(expression::ExprUnary {
            op: expr.op,
            expr: Box::new(src),
        }))
    }
    pub(crate) fn process_expression_binary(
        &mut self,
        expr: ast::ExprBinary,
    ) -> Result<Expression, CompileError> {
        let lhs = self.process_expression(*expr.lhs)?;
        let rhs = self.process_expression(*expr.rhs)?;
        Ok(Expression::Binary(expression::ExprBinary {
            op: expr.op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }
    pub(crate) fn process_expression_initializerlist(
        &mut self,
        expr: ast::ExprInitializerList,
    ) -> Result<Expression, CompileError> {
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
    pub fn type_sizeof(&self, typename: &PrimitiveType) -> Result<usize, CompileError> {
        Ok(match typename {
            PrimitiveType::Void => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::UInt8 | PrimitiveType::Int8 => 1,
            PrimitiveType::UInt16 | PrimitiveType::Int16 => 2,
            PrimitiveType::UInt32 | PrimitiveType::Int32 | PrimitiveType::Float32 => 4,
            PrimitiveType::UInt64 | PrimitiveType::Int64 | PrimitiveType::Float64 => 8,
            PrimitiveType::Pointer(_) => 8,
            PrimitiveType::Array(ArrayType { type_, size }) => {
                self.type_sizeof(&type_.type_)? * (*size)
            }
            PrimitiveType::Function(_) => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::Struct | PrimitiveType::Union | PrimitiveType::Enum => {
                unreachable!("size_of for struct/union/enum")
            }
        })
    }
    pub fn type_alignof(&self, typename: &PrimitiveType) -> Result<usize, CompileError> {
        Ok(match typename {
            PrimitiveType::Void => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::UInt8 | PrimitiveType::Int8 => 1,
            PrimitiveType::UInt16 | PrimitiveType::Int16 => 2,
            PrimitiveType::UInt32 | PrimitiveType::Int32 | PrimitiveType::Float32 => 4,
            PrimitiveType::UInt64 | PrimitiveType::Int64 | PrimitiveType::Float64 => 8,
            PrimitiveType::Pointer(_) => 8,
            PrimitiveType::Array(ArrayType { type_, size: _ }) => {
                self.type_alignof(&type_.type_)?
            }
            PrimitiveType::Function(_) => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::Struct | PrimitiveType::Union | PrimitiveType::Enum => {
                unreachable!("size_of for struct/union/enum")
            }
        })
    }

    pub(crate) fn process_specs(
        &mut self,
        mut specs: impl Iterator<Item = ast::SpecifierQualifier>,
    ) -> Result<CVType, CompileError> {
        unimplemented!("process_specs")
    }
    pub(crate) fn process_decl_specs(
        &mut self,
        mut specs: impl Iterator<Item = ast::DeclarationSpecifier>,
    ) -> Result<(StorageQualifier, CVType), CompileError> {
        unimplemented!("process_specs")
    }
    pub(crate) fn process_declarator_identifier(
        &mut self,
        decl: ast::DeclIdentifier,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        Ok(CombinedDeclarator {
            name: Some(decl.name),
            cv_type: base_type,
        })
    }
    pub(crate) fn process_declarator_pointer(
        &mut self,
        decl: ast::DeclPointer,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.cv_type = CVType::from_primitive(PrimitiveType::Pointer(Box::new(res.cv_type)));
            Ok(res)
        } else {
            Ok(CombinedDeclarator {
                name: None,
                cv_type: CVType::from_primitive(PrimitiveType::Pointer(Box::new(base_type))),
            })
        }
    }
    pub(crate) fn process_declarator_array_fixed(
        &mut self,
        decl: ast::DeclArrayFixed,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        let size = self.process_expression(decl.size)?;
        let size = match size {
            Expression::I8(v) => {
                if v < 0 {
                    return Err(CompileError::NegativeArraySize);
                } else {
                    v as usize
                }
            }
            Expression::I16(v) => {
                if v < 0 {
                    return Err(CompileError::NegativeArraySize);
                } else {
                    v as usize
                }
            }
            Expression::I32(v) => {
                if v < 0 {
                    return Err(CompileError::NegativeArraySize);
                } else {
                    v as usize
                }
            }
            Expression::I64(v) => {
                if v < 0 {
                    return Err(CompileError::NegativeArraySize);
                } else {
                    v as usize
                }
            }
            Expression::U8(v) => v as usize,
            Expression::U16(v) => v as usize,
            Expression::U32(v) => v as usize,
            Expression::U64(v) => v as usize,
            _ => return Err(CompileError::ArraySizeNotInteger),
        };
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.cv_type = CVType::from_primitive(PrimitiveType::Array(ArrayType {
                type_: Box::new(res.cv_type),
                size: size,
            }));
            Ok(res)
        } else {
            Ok(CombinedDeclarator {
                name: None,
                cv_type: CVType::from_primitive(PrimitiveType::Array(ArrayType {
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
    ) -> Result<CombinedDeclarator, CompileError> {
    }
    pub(crate) fn process_declarator_function(
        &mut self,
        decl: ast::DeclFunction,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        if let Some(decl) = decl.declarator {
            let res = self.process_declarator(*decl, base_type)?;
        } else {
        }
    }
    pub(crate) fn process_declarator_const(
        &mut self,
        decl: ast::DeclConst,
        mut base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.cv_type.const_ = true;
            Ok(res)
        } else {
            base_type.const_ = true;
            Ok(CombinedDeclarator {
                name: None,
                cv_type: base_type,
            })
        }
    }
    pub(crate) fn process_declarator_volatile(
        &mut self,
        decl: ast::DeclVolatile,
        mut base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.cv_type.volatile = true;
            Ok(res)
        } else {
            base_type.volatile = true;
            Ok(CombinedDeclarator {
                name: None,
                cv_type: base_type,
            })
        }
    }
}
