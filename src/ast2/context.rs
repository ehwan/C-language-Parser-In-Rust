use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use super::declarator;
use super::expression;
use super::statement;
use super::Address;
use super::ArrayType;
use super::CVType;
use super::CombinedDeclarator;
use super::CompileError;
use super::EnumType;
use super::ExprBinaryOp;
use super::ExprCast;
use super::ExprUnaryOp;
use super::Expression;
use super::Float;
use super::FunctionType;
use super::Integer;
use super::PrimitiveType;
use super::Statement;
use super::StmtSwitchCase;
use super::StmtVariableDeclaration;
use super::StorageQualifier;
use super::StructType;
use super::TranslationUnit;
use super::VariableInfo;
use crate::ast;

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

    /// constant text section
    pub text: Vec<u8>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            scope_counter: 0,
            global_scope: GlobalScope::new(),
            function_scope: None,
            scopes: Vec::new(),
            text: Vec::new(),
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
        loop {
            let scope = self.scopes.pop().unwrap();
            match scope {
                Scope::Switch(scope) => return Ok(scope),
                Scope::Variable(scope) => {
                    self.end_varaible_scope(scope)?;
                }
                _ => unreachable!("end_switch_scope: unexpected scope"),
            }
        }
    }
    fn begin_loop_scope(&mut self) -> Result<(), CompileError> {
        let scope = LoopScope {
            id: self.generate_scope_id(),
        };
        self.scopes.push(Scope::Loop(scope));
        Ok(())
    }
    fn end_loop_scope(&mut self) -> Result<LoopScope, CompileError> {
        loop {
            let scope = self.scopes.pop().unwrap();
            match scope {
                Scope::Loop(scope) => return Ok(scope),
                Scope::Variable(scope) => {
                    self.end_varaible_scope(scope)?;
                }
                _ => unreachable!("end_switch_scope: unexpected scope"),
            }
        }
    }

    fn begin_scope(&mut self) -> Result<(), CompileError> {
        let scope = BlockScope {
            id: self.generate_scope_id(),
            typedefs: Default::default(),
        };
        self.scopes.push(Scope::Block(scope));
        Ok(())
    }
    fn nearest_scope(&mut self) -> Option<&mut BlockScope> {
        for scope in self.scopes.iter_mut().rev() {
            match scope {
                Scope::Block(scope) => return Some(scope),
                _ => {}
            }
        }
        None
    }
    fn end_scope(&mut self) -> Result<BlockScope, CompileError> {
        loop {
            let scope = self.scopes.pop().unwrap();
            match scope {
                Scope::Block(scope) => return Ok(scope),
                Scope::Variable(scope) => {
                    self.end_varaible_scope(scope)?;
                }
                _ => unreachable!("end_switch_scope: unexpected scope"),
            }
        }
    }

    fn begin_function_scope(
        &mut self,
        name: String,
        type_: FunctionType,
    ) -> Result<(), CompileError> {
        if self.function_scope.is_some() {
            return Err(CompileError::NestedFunctionDefinition);
        }
        let scope = FunctionScope::new(name, type_);
        self.function_scope = Some(scope);
        Ok(())
    }
    fn end_function_scope(&mut self) -> Result<FunctionScope, CompileError> {
        while let Some(scope) = self.scopes.pop() {
            match scope {
                Scope::Variable(scope) => {
                    self.end_varaible_scope(scope)?;
                }
                _ => unreachable!("end_switch_scope: unexpected scope"),
            }
        }
        Ok(std::mem::take(&mut self.function_scope).unwrap())
    }

    fn begin_variable_scope(
        &mut self,
        name: String,
        cv_type: CVType,
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

        let size = cv_type.type_.sizeof()?;
        let align = cv_type.type_.alignof()?;
        let offset = self.function_scope.as_mut().unwrap().pool.push(size, align);
        let varinfo = VariableInfo {
            name: name.clone(),
            address: Address::Local(offset),
            cv_type,
        };
        let scope = VariableScope {
            name,
            info: varinfo.clone(),
        };
        self.scopes.push(Scope::Variable(scope));
        Ok(varinfo)
    }
    fn end_varaible_scope(&mut self, scope: VariableScope) -> Result<(), CompileError> {
        if let Some(func) = &mut self.function_scope {
            func.pool.pop();
        } else {
            self.global_scope.pool.pop();
            self.global_scope.variables.remove(&scope.name);
        }
        Ok(())
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

    pub fn process_translation_unit(
        &mut self,
        tu: ast::TranslationUnit,
    ) -> Result<TranslationUnit, CompileError> {
        let mut statements = Vec::new();

        for item in tu.statements.into_iter() {
            let statement = self.process_statement(item)?;
            statements.push(statement);
        }

        Ok(TranslationUnit {
            statements,
            text: std::mem::take(&mut self.text),
        })
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
        match cond.cv_type()?.type_ {
            PrimitiveType::Integer(_) => {}
            PrimitiveType::Pointer(_) => {}
            type_ => return Err(CompileError::InvalidIfCondition(type_)),
        }
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
        let mut default = None;
        for (idx, s) in body.statements.into_iter().enumerate() {
            match s {
                Statement::_Case(c) => {
                    cases.push(StmtSwitchCase {
                        value: Some(c.value),
                        statements: vec![*c.statement],
                    });
                }
                Statement::_Default(d) => {
                    default = Some(idx);
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
        Ok(Statement::Switch(statement::StmtSwitch {
            value,
            cases,
            default,
        }))
    }
    pub(crate) fn process_statement_case(
        &mut self,
        stmt: ast::StmtCase,
    ) -> Result<Statement, CompileError> {
        // @TODO comparable check
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
        if !cond.cv_type()?.type_.is_bool_castable() {
            return Err(CompileError::ConditionalNotBool);
        }

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
        if !cond.cv_type()?.type_.is_bool_castable() {
            return Err(CompileError::ConditionalNotBool);
        }

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
        if !cond.cv_type()?.type_.is_bool_castable() {
            return Err(CompileError::ConditionalNotBool);
        }
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
        let (_storage_qualifier, base_type) = self.process_decl_specs(stmt.specs.into_iter())?;

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
                                    let rhs_type = init.cv_type()?.type_;
                                    if !rhs_type.is_implicitly_castable(&varinfo.cv_type.type_) {
                                        return Err(CompileError::InitializeTypeMismatch(
                                            varinfo.cv_type.type_,
                                            init.cv_type()?.type_,
                                        ));
                                    }
                                    if rhs_type != varinfo.cv_type.type_ {
                                        // implicit cast
                                        let lhs_type = varinfo.cv_type.type_.clone();
                                        pairs.push((
                                            varinfo,
                                            Expression::Cast(ExprCast {
                                                type_: lhs_type,
                                                expr: Box::new(init),
                                            }),
                                        ));
                                    } else {
                                        pairs.push((varinfo, init));
                                    }
                                }
                            } else {
                                if self.global_scope.variables.contains_key(&name) {
                                    return Err(CompileError::MultipleVariableDefinition(name));
                                }

                                let size = init_type.cv_type.type_.sizeof()?;
                                let align = init_type.cv_type.type_.alignof()?;
                                let offset = self.global_scope.pool.push(size, align);
                                let varinfo = VariableInfo {
                                    name: name.clone(),
                                    address: Address::Global(offset),
                                    cv_type: init_type.cv_type,
                                };
                                self.global_scope.variables.insert(name, varinfo.clone());
                                if let Some(init) = init.initializer {
                                    let init = self.process_expression(init)?;
                                    let rhs_type = init.cv_type()?.type_;
                                    if !rhs_type.is_implicitly_castable(&varinfo.cv_type.type_) {
                                        return Err(CompileError::InitializeTypeMismatch(
                                            varinfo.cv_type.type_,
                                            init.cv_type()?.type_,
                                        ));
                                    }
                                    if rhs_type != varinfo.cv_type.type_ {
                                        // implicit cast
                                        let lhs_type = varinfo.cv_type.type_.clone();
                                        pairs.push((
                                            varinfo,
                                            Expression::Cast(ExprCast {
                                                type_: lhs_type,
                                                expr: Box::new(init),
                                            }),
                                        ));
                                    } else {
                                        pairs.push((varinfo, init));
                                    }
                                } else {
                                    // @TODO
                                    // default value since this variable is in static storage
                                }
                            }
                        }
                    }
                }
                if pairs.is_empty() {
                    Ok(Statement::None)
                } else {
                    Ok(Statement::VariableDeclaration(StmtVariableDeclaration {
                        pairs,
                    }))
                }
            }
            None => {
                // struct, union, enum type definition
                // or just ill-formed declaration (but it's not an error)

                match base_type.type_ {
                    PrimitiveType::Struct(struct_definition) => {
                        match struct_definition.body {
                            Some(struct_body) => {
                                let Some(name) = struct_definition.name else {
                                    // anonymous struct definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's struct declaration of same type

                                        let PrimitiveType::Struct(other_struct_def) =
                                            &mut old.type_
                                        else {
                                            // it is not struct,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old struct has definition, it's redifinition
                                        if other_struct_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_struct_def.body = Some(struct_body);
                                    } else {
                                        let struct_type = StructType {
                                            name: Some(name.clone()),
                                            body: Some(struct_body),
                                        };
                                        let struct_type = PrimitiveType::Struct(struct_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(struct_type));
                                    }
                                } else {
                                    // global scope

                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's struct declaration of same type

                                        let PrimitiveType::Struct(other_struct_def) =
                                            &mut old.type_
                                        else {
                                            // it is not struct,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old struct has definition, it's redifinition
                                        if other_struct_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_struct_def.body = Some(struct_body);
                                    } else {
                                        let struct_type = StructType {
                                            name: Some(name.clone()),
                                            body: Some(struct_body),
                                        };
                                        let struct_type = PrimitiveType::Struct(struct_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(struct_type));
                                    }
                                }
                            }
                            None => {
                                // declaration
                                let Some(name) = struct_definition.name else {
                                    // anonymous struct definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's struct declaration of same type

                                        let PrimitiveType::Struct(_other_struct_def) = &old.type_
                                        else {
                                            // it is not struct,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let struct_type = StructType {
                                            name: Some(name.clone()),
                                            body: None,
                                        };
                                        let struct_type = PrimitiveType::Struct(struct_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(struct_type));
                                    }
                                } else {
                                    // global scope
                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's struct declaration of same type

                                        let PrimitiveType::Struct(_other_struct_def) = &old.type_
                                        else {
                                            // it is not struct,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let struct_type = StructType {
                                            name: Some(name.clone()),
                                            body: None,
                                        };
                                        let struct_type = PrimitiveType::Struct(struct_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(struct_type));
                                    }
                                }
                            }
                        }
                        Ok(Statement::None)
                    }
                    PrimitiveType::Union(union_definition) => {
                        match union_definition.body {
                            Some(union_body) => {
                                let Some(name) = union_definition.name else {
                                    // anonymous union definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's union declaration of same type

                                        let PrimitiveType::Union(other_union_def) = &mut old.type_
                                        else {
                                            // it is not union,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old union has definition, it's redifinition
                                        if other_union_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_union_def.body = Some(union_body);
                                    } else {
                                        let union_type = StructType {
                                            name: Some(name.clone()),
                                            body: Some(union_body),
                                        };
                                        let union_type = PrimitiveType::Union(union_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(union_type));
                                    }
                                } else {
                                    // global scope

                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's union declaration of same type

                                        let PrimitiveType::Union(other_union_def) = &mut old.type_
                                        else {
                                            // it is not union,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old union has definition, it's redifinition
                                        if other_union_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_union_def.body = Some(union_body);
                                    } else {
                                        let union_type = StructType {
                                            name: Some(name.clone()),
                                            body: Some(union_body),
                                        };
                                        let union_type = PrimitiveType::Union(union_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(union_type));
                                    }
                                }
                            }
                            None => {
                                // declaration
                                let Some(name) = union_definition.name else {
                                    // anonymous union definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's union declaration of same type

                                        let PrimitiveType::Union(_other_union_def) = &old.type_
                                        else {
                                            // it is not union,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let union_type = StructType {
                                            name: Some(name.clone()),
                                            body: None,
                                        };
                                        let union_type = PrimitiveType::Union(union_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(union_type));
                                    }
                                } else {
                                    // global scope
                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's union declaration of same type

                                        let PrimitiveType::Union(_other_union_def) = &old.type_
                                        else {
                                            // it is not union,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let union_type = StructType {
                                            name: Some(name.clone()),
                                            body: None,
                                        };
                                        let union_type = PrimitiveType::Union(union_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(union_type));
                                    }
                                }
                            }
                        }
                        Ok(Statement::None)
                    }
                    PrimitiveType::Enum(enum_definition) => {
                        match enum_definition.body {
                            Some(enum_body) => {
                                let Some(name) = enum_definition.name else {
                                    // anonymous enum definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's enum declaration of same type

                                        let PrimitiveType::Enum(other_enum_def) = &mut old.type_
                                        else {
                                            // it is not enum,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old enum has definition, it's redifinition
                                        if other_enum_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_enum_def.body = Some(enum_body);
                                    } else {
                                        let enum_type = EnumType {
                                            name: Some(name.clone()),
                                            body: Some(enum_body),
                                            type_: enum_definition.type_,
                                        };
                                        let enum_type = PrimitiveType::Enum(enum_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(enum_type));
                                    }
                                } else {
                                    // global scope

                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get_mut(&name) {
                                        // type name `name` exists.
                                        // check if it's enum declaration of same type

                                        let PrimitiveType::Enum(other_enum_def) = &mut old.type_
                                        else {
                                            // it is not enum,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };

                                        // if old enum has definition, it's redifinition
                                        if other_enum_def.body.is_some() {
                                            return Err(CompileError::TypeRedifinition(name));
                                        }
                                        other_enum_def.body = Some(enum_body);
                                    } else {
                                        let enum_type = EnumType {
                                            name: Some(name.clone()),
                                            body: Some(enum_body),
                                            type_: enum_definition.type_,
                                        };
                                        let enum_type = PrimitiveType::Enum(enum_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(enum_type));
                                    }
                                }
                            }
                            None => {
                                // declaration
                                let Some(name) = enum_definition.name else {
                                    // anonymous enum definition, ill-formed.
                                    // skip this statement
                                    return Ok(Statement::None);
                                };
                                if let Some(current_scope) = self.nearest_scope() {
                                    // check if there is other variable with same name
                                    if let Some(old) = current_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's enum declaration of same type

                                        let PrimitiveType::Enum(_other_enum_def) = &old.type_
                                        else {
                                            // it is not enum,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let enum_type = EnumType {
                                            name: Some(name.clone()),
                                            body: None,
                                            type_: enum_definition.type_,
                                        };
                                        let enum_type = PrimitiveType::Enum(enum_type);
                                        current_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(enum_type));
                                    }
                                } else {
                                    // global scope
                                    // check if there is other variable with same name
                                    if let Some(old) = self.global_scope.typedefs.get(&name) {
                                        // type name `name` exists.
                                        // check if it's enum declaration of same type

                                        let PrimitiveType::Enum(_other_enum_def) = &old.type_
                                        else {
                                            // it is not enum,
                                            // redifinition of type name `name`
                                            return Err(CompileError::TypeRedifinition(name));
                                        };
                                    } else {
                                        let enum_type = EnumType {
                                            name: Some(name.clone()),
                                            body: None,
                                            type_: enum_definition.type_,
                                        };
                                        let enum_type = PrimitiveType::Enum(enum_type);
                                        self.global_scope
                                            .typedefs
                                            .insert(name, CVType::from_primitive(enum_type));
                                    }
                                }
                            }
                        }
                        Ok(Statement::None)
                    }

                    _ => {
                        // ill-formed declaration
                        // int; <--
                        // but it's not an error
                        Ok(Statement::None)
                    }
                }
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

        let (_storage_qualifier, base_type) = match stmt.specs {
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
                self.begin_scope()?;
                let body = self.process_statement(*stmt.body)?;
                self.end_scope()?;
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
        Ok(Expression::Signed(expr.value as i64, Integer::Int8))
    }
    pub(crate) fn process_expression_constantinteger(
        &mut self,
        expr: ast::ExprConstantInteger,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Signed(expr.value as i64, Integer::Int32))
    }
    pub(crate) fn process_expression_constantunsignedinteger(
        &mut self,
        expr: ast::ExprConstantUnsignedInteger,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Unsigned(expr.value as u64, Integer::UInt32))
    }
    pub(crate) fn process_expression_constantlong(
        &mut self,
        expr: ast::ExprConstantLong,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Signed(expr.value, Integer::Int64))
    }
    pub(crate) fn process_expression_constantunsignedlong(
        &mut self,
        expr: ast::ExprConstantUnsignedLong,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Unsigned(expr.value, Integer::UInt64))
    }
    pub(crate) fn process_expression_constantfloat(
        &mut self,
        expr: ast::ExprConstantFloat,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Float(expr.value as f64, Float::Float32))
    }
    pub(crate) fn process_expression_constantdouble(
        &mut self,
        expr: ast::ExprConstantDouble,
    ) -> Result<Expression, CompileError> {
        Ok(Expression::Float(expr.value, Float::Float64))
    }
    pub(crate) fn process_expression_string(
        &mut self,
        expr: ast::ExprString,
    ) -> Result<Expression, CompileError> {
        let addr = self.text.len();
        self.text.reserve(expr.value.len() + 1);
        self.text.extend_from_slice(expr.value.as_bytes());
        self.text.push(0u8);
        Ok(Expression::String(Address::Text(addr)))
    }
    pub(crate) fn process_expression_cast(
        &mut self,
        expr: ast::ExprCast,
    ) -> Result<Expression, CompileError> {
        let base_type = self.process_specs(expr.typename.specs.into_iter())?;
        let type_to = if let Some(decl) = expr.typename.declarator {
            let decl = self.process_declarator(*decl, base_type)?;
            decl.cv_type.type_
        } else {
            base_type.type_
        };
        let src = self.process_expression(*expr.src)?;
        if !src.cv_type()?.type_.is_castable(&type_to) {
            return Err(CompileError::InvalidCast(src.cv_type()?.type_, type_to));
        } else {
            Ok(Expression::Cast(expression::ExprCast {
                expr: Box::new(src),
                type_: type_to,
            }))
        }
    }
    pub(crate) fn process_expression_bracket(
        &mut self,
        expr: ast::ExprBracket,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match src.cv_type()?.type_ {
            PrimitiveType::Array(_) | PrimitiveType::Pointer(_) => {}
            _ => return Err(CompileError::BracketOnNonArrayOrPointer),
        }
        let index = self.process_expression(*expr.index)?;
        if !matches!(index.cv_type()?.type_, PrimitiveType::Integer(_)) {
            return Err(CompileError::BracketIndexNotInteger);
        }

        Ok(Expression::Bracket(expression::ExprBracket {
            src: Box::new(src),
            index: Box::new(index),
        }))
    }
    pub(crate) fn process_expression_conditional(
        &mut self,
        expr: ast::ExprConditional,
    ) -> Result<Expression, CompileError> {
        let cond = self.process_expression(*expr.cond)?;
        let then = self.process_expression(*expr.then_expr)?;
        let else_ = self.process_expression(*expr.else_expr)?;
        if !cond.cv_type()?.type_.is_bool_castable() {
            return Err(CompileError::ConditionalNotBool);
        }
        match &cond {
            Expression::Signed(x, _) => {
                if *x == 0 {
                    return Ok(else_);
                } else {
                    return Ok(then);
                }
            }
            Expression::Unsigned(x, _) => {
                if *x == 0 {
                    return Ok(else_);
                } else {
                    return Ok(then);
                }
            }
            _ => {}
        }
        let then_type = then.cv_type()?.type_;
        let else_type = else_.cv_type()?.type_;
        let common_type = match then_type.common_type(&else_type) {
            Some(common) => common,
            None => return Err(CompileError::NoCommonType(then_type, else_type)),
        };
        Ok(Expression::Conditional(expression::ExprConditional {
            cond: Box::new(cond),
            then_expr: if then_type != common_type {
                Box::new(Expression::Cast(super::ExprCast {
                    expr: Box::new(then),
                    type_: common_type.clone(),
                }))
            } else {
                Box::new(then)
            },
            else_expr: if else_type != common_type {
                Box::new(Expression::Cast(super::ExprCast {
                    expr: Box::new(else_),
                    type_: common_type,
                }))
            } else {
                Box::new(else_)
            },
        }))
    }

    pub(crate) fn process_expression_member(
        &mut self,
        expr: ast::ExprMember,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match src.cv_type()?.type_ {
            PrimitiveType::Struct(s) | PrimitiveType::Union(s) => {
                let Some(body) = &s.body else {
                    return Err(CompileError::MemberOnIncompleteType);
                };

                for member in body.members.iter() {
                    if expr.member == member.name {
                        return Ok(Expression::Member(expression::ExprMember {
                            src: Box::new(src),
                            member_offset: member.offset,
                            member_type: member.cv_type.clone(),
                        }));
                    }
                }

                return Err(CompileError::MemberNotFound(expr.member));
            }

            _ => return Err(CompileError::MemberOnNonStructOrUnion),
        }
    }
    pub(crate) fn process_expression_arrow(
        &mut self,
        expr: ast::ExprArrow,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        let PrimitiveType::Pointer(src_type) = src.cv_type()?.type_ else {
            return Err(CompileError::ArrowOnNonPointer);
        };
        match src_type.type_ {
            PrimitiveType::Struct(s) | PrimitiveType::Union(s) => {
                let Some(body) = &s.body else {
                    return Err(CompileError::ArrowOnIncompleteType);
                };

                for member in body.members.iter() {
                    if expr.member == member.name {
                        return Ok(Expression::Arrow(expression::ExprMember {
                            src: Box::new(src),
                            member_offset: member.offset,
                            member_type: member.cv_type.clone(),
                        }));
                    }
                }

                return Err(CompileError::ArrowNotFound(expr.member));
            }

            _ => return Err(CompileError::ArrowOnNonStructOrUnion),
        }
    }
    pub(crate) fn process_expression_paren(
        &mut self,
        expr: ast::ExprParen,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        match src.cv_type()?.type_ {
            PrimitiveType::Function(func_info) => {
                if expr.args.len() != func_info.args.len() {
                    if !func_info.variadic || !(expr.args.len() > func_info.args.len()) {
                        return Err(CompileError::CallWithWrongNumberOfArguments);
                    }
                }

                let args = expr
                    .args
                    .into_iter()
                    .enumerate()
                    .map(|(idx, expr)| -> Result<Expression, CompileError> {
                        let arg_expr = self.process_expression(expr)?;
                        let arg_type = arg_expr.cv_type()?.type_;

                        if !arg_type.is_implicitly_castable(&func_info.args[idx].type_) {
                            return Err(CompileError::CallWithWrongArgumentType);
                        }
                        Ok(arg_expr)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Expression::Paren(expression::ExprParen {
                    src: Box::new(src),
                    args,
                }))
            }
            _ => return Err(CompileError::CallNonFunction),
        }
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
        Ok(Expression::Unsigned(
            typename.type_.sizeof()? as u64,
            Integer::UInt64,
        ))
    }
    pub(crate) fn process_expression_sizeofexpr(
        &mut self,
        expr: ast::ExprSizeOfExpr,
    ) -> Result<Expression, CompileError> {
        let expr = self.process_expression(*expr.expr)?;
        let typename = expr.cv_type()?;
        Ok(Expression::Unsigned(
            typename.type_.sizeof()? as u64,
            Integer::UInt64,
        ))
    }
    pub(crate) fn process_expression_unary(
        &mut self,
        expr: ast::ExprUnary,
    ) -> Result<Expression, CompileError> {
        let src = self.process_expression(*expr.src)?;
        let src_type = src.cv_type()?;
        match expr.op {
            ExprUnaryOp::AddressOf => {
                if !src.is_address() {
                    return Err(CompileError::NotAssignable);
                }
            }
            ExprUnaryOp::BitwiseNot => {
                if !matches!(src_type.type_, PrimitiveType::Integer(_)) {
                    return Err(CompileError::BitwiseOpOnNonInteger);
                }
                // compile-time constant
                match src {
                    Expression::Signed(x, type_) => {
                        return Ok(Expression::Unsigned((!x) as u64, type_.to_unsigned()));
                    }
                    Expression::Unsigned(x, type_) => {
                        return Ok(Expression::Unsigned(!x as u64, type_.to_unsigned()));
                    }
                    _ => {}
                }
            }
            ExprUnaryOp::DecrementPost => {
                if !matches!(
                    src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_)
                ) {
                    return Err(CompileError::BitwiseOpOnNonInteger);
                }
                if src_type.const_ {
                    return Err(CompileError::AssignToConst);
                }
                if !src.is_address() {
                    return Err(CompileError::NotAssignable);
                }
            }
            ExprUnaryOp::DecrementPre => {
                if !matches!(
                    src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_)
                ) {
                    return Err(CompileError::BitwiseOpOnNonInteger);
                }
                if src_type.const_ {
                    return Err(CompileError::AssignToConst);
                }
                if !src.is_address() {
                    return Err(CompileError::NotAssignable);
                }
            }
            ExprUnaryOp::IncrementPost => {
                if !matches!(
                    src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_)
                ) {
                    return Err(CompileError::BitwiseOpOnNonInteger);
                }
                if src_type.const_ {
                    return Err(CompileError::AssignToConst);
                }
                if !src.is_address() {
                    return Err(CompileError::NotAssignable);
                }
            }
            ExprUnaryOp::IncrementPre => {
                if !matches!(
                    src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_)
                ) {
                    return Err(CompileError::BitwiseOpOnNonInteger);
                }
                if src_type.const_ {
                    return Err(CompileError::AssignToConst);
                }
                if !src.is_address() {
                    return Err(CompileError::NotAssignable);
                }
            }
            ExprUnaryOp::LogicalNot => {
                if !src_type.type_.is_bool_castable() {
                    return Err(CompileError::LogicalOpOnNonBool);
                }

                // compile-time constant
                match src {
                    Expression::Signed(x, _) => {
                        return Ok(Expression::Signed(
                            if x == 0 { 1 } else { 0 },
                            Integer::Int32,
                        ))
                    }
                    Expression::Unsigned(x, _) => {
                        return Ok(Expression::Signed(
                            if x == 0 { 1 } else { 0 },
                            Integer::Int32,
                        ))
                    }
                    _ => {}
                }
            }
            ExprUnaryOp::Minus => {
                if !matches!(
                    &src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Float(_)
                ) {
                    return Err(CompileError::ArithmeticOpOnNonNumeric);
                }
                match src {
                    Expression::Signed(x, type_) => return Ok(Expression::Signed(-x, type_)),
                    Expression::Unsigned(x, type_) => {
                        return Ok(Expression::Unsigned((-(x as i64)) as u64, type_));
                    }
                    Expression::Float(x, type_) => return Ok(Expression::Float(-x, type_)),
                    _ => {}
                }
            }
            ExprUnaryOp::Dereference => {
                if !matches!(
                    src_type.type_,
                    PrimitiveType::Pointer(_) | PrimitiveType::Array(_)
                ) {
                    return Err(CompileError::DereferenceOnNonPointer);
                }
            }
            ExprUnaryOp::Plus => {
                if !matches!(
                    &src_type.type_,
                    PrimitiveType::Integer(_) | PrimitiveType::Float(_)
                ) {
                    return Err(CompileError::ArithmeticOpOnNonNumeric);
                }
                return Ok(src);
            }
        }
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
        let lhs_type = lhs.cv_type()?.type_;
        let rhs_type = rhs.cv_type()?.type_;

        match expr.op {
            ExprBinaryOp::Add => match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        lhs
                    };

                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(f),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(a), PrimitiveType::Pointer(p)) => {
                    let sizeof = p.sizeof()? as u64;

                    let rhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let lhs = if a.sizeof() != 64 {
                        if a.is_signed() {
                            let lhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            lhs
                        } else {
                            let lhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            lhs
                        }
                    } else {
                        if a.is_signed() {
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            lhs
                        } else {
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            lhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(a), PrimitiveType::Array(p)) => {
                    let sizeof = p.cv_type.sizeof()? as u64;

                    let rhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let lhs = if a.sizeof() != 64 {
                        if a.is_signed() {
                            let lhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            lhs
                        } else {
                            let lhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            lhs
                        }
                    } else {
                        if a.is_signed() {
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            lhs
                        } else {
                            let lhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(lhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            lhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(f),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        lhs
                    };

                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(p), PrimitiveType::Integer(b)) => {
                    let sizeof = p.sizeof()? as u64;

                    let lhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let rhs = if b.sizeof() != 64 {
                        if b.is_signed() {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    } else {
                        if b.is_signed() {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(p), PrimitiveType::Integer(b)) => {
                    let sizeof = p.cv_type.sizeof()? as u64;

                    let lhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let rhs = if b.sizeof() != 64 {
                        if b.is_signed() {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    } else {
                        if b.is_signed() {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Add,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
            },

            ExprBinaryOp::Sub => match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        lhs
                    };

                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(f),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(f),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        lhs
                    };

                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(p), PrimitiveType::Integer(b)) => {
                    let sizeof = p.sizeof()? as u64;

                    let lhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let rhs = if b.sizeof() != 64 {
                        if b.is_signed() {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    } else {
                        if b.is_signed() {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Sub,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(p), PrimitiveType::Integer(b)) => {
                    let sizeof = p.cv_type.sizeof()? as u64;

                    let lhs = Expression::Cast(expression::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Integer(Integer::UInt64),
                    });

                    let rhs = if b.sizeof() != 64 {
                        if b.is_signed() {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::Int64),
                            });

                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Cast(expression::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::UInt64),
                            });
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    } else {
                        if b.is_signed() {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Signed(sizeof as i64, Integer::Int64)),
                            });
                            rhs
                        } else {
                            let rhs = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mul,
                                lhs: Box::new(rhs),
                                rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                            });
                            rhs
                        }
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: ExprBinaryOp::Sub,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
            },
            ExprBinaryOp::AddAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                let lhs_cloned = lhs.clone();
                let binary_op = match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        let common_type = a.common_type(&b);
                        let lhs = if a != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            })
                        } else {
                            lhs
                        };

                        let rhs = if b != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            })
                        } else {
                            rhs
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }
                    (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => {
                        let lhs = Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(f),
                        });

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }

                    (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => {
                        let rhs = Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(f),
                        });

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }
                    (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                        let common_type = a.common_type(&b);
                        let lhs = if a != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Float(common_type.clone()),
                            })
                        } else {
                            lhs
                        };

                        let rhs = if b != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Float(common_type.clone()),
                            })
                        } else {
                            rhs
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }

                    (PrimitiveType::Pointer(p), PrimitiveType::Integer(b)) => {
                        let sizeof = p.sizeof()? as u64;

                        let lhs = Expression::Cast(expression::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(Integer::UInt64),
                        });

                        let rhs = if b.sizeof() != 64 {
                            if b.is_signed() {
                                let rhs = Expression::Cast(expression::ExprCast {
                                    expr: Box::new(rhs),
                                    type_: PrimitiveType::Integer(Integer::Int64),
                                });

                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Signed(
                                        sizeof as i64,
                                        Integer::Int64,
                                    )),
                                });
                                rhs
                            } else {
                                let rhs = Expression::Cast(expression::ExprCast {
                                    expr: Box::new(rhs),
                                    type_: PrimitiveType::Integer(Integer::UInt64),
                                });
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                                });
                                rhs
                            }
                        } else {
                            if b.is_signed() {
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Signed(
                                        sizeof as i64,
                                        Integer::Int64,
                                    )),
                                });
                                rhs
                            } else {
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                                });
                                rhs
                            }
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: ExprBinaryOp::Add,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }

                    _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
                };

                let rhs = if lhs_cloned.cv_type()?.type_ != binary_op.cv_type()?.type_ {
                    Expression::Cast(super::ExprCast {
                        expr: Box::new(binary_op),
                        type_: lhs_cloned.cv_type()?.type_.clone(),
                    })
                } else {
                    binary_op
                };

                let assign_op = Expression::Binary(expression::ExprBinary {
                    op: ExprBinaryOp::Assign,
                    lhs: Box::new(lhs_cloned),
                    rhs: Box::new(rhs),
                });
                return Ok(assign_op);
            }
            ExprBinaryOp::SubAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                let lhs_cloned = lhs.clone();
                let binary_op = match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        let common_type = a.common_type(&b);
                        let lhs = if a != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            })
                        } else {
                            lhs
                        };

                        let rhs = if b != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            })
                        } else {
                            rhs
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }
                    (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => {
                        let lhs = Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(f),
                        });

                        return Ok(Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }));
                    }

                    (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => {
                        let rhs = Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(f),
                        });

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }
                    (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                        let common_type = a.common_type(&b);
                        let lhs = if a != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Float(common_type.clone()),
                            })
                        } else {
                            lhs
                        };

                        let rhs = if b != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Float(common_type.clone()),
                            })
                        } else {
                            rhs
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }

                    (PrimitiveType::Pointer(p), PrimitiveType::Integer(b)) => {
                        let sizeof = p.sizeof()? as u64;

                        let lhs = Expression::Cast(expression::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(Integer::UInt64),
                        });

                        let rhs = if b.sizeof() != 64 {
                            if b.is_signed() {
                                let rhs = Expression::Cast(expression::ExprCast {
                                    expr: Box::new(rhs),
                                    type_: PrimitiveType::Integer(Integer::Int64),
                                });

                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Signed(
                                        sizeof as i64,
                                        Integer::Int64,
                                    )),
                                });
                                rhs
                            } else {
                                let rhs = Expression::Cast(expression::ExprCast {
                                    expr: Box::new(rhs),
                                    type_: PrimitiveType::Integer(Integer::UInt64),
                                });
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                                });
                                rhs
                            }
                        } else {
                            if b.is_signed() {
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Signed(
                                        sizeof as i64,
                                        Integer::Int64,
                                    )),
                                });
                                rhs
                            } else {
                                let rhs = Expression::Binary(expression::ExprBinary {
                                    op: ExprBinaryOp::Mul,
                                    lhs: Box::new(rhs),
                                    rhs: Box::new(Expression::Unsigned(sizeof, Integer::UInt64)),
                                });
                                rhs
                            }
                        };

                        Expression::Binary(expression::ExprBinary {
                            op: ExprBinaryOp::Sub,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }

                    _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
                };

                let rhs = if lhs_cloned.cv_type()?.type_ != binary_op.cv_type()?.type_ {
                    Expression::Cast(super::ExprCast {
                        expr: Box::new(binary_op),
                        type_: lhs_cloned.cv_type()?.type_.clone(),
                    })
                } else {
                    binary_op
                };

                let assign_op = Expression::Binary(expression::ExprBinary {
                    op: ExprBinaryOp::Assign,
                    lhs: Box::new(lhs_cloned),
                    rhs: Box::new(rhs),
                });
                return Ok(assign_op);
            }
            ExprBinaryOp::Mul | ExprBinaryOp::Div => match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);

                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(b),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(a),
                    });

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);

                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
            },
            ExprBinaryOp::MulAssign | ExprBinaryOp::DivAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                let common_type = match (&lhs_type, &rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        PrimitiveType::Integer(a.common_type(&b))
                    }
                    (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => {
                        PrimitiveType::Float(*f)
                    }

                    (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => {
                        PrimitiveType::Float(*f)
                    }
                    (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                        PrimitiveType::Float(a.common_type(&b))
                    }

                    _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
                };

                let lhs_casted = if lhs_type != common_type {
                    Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs.clone()),
                        type_: common_type.clone(),
                    })
                } else {
                    lhs.clone()
                };
                let rhs = if rhs_type != common_type {
                    Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: common_type.clone(),
                    })
                } else {
                    rhs
                };

                let op = Expression::Binary(expression::ExprBinary {
                    op: expr.op,
                    lhs: Box::new(lhs_casted),
                    rhs: Box::new(rhs),
                });
                let assign_op = Expression::Binary(expression::ExprBinary {
                    op: ExprBinaryOp::Assign,
                    lhs: Box::new(lhs),
                    rhs: Box::new(op),
                });
                return Ok(assign_op);
            }
            ExprBinaryOp::Mod => match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);

                    let lhs = if a != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type.clone()),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
            },
            ExprBinaryOp::ModAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        let common_type = a.common_type(&b);

                        let rhs = if b != common_type {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            })
                        } else {
                            rhs
                        };

                        if a != common_type {
                            let lhs_casted = Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs.clone()),
                                type_: PrimitiveType::Integer(common_type.clone()),
                            });
                            let mod_op = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mod,
                                lhs: Box::new(lhs_casted),
                                rhs: Box::new(rhs),
                            });
                            let mod_op = Expression::Cast(super::ExprCast {
                                expr: Box::new(mod_op),
                                type_: PrimitiveType::Integer(a.clone()),
                            });
                            let assign_op = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Assign,
                                lhs: Box::new(lhs),
                                rhs: Box::new(mod_op),
                            });

                            return Ok(assign_op);
                        } else {
                            let lhs_casted = lhs.clone();
                            let mod_op = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Mod,
                                lhs: Box::new(lhs_casted),
                                rhs: Box::new(rhs),
                            });
                            let assign_op = Expression::Binary(expression::ExprBinary {
                                op: ExprBinaryOp::Assign,
                                lhs: Box::new(lhs),
                                rhs: Box::new(mod_op),
                            });

                            return Ok(assign_op);
                        }
                    }

                    _ => return Err(CompileError::ArithmeticOpOnNonNumeric),
                }
            }
            ExprBinaryOp::BitwiseAnd | ExprBinaryOp::BitwiseOr | ExprBinaryOp::BitwiseXor => {
                match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        let common = a.common_type(&b);
                        let lhs = if a != common {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(lhs),
                                type_: PrimitiveType::Integer(common.clone()),
                            })
                        } else {
                            lhs
                        };
                        let rhs = if b != common {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(common.clone()),
                            })
                        } else {
                            rhs
                        };

                        return Ok(Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }));
                    }
                    _ => return Err(CompileError::BitwiseOpOnNonInteger),
                }
            }
            ExprBinaryOp::BitwiseAndAssign
            | ExprBinaryOp::BitwiseOrAssign
            | ExprBinaryOp::BitwiseXorAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                        let rhs = if a != b {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(a.clone()),
                            })
                        } else {
                            rhs
                        };
                        return Ok(Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }));
                    }
                    _ => return Err(CompileError::BitwiseOpOnNonInteger),
                }
            }
            ExprBinaryOp::ShiftLeft | ExprBinaryOp::ShiftRight => match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(_), PrimitiveType::Integer(b)) => {
                    let rhs = if b != Integer::UInt8 {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(Integer::UInt8),
                        })
                    } else {
                        rhs
                    };

                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                _ => return Err(CompileError::BitwiseOpOnNonInteger),
            },
            ExprBinaryOp::ShiftLeftAssign | ExprBinaryOp::ShiftRightAssign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }

                match (lhs_type, rhs_type) {
                    (PrimitiveType::Integer(_), PrimitiveType::Integer(b)) => {
                        let rhs = if b != Integer::UInt8 {
                            Expression::Cast(super::ExprCast {
                                expr: Box::new(rhs),
                                type_: PrimitiveType::Integer(Integer::UInt8),
                            })
                        } else {
                            rhs
                        };

                        return Ok(Expression::Binary(expression::ExprBinary {
                            op: expr.op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        }));
                    }
                    _ => return Err(CompileError::BitwiseOpOnNonInteger),
                }
            }
            ExprBinaryOp::Equal => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::NotEqual => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::LessThan => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::LessThanOrEqual => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::GreaterThan => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::GreaterThanOrEqual => match (&lhs_type, &rhs_type) {
                (PrimitiveType::Integer(a), PrimitiveType::Integer(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Integer(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Integer(_), PrimitiveType::Float(b)) => {
                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Float(*b),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Float(a), PrimitiveType::Integer(_)) => {
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Float(*a),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Float(a), PrimitiveType::Float(b)) => {
                    let common_type = a.common_type(&b);
                    let lhs = if a != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(lhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        lhs
                    };
                    let rhs = if b != &common_type {
                        Expression::Cast(super::ExprCast {
                            expr: Box::new(rhs),
                            type_: PrimitiveType::Float(common_type),
                        })
                    } else {
                        rhs
                    };
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => {
                    if a != b {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }
                }
                (PrimitiveType::Pointer(a), PrimitiveType::Array(b)) => {
                    if a != &b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Pointer(b)) => {
                    if &a.cv_type != b {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                (PrimitiveType::Array(a), PrimitiveType::Array(b)) => {
                    if a.cv_type != b.cv_type {
                        return Err(CompileError::DistinctPointer(
                            a.cv_type.as_ref().clone(),
                            b.cv_type.as_ref().clone(),
                        ));
                    }

                    let lhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(lhs),
                        type_: PrimitiveType::Pointer(a.cv_type.clone()),
                    });
                    let rhs = Expression::Cast(super::ExprCast {
                        expr: Box::new(rhs),
                        type_: PrimitiveType::Pointer(b.cv_type.clone()),
                    });
                    return Ok(Expression::Binary(expression::ExprBinary {
                        op: expr.op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }

                _ => return Err(CompileError::InvalidOperandType(lhs_type, rhs_type)),
            },
            ExprBinaryOp::LogicalAnd => {
                if !lhs_type.is_bool_castable() || !rhs_type.is_bool_castable() {
                    return Err(CompileError::LogicalOpOnNonBool);
                }
            }
            ExprBinaryOp::LogicalOr => {
                if !lhs_type.is_bool_castable() || !rhs_type.is_bool_castable() {
                    return Err(CompileError::LogicalOpOnNonBool);
                }
            }

            ExprBinaryOp::Comma => {}
            ExprBinaryOp::Assign => {
                if !lhs.is_address() {
                    return Err(CompileError::NotAssignable);
                }
                if lhs.cv_type()?.const_ {
                    return Err(CompileError::AssignToConst);
                }
                if !rhs_type.is_implicitly_castable(&lhs_type) {
                    return Err(CompileError::InvalidOperandType(lhs_type, rhs_type));
                }
                let rhs = if lhs_type != rhs_type {
                    Expression::Cast(expression::ExprCast {
                        expr: Box::new(rhs),
                        type_: lhs_type.clone(),
                    })
                } else {
                    rhs
                };
                return Ok(Expression::Binary(expression::ExprBinary {
                    op: expr.op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }));
            }
        }

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
    pub(crate) fn process_specs(
        &mut self,
        specs: impl Iterator<Item = ast::SpecifierQualifier>,
    ) -> Result<CVType, CompileError> {
        let mut collector = declarator::SpecifierQualifierCollector::new();
        for s in specs {
            match s {
                ast::SpecifierQualifier::TypeQualifier(t) => match t {
                    ast::TypeQualifier::Const => collector.set_const()?,
                    ast::TypeQualifier::Volatile => collector.set_volatile()?,
                },
                ast::SpecifierQualifier::TypeSpecifier(s) => match s {
                    ast::TypeSpecifier::Void => collector.set_void()?,
                    ast::TypeSpecifier::Char => collector.set_char()?,
                    ast::TypeSpecifier::Short => collector.set_short()?,
                    ast::TypeSpecifier::Int => collector.set_int()?,
                    ast::TypeSpecifier::Long => collector.set_long()?,
                    ast::TypeSpecifier::Float => collector.set_float()?,
                    ast::TypeSpecifier::Double => collector.set_double()?,
                    ast::TypeSpecifier::Signed => collector.set_signed()?,
                    ast::TypeSpecifier::Unsigned => collector.set_unsigned()?,
                    ast::TypeSpecifier::Typename(name) => {
                        let mut found = false;
                        for scope in self.scopes.iter().rev() {
                            if let Scope::Block(scope) = scope {
                                if let Some(typedef) = scope.typedefs.get(&name) {
                                    collector.set_typename(typedef.clone())?;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if !found {
                            if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                collector.set_typename(typedef.clone())?;
                            } else {
                                return Err(CompileError::TypeNotFound(name));
                            }
                        }
                    }
                    ast::TypeSpecifier::StructOrUnion(s) => {
                        if let Some(definition) = s.decls {
                            let mut names = HashSet::new();
                            let mut members = Vec::new();
                            for def in definition.into_iter() {
                                let base_type = self.process_specs(def.specs.into_iter())?;
                                for decl in def.declarators.into_iter() {
                                    let d = self.process_declarator(decl, base_type.clone())?;
                                    let Some(name) = d.name else {
                                        return Err(CompileError::DeclarationWithoutName);
                                    };
                                    if names.insert(name.clone()) == false {
                                        return Err(CompileError::MemberRedefined(name));
                                    }

                                    members.push((name, d.cv_type));
                                }
                            }
                            let struct_type = if s.is_struct {
                                let struct_type = StructType::struct_from_decls(s.name, members);
                                CVType::from_primitive(PrimitiveType::Struct(struct_type))
                            } else {
                                let union_type = StructType::union_from_decls(s.name, members);
                                CVType::from_primitive(PrimitiveType::Union(union_type))
                            };
                            collector.set_typename(struct_type)?;
                        } else {
                            let Some(name) = s.name else {
                                return Err(CompileError::IncompleteType);
                            };

                            // search for type definition `name`
                            let mut found = false;
                            for scope in self.scopes.iter().rev() {
                                if let Scope::Block(scope) = scope {
                                    if let Some(typedef) = scope.typedefs.get(&name) {
                                        if s.is_struct && !typedef.type_.is_struct() {
                                            return Err(CompileError::StructMismatch(name));
                                        }
                                        if !s.is_struct && !typedef.type_.is_union() {
                                            return Err(CompileError::UnionMismatch(name));
                                        }
                                        collector.set_typename(typedef.clone())?;
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                    if s.is_struct && !typedef.type_.is_struct() {
                                        return Err(CompileError::StructMismatch(name));
                                    }
                                    if !s.is_struct && !typedef.type_.is_union() {
                                        return Err(CompileError::UnionMismatch(name));
                                    }
                                    collector.set_typename(typedef.clone())?;
                                } else {
                                    return Err(CompileError::TypeNotFound(name));
                                }
                            }
                        }
                    }
                    ast::TypeSpecifier::Enum(e) => {
                        if let Some(definition) = e.enumerators {
                            let mut names = HashSet::new();
                            let mut members = Vec::new();
                            for def in definition.into_iter() {
                                if names.insert(def.name.clone()) == false {
                                    return Err(CompileError::MemberRedefined(def.name));
                                }

                                if let Some(expr) = def.value {
                                    let expr = self.process_expression(expr)?;
                                    let value = match expr {
                                        Expression::Signed(v, _) => v,
                                        Expression::Unsigned(v, _) => v as i64,
                                        _ => return Err(CompileError::EnumValueNotInteger),
                                    };
                                    members.push((def.name, Some(value)));
                                } else {
                                    members.push((def.name, None));
                                }
                            }
                            let enum_type = EnumType::enum_from_decls(e.name, members);
                            let enum_type = PrimitiveType::Enum(enum_type);
                            let enum_type = CVType::from_primitive(enum_type);
                            collector.set_typename(enum_type)?;
                        } else {
                            let Some(name) = e.name else {
                                return Err(CompileError::IncompleteType);
                            };

                            // search for type definition `name`
                            let mut found = false;
                            for scope in self.scopes.iter().rev() {
                                if let Scope::Block(scope) = scope {
                                    if let Some(typedef) = scope.typedefs.get(&name) {
                                        if !typedef.type_.is_enum() {
                                            return Err(CompileError::EnumMismatch(name));
                                        }
                                        collector.set_typename(typedef.clone())?;
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                    if !typedef.type_.is_enum() {
                                        return Err(CompileError::EnumMismatch(name));
                                    }
                                    collector.set_typename(typedef.clone())?;
                                } else {
                                    return Err(CompileError::TypeNotFound(name));
                                }
                            }
                        }
                    }
                },
            }
        }
        collector.into_type()
    }
    pub(crate) fn process_decl_specs(
        &mut self,
        specs: impl Iterator<Item = ast::DeclarationSpecifier>,
    ) -> Result<(StorageQualifier, CVType), CompileError> {
        let mut storage_qualifier = StorageQualifier::new();
        let mut collector = declarator::SpecifierQualifierCollector::new();

        for s in specs {
            match s {
                ast::DeclarationSpecifier::StorageClassSpecifier(s) => match s {
                    ast::StorageClassSpecifier::Auto => storage_qualifier.auto = true,
                    ast::StorageClassSpecifier::Register => storage_qualifier.register = true,
                    ast::StorageClassSpecifier::Static => storage_qualifier.static_ = true,
                    ast::StorageClassSpecifier::Extern => storage_qualifier.extern_ = true,
                    ast::StorageClassSpecifier::Typedef => storage_qualifier.typedef = true,
                },
                ast::DeclarationSpecifier::TypeQualifier(t) => match t {
                    ast::TypeQualifier::Const => collector.set_const()?,
                    ast::TypeQualifier::Volatile => collector.set_volatile()?,
                },
                ast::DeclarationSpecifier::TypeSpecifier(s) => match s {
                    ast::TypeSpecifier::Void => collector.set_void()?,
                    ast::TypeSpecifier::Char => collector.set_char()?,
                    ast::TypeSpecifier::Short => collector.set_short()?,
                    ast::TypeSpecifier::Int => collector.set_int()?,
                    ast::TypeSpecifier::Long => collector.set_long()?,
                    ast::TypeSpecifier::Float => collector.set_float()?,
                    ast::TypeSpecifier::Double => collector.set_double()?,
                    ast::TypeSpecifier::Signed => collector.set_signed()?,
                    ast::TypeSpecifier::Unsigned => collector.set_unsigned()?,
                    ast::TypeSpecifier::Typename(name) => {
                        let mut found = false;
                        for scope in self.scopes.iter().rev() {
                            if let Scope::Block(scope) = scope {
                                if let Some(typedef) = scope.typedefs.get(&name) {
                                    collector.set_typename(typedef.clone())?;
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if !found {
                            if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                collector.set_typename(typedef.clone())?;
                            } else {
                                return Err(CompileError::TypeNotFound(name));
                            }
                        }
                    }
                    ast::TypeSpecifier::StructOrUnion(s) => {
                        if let Some(definition) = s.decls {
                            let mut names = HashSet::new();
                            let mut members = Vec::new();
                            for def in definition.into_iter() {
                                let base_type = self.process_specs(def.specs.into_iter())?;
                                for decl in def.declarators.into_iter() {
                                    let d = self.process_declarator(decl, base_type.clone())?;
                                    let Some(name) = d.name else {
                                        return Err(CompileError::DeclarationWithoutName);
                                    };
                                    if names.insert(name.clone()) == false {
                                        return Err(CompileError::MemberRedefined(name));
                                    }

                                    members.push((name, d.cv_type));
                                }
                            }
                            let struct_type = if s.is_struct {
                                let struct_type = StructType::struct_from_decls(s.name, members);
                                CVType::from_primitive(PrimitiveType::Struct(struct_type))
                            } else {
                                let union_type = StructType::union_from_decls(s.name, members);
                                CVType::from_primitive(PrimitiveType::Union(union_type))
                            };
                            collector.set_typename(struct_type)?;
                        } else {
                            let Some(name) = s.name else {
                                return Err(CompileError::IncompleteType);
                            };

                            // search for type definition `name`
                            let mut found = false;
                            for scope in self.scopes.iter().rev() {
                                if let Scope::Block(scope) = scope {
                                    if let Some(typedef) = scope.typedefs.get(&name) {
                                        if s.is_struct && !typedef.type_.is_struct() {
                                            return Err(CompileError::StructMismatch(name));
                                        }
                                        if !s.is_struct && !typedef.type_.is_union() {
                                            return Err(CompileError::UnionMismatch(name));
                                        }
                                        collector.set_typename(typedef.clone())?;
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                    if s.is_struct && !typedef.type_.is_struct() {
                                        return Err(CompileError::StructMismatch(name));
                                    }
                                    if !s.is_struct && !typedef.type_.is_union() {
                                        return Err(CompileError::UnionMismatch(name));
                                    }
                                    collector.set_typename(typedef.clone())?;
                                } else {
                                    return Err(CompileError::TypeNotFound(name));
                                }
                            }
                        }
                    }
                    ast::TypeSpecifier::Enum(e) => {
                        if let Some(definition) = e.enumerators {
                            let mut names = HashSet::new();
                            let mut members = Vec::new();
                            for def in definition.into_iter() {
                                if names.insert(def.name.clone()) == false {
                                    return Err(CompileError::MemberRedefined(def.name));
                                }

                                if let Some(expr) = def.value {
                                    let expr = self.process_expression(expr)?;
                                    let value = match expr {
                                        Expression::Signed(v, _) => v,
                                        Expression::Unsigned(v, _) => v as i64,
                                        _ => return Err(CompileError::EnumValueNotInteger),
                                    };
                                    members.push((def.name, Some(value)));
                                } else {
                                    members.push((def.name, None));
                                }
                            }
                            let enum_type = EnumType::enum_from_decls(e.name, members);
                            let enum_type = PrimitiveType::Enum(enum_type);
                            let enum_type = CVType::from_primitive(enum_type);
                            collector.set_typename(enum_type)?;
                        } else {
                            let Some(name) = e.name else {
                                return Err(CompileError::IncompleteType);
                            };

                            // search for type definition `name`
                            let mut found = false;
                            for scope in self.scopes.iter().rev() {
                                if let Scope::Block(scope) = scope {
                                    if let Some(typedef) = scope.typedefs.get(&name) {
                                        if !typedef.type_.is_enum() {
                                            return Err(CompileError::EnumMismatch(name));
                                        }
                                        collector.set_typename(typedef.clone())?;
                                        found = true;
                                        break;
                                    }
                                }
                            }
                            if !found {
                                if let Some(typedef) = self.global_scope.typedefs.get(&name) {
                                    if !typedef.type_.is_enum() {
                                        return Err(CompileError::EnumMismatch(name));
                                    }
                                    collector.set_typename(typedef.clone())?;
                                } else {
                                    return Err(CompileError::TypeNotFound(name));
                                }
                            }
                        }
                    }
                },
            }
        }
        Ok((storage_qualifier, collector.into_type()?))
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
            Expression::Signed(v, _) => {
                if v < 0 {
                    return Err(CompileError::NegativeArraySize);
                } else {
                    v as usize
                }
            }
            Expression::Unsigned(v, _) => v as usize,
            _ => return Err(CompileError::ArraySizeNotInteger),
        };
        if let Some(decl) = decl.declarator {
            let mut res = self.process_declarator(*decl, base_type)?;
            res.cv_type = CVType::from_primitive(PrimitiveType::Array(ArrayType {
                cv_type: Box::new(res.cv_type),
                size: size,
            }));
            Ok(res)
        } else {
            Ok(CombinedDeclarator {
                name: None,
                cv_type: CVType::from_primitive(PrimitiveType::Array(ArrayType {
                    cv_type: Box::new(base_type),
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
        unimplemented!("process_declarator_array_unbounded")
    }
    pub(crate) fn process_declarator_function(
        &mut self,
        decl: ast::DeclFunction,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, CompileError> {
        let args = decl
            .params
            .params
            .into_iter()
            .map(|param| -> Result<CVType, CompileError> {
                // @TODO storage_qualifier check
                let (_storage_qualifier, base_type) =
                    self.process_decl_specs(param.specs.into_iter())?;

                if let Some(decl) = param.declarator {
                    let res = self.process_declarator(*decl, base_type)?;
                    Ok(res.cv_type)
                } else {
                    Ok(base_type)
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        let variadic = decl.params.variadic;

        if let Some(decl) = decl.declarator {
            let res = self.process_declarator(*decl, base_type)?;
            let func_type = FunctionType {
                args,
                variadic,
                return_type: Box::new(res.cv_type),
            };
            Ok(CombinedDeclarator {
                cv_type: CVType::from_primitive(PrimitiveType::Function(func_type)),
                name: res.name,
            })
        } else {
            let func_type = FunctionType {
                args,
                variadic,
                return_type: Box::new(base_type),
            };
            Ok(CombinedDeclarator {
                cv_type: CVType::from_primitive(PrimitiveType::Function(func_type)),
                name: None,
            })
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
