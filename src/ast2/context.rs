use super::expression;
use super::statement;
use super::CVType;
use super::CombinedDeclarator;
use super::ConversionError;
use super::Expression;
use super::Statement;
use crate::ast;

pub struct Context {
    // pub function_scope: Option<FunctionScope>,
    // pub scopes: Vec<Scope>,
    // pub scope_counter: usize,

    // pub labels: HashMap<String, Rc<RefCell<LabelInfo>>>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            // scopes: Vec::new(),
            // scope_counter: 0,
            // labels: Default::default(),
        }
    }

    /// return index on local stack for new variable
    /*
    fn new_offset(&self) -> usize {
        for parent in self.scopes.iter().rev() {
            match parent {
                Scope::Block(blk) => return blk.offset + blk.variables.len(),
                Scope::Function(_) => return 0,
            }
        }
        0
    }
    pub fn begin_scope(&mut self, is_loop: bool) {
        let offset = self.new_offset();
        self.scope_counter += 1;
        self.scopes.push(Scope::Block(ScopeBlock {
            id: self.scope_counter,
            max_variables: 0,
            offset,
            variables: Vec::new(),
            is_loop,
            labels: Vec::new(),
        }));
    }
    /// close all local variable scopes, count up variables and re calculate stack size.
    fn end_scope(&mut self) -> Scope {
        let scope = self.scopes.pop().unwrap();

        if let Scope::Block(blk) = &scope {
            for label in blk.labels.iter() {
                self.labels.remove(label);
            }
            match self.scopes.last_mut() {
                Some(Scope::Block(parent)) => {
                    let vars = parent.variables.len() + blk.max_variables;
                    parent.max_variables = parent.max_variables.max(vars);
                }
                Some(Scope::Function(parent)) => {
                    parent.max_variables = parent.max_variables.max(blk.max_variables);
                }
                _ => {}
            }
        } else {
            unreachable!("end_scope - block scope not opened?");
        }

        scope
    }
    /// return local stack offset
    fn begin_variable_scope(&mut self, name: String) -> Rc<RefCell<VariableInfo>> {
        if let Some(Scope::Block(blk)) = self.scopes.last_mut() {
            let offset = blk.offset + blk.variables.len();
            let varinfo = Rc::new(RefCell::new(VariableInfo {
                name,
                is_reference: false,
                offset,
            }));
            blk.variables.push(Rc::clone(&varinfo));
            blk.max_variables = blk.max_variables.max(blk.variables.len());
            varinfo
        } else {
            unreachable!("begin_variable_scope - block scope not opened?");
        }
    }
    /// search for local variable name `name`
    fn search_local_variable(&mut self, name: &str) -> Option<ExprLocalVariable> {
        let mut function_scopes = Vec::new();
        let mut found = None;
        'a: for scope in self.scopes.iter_mut().rev() {
            match scope {
                Scope::Block(blk) => {
                    for var in blk.variables.iter().rev() {
                        if var.borrow().name == name {
                            found = Some(ExprLocalVariable::Stack(
                                var.borrow().offset,
                                name.to_string(),
                            ));
                            break 'a;
                        }
                    }
                }
                Scope::Function(func) => {
                    for (upvalue_idx, upvalue) in func.upvalues.iter().enumerate() {
                        if upvalue.name.as_str() == name {
                            found = Some(ExprLocalVariable::Upvalue(upvalue_idx, name.to_string()));
                            break 'a;
                        }
                    }
                    function_scopes.push(func);
                }
            }
        }
        if let Some(mut found) = found {
            for scope in function_scopes.into_iter().rev() {
                let upvalue_idx = scope.upvalues.len();
                scope.upvalues.push(UpvalueInfo {
                    name: name.to_string(),
                    from: found,
                });
                found = ExprLocalVariable::Upvalue(upvalue_idx, name.to_string());
            }
            Some(found)
        } else {
            None
        }
    }
    fn begin_function_scope(&mut self, variadic: bool) {
        self.scope_counter += 1;
        self.scopes.push(Scope::Function(ScopeFunction {
            id: self.scope_counter,
            max_variables: 0,
            upvalues: Vec::new(),
            variadic,
        }));
    }
    fn end_function_scope(&mut self) -> ScopeFunction {
        if let Some(Scope::Function(scope)) = self.scopes.pop() {
            scope
        } else {
            unreachable!("end_function_scope - function scope not opened? - 2");
        }
    }
    fn nearest_function_scope(&mut self) -> Option<&mut ScopeFunction> {
        for scope in self.scopes.iter_mut().rev() {
            match scope {
                Scope::Function(func) => return Some(func),
                _ => {}
            }
        }
        None
    }

    fn scope_tree(&self) -> Vec<usize> {
        let mut tree = Vec::new();
        for scope in self.scopes.iter().rev() {
            match scope {
                Scope::Block(blk) => tree.push(blk.id),
                Scope::Function(_) => break,
            }
        }
        tree.reverse();
        tree
    }
    */

    // pub fn process(&mut self, block: lua_parser::Block) -> Result<crate::Block, ProcessError> {
    //     self.begin_scope(false);
    //     self.process_block(block, false, false)
    // }

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

impl Context {
    pub(crate) fn process_statement_null(
        &mut self,
        stmt: ast::StmtNull,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_expression(
        &mut self,
        stmt: ast::StmtExpression,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_labeled(
        &mut self,
        stmt: ast::StmtLabeled,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_compound(
        &mut self,
        stmt: ast::StmtCompound,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_if(
        &mut self,
        stmt: ast::StmtIf,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_switch(
        &mut self,
        stmt: ast::StmtSwitch,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_case(
        &mut self,
        stmt: ast::StmtCase,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_default(
        &mut self,
        stmt: ast::StmtDefault,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_continue(
        &mut self,
        stmt: ast::StmtContinue,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_break(
        &mut self,
        stmt: ast::StmtBreak,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_while(
        &mut self,
        stmt: ast::StmtWhile,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_dowhile(
        &mut self,
        stmt: ast::StmtDoWhile,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_for(
        &mut self,
        stmt: ast::StmtFor,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_goto(
        &mut self,
        stmt: ast::StmtGoto,
    ) -> Result<Statement, ConversionError> {
    }
    pub(crate) fn process_statement_return(
        &mut self,
        stmt: ast::StmtReturn,
    ) -> Result<Statement, ConversionError> {
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
    }
}

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
    }
}

impl Context {
    pub(crate) fn process_declarator_identifier(
        &mut self,
        decl: ast::DeclIdentifier,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
    pub(crate) fn process_declarator_pointer(
        &mut self,
        decl: ast::DeclPointer,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
    pub(crate) fn process_declarator_array_fixed(
        &mut self,
        decl: ast::DeclArrayFixed,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
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
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
    pub(crate) fn process_declarator_volatile(
        &mut self,
        decl: ast::DeclVolatile,
        base_type: CVType,
    ) -> Result<CombinedDeclarator, ConversionError> {
    }
}
