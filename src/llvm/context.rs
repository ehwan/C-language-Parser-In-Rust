use std::collections::BTreeMap;

use inkwell::{
    types::BasicTypeEnum,
    values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, IntValue},
    AddressSpace,
};

use super::error::CompileError;
use crate::semantic::*;

pub struct Context {
    context: inkwell::context::Context,
}
impl Context {
    pub fn new() -> Self {
        let context = inkwell::context::Context::create();
        Self { context }
    }

    pub fn compile<'ctx>(
        &'ctx self,
        ast: TranslationUnit,
    ) -> Result<ContextInternal<'ctx>, CompileError> {
        let mut internal = ContextInternal::new(&self.context);
        internal.compile(ast)?;
        Ok(internal)
    }
}

pub type ValueType<'ctx> = AnyValueEnum<'ctx>;

#[derive(Debug, Clone)]
pub struct LoopContext<'ctx> {
    pub break_block: inkwell::basic_block::BasicBlock<'ctx>,
    pub continue_block: Option<inkwell::basic_block::BasicBlock<'ctx>>,
}

pub struct ContextInternal<'ctx> {
    context: &'ctx inkwell::context::Context,
    pub module: inkwell::module::Module<'ctx>,
    pub builder: inkwell::builder::Builder<'ctx>,
    pub execution: inkwell::execution_engine::ExecutionEngine<'ctx>,

    variable_map: BTreeMap<usize, inkwell::values::AnyValueEnum<'ctx>>,
    current_function: Option<inkwell::values::FunctionValue<'ctx>>,
    loop_stack: Vec<LoopContext<'ctx>>,
}

impl<'ctx> ContextInternal<'ctx> {
    fn new(context: &'ctx inkwell::context::Context) -> Self {
        let module = context.create_module("main_module");
        let execution = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            execution,
            variable_map: Default::default(),
            current_function: None,
            loop_stack: Default::default(),
        }
    }

    pub fn run(&self) -> i32 {
        type MainFunc = unsafe extern "C" fn() -> i32;
        unsafe {
            let main_func = self.execution.get_function::<MainFunc>("main").unwrap();
            main_func.call()
        }
    }

    fn compile(&mut self, mut ast: TranslationUnit) -> Result<(), CompileError> {
        for (var_name, var_def) in ast.variables.into_iter() {
            // functions are also in this map, but we skip them here
            if var_def.cv_type.type_.is_function() {
                let function_type = var_def
                    .cv_type
                    .type_
                    .to_llvm_type(&self.context)
                    .into_function_type();

                let linkage =
                    if var_def.storage == Some(crate::semantic::StorageClassSpecifier::Extern) {
                        Some(inkwell::module::Linkage::External)
                    } else {
                        None
                    };
                let function = self
                    .module
                    .add_function(&var_def.name, function_type, linkage);
                self.variable_map
                    .insert(var_def.uid, function.as_any_value_enum());
            } else {
                let basic_type: BasicTypeEnum = var_def
                    .cv_type
                    .type_
                    .to_llvm_type(&self.context)
                    .try_into()
                    .unwrap();
                let global_var =
                    self.module
                        .add_global(basic_type, Some(AddressSpace::default()), &var_name);

                self.variable_map.insert(
                    var_def.uid,
                    global_var.as_pointer_value().as_any_value_enum(),
                );
            }
        }

        // add functions before compiling function bodies
        for (func_name, func_def) in ast.functions.iter() {
            let function_type = func_def.type_.to_llvm_type(&self.context);
            if !self.variable_map.contains_key(&func_def.uid) {
                let function = self
                    .module
                    .add_function(&func_name, function_type, None)
                    .as_any_value_enum();
                self.variable_map.insert(func_def.uid, function);
            }
        }

        for (func_name, func_def) in ast.functions.into_iter() {
            let function = self
                .variable_map
                .get(&func_def.uid)
                .unwrap()
                .into_function_value();
            self.current_function = Some(function.clone());

            let function_block = self.context.append_basic_block(function, "func_block");
            self.builder.position_at_end(function_block);

            // allocate space for arguments and store argument values
            for (idx, arg_value) in function.get_param_iter().enumerate() {
                let arg_info = &func_def.args[idx];
                let arg_name = &arg_info.name;
                let arg_type: BasicTypeEnum = func_def.args[idx]
                    .cv_type
                    .type_
                    .to_llvm_type(&self.context)
                    .try_into()
                    .unwrap();
                let arg_alloca = self
                    .builder
                    .build_alloca(arg_type, arg_name)
                    .map_err(CompileError::BuilderError)?;

                self.builder
                    .build_store(arg_alloca, arg_value)
                    .map_err(CompileError::BuilderError)?;

                self.variable_map
                    .insert(arg_info.uid, arg_alloca.as_any_value_enum());
            }
            if func_name == "main" {
                for s in std::mem::take(&mut ast.statements) {
                    self.compile_statement(s)?;
                }
            }
            self.compile_statement(*func_def.body)?;
        }

        self.current_function = None;
        Ok(())
    }
    fn is_block_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .as_ref()
            .unwrap()
            .get_terminator()
            .is_some()
    }
    fn compile_statement(&mut self, stmt: Statement) -> Result<(), CompileError> {
        if self.is_block_terminated() {
            return Ok(());
        }
        match stmt {
            Statement::None => Ok(()),
            Statement::Expression(stmt) => self.compile_statement_expression(stmt),
            Statement::Labeled(stmt) => self.compile_statement_labeled(stmt),
            Statement::Goto(stmt) => self.compile_statement_goto(stmt),
            Statement::Compound(stmt) => self.compile_statement_compound(stmt),
            Statement::If(stmt) => self.compile_statement_if(stmt),
            Statement::Switch(stmt) => self.compile_statement_switch(stmt),
            Statement::Continue => self.compile_statement_continue(),
            Statement::Break => self.compile_statement_break(),
            Statement::Return(stmt) => self.compile_statement_return(stmt),
            Statement::For(stmt) => self.compile_statement_for(stmt),
            Statement::While(stmt) => self.compile_statement_while(stmt),
            Statement::DoWhile(stmt) => self.compile_statement_dowhile(stmt),
            Statement::VariableDeclaration(stmt) => {
                self.compile_statement_variabledeclaration(stmt)
            }
            Statement::_Case(_) | Statement::_Default(_) => {
                unreachable!("unreachable statement: {:?}", stmt)
            }
        }
    }
    fn compile_expression(&mut self, expr: Expression) -> Result<ValueType<'ctx>, CompileError> {
        match expr {
            Expression::Integer(value, type_) => self.compile_expression_integer(value, type_),
            Expression::Float(value, type_) => self.compile_expression_float(value, type_),
            Expression::String(value) => self.compile_expression_string(value),
            Expression::Variable(info) => self.compile_expression_variable(info),
            Expression::Conditional(expr) => self.compile_expression_conditional(expr),
            Expression::Cast(expr) => self.compile_expression_cast(expr),
            Expression::Member(expr) => self.compile_expression_member(expr),
            Expression::Arrow(expr) => self.compile_expression_arrow(expr),
            Expression::Paren(expr) => self.compile_expression_paren(expr),
            Expression::Bracket(expr) => self.compile_expression_bracket(expr),
            Expression::Unary(expr) => self.compile_expression_unary(expr),
            Expression::Binary(expr) => self.compile_expression_binary(expr),
            Expression::InitializerList(expr) => self.compile_expression_initializerlist(expr),
            Expression::Default(t) => self.compile_expression_default(t),
        }
    }
}

// statements
impl<'ctx> ContextInternal<'ctx> {
    fn compile_statement_expression(&mut self, stmt: StmtExpression) -> Result<(), CompileError> {
        self.compile_expression(stmt.expression)?;
        Ok(())
    }
    fn compile_statement_labeled(&mut self, stmt: StmtLabeled) -> Result<(), CompileError> {
        unimplemented!("labeled statement is not supported yet");
        Ok(())
    }
    fn compile_statement_goto(&mut self, stmt: StmtGoto) -> Result<(), CompileError> {
        unimplemented!("goto statement is not supported yet");
        Ok(())
    }
    fn compile_statement_compound(&mut self, stmt: StmtCompound) -> Result<(), CompileError> {
        for s in stmt.statements {
            self.compile_statement(s)?;
        }
        Ok(())
    }
    fn to_1bit_bool(&self, val: AnyValueEnum<'ctx>) -> Result<IntValue<'ctx>, CompileError> {
        match val {
            AnyValueEnum::IntValue(val) => {
                let zero = val.get_type().const_zero();
                self.builder
                    .build_int_compare(inkwell::IntPredicate::NE, val, zero, "ifcond")
                    .map_err(CompileError::BuilderError)
            }
            AnyValueEnum::PointerValue(val) => self
                .builder
                .build_is_not_null(val, "ifcond_ptr")
                .map_err(CompileError::BuilderError),
            _ => unreachable!("if condition must be integer or pointer type"),
        }
    }
    fn compile_statement_if(&mut self, stmt: StmtIf) -> Result<(), CompileError> {
        // create blocks
        let then_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "then_block");
        let else_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "else_block");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "merge_block");

        let cond = self.compile_expression(stmt.condition)?;
        let cond1bit = self.to_1bit_bool(cond)?;
        self.builder
            .build_conditional_branch(cond1bit, then_block, else_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(then_block);
        self.compile_statement(*stmt.then)?;
        if !self.is_block_terminated() {
            self.builder
                .build_unconditional_branch(merge_block)
                .map_err(CompileError::BuilderError)?;
        }

        self.builder.position_at_end(else_block);
        if let Some(else_stmt) = stmt.else_ {
            self.compile_statement(*else_stmt)?;
        }
        if !self.is_block_terminated() {
            self.builder
                .build_unconditional_branch(merge_block)
                .map_err(CompileError::BuilderError)?;
        }

        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn compile_statement_switch(&mut self, stmt: StmtSwitch) -> Result<(), CompileError> {
        let value = self.compile_expression_deref(stmt.value)?;

        let mut body_blocks = Vec::new();
        let mut cond_blocks = Vec::new();
        for _ in 0..stmt.cases.len() {
            let block = self
                .context
                .append_basic_block(self.current_function.unwrap(), "switch_body");
            body_blocks.push(block);

            let cond_block = self
                .context
                .append_basic_block(self.current_function.unwrap(), "switch_cond");
            cond_blocks.push(cond_block);
        }
        let break_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "switch_break");
        body_blocks.push(break_block);

        if let Some(default_case) = stmt.default {
            cond_blocks.push(body_blocks[default_case]);
        } else {
            cond_blocks.push(break_block);
        }

        self.loop_stack.push(LoopContext {
            break_block,
            continue_block: None,
        });
        if stmt.default == Some(0) {
            self.builder
                .build_unconditional_branch(cond_blocks[1])
                .map_err(CompileError::BuilderError)?;
        } else {
            self.builder
                .build_unconditional_branch(cond_blocks[0])
                .map_err(CompileError::BuilderError)?;
        }

        let l = stmt.cases.len();
        debug_assert!(l > 0);
        for (i, case) in stmt.cases.into_iter().enumerate() {
            if let Some(cond) = case.value {
                self.builder.position_at_end(cond_blocks[i]);
                let cond_val = self.compile_expression_deref(cond)?;

                // @TODO type match
                let cond1bit = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        value.into_int_value(),
                        cond_val.into_int_value(),
                        "switch_cond",
                    )
                    .map_err(CompileError::BuilderError)?;

                let next_block = if Some(i + 1) == stmt.default {
                    cond_blocks[i + 2]
                } else {
                    cond_blocks[i + 1]
                };

                self.builder
                    .build_conditional_branch(cond1bit, body_blocks[i], next_block)
                    .map_err(CompileError::BuilderError)?;
            }

            self.builder.position_at_end(body_blocks[i]);
            for stmt in case.statements {
                self.compile_statement(stmt)?;
            }
            if !self.is_block_terminated() {
                let next_block = body_blocks[i + 1];
                self.builder
                    .build_unconditional_branch(next_block)
                    .map_err(CompileError::BuilderError)?;
            }
        }
        self.builder.position_at_end(break_block);
        self.loop_stack.pop();
        Ok(())
    }
    fn compile_statement_continue(&mut self) -> Result<(), CompileError> {
        for l in self.loop_stack.iter().rev() {
            if let Some(continue_block) = l.continue_block {
                self.builder
                    .build_unconditional_branch(continue_block)
                    .map_err(CompileError::BuilderError)?;
                return Ok(());
            }
        }
        Ok(())
    }
    fn compile_statement_break(&mut self) -> Result<(), CompileError> {
        let last_loop_context = self.loop_stack.last().unwrap();
        self.builder
            .build_unconditional_branch(last_loop_context.break_block)
            .map_err(CompileError::BuilderError)?;
        Ok(())
    }
    fn compile_statement_return(&mut self, stmt: StmtReturn) -> Result<(), CompileError> {
        if let Some(val) = stmt.expression {
            let ret_val: BasicValueEnum = self.compile_expression_deref(val)?.try_into().unwrap();
            self.builder
                .build_return(Some(&ret_val))
                .map_err(CompileError::BuilderError)?;
        } else {
            self.builder
                .build_return(None)
                .map_err(CompileError::BuilderError)?;
        }
        Ok(())
    }
    fn compile_statement_for(&mut self, stmt: StmtFor) -> Result<(), CompileError> {
        /*
        init

        body_block:
            body

        continue_block:
            next
            if cond goto body_block else break_block

        break_block:
        */
        self.compile_statement(*stmt.init)?;

        let body_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "body_block");
        let continue_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "continue_block");
        let break_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "break_block");

        self.loop_stack.push(LoopContext {
            break_block,
            continue_block: Some(continue_block),
        });

        self.builder
            .build_unconditional_branch(body_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(body_block);
        self.compile_statement(*stmt.body)?;
        if !self.is_block_terminated() {
            self.builder
                .build_unconditional_branch(continue_block)
                .map_err(CompileError::BuilderError)?;
        }

        self.builder.position_at_end(continue_block);
        if let Some(next) = stmt.next {
            self.compile_expression(next)?;
        }
        let cond = self.compile_expression(stmt.condition)?;
        let cond1bit = self.to_1bit_bool(cond)?;
        self.builder
            .build_conditional_branch(cond1bit, body_block, break_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(break_block);

        self.loop_stack.pop();
        Ok(())
    }
    fn compile_statement_while(&mut self, stmt: StmtWhile) -> Result<(), CompileError> {
        /*
        continue_block:
            eval_cond
            if cond goto body_block else break_block

        body_block:
            body
            goto continue_block:

        break_block:
         */
        let continue_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "continue_block");
        let body_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "body_block");
        let break_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "break_block");
        self.loop_stack.push(LoopContext {
            break_block,
            continue_block: Some(continue_block),
        });

        self.builder
            .build_unconditional_branch(continue_block)
            .map_err(CompileError::BuilderError)?;
        self.builder.position_at_end(continue_block);
        let cond = self.compile_expression(stmt.condition)?;
        let cond1bit = self.to_1bit_bool(cond)?;
        self.builder
            .build_conditional_branch(cond1bit, body_block, break_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(body_block);
        self.compile_statement(*stmt.body)?;
        if !self.is_block_terminated() {
            self.builder
                .build_unconditional_branch(continue_block)
                .map_err(CompileError::BuilderError)?;
        }

        self.builder.position_at_end(break_block);
        self.loop_stack.pop();
        Ok(())
    }
    fn compile_statement_dowhile(&mut self, stmt: StmtDoWhile) -> Result<(), CompileError> {
        /*
        body_block:
            body

        continue_block:
            eval_cond
            if cond goto body_block

        break_block:
         */
        let body_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "body_block");
        let continue_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "continue_block");
        let break_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "break_block");
        self.loop_stack.push(LoopContext {
            break_block,
            continue_block: Some(continue_block),
        });

        self.builder
            .build_unconditional_branch(body_block)
            .map_err(CompileError::BuilderError)?;
        self.builder.position_at_end(body_block);
        self.compile_statement(*stmt.body)?;
        if !self.is_block_terminated() {
            self.builder
                .build_unconditional_branch(continue_block)
                .map_err(CompileError::BuilderError)?;
        }

        self.builder.position_at_end(continue_block);
        let cond = self.compile_expression(stmt.condition)?;
        let cond1bit = self.to_1bit_bool(cond)?;
        self.builder
            .build_conditional_branch(cond1bit, body_block, break_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(break_block);
        self.loop_stack.pop();
        Ok(())
    }
    fn compile_statement_variabledeclaration(
        &mut self,
        stmt: StmtVariableDeclaration,
    ) -> Result<(), CompileError> {
        for (var_info, init_value) in stmt.pairs {
            let type_: BasicTypeEnum = var_info
                .cv_type
                .type_
                .to_llvm_type(&self.context)
                .try_into()
                .unwrap();
            let var = self
                .builder
                .build_alloca(type_, &var_info.name)
                .map_err(CompileError::BuilderError)?;
            self.variable_map
                .insert(var_info.uid, var.as_any_value_enum());

            if let Some(init_value) = init_value {
                let init_value: BasicValueEnum =
                    self.compile_expression(init_value)?.try_into().unwrap();
                self.builder
                    .build_store(var, init_value)
                    .map_err(CompileError::BuilderError)?;
            }
        }
        Ok(())
    }
}

// expressions
impl<'ctx> ContextInternal<'ctx> {
    fn compile_expression_deref(
        &mut self,
        expr: Expression,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let is_ref = expr.is_reference();
        let expr = self.compile_expression(expr)?;
        if is_ref {
            let ptr = expr.into_pointer_value();
            self.builder
                .build_load(ptr, "load")
                .map_err(CompileError::BuilderError)
                .map(|loaded| loaded.as_any_value_enum())
        } else {
            Ok(expr)
        }
    }
    fn compile_expression_integer(
        &mut self,
        value: i64,
        type_: Integer,
    ) -> Result<ValueType<'ctx>, CompileError> {
        Ok(match type_ {
            Integer::Int8 => self
                .context
                .i8_type()
                .const_int(value as u64, false)
                .as_any_value_enum(),
            Integer::Int16 => self
                .context
                .i16_type()
                .const_int(value as u64, false)
                .as_any_value_enum(),
            Integer::Int32 => self
                .context
                .i32_type()
                .const_int(value as u64, false)
                .as_any_value_enum(),
            Integer::Int64 => self
                .context
                .i64_type()
                .const_int(value as u64, false)
                .as_any_value_enum(),
            Integer::UInt8 => self
                .context
                .i8_type()
                .const_int(value as u64, true)
                .as_any_value_enum(),
            Integer::UInt16 => self
                .context
                .i16_type()
                .const_int(value as u64, true)
                .as_any_value_enum(),
            Integer::UInt32 => self
                .context
                .i32_type()
                .const_int(value as u64, true)
                .as_any_value_enum(),
            Integer::UInt64 => self
                .context
                .i64_type()
                .const_int(value as u64, true)
                .as_any_value_enum(),
        })
    }
    fn compile_expression_float(
        &mut self,
        value: f64,
        type_: Float,
    ) -> Result<ValueType<'ctx>, CompileError> {
        match type_ {
            Float::Float32 => Ok(self
                .context
                .f32_type()
                .const_float(value)
                .as_any_value_enum()),
            Float::Float64 => Ok(self
                .context
                .f64_type()
                .const_float(value)
                .as_any_value_enum()),
        }
    }
    fn compile_expression_string(
        &mut self,
        value: String,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let str_ptr = self
            .builder
            .build_global_string_ptr(&value, "str")
            .map_err(CompileError::BuilderError)?;
        Ok(str_ptr.as_pointer_value().as_any_value_enum())
    }
    fn compile_expression_variable(
        &mut self,
        info: VariableInfo,
    ) -> Result<ValueType<'ctx>, CompileError> {
        // this unwrap must not fail if semantic analysis is done correctly
        Ok(self.variable_map.get(&info.uid).unwrap().clone())
    }
    fn compile_expression_conditional(
        &mut self,
        expr: ExprConditional,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let cond = self.compile_expression(*expr.cond)?;
        let cond = self.to_1bit_bool(cond)?;

        let then_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "cond_then");
        let else_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "cond_else");
        let merge_block = self
            .context
            .append_basic_block(self.current_function.unwrap(), "cond_merge");

        self.builder
            .build_conditional_branch(cond, then_block, else_block)
            .map_err(CompileError::BuilderError)?;
        self.builder.position_at_end(then_block);
        let then: BasicValueEnum = self
            .compile_expression(*expr.then_expr)?
            .try_into()
            .unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(else_block);
        let else_: BasicValueEnum = self
            .compile_expression(*expr.else_expr)?
            .try_into()
            .unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .map_err(CompileError::BuilderError)?;

        self.builder.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(then.get_type(), "cond_phi")
            .map_err(CompileError::BuilderError)?;
        phi.add_incoming(&[(&then, then_block), (&else_, else_block)]);

        Ok(phi.as_basic_value().as_any_value_enum())
    }
    fn compile_expression_cast(&mut self, expr: ExprCast) -> Result<ValueType<'ctx>, CompileError> {
        let lhs_type = expr.expr.primitive_type().unwrap();
        let lhs = self.compile_expression_deref(*expr.expr)?;
        let rhs_type = expr.type_;
        if lhs_type != rhs_type {
            match (lhs_type, rhs_type) {
                (PrimitiveType::Integer(lhs_type), PrimitiveType::Integer(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_int_type();
                    if lhs_type.is_signed() {
                        self.builder
                            .build_int_cast_sign_flag(
                                lhs.into_int_value(),
                                rhs_type_llvm,
                                true,
                                "int_cast",
                            )
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    } else {
                        self.builder
                            .build_int_cast(lhs.into_int_value(), rhs_type_llvm, "int_cast")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    }
                }
                (PrimitiveType::Integer(lhs_type), PrimitiveType::Float(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_float_type();
                    if lhs_type.is_signed() {
                        self.builder
                            .build_signed_int_to_float(
                                lhs.into_int_value(),
                                rhs_type_llvm,
                                "int_cast",
                            )
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    } else {
                        self.builder
                            .build_unsigned_int_to_float(
                                lhs.into_int_value(),
                                rhs_type_llvm,
                                "int_cast",
                            )
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    }
                }
                (PrimitiveType::Integer(_), PrimitiveType::Pointer(rhs_type)) => {
                    // rhs_type.to_llvm_type(context)
                    let rhs_type_llvm = PrimitiveType::Pointer(rhs_type)
                        .to_llvm_type(&self.context)
                        .into_pointer_type();
                    self.builder
                        .build_int_to_ptr(lhs.into_int_value(), rhs_type_llvm, "int_cast")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }

                (PrimitiveType::Float(_), PrimitiveType::Integer(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_int_type();
                    self.builder
                        .build_float_to_signed_int(lhs.into_float_value(), rhs_type_llvm, "fint")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }
                (PrimitiveType::Float(_), PrimitiveType::Float(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_float_type();
                    self.builder
                        .build_float_cast(lhs.into_float_value(), rhs_type_llvm, "ffloat")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }
                (PrimitiveType::Pointer(_), PrimitiveType::Integer(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_int_type();
                    self.builder
                        .build_ptr_to_int(lhs.into_pointer_value(), rhs_type_llvm, "ptr_int")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }
                (PrimitiveType::Pointer(_), PrimitiveType::Pointer(rhs_type)) => {
                    let rhs_type_llvm = PrimitiveType::Pointer(rhs_type)
                        .to_llvm_type(&self.context)
                        .into_pointer_type();
                    self.builder
                        .build_pointer_cast(lhs.into_pointer_value(), rhs_type_llvm, "ptr_ptr")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }
                (PrimitiveType::Array(_), PrimitiveType::Pointer(rhs_type)) => {
                    let rhs_type_llvm = PrimitiveType::Pointer(rhs_type)
                        .to_llvm_type(&self.context)
                        .into_pointer_type();
                    self.builder
                        .build_pointer_cast(lhs.into_pointer_value(), rhs_type_llvm, "ptr_ptr")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }
                (PrimitiveType::Array(_), PrimitiveType::Integer(rhs_type)) => {
                    let rhs_type_llvm = rhs_type.to_llvm_type(&self.context).into_int_type();
                    self.builder
                        .build_ptr_to_int(lhs.into_pointer_value(), rhs_type_llvm, "ptr_int")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum())
                }

                (lhs_type, rhs_type) => {
                    unreachable!("invalid cast from {:?} to {:?}", lhs_type, rhs_type)
                }
            }
        } else {
            Ok(lhs.as_any_value_enum())
        }
    }
    fn compile_expression_member(
        &mut self,
        expr: ExprMember,
    ) -> Result<ValueType<'ctx>, CompileError> {
        unimplemented!("member expression is not supported yet");
        // let src = self.compile_expression(*expr.src)?;
        // Ok(src
        //     .get_field_at_index(expr.member_index as u32)
        //     .unwrap()
        //     .into())
    }
    fn compile_expression_arrow(
        &mut self,
        expr: ExprMember,
    ) -> Result<ValueType<'ctx>, CompileError> {
        unreachable!("arrow expression is not supported yet")
        // let src = self.compile_expression(*expr.src)?.into_pointer_value();
    }
    fn compile_expression_paren(
        &mut self,
        expr: ExprParen,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let func = self.compile_expression(*expr.src)?.into_function_value();
        let args = expr
            .args
            .into_iter()
            .map(|arg| {
                self.compile_expression_deref(arg)
                    .map(|v| v.try_into().unwrap())
            })
            .collect::<Result<Vec<_>, CompileError>>()?;
        let res = self
            .builder
            .build_call(func, &args, "function_call")
            .map_err(CompileError::BuilderError)?;
        Ok(res.as_any_value_enum())
    }
    fn compile_expression_bracket(
        &mut self,
        expr: ExprBracket,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let src = self
            .compile_expression_deref(*expr.src)?
            .into_pointer_value();
        let index = self.compile_expression(*expr.index)?.into_int_value();
        unsafe {
            self.builder
                .build_gep(src, &[index], "array_access")
                .map_err(CompileError::BuilderError)
                .map(|v| v.as_any_value_enum())
        }
    }
    fn compile_expression_unary(
        &mut self,
        expr: ExprUnary,
    ) -> Result<ValueType<'ctx>, CompileError> {
        use crate::semantic::ExprUnaryOp;
        match expr.op {
            ExprUnaryOp::Plus => self.compile_expression_deref(*expr.expr),
            ExprUnaryOp::Minus => match self.compile_expression_deref(*expr.expr)? {
                AnyValueEnum::IntValue(value) => self
                    .builder
                    .build_int_neg(value, "neg")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum()),
                AnyValueEnum::FloatValue(value) => self
                    .builder
                    .build_float_neg(value, "fneg")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum()),
                _ => unreachable!("UnaryMinus on non-numeric type"),
            },
            ExprUnaryOp::LogicalNot => {
                let src = self.compile_expression_deref(*expr.expr)?;
                match src {
                    AnyValueEnum::IntValue(int_value) => {
                        let zero = int_value.get_type().const_zero();
                        let res1bit = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::EQ,
                                int_value,
                                zero,
                                "logical_not",
                            )
                            .map_err(CompileError::BuilderError)?;
                        self.builder
                            .build_int_z_extend(res1bit, self.context.i8_type(), "zext")
                            .map_err(CompileError::BuilderError)
                            .map(|cmp| cmp.as_any_value_enum())
                    }
                    AnyValueEnum::PointerValue(ptr_value) => {
                        let res1bit = self
                            .builder
                            .build_is_not_null(ptr_value, "logical_not_ptr")
                            .map_err(CompileError::BuilderError)?;
                        self.builder
                            .build_int_z_extend(res1bit, self.context.i8_type(), "zext_ptr")
                            .map_err(CompileError::BuilderError)
                            .map(|cmp| cmp.as_any_value_enum())
                    }

                    _ => unreachable!("LogicalNot on non-integer type"),
                }
            }

            ExprUnaryOp::BitwiseNot => {
                let src = self.compile_expression_deref(*expr.expr)?;

                match src {
                    AnyValueEnum::IntValue(int_value) => self
                        .builder
                        .build_not(int_value, "bitwise_not")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    AnyValueEnum::PointerValue(ptr_value) => self
                        .builder
                        .build_not(ptr_value, "bitwise_not")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),

                    _ => unreachable!("LogicalNot on non-integer type"),
                }
            }
            ExprUnaryOp::Dereference => self.compile_expression_deref(*expr.expr),
            ExprUnaryOp::AddressOf => self.compile_expression(*expr.expr),
            ExprUnaryOp::IncrementPre => {
                let ptr = self.compile_expression(*expr.expr)?.into_pointer_value();
                let val = self
                    .builder
                    .build_load(ptr, "load")
                    .map_err(CompileError::BuilderError)?;
                let ret = match val {
                    BasicValueEnum::IntValue(int_value) => {
                        let one = int_value.get_type().const_int(1, false);
                        self.builder
                            .build_int_add(int_value, one, "inc")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    }
                    BasicValueEnum::PointerValue(ptr_value) => unsafe {
                        let one = self.context.i32_type().const_int(1, false);
                        self.builder
                            .build_gep(ptr_value, &[one], "ptr_inc")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    },
                    _ => unreachable!("IncrementPre on non-integer/pointer type"),
                }?;
                self.builder
                    .build_store(ptr, ret)
                    .map_err(CompileError::BuilderError)?;
                Ok(ptr.as_any_value_enum())
            }
            ExprUnaryOp::DecrementPre => {
                let ptr = self.compile_expression(*expr.expr)?.into_pointer_value();
                let val = self
                    .builder
                    .build_load(ptr, "load")
                    .map_err(CompileError::BuilderError)?;
                let ret = match val {
                    BasicValueEnum::IntValue(int_value) => {
                        let one = int_value.get_type().const_int(1, false);
                        self.builder
                            .build_int_sub(int_value, one, "dec")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    }
                    BasicValueEnum::PointerValue(ptr_value) => unsafe {
                        let mone = self
                            .context
                            .i32_type()
                            .const_int(1 as u64, true)
                            .const_neg();
                        self.builder
                            .build_gep(ptr_value, &[mone], "ptr_dec")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    },
                    _ => unreachable!("DecrementPre on non-integer/pointer type"),
                }?;
                self.builder
                    .build_store(ptr, ret)
                    .map_err(CompileError::BuilderError)?;
                Ok(ptr.as_any_value_enum())
            }
            ExprUnaryOp::IncrementPost => {
                let ptr = self.compile_expression(*expr.expr)?.into_pointer_value();
                let val = self
                    .builder
                    .build_load(ptr, "load")
                    .map_err(CompileError::BuilderError)?;
                let ret = match val {
                    BasicValueEnum::IntValue(int_value) => {
                        let one = int_value.get_type().const_int(1, false);
                        self.builder
                            .build_int_add(int_value, one, "inc")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    }
                    BasicValueEnum::PointerValue(ptr_value) => unsafe {
                        let one = self.context.i32_type().const_int(1, false);
                        self.builder
                            .build_gep(ptr_value, &[one], "ptr_inc")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    },
                    _ => unreachable!("IncrementPost on non-integer/pointer type"),
                }?;
                self.builder
                    .build_store(ptr, ret)
                    .map_err(CompileError::BuilderError)?;
                Ok(val.as_any_value_enum())
            }
            ExprUnaryOp::DecrementPost => {
                let ptr = self.compile_expression(*expr.expr)?.into_pointer_value();
                let val = self
                    .builder
                    .build_load(ptr, "load")
                    .map_err(CompileError::BuilderError)?;
                let ret = match val {
                    BasicValueEnum::IntValue(int_value) => {
                        let one = int_value.get_type().const_int(1, false);
                        self.builder
                            .build_int_sub(int_value, one, "dec")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    }
                    BasicValueEnum::PointerValue(ptr_value) => unsafe {
                        let mone = self
                            .context
                            .i32_type()
                            .const_int(1 as u64, true)
                            .const_neg();
                        self.builder
                            .build_gep(ptr_value, &[mone], "ptr_dec")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_basic_value_enum())
                    },
                    _ => unreachable!("DecrementPost on non-integer/pointer type"),
                }?;
                self.builder
                    .build_store(ptr, ret)
                    .map_err(CompileError::BuilderError)?;
                Ok(val.as_any_value_enum())
            }
        }
    }
    fn compile_expression_binary(
        &mut self,
        expr: ExprBinary,
    ) -> Result<ValueType<'ctx>, CompileError> {
        use crate::semantic::ExprBinaryOp;
        match expr.op {
            ExprBinaryOp::Add => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_int_add(lhs, rhs, "add")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_add(lhs, rhs, "fadd")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    (AnyValueEnum::PointerValue(ptr), AnyValueEnum::IntValue(int)) => unsafe {
                        self.builder
                            .build_gep(ptr, &[int], "ptr_add")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    },
                    _ => unreachable!("BinaryAdd on invalid type"),
                }
            }
            ExprBinaryOp::Sub => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_int_sub(lhs, rhs, "sub")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_sub(lhs, rhs, "fsub")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    (AnyValueEnum::PointerValue(ptr), AnyValueEnum::IntValue(int)) => unsafe {
                        let idx = self
                            .builder
                            .build_int_neg(int, "index_neg")
                            .map_err(CompileError::BuilderError)?;
                        self.builder
                            .build_gep(ptr, &[idx], "ptr_sub")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    },
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => {
                        let diff = self
                            .builder
                            .build_ptr_diff(lhs, rhs, "ptr_diff")
                            .map_err(CompileError::BuilderError)?;
                        if diff.get_type() != self.context.i64_type() {
                            self.builder
                                .build_int_s_extend(diff, self.context.i64_type(), "ptr_diff_cast")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        } else {
                            Ok(diff.as_any_value_enum())
                        }
                    }
                    _ => unreachable!("Sub on non-numeric type"),
                }
            }
            ExprBinaryOp::Mul => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_int_mul(lhs, rhs, "mul")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_mul(lhs, rhs, "fmul")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    _ => unreachable!("Mul on non-numeric type"),
                }
            }
            ExprBinaryOp::Div => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_signed_div(lhs, rhs, "sdiv")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        } else {
                            self.builder
                                .build_int_unsigned_div(lhs, rhs, "udiv")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        }
                    }
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_div(lhs, rhs, "fdiv")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),
                    _ => unreachable!("Div on non-numeric type"),
                }
            }
            ExprBinaryOp::Mod => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_signed_rem(lhs, rhs, "srem")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        } else {
                            self.builder
                                .build_int_unsigned_rem(lhs, rhs, "urem")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        }
                    }
                    _ => unreachable!("Div on non-numeric type"),
                }
            }

            ExprBinaryOp::BitwiseAnd => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_and(lhs, rhs, "bitand")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),

                    _ => unreachable!("BitwiseAnd on non-integer type"),
                }
            }
            ExprBinaryOp::BitwiseOr => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_or(lhs, rhs, "bitor")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),

                    _ => unreachable!("BitwiseOr on non-integer type"),
                }
            }
            ExprBinaryOp::BitwiseXor => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_xor(lhs, rhs, "bitxor")
                        .map_err(CompileError::BuilderError)
                        .map(|v| v.as_any_value_enum()),

                    _ => unreachable!("BitwiseXor on non-integer type"),
                }
            }
            ExprBinaryOp::ShiftLeft => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(mut rhs)) => {
                        // lhs rhs must be same-width integer
                        if rhs.get_type().get_bit_width() != lhs.get_type().get_bit_width() {
                            rhs = self
                                .builder
                                .build_int_cast(rhs, lhs.get_type(), "cast")
                                .map_err(CompileError::BuilderError)?;
                        }
                        self.builder
                            .build_left_shift(lhs, rhs, "shl")
                            .map_err(CompileError::BuilderError)
                            .map(|v| v.as_any_value_enum())
                    }
                    _ => unreachable!("ShiftLeft on non-integer type"),
                }
            }
            ExprBinaryOp::ShiftRight => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(mut rhs)) => {
                        // lhs rhs must be same-width integer
                        if rhs.get_type().get_bit_width() != lhs.get_type().get_bit_width() {
                            rhs = self
                                .builder
                                .build_int_cast(rhs, lhs.get_type(), "cast")
                                .map_err(CompileError::BuilderError)?;
                        }

                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_right_shift(lhs, rhs, true, "ashr")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        } else {
                            self.builder
                                .build_right_shift(lhs, rhs, false, "lshr")
                                .map_err(CompileError::BuilderError)
                                .map(|v| v.as_any_value_enum())
                        }
                    }
                    _ => unreachable!("ShiftRight on non-integer type"),
                }
            }
            ExprBinaryOp::Equal => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "eq")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OEQ, lhs, rhs, "feq")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "ptr_eq")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("Equal on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "ptr_zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }
            ExprBinaryOp::NotEqual => {
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "ne")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::ONE, lhs, rhs, "fne")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "ptr_ne")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("Equal on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "ptr_zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }
            ExprBinaryOp::LessThan => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "slt")
                                .map_err(CompileError::BuilderError)?
                        } else {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::ULT, lhs, rhs, "ult")
                                .map_err(CompileError::BuilderError)?
                        }
                    }
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OLT, lhs, rhs, "folt")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::ULT, lhs, rhs, "ptr_ult")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("LessThan on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }
            ExprBinaryOp::LessThanOrEqual => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::SLE, lhs, rhs, "sle")
                                .map_err(CompileError::BuilderError)?
                        } else {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::ULE, lhs, rhs, "ule")
                                .map_err(CompileError::BuilderError)?
                        }
                    }
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OLE, lhs, rhs, "fole")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::ULE, lhs, rhs, "ptr_ule")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("LessThan on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }

            ExprBinaryOp::GreaterThan => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::SGT, lhs, rhs, "sgt")
                                .map_err(CompileError::BuilderError)?
                        } else {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::UGT, lhs, rhs, "ugt")
                                .map_err(CompileError::BuilderError)?
                        }
                    }
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OGT, lhs, rhs, "fogt")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::UGT, lhs, rhs, "ptr_ugt")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("LessThan on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }
            ExprBinaryOp::GreaterThanOrEqual => {
                let lhs_type = expr.lhs.cv_type().unwrap().type_;
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                let res_bool = match (lhs, rhs) {
                    (AnyValueEnum::IntValue(lhs), AnyValueEnum::IntValue(rhs)) => {
                        let PrimitiveType::Integer(int_type) = lhs_type else {
                            unreachable!();
                        };
                        if int_type.is_signed() {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::SGE, lhs, rhs, "sge")
                                .map_err(CompileError::BuilderError)?
                        } else {
                            self.builder
                                .build_int_compare(inkwell::IntPredicate::UGE, lhs, rhs, "uge")
                                .map_err(CompileError::BuilderError)?
                        }
                    }
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OGE, lhs, rhs, "foge")
                        .map_err(CompileError::BuilderError)?,
                    (AnyValueEnum::PointerValue(lhs), AnyValueEnum::PointerValue(rhs)) => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::UGE, lhs, rhs, "ptr_uge")
                        .map_err(CompileError::BuilderError)?,
                    _ => unreachable!("LessThan on non-integer/pointer type"),
                };
                self.builder
                    .build_int_z_extend(res_bool, self.context.i8_type(), "zext")
                    .map_err(CompileError::BuilderError)
                    .map(|v| v.as_any_value_enum())
            }
            ExprBinaryOp::Assign(_) => {
                let lhs_ptr = self.compile_expression(*expr.lhs)?.into_pointer_value();
                let rhs = self.compile_expression_deref(*expr.rhs)?;

                match rhs {
                    AnyValueEnum::IntValue(rhs) => {
                        self.builder
                            .build_store(lhs_ptr, rhs)
                            .map_err(CompileError::BuilderError)?;
                    }
                    AnyValueEnum::FloatValue(rhs) => {
                        self.builder
                            .build_store(lhs_ptr, rhs)
                            .map_err(CompileError::BuilderError)?;
                    }
                    AnyValueEnum::PointerValue(rhs) => {
                        self.builder
                            .build_store(lhs_ptr, rhs)
                            .map_err(CompileError::BuilderError)?;
                    }
                    AnyValueEnum::StructValue(rhs) => {
                        self.builder
                            .build_store(lhs_ptr, rhs)
                            .map_err(CompileError::BuilderError)?;
                    }
                    _ => unreachable!("Assign to non-pointer type"),
                }

                Ok(lhs_ptr.as_any_value_enum())
            }
            ExprBinaryOp::LogicalAnd => {
                let then_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_and_then");
                let else_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_and_else");
                let merge_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_and_merge");
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let lhs = self.to_1bit_bool(lhs)?;
                self.builder
                    .build_conditional_branch(lhs, then_block, else_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(then_block);
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                let rhs = self.to_1bit_bool(rhs)?;
                let rhs = self
                    .builder
                    .build_int_z_extend(rhs, self.context.i8_type(), "logical_and_rhs_zext")
                    .map_err(CompileError::BuilderError)?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(else_block);
                // false
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.context.i8_type(), "logical_and_phi")
                    .map_err(CompileError::BuilderError)?;
                phi.add_incoming(&[
                    (&rhs, then_block),
                    (&self.context.i8_type().const_int(0, false), else_block),
                ]);

                Ok(phi.as_basic_value().into_int_value().as_any_value_enum())
            }
            ExprBinaryOp::LogicalOr => {
                let then_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_or_then");
                let else_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_or_else");
                let merge_block = self
                    .context
                    .append_basic_block(self.current_function.unwrap(), "logical_or_merge");
                let lhs = self.compile_expression_deref(*expr.lhs)?;
                let lhs = self.to_1bit_bool(lhs)?;
                self.builder
                    .build_conditional_branch(lhs, then_block, else_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(then_block);
                // true
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(else_block);
                let rhs = self.compile_expression_deref(*expr.rhs)?;
                let rhs = self.to_1bit_bool(rhs)?;
                let rhs = self
                    .builder
                    .build_int_z_extend(rhs, self.context.i8_type(), "logical_and_rhs_zext")
                    .map_err(CompileError::BuilderError)?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(CompileError::BuilderError)?;

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.context.i8_type(), "logical_and_phi")
                    .map_err(CompileError::BuilderError)?;
                phi.add_incoming(&[
                    (&self.context.i8_type().const_int(1, false), then_block),
                    (&rhs, else_block),
                ]);

                Ok(phi.as_basic_value().into_int_value().as_any_value_enum())
            }

            /*
            ExprBinaryOp::Comma => {}
            */
            ExprBinaryOp::AddAssign
            | ExprBinaryOp::SubAssign
            | ExprBinaryOp::MulAssign
            | ExprBinaryOp::DivAssign
            | ExprBinaryOp::ModAssign
            | ExprBinaryOp::BitwiseAndAssign
            | ExprBinaryOp::BitwiseOrAssign
            | ExprBinaryOp::BitwiseXorAssign
            | ExprBinaryOp::ShiftLeftAssign
            | ExprBinaryOp::ShiftRightAssign => unreachable!("Should be handled before"),
            _ => unimplemented!("binary expression {:?} is not supported yet", expr.op),
        }
    }
    fn compile_expression_initializerlist(
        &mut self,
        expr: ExprInitializerList,
    ) -> Result<ValueType<'ctx>, CompileError> {
        unimplemented!("initializer list expression is not supported yet")
    }

    fn compile_expression_default(
        &mut self,
        t: PrimitiveType,
    ) -> Result<ValueType<'ctx>, CompileError> {
        let t: BasicTypeEnum = t.to_llvm_type(&self.context).try_into().unwrap();
        Ok(t.const_zero().as_any_value_enum())
    }
}
