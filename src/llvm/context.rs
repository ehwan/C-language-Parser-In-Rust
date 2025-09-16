use inkwell::{
    types::{AnyType, AnyTypeEnum, AsTypeRef, BasicType, BasicTypeEnum},
    AddressSpace,
};

use crate::{llvm::context, semantic::*};
pub struct Context {
    context: inkwell::context::Context,
}
impl Context {
    pub fn new() -> Self {
        let context = inkwell::context::Context::create();
        Self { context }
    }

    pub fn compile(&mut self, ast: TranslationUnit) -> Result<(), CompileError> {
        let mut internal = ContextInternal::new(&self.context);
        internal.compile(ast)
    }
}

pub type CompileError = ();

impl PrimitiveType {
    fn to_llvm_type<'ctx>(&self, context: &'ctx inkwell::context::Context) -> AnyTypeEnum<'ctx> {
        match self {
            PrimitiveType::Void => context.void_type().as_any_type_enum(),
            PrimitiveType::Integer(integer) => match integer {
                Integer::Int8 => context.i8_type().as_any_type_enum(),
                Integer::Int16 => context.i16_type().as_any_type_enum(),
                Integer::Int32 => context.i32_type().as_any_type_enum(),
                Integer::Int64 => context.i64_type().as_any_type_enum(),
                Integer::UInt8 => context.i8_type().as_any_type_enum(),
                Integer::UInt16 => context.i16_type().as_any_type_enum(),
                Integer::UInt32 => context.i32_type().as_any_type_enum(),
                Integer::UInt64 => context.i64_type().as_any_type_enum(),
            },
            PrimitiveType::Float(float) => match float {
                Float::Float32 => context.f32_type().as_any_type_enum(),
                Float::Float64 => context.f64_type().as_any_type_enum(),
            },

            PrimitiveType::Pointer(_) => context.i64_type().as_any_type_enum(),
            PrimitiveType::Array(array) => {
                let basic_type: BasicTypeEnum = array
                    .cv_type
                    .type_
                    .to_llvm_type(context)
                    .try_into()
                    .unwrap();
                basic_type.array_type(array.size as u32).as_any_type_enum()
            }
            PrimitiveType::Struct(s) => {
                let body = s.body.as_ref().unwrap();
                let members = body
                    .members
                    .iter()
                    .map(|m| {
                        let basic_type: BasicTypeEnum =
                            m.cv_type.type_.to_llvm_type(context).try_into().unwrap();
                        basic_type
                    })
                    .collect::<Vec<_>>();
                context.struct_type(&members, false).as_any_type_enum()
            }
            PrimitiveType::Union(_) => {
                unreachable!("union type is not supported yet")
            }
            PrimitiveType::Enum(_) => context.i64_type().as_any_type_enum(),
            PrimitiveType::Function(f) => {
                let return_type: BasicTypeEnum =
                    f.return_type.to_llvm_type(context).try_into().unwrap();

                let param_types = f
                    .args
                    .iter()
                    .map(|p| {
                        let basic_type: BasicTypeEnum =
                            p.cv_type.type_.to_llvm_type(context).try_into().unwrap();
                        basic_type.into()
                    })
                    .collect::<Vec<_>>();
                return_type
                    .fn_type(&param_types, f.variadic)
                    .as_any_type_enum()
            }
        }
    }
}
pub struct ContextInternal<'ctx> {
    context: &'ctx inkwell::context::Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,
}

impl<'ctx> ContextInternal<'ctx> {
    pub fn new(context: &'ctx inkwell::context::Context) -> Self {
        let module = context.create_module("main_module");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
        }
    }

    pub fn compile(&mut self, ast: TranslationUnit) -> Result<(), CompileError> {
        for (var_name, var_def) in ast.variables.into_iter() {
            let basic_type: BasicTypeEnum = var_def
                .cv_type
                .type_
                .to_llvm_type(&self.context)
                .try_into()
                .unwrap();
            let global_var =
                self.module
                    .add_global(basic_type, Some(AddressSpace::default()), &var_name);
        }
        for (func_name, func_def) in ast.functions.into_iter() {
            let args = func_def.type_.args.clone();
            let function_type = PrimitiveType::Function(func_def.type_)
                .to_llvm_type(&self.context)
                .into_function_type();

            let function = self.module.add_function(&func_name, function_type, None);
            let function_block = self.context.append_basic_block(function, "func_block");
            self.builder.position_at_end(function_block);

            // inkwell::types::AnyType
            // context.i16_type().as_any_type_enum()
        }
        Ok(())
    }
    fn compile_statement(&mut self, stmt: Statement) -> Result<(), CompileError> {
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
            _ => {
                unreachable!("unreachable statement: {:?}", stmt)
            }
        }
    }
    fn compile_expression(&mut self, expr: Expression) -> Result<(), CompileError> {
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
            _ => {
                unreachable!("unreachable expression: {:?}", expr);
            }
        }
    }
}

impl<'ctx> ContextInternal<'ctx> {
    fn compile_statement_expression(&mut self, stmt: StmtExpression) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_labeled(&mut self, stmt: StmtLabeled) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_goto(&mut self, stmt: StmtGoto) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_compound(&mut self, stmt: StmtCompound) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_if(&mut self, stmt: StmtIf) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_switch(&mut self, stmt: StmtSwitch) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_continue(&mut self) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_break(&mut self) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_return(&mut self, stmt: StmtReturn) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_for(&mut self, stmt: StmtFor) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_while(&mut self, stmt: StmtWhile) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_dowhile(&mut self, stmt: StmtDoWhile) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_statement_variabledeclaration(
        &mut self,
        stmt: StmtVariableDeclaration,
    ) -> Result<(), CompileError> {
        Ok(())
    }
}

impl<'ctx> ContextInternal<'ctx> {
    fn compile_expression_integer(
        &mut self,
        value: i64,
        type_: Integer,
    ) -> Result<(), CompileError> {
        match type_ {
            Integer::Int8 => {}
            Integer::Int16 => {}
            Integer::Int32 => {}
            Integer::Int64 => {}
            Integer::UInt8 => {}
            Integer::UInt16 => {}
            Integer::UInt32 => {}
            Integer::UInt64 => {}
        }
        Ok(())
    }
    fn compile_expression_float(&mut self, value: f64, type_: Float) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_string(&mut self, value: String) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_variable(&mut self, info: VariableInfo) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_conditional(
        &mut self,
        expr: ExprConditional,
    ) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_cast(&mut self, expr: ExprCast) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_member(&mut self, expr: ExprMember) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_arrow(&mut self, expr: ExprMember) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_paren(&mut self, expr: ExprParen) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_bracket(&mut self, expr: ExprBracket) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_unary(&mut self, expr: ExprUnary) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_binary(&mut self, expr: ExprBinary) -> Result<(), CompileError> {
        Ok(())
    }
    fn compile_expression_initializerlist(
        &mut self,
        expr: ExprInitializerList,
    ) -> Result<(), CompileError> {
        Ok(())
    }
}
