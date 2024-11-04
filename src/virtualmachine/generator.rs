use super::Instruction;
use super::LabelType;
use super::VirtualMachine;

use crate::ast2;
use ast2::Address;
use ast2::CompileError;
use ast2::Expression;
use ast2::Statement;
use ast2::VariableInfo;

#[derive(Debug)]
pub struct InstructionGenerator {
    /// generated instructions
    pub instructions: Vec<Instruction>,

    /// label map
    pub labels: Vec<Option<usize>>,
    /// label stack for continue, break
    ///                  default, break for switch statement
    pub label_stack: Vec<(LabelType, LabelType)>,

    /// where constant data is stored
    /// for same-address system with stack data, it will be merged into stack upon program execution
    ///
    /// commonly, this is used for read-only data
    /// but we don't have `const` qualifier yet
    /// so it is mutable ...
    pub text_section: Vec<u8>,
}
impl InstructionGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: Vec::new(),
            label_stack: Vec::new(),
            text_section: Vec::new(),
        }
    }

    fn generate_label(&mut self) -> LabelType {
        let label = self.labels.len();
        self.labels.push(None);
        label
    }
    /// link label name to current instruction address
    fn set_label(&mut self, label: LabelType) {
        let addr = self.instructions.len();
        self.labels[label] = Some(addr);
    }

    pub fn emit(
        &mut self,
        translation_unit: ast2::TranslationUnit,
    ) -> Result<VirtualMachine, CompileError> {
        for statement in translation_unit.statements {
            self.emit_statement(statement)?;
        }

        let mut vm = VirtualMachine::new();
        vm.label_map = std::mem::take(&mut self.labels)
            .into_iter()
            .map(|x| x.unwrap())
            .collect();
        vm.instructions = std::mem::take(&mut self.instructions);
        Ok(vm)
    }

    pub fn emit_statement(&mut self, statement: Statement) -> Result<(), CompileError> {
        match statement {
            Statement::None => Ok(()),
            Statement::Expression(stmt) => self.emit_statement_expression(stmt),
            Statement::Labeled(stmt) => self.emit_statement_labeled(stmt),
            Statement::Goto(stmt) => self.emit_statement_goto(stmt),
            Statement::Compound(stmt) => self.emit_statement_compound(stmt),
            Statement::If(stmt) => self.emit_statement_if(stmt),
            Statement::Switch(stmt) => self.emit_statement_switch(stmt),
            Statement::_Case(_) => unreachable!("_Case should not be visible to end-users"),
            Statement::_Default(_) => {
                unreachable!("_Default should not be visible to end-users")
            }
            Statement::Continue => self.emit_statement_continue(),
            Statement::Break => self.emit_statement_break(),
            Statement::Return(stmt) => self.emit_statement_return(stmt),
            Statement::For(stmt) => self.emit_statement_for(stmt),
            Statement::While(stmt) => self.emit_statement_while(stmt),
            Statement::DoWhile(stmt) => self.emit_statement_dowhile(stmt),
            Statement::VariableDeclaration(stmt) => self.emit_statement_variable_declaration(stmt),
        }
    }

    pub fn emit_expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::I8(expr) => self.emit_expression_i8(expr),
            Expression::I16(expr) => self.emit_expression_i16(expr),
            Expression::I32(expr) => self.emit_expression_i32(expr),
            Expression::I64(expr) => self.emit_expression_i64(expr),
            Expression::U8(expr) => self.emit_expression_u8(expr),
            Expression::U16(expr) => self.emit_expression_u16(expr),
            Expression::U32(expr) => self.emit_expression_u32(expr),
            Expression::U64(expr) => self.emit_expression_u64(expr),
            Expression::F32(expr) => self.emit_expression_f32(expr),
            Expression::F64(expr) => self.emit_expression_f64(expr),
            Expression::String(expr) => self.emit_expression_string(expr),
            Expression::Variable(expr) => self.emit_expression_variable(expr),
            Expression::Conditional(expr) => self.emit_expression_conditional(expr),
            Expression::Cast(expr) => self.emit_expression_cast(expr),
            Expression::Member(expr) => self.emit_expression_member(expr),
            Expression::Arrow(expr) => self.emit_expression_arrow(expr),
            Expression::Paren(expr) => self.emit_expression_paren(expr),
            Expression::Bracket(expr) => self.emit_expression_bracket(expr),
            Expression::Unary(expr) => self.emit_expression_unary(expr),
            Expression::Binary(expr) => self.emit_expression_binary(expr),
            Expression::InitializerList(expr) => self.emit_expression_initializerlist(expr),
        }
    }
    pub fn emit_expression_derefed(&mut self, expression: Expression) -> Result<(), CompileError> {
        let size = expression.cv_type()?.type_.sizeof()?;
        if expression.is_address() {
            self.emit_expression(expression)?;
            self.instructions.push(Instruction::Deref(size));
        } else {
            self.emit_expression(expression)?;
        }
        Ok(())
    }
}

impl InstructionGenerator {
    fn emit_statement_expression(
        &mut self,
        stmt: ast2::StmtExpression,
    ) -> Result<(), CompileError> {
        self.emit_expression(*stmt.expr)
    }
    fn emit_statement_labeled(&mut self, stmt: ast2::StmtLabeled) -> Result<(), CompileError> {}
    fn emit_statement_goto(&mut self, stmt: ast2::StmtGoto) -> Result<(), CompileError> {}
    fn emit_statement_compound(&mut self, stmt: ast2::StmtCompound) -> Result<(), CompileError> {}
    fn emit_statement_if(&mut self, stmt: ast2::StmtIf) -> Result<(), CompileError> {}
    fn emit_statement_switch(&mut self, stmt: ast2::StmtSwitch) -> Result<(), CompileError> {}
    fn emit_statement_continue(&mut self) -> Result<(), CompileError> {}
    fn emit_statement_break(&mut self) -> Result<(), CompileError> {}
    fn emit_statement_return(&mut self, stmt: ast2::StmtReturn) -> Result<(), CompileError> {}
    fn emit_statement_for(&mut self, stmt: ast2::StmtFor) -> Result<(), CompileError> {}
    fn emit_statement_while(&mut self, stmt: ast2::StmtWhile) -> Result<(), CompileError> {}
    fn emit_statement_dowhile(&mut self, stmt: ast2::StmtDoWhile) -> Result<(), CompileError> {}
    fn emit_statement_variable_declaration(
        &mut self,
        stmt: ast2::StmtVariableDeclaration,
    ) -> Result<(), CompileError> {
    }
}

impl InstructionGenerator {
    fn emit_expression_i8(&mut self, expr: i8) -> Result<(), CompileError> {
        self.instructions.push(Instruction::I64(expr as i64));
        Ok(())
    }
    fn emit_expression_i16(&mut self, expr: i16) -> Result<(), CompileError> {
        self.instructions.push(Instruction::I64(expr as i64));
        Ok(())
    }
    fn emit_expression_i32(&mut self, expr: i32) -> Result<(), CompileError> {
        self.instructions.push(Instruction::I64(expr as i64));
        Ok(())
    }
    fn emit_expression_i64(&mut self, expr: i64) -> Result<(), CompileError> {
        self.instructions.push(Instruction::I64(expr));
        Ok(())
    }
    fn emit_expression_u8(&mut self, expr: u8) -> Result<(), CompileError> {
        self.instructions.push(Instruction::U64(expr as u64));
        Ok(())
    }
    fn emit_expression_u16(&mut self, expr: u16) -> Result<(), CompileError> {
        self.instructions.push(Instruction::U64(expr as u64));
        Ok(())
    }
    fn emit_expression_u32(&mut self, expr: u32) -> Result<(), CompileError> {
        self.instructions.push(Instruction::U64(expr as u64));
        Ok(())
    }
    fn emit_expression_u64(&mut self, expr: u64) -> Result<(), CompileError> {
        self.instructions.push(Instruction::U64(expr));
        Ok(())
    }
    fn emit_expression_f32(&mut self, expr: f32) -> Result<(), CompileError> {
        let bits = expr.to_bits();
        self.instructions.push(Instruction::U64(bits as u64));
        Ok(())
    }
    fn emit_expression_f64(&mut self, expr: f64) -> Result<(), CompileError> {
        let bits = expr.to_bits();
        self.instructions.push(Instruction::U64(bits));
        Ok(())
    }
    fn emit_expression_string(&mut self, expr: String) -> Result<(), CompileError> {
        unimplemented!("emit_expression_string");
    }
    fn emit_expression_variable(&mut self, expr: VariableInfo) -> Result<(), CompileError> {
        self.instructions
            .push(Instruction::U64(expr.address.into_u64()));
        Ok(())
    }
    fn emit_expression_conditional(
        &mut self,
        expr: ast2::ExprConditional,
    ) -> Result<(), CompileError> {
        let else_label = self.generate_label();
        let end_label = self.generate_label();
        let cond_size = expr.cond.cv_type()?.type_.sizeof()?;

        self.emit_expression_derefed(*expr.cond)?;
        self.instructions
            .push(Instruction::JumpZero(cond_size, else_label));

        let then_size = expr.then_expr.cv_type()?.type_.sizeof()?;
        self.emit_expression_derefed(*expr.then_expr)?;
        self.instructions.push(Instruction::Jump(end_label));
        self.set_label(else_label);
        self.emit_expression_derefed(*expr.else_expr)?;
        self.else_expr.emit(instructions);
        if self.else_expr.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            }));
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            }));
        }
        instructions.set_label(&end_label);
    }
    fn emit_expression_cast(&mut self, expr: ast2::ExprCast) -> Result<(), CompileError> {}
    fn emit_expression_member(&mut self, expr: ast2::ExprMember) -> Result<(), CompileError> {}
    fn emit_expression_arrow(&mut self, expr: ast2::ExprMember) -> Result<(), CompileError> {}
    fn emit_expression_paren(&mut self, expr: ast2::ExprParen) -> Result<(), CompileError> {}
    fn emit_expression_bracket(&mut self, expr: ast2::ExprBracket) -> Result<(), CompileError> {}
    fn emit_expression_unary(&mut self, expr: ast2::ExprUnary) -> Result<(), CompileError> {}
    fn emit_expression_binary(&mut self, expr: ast2::ExprBinary) -> Result<(), CompileError> {}
    fn emit_expression_initializerlist(
        &mut self,
        expr: ast2::ExprInitializerList,
    ) -> Result<(), CompileError> {
    }
}
