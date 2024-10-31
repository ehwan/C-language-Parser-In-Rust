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
            Statement::None => {}
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
        Ok(())
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
        Ok(())
    }
}

impl InstructionGenerator {
    fn emit_statement_expression(&mut self, stmt: ast2::StmtExpression) {}
    fn emit_statement_labeled(&mut self, stmt: ast2::StmtLabeled) {}
    fn emit_statement_goto(&mut self, stmt: ast2::StmtGoto) {}
    fn emit_statement_compound(&mut self, stmt: ast2::StmtCompound) {}
    fn emit_statement_if(&mut self, stmt: ast2::StmtIf) {}
    fn emit_statement_switch(&mut self, stmt: ast2::StmtSwitch) {}
    fn emit_statement_continue(&mut self) {}
    fn emit_statement_break(&mut self) {}
    fn emit_statement_return(&mut self, stmt: ast2::StmtReturn) {}
    fn emit_statement_for(&mut self, stmt: ast2::StmtFor) {}
    fn emit_statement_while(&mut self, stmt: ast2::StmtWhile) {}
    fn emit_statement_dowhile(&mut self, stmt: ast2::StmtDoWhile) {}
    fn emit_statement_variable_declaration(&mut self, stmt: ast2::StmtVariableDeclaration) {}
}

impl InstructionGenerator {
    fn emit_expression_i8(&mut self, expr: i8) {
        self.instructions.push(Instruction::I64(expr as i64));
    }
    fn emit_expression_i16(&mut self, expr: i16) {
        self.instructions.push(Instruction::I64(expr as i64));
    }
    fn emit_expression_i32(&mut self, expr: i32) {
        self.instructions.push(Instruction::I64(expr as i64));
    }
    fn emit_expression_i64(&mut self, expr: i64) {
        self.instructions.push(Instruction::I64(expr));
    }
    fn emit_expression_u8(&mut self, expr: u8) {
        self.instructions.push(Instruction::U64(expr as u64));
    }
    fn emit_expression_u16(&mut self, expr: u16) {
        self.instructions.push(Instruction::U64(expr as u64));
    }
    fn emit_expression_u32(&mut self, expr: u32) {
        self.instructions.push(Instruction::U64(expr as u64));
    }
    fn emit_expression_u64(&mut self, expr: u64) {
        self.instructions.push(Instruction::U64(expr));
    }
    fn emit_expression_f32(&mut self, expr: f32) {
        let bits = expr.to_bits();
        self.instructions.push(Instruction::U64(bits as u64));
    }
    fn emit_expression_f64(&mut self, expr: f64) {
        let bits = expr.to_bits();
        self.instructions.push(Instruction::U64(bits));
    }
    fn emit_expression_string(&mut self, expr: String) {
        unimplemented!("emit_expression_string");
    }
    fn emit_expression_variable(&mut self, expr: VariableInfo) {
        self.instructions
            .push(Instruction::U64(expr.address.into_u64()));
    }
    fn emit_expression_conditional(&mut self, expr: ast2::ExprConditional) {
        let else_label = self.generate_label();
        let end_label = self.generate_label();

        if self.cond.is_return_reference(instructions) {
            self.emit_expression(*expr.cond);

            instructions.push(Instruction::JumpZero(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            }));
        } else {
            self.emit_expression(*expr.cond);

            instructions.push(Instruction::JumpZero(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }

        self.then_expr.emit(instructions);
        if self.then_expr.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            }));
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::Jump(Jump {
            label: end_label.clone(),
        }));
        instructions.set_label(&else_label);
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
    fn emit_expression_cast(&mut self, expr: ast2::ExprCast) {}
    fn emit_expression_member(&mut self, expr: ast2::ExprMember) {}
    fn emit_expression_arrow(&mut self, expr: ast2::ExprMember) {}
    fn emit_expression_paren(&mut self, expr: ast2::ExprParen) {}
    fn emit_expression_bracket(&mut self, expr: ast2::ExprBracket) {}
    fn emit_expression_unary(&mut self, expr: ast2::ExprUnary) {}
    fn emit_expression_binary(&mut self, expr: ast2::ExprBinary) {}
    fn emit_expression_initializerlist(&mut self, expr: ast2::ExprInitializerList) {}
}
