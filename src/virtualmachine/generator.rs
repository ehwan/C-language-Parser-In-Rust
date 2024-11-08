use std::collections::HashMap;

use super::Instruction;
use super::LabelType;
use super::Operand;
use super::SizeType;
use super::VirtualMachine;

use crate::ast2;
use ast2::Address;
use ast2::CompileError;
use ast2::ExprBinaryOp;
use ast2::ExprUnaryOp;
use ast2::Expression;
use ast2::Float;
use ast2::Integer;
use ast2::PrimitiveType;
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

    pub(crate) user_defined_labels: HashMap<String, LabelType>,
}
impl InstructionGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: Vec::new(),
            label_stack: Vec::new(),
            user_defined_labels: HashMap::new(),
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
        // @TODO functions
        vm.text = translation_unit.text;
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

    pub fn emit_expression(&mut self, expression: Expression) -> Result<SizeType, CompileError> {
        match expression {
            Expression::Signed(value, type_) => self.emit_expression_integer(value as u64, type_),
            Expression::Unsigned(value, type_) => self.emit_expression_integer(value, type_),
            Expression::Float(value, type_) => self.emit_expression_float(value, type_),
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
    pub fn emit_expression_derefed(
        &mut self,
        expression: Expression,
    ) -> Result<SizeType, CompileError> {
        if expression.is_address() {
            let size = expression.cv_type()?.type_.sizeof()?;
            self.emit_expression(expression)?;
            match size {
                1 => {
                    self.instructions.push(Instruction::Move(
                        SizeType::Byte,
                        Operand::Deref(0),
                        Operand::Register(0),
                    ));
                    Ok(SizeType::Byte)
                }
                2 => {
                    self.instructions.push(Instruction::Move(
                        SizeType::Word,
                        Operand::Deref(0),
                        Operand::Register(0),
                    ));
                    Ok(SizeType::Word)
                }
                4 => {
                    self.instructions.push(Instruction::Move(
                        SizeType::DWord,
                        Operand::Deref(0),
                        Operand::Register(0),
                    ));
                    Ok(SizeType::DWord)
                }
                8 => {
                    self.instructions.push(Instruction::Move(
                        SizeType::QWord,
                        Operand::Deref(0),
                        Operand::Register(0),
                    ));
                    Ok(SizeType::QWord)
                }
                _ => unreachable!("emit_expression_derefed: {:?}", size),
            }
        } else {
            self.emit_expression(expression)
        }
    }
}

impl InstructionGenerator {
    fn emit_statement_expression(
        &mut self,
        stmt: ast2::StmtExpression,
    ) -> Result<(), CompileError> {
        self.emit_expression(stmt.expression)?;
        Ok(())
    }
    fn emit_statement_labeled(&mut self, stmt: ast2::StmtLabeled) -> Result<(), CompileError> {
        let name = stmt.label.borrow().name.clone();

        let label = if let Some(label) = self.user_defined_labels.get(&name) {
            *label
        } else {
            let label = self.generate_label();
            self.user_defined_labels.insert(name.clone(), label);
            label
        };
        self.set_label(label);
        self.emit_statement(*stmt.statement)
    }
    fn emit_statement_goto(&mut self, stmt: ast2::StmtGoto) -> Result<(), CompileError> {
        let name = stmt.label.borrow().name.clone();

        let label = if let Some(label) = self.user_defined_labels.get(&name) {
            *label
        } else {
            let label = self.generate_label();
            self.user_defined_labels.insert(name.clone(), label);
            label
        };

        self.instructions.push(Instruction::Jump(label));

        Ok(())
    }
    fn emit_statement_compound(&mut self, stmt: ast2::StmtCompound) -> Result<(), CompileError> {
        for s in stmt.statements {
            self.emit_statement(s)?;
        }
        Ok(())
    }
    fn emit_statement_if(&mut self, stmt: ast2::StmtIf) -> Result<(), CompileError> {
        let else_label = self.generate_label();
        let end_label = self.generate_label();
        let cond_size = self.emit_expression_derefed(stmt.condition)?;

        self.instructions.push(Instruction::JumpZero(
            cond_size,
            Operand::Register(0),
            else_label,
        ));
        self.emit_statement(*stmt.then)?;

        if let Some(else_stmt) = stmt.else_ {
            self.instructions.push(Instruction::Jump(end_label));
            self.set_label(else_label);
            self.emit_statement(*else_stmt)?;
        } else {
            self.set_label(else_label);
        }
        self.set_label(end_label);
        Ok(())
    }
    fn emit_statement_switch(&mut self, stmt: ast2::StmtSwitch) -> Result<(), CompileError> {
        unimplemented!("emit_statement_switch")
    }
    fn emit_statement_continue(&mut self) -> Result<(), CompileError> {
        let (continue_label, _) = self.label_stack.last().unwrap();
        self.instructions.push(Instruction::Jump(*continue_label));
        Ok(())
    }
    fn emit_statement_break(&mut self) -> Result<(), CompileError> {
        let (_, end_label) = self.label_stack.last().unwrap();
        self.instructions.push(Instruction::Jump(*end_label));
        Ok(())
    }
    fn emit_statement_return(&mut self, stmt: ast2::StmtReturn) -> Result<(), CompileError> {
        unimplemented!("emit_statement_return")
    }
    fn emit_statement_for(&mut self, stmt: ast2::StmtFor) -> Result<(), CompileError> {
        let start_label = self.generate_label();
        let continue_label = self.generate_label();
        let end_label = self.generate_label();

        self.label_stack.push((continue_label, end_label));

        self.emit_statement(*stmt.init)?;

        self.set_label(start_label);
        let cond_size = self.emit_expression_derefed(stmt.condition)?;
        self.instructions.push(Instruction::JumpZero(
            cond_size,
            Operand::Register(0),
            end_label,
        ));

        self.emit_statement(*stmt.body)?;
        self.set_label(continue_label);
        if let Some(next) = stmt.next {
            self.emit_expression(next)?;
        }
        self.instructions.push(Instruction::Jump(start_label));

        self.set_label(end_label);
        self.label_stack.pop();

        Ok(())
    }
    fn emit_statement_while(&mut self, stmt: ast2::StmtWhile) -> Result<(), CompileError> {
        let continue_label = self.generate_label();
        let end_label = self.generate_label();

        self.label_stack.push((continue_label, end_label));
        self.set_label(continue_label);
        let cond_size = self.emit_expression_derefed(stmt.condition)?;
        self.instructions.push(Instruction::JumpZero(
            cond_size,
            Operand::Register(0),
            end_label,
        ));

        self.emit_statement(*stmt.body)?;
        self.instructions.push(Instruction::Jump(continue_label));
        self.set_label(end_label);
        self.label_stack.pop();

        Ok(())
    }
    fn emit_statement_dowhile(&mut self, stmt: ast2::StmtDoWhile) -> Result<(), CompileError> {
        let start_label = self.generate_label();
        let continue_label = self.generate_label();
        let end_label = self.generate_label();

        self.label_stack.push((continue_label, end_label));
        self.set_label(start_label);
        self.emit_statement(*stmt.body)?;

        self.set_label(continue_label);
        let cond_size = self.emit_expression_derefed(stmt.condition)?;
        self.instructions.push(Instruction::JumpZero(
            cond_size,
            Operand::Register(0),
            end_label,
        ));
        self.instructions.push(Instruction::Jump(start_label));
        self.set_label(end_label);
        self.label_stack.pop();

        Ok(())
    }
    fn emit_statement_variable_declaration(
        &mut self,
        stmt: ast2::StmtVariableDeclaration,
    ) -> Result<(), CompileError> {
        for (var, init) in stmt.pairs.into_iter() {
            let addr = var.address.into_u64();
            if init.is_address() {
                let type_size = init.cv_type()?.type_.sizeof()?;
                self.emit_expression(init)?;
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Constant(addr),
                    Operand::Register(2),
                ));
                self.instructions.push(Instruction::Memcpy(
                    type_size,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
            } else {
                let size = self.emit_expression(init)?;
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Constant(addr),
                    Operand::Register(2),
                ));
                self.instructions.push(Instruction::Move(
                    size,
                    Operand::Register(0),
                    Operand::Deref(2),
                ));
            }
        }
        Ok(())
    }
}

impl InstructionGenerator {
    fn emit_expression_integer(
        &mut self,
        value: u64,
        type_: Integer,
    ) -> Result<SizeType, CompileError> {
        match type_ {
            Integer::Int8 => {
                self.instructions.push(Instruction::Move(
                    SizeType::Byte,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::Byte)
            }
            Integer::Int16 => {
                self.instructions.push(Instruction::Move(
                    SizeType::Word,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::Word)
            }
            Integer::Int32 => {
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            Integer::Int64 => {
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::QWord)
            }
            Integer::UInt8 => {
                self.instructions.push(Instruction::Move(
                    SizeType::Byte,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::Byte)
            }
            Integer::UInt16 => {
                self.instructions.push(Instruction::Move(
                    SizeType::Word,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::Word)
            }
            Integer::UInt32 => {
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            Integer::UInt64 => {
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Constant(value),
                    Operand::Register(0),
                ));
                Ok(SizeType::QWord)
            }
        }
    }
    fn emit_expression_float(
        &mut self,
        value: f64,
        type_: Float,
    ) -> Result<SizeType, CompileError> {
        match type_ {
            Float::Float32 => {
                let bits = (value as f32).to_bits();
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(bits as u64),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            Float::Float64 => {
                let bits = value.to_bits();
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Constant(bits),
                    Operand::Register(0),
                ));
                Ok(SizeType::QWord)
            }
        }
    }
    fn emit_expression_string(&mut self, addr: Address) -> Result<SizeType, CompileError> {
        let addr = addr.into_u64();
        self.instructions.push(Instruction::Move(
            SizeType::QWord,
            Operand::Constant(addr),
            Operand::Register(0),
        ));
        Ok(SizeType::QWord)
    }
    fn emit_expression_variable(&mut self, expr: VariableInfo) -> Result<SizeType, CompileError> {
        let addr = expr.address.into_u64();
        self.instructions.push(Instruction::Move(
            SizeType::QWord,
            Operand::Constant(addr),
            Operand::Register(0),
        ));
        Ok(SizeType::QWord)
    }
    fn emit_expression_conditional(
        &mut self,
        expr: ast2::ExprConditional,
    ) -> Result<SizeType, CompileError> {
        let else_label = self.generate_label();
        let end_label = self.generate_label();

        let cond_size = self.emit_expression_derefed(*expr.cond)?;
        self.instructions.push(Instruction::JumpZero(
            cond_size,
            Operand::Register(0),
            else_label,
        ));

        if expr.else_expr.is_address() && expr.then_expr.is_address() {
            self.emit_expression(*expr.then_expr)?;
            self.instructions.push(Instruction::Jump(end_label));
            self.set_label(else_label);
            let ret = self.emit_expression(*expr.else_expr)?;
            self.set_label(end_label);
            Ok(ret)
        } else {
            self.emit_expression_derefed(*expr.then_expr)?;
            self.instructions.push(Instruction::Jump(end_label));
            self.set_label(else_label);
            let ret = self.emit_expression_derefed(*expr.else_expr)?;
            self.set_label(end_label);
            Ok(ret)
        }
    }
    fn emit_expression_cast(&mut self, expr: ast2::ExprCast) -> Result<SizeType, CompileError> {
        let from_type = expr.expr.cv_type()?.type_;

        match (from_type, expr.type_) {
            (PrimitiveType::Integer(from), PrimitiveType::Integer(to)) => {
                self.emit_expression_derefed(*expr.expr)?;
                let from_size = from.sizeof();
                let to_size = to.sizeof();
                if from_size != to_size {
                    self.instructions.push(Instruction::I2I(from, to));
                }
                Ok(SizeType::from_size(to_size))
            }
            (PrimitiveType::Integer(from), PrimitiveType::Float(to)) => {
                self.emit_expression_derefed(*expr.expr)?;
                self.instructions.push(Instruction::I2F(from, to));
                Ok(SizeType::from_size(to.sizeof()))
            }
            (PrimitiveType::Integer(from), PrimitiveType::Pointer(_)) => {
                self.emit_expression_derefed(*expr.expr)?;
                if from.sizeof() != 8 {
                    self.instructions
                        .push(Instruction::I2I(from, Integer::UInt64));
                }
                Ok(SizeType::QWord)
            }

            (PrimitiveType::Float(from), PrimitiveType::Integer(to)) => {
                self.emit_expression_derefed(*expr.expr)?;
                self.instructions.push(Instruction::F2I(from, to));
                Ok(SizeType::from_size(to.sizeof()))
            }
            (PrimitiveType::Float(from), PrimitiveType::Float(to)) => {
                self.emit_expression_derefed(*expr.expr)?;
                if from != to {
                    self.instructions.push(Instruction::F2F(from, to));
                }
                Ok(SizeType::from_size(to.sizeof()))
            }

            (PrimitiveType::Pointer(_), PrimitiveType::Integer(to)) => {
                self.emit_expression_derefed(*expr.expr)?;
                let to_size = to.sizeof();
                if 64 != to_size {
                    self.instructions
                        .push(Instruction::I2I(Integer::UInt64, to));
                }
                Ok(SizeType::from_size(to_size))
            }
            (PrimitiveType::Pointer(_), PrimitiveType::Pointer(_)) => {
                self.emit_expression_derefed(*expr.expr)?;
                // do nothing
                Ok(SizeType::QWord)
            }

            (PrimitiveType::Array(_), PrimitiveType::Pointer(_)) => {
                self.emit_expression(*expr.expr)?;
                // do nothing
                Ok(SizeType::QWord)
            }
            (PrimitiveType::Array(_), PrimitiveType::Integer(to)) => {
                self.emit_expression(*expr.expr)?;
                let to_size = to.sizeof();
                if 64 != to_size {
                    self.instructions
                        .push(Instruction::I2I(Integer::UInt64, to));
                }
                Ok(SizeType::from_size(to_size))
            }
            _ => unreachable!("emit_expression_cast"),
        }
    }
    fn emit_expression_member(&mut self, expr: ast2::ExprMember) -> Result<SizeType, CompileError> {
        debug_assert!(expr.src.is_address());
        self.emit_expression(*expr.src)?;
        self.instructions.push(Instruction::AddI(
            SizeType::QWord,
            Operand::Register(0),
            Operand::Constant(expr.member_offset as u64),
            Operand::Register(0),
        ));
        Ok(SizeType::QWord)
    }
    fn emit_expression_arrow(&mut self, expr: ast2::ExprMember) -> Result<SizeType, CompileError> {
        // expr must be pointer
        self.emit_expression_derefed(*expr.src)?;
        self.instructions.push(Instruction::AddI(
            SizeType::QWord,
            Operand::Register(0),
            Operand::Constant(expr.member_offset as u64),
            Operand::Register(0),
        ));
        Ok(SizeType::QWord)
    }
    fn emit_expression_paren(&mut self, expr: ast2::ExprParen) -> Result<SizeType, CompileError> {
        unimplemented!("emit_expression_paren")
    }
    fn emit_expression_bracket(
        &mut self,
        expr: ast2::ExprBracket,
    ) -> Result<SizeType, CompileError> {
        unreachable!("emit_expression_bracket");
    }
    fn emit_expression_unary(&mut self, expr: ast2::ExprUnary) -> Result<SizeType, CompileError> {
        match expr.op {
            ExprUnaryOp::AddressOf => {
                self.emit_expression(*expr.expr)
                // nothing
            }
            ExprUnaryOp::BitwiseNot => {
                let size = self.emit_expression_derefed(*expr.expr)?;
                self.instructions
                    .push(Instruction::BitNot(size, Operand::Register(0)));
                Ok(size)
            }
            ExprUnaryOp::DecrementPost => {
                let type_ = expr.expr.cv_type()?.type_;
                let dec_amount = match &type_ {
                    PrimitiveType::Integer(_) => 1,
                    PrimitiveType::Pointer(p) => p.sizeof()?,
                    _ => unreachable!("emit_expression_unary: DecrementPost"),
                };
                let size = SizeType::from_size(type_.sizeof()?);
                self.emit_expression(*expr.expr)?;
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                self.instructions.push(Instruction::Move(
                    size,
                    Operand::Deref(0),
                    Operand::Register(0),
                ));
                self.instructions.push(Instruction::SubI(
                    size,
                    Operand::Deref(2),
                    Operand::Constant(dec_amount as u64),
                    Operand::Deref(2),
                ));
                Ok(size)
            }
            ExprUnaryOp::DecrementPre => {
                let type_ = expr.expr.cv_type()?.type_;
                let dec_amount = match &type_ {
                    PrimitiveType::Integer(_) => 1,
                    PrimitiveType::Pointer(p) => p.sizeof()?,
                    _ => unreachable!("emit_expression_unary: DecrementPre"),
                };
                let size = SizeType::from_size(type_.sizeof()?);
                self.emit_expression(*expr.expr)?;
                self.instructions.push(Instruction::SubI(
                    size,
                    Operand::Deref(0),
                    Operand::Constant(dec_amount as u64),
                    Operand::Deref(0),
                ));
                Ok(SizeType::QWord)
            }
            ExprUnaryOp::IncrementPost => {
                let type_ = expr.expr.cv_type()?.type_;
                let inc_amount = match &type_ {
                    PrimitiveType::Integer(_) => 1,
                    PrimitiveType::Pointer(p) => p.sizeof()?,
                    _ => unreachable!("emit_expression_unary: IncrementPost"),
                };
                let size = SizeType::from_size(type_.sizeof()?);
                self.emit_expression(*expr.expr)?;
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                self.instructions.push(Instruction::Move(
                    size,
                    Operand::Deref(0),
                    Operand::Register(0),
                ));
                self.instructions.push(Instruction::AddI(
                    size,
                    Operand::Deref(2),
                    Operand::Constant(inc_amount as u64),
                    Operand::Deref(2),
                ));
                Ok(size)
            }
            ExprUnaryOp::IncrementPre => {
                let type_ = expr.expr.cv_type()?.type_;
                let dec_amount = match &type_ {
                    PrimitiveType::Integer(_) => 1,
                    PrimitiveType::Pointer(p) => p.sizeof()?,
                    _ => unreachable!("emit_expression_unary: IncrementPre"),
                };
                let size = SizeType::from_size(type_.sizeof()?);
                self.emit_expression(*expr.expr)?;
                self.instructions.push(Instruction::AddI(
                    size,
                    Operand::Deref(0),
                    Operand::Constant(dec_amount as u64),
                    Operand::Deref(0),
                ));
                Ok(SizeType::QWord)
            }
            ExprUnaryOp::LogicalNot => {
                let size = self.emit_expression_derefed(*expr.expr)?;
                self.instructions.push(Instruction::LogicalNot(
                    size,
                    Operand::Register(0),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            ExprUnaryOp::Minus => {
                let size = self.emit_expression_derefed(*expr.expr)?;
                self.instructions
                    .push(Instruction::Neg(size, Operand::Register(0)));
                Ok(size)
            }
            ExprUnaryOp::Dereference => {
                self.emit_expression(*expr.expr)?;
                Ok(SizeType::QWord)
            }
            ExprUnaryOp::Plus => {
                // unary plus was filtered out by ast2
                unreachable!("emit_expression_unary: Plus")
            }
        }
    }
    fn emit_expression_binary(&mut self, expr: ast2::ExprBinary) -> Result<SizeType, CompileError> {
        match expr.op {
            ExprBinaryOp::BitwiseAnd => {
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                self.instructions.push(Instruction::BitAnd(
                    lhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                Ok(lhs)
            }
            ExprBinaryOp::BitwiseOr => {
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                self.instructions.push(Instruction::BitOr(
                    lhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                Ok(lhs)
            }
            ExprBinaryOp::BitwiseXor => {
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                self.instructions.push(Instruction::BitXor(
                    lhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                Ok(lhs)
            }

            ExprBinaryOp::BitwiseAndAssign => {
                self.emit_expression(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                self.instructions.push(Instruction::Move(
                    rhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                self.instructions
                    .push(Instruction::Pop(SizeType::QWord, Operand::Register(0)));
                self.instructions.push(Instruction::BitAnd(
                    rhs,
                    Operand::Deref(0),
                    Operand::Register(2),
                ));
                Ok(SizeType::QWord)
            }
            ExprBinaryOp::BitwiseOrAssign => {
                self.emit_expression(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                self.instructions.push(Instruction::Move(
                    rhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                self.instructions
                    .push(Instruction::Pop(SizeType::QWord, Operand::Register(0)));
                self.instructions.push(Instruction::BitOr(
                    rhs,
                    Operand::Deref(0),
                    Operand::Register(2),
                ));
                Ok(SizeType::QWord)
            }

            ExprBinaryOp::BitwiseXorAssign => {
                self.emit_expression(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                self.instructions.push(Instruction::Move(
                    rhs,
                    Operand::Register(0),
                    Operand::Register(2),
                ));
                self.instructions
                    .push(Instruction::Pop(SizeType::QWord, Operand::Register(0)));
                self.instructions.push(Instruction::BitXor(
                    rhs,
                    Operand::Deref(0),
                    Operand::Register(2),
                ));
                Ok(SizeType::QWord)
            }

            ExprBinaryOp::ShiftLeft => {
                let lhs_type = match expr.lhs.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => i,
                    _ => unreachable!("emit_expression_binary: ShiftLeft"),
                };
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                if lhs_type.is_signed() {
                    self.instructions.push(Instruction::ShiftLeftI(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                    ));
                } else {
                    self.instructions.push(Instruction::ShiftLeftU(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                    ));
                }
                self.instructions.push(Instruction::Move(
                    lhs,
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                Ok(lhs)
            }

            ExprBinaryOp::ShiftRight => {
                let lhs_type = match expr.lhs.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => i,
                    _ => unreachable!("emit_expression_binary: ShiftLeft"),
                };
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                if lhs_type.is_signed() {
                    self.instructions.push(Instruction::ShiftRightI(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                    ));
                } else {
                    self.instructions.push(Instruction::ShiftRightU(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                    ));
                }
                self.instructions.push(Instruction::Move(
                    lhs,
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                Ok(lhs)
            }

            ExprBinaryOp::ShiftLeftAssign => {
                let lhs_type = match expr.lhs.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => i,
                    _ => unreachable!("emit_expression_binary: ShiftLeft"),
                };
                let lhs_size = SizeType::from_size(lhs_type.sizeof());
                self.emit_expression(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(SizeType::QWord, Operand::Register(2)));
                if lhs_type.is_signed() {
                    self.instructions.push(Instruction::ShiftRightI(
                        lhs_size,
                        Operand::Deref(2),
                        Operand::Register(0),
                    ));
                } else {
                    self.instructions.push(Instruction::ShiftRightU(
                        lhs_size,
                        Operand::Deref(2),
                        Operand::Register(0),
                    ));
                }
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                Ok(SizeType::QWord)
            }

            ExprBinaryOp::ShiftRightAssign => {
                let lhs_type = match expr.lhs.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => i,
                    _ => unreachable!("emit_expression_binary: ShiftLeft"),
                };
                let lhs_size = SizeType::from_size(lhs_type.sizeof());
                self.emit_expression(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(SizeType::QWord, Operand::Register(2)));
                if lhs_type.is_signed() {
                    self.instructions.push(Instruction::ShiftRightI(
                        lhs_size,
                        Operand::Deref(2),
                        Operand::Register(0),
                    ));
                } else {
                    self.instructions.push(Instruction::ShiftRightU(
                        lhs_size,
                        Operand::Deref(2),
                        Operand::Register(0),
                    ));
                }
                self.instructions.push(Instruction::Move(
                    SizeType::QWord,
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                Ok(SizeType::QWord)
            }

            ExprBinaryOp::LogicalAnd => {
                let lhs_type = expr.lhs.cv_type()?.type_;
                let rhs_type = expr.rhs.cv_type()?.type_;

                let end_label = self.generate_label();
                let false_label = self.generate_label();

                let lhs_size = match lhs_type {
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_) => {
                        self.emit_expression_derefed(*expr.lhs)?
                    }
                    PrimitiveType::Array(_) => self.emit_expression(*expr.lhs)?,
                    _ => unreachable!("emit_expression_binary: {:?}", lhs_type),
                };
                self.instructions.push(Instruction::JumpZero(
                    lhs_size,
                    Operand::Register(0),
                    false_label,
                ));

                let rhs_size = match rhs_type {
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_) => {
                        self.emit_expression_derefed(*expr.rhs)?
                    }
                    PrimitiveType::Array(_) => self.emit_expression(*expr.rhs)?,
                    _ => unreachable!("emit_expression_binary: {:?}", rhs_type),
                };
                self.instructions.push(Instruction::JumpZero(
                    rhs_size,
                    Operand::Register(0),
                    false_label,
                ));
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(1),
                    Operand::Register(0),
                ));
                self.instructions.push(Instruction::Jump(end_label));

                self.set_label(false_label);
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(0),
                    Operand::Register(0),
                ));
                self.set_label(end_label);

                Ok(SizeType::DWord)
            }

            ExprBinaryOp::LogicalOr => {
                let lhs_type = expr.lhs.cv_type()?.type_;
                let rhs_type = expr.rhs.cv_type()?.type_;

                let end_label = self.generate_label();
                let true_label = self.generate_label();

                let lhs_size = match lhs_type {
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_) => {
                        self.emit_expression_derefed(*expr.lhs)?
                    }
                    PrimitiveType::Array(_) => self.emit_expression(*expr.lhs)?,
                    _ => unreachable!("emit_expression_binary: {:?}", lhs_type),
                };
                self.instructions.push(Instruction::JumpNotZero(
                    lhs_size,
                    Operand::Register(0),
                    true_label,
                ));

                let rhs_size = match rhs_type {
                    PrimitiveType::Integer(_) | PrimitiveType::Pointer(_) => {
                        self.emit_expression_derefed(*expr.rhs)?
                    }
                    PrimitiveType::Array(_) => self.emit_expression(*expr.rhs)?,
                    _ => unreachable!("emit_expression_binary: {:?}", rhs_type),
                };
                self.instructions.push(Instruction::JumpNotZero(
                    rhs_size,
                    Operand::Register(0),
                    true_label,
                ));
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(0),
                    Operand::Register(0),
                ));
                self.instructions.push(Instruction::Jump(end_label));

                self.set_label(true_label);
                self.instructions.push(Instruction::Move(
                    SizeType::DWord,
                    Operand::Constant(1),
                    Operand::Register(0),
                ));
                self.set_label(end_label);

                Ok(SizeType::DWord)
            }

            ExprBinaryOp::Comma => {
                self.emit_expression(*expr.lhs)?;
                self.emit_expression(*expr.rhs)
            }

            ExprBinaryOp::Assign => {
                let lhs_type_size = expr.lhs.cv_type()?.type_.sizeof()?;
                if expr.rhs.is_address() {
                    self.emit_expression(*expr.rhs)?;
                    self.instructions
                        .push(Instruction::Push(SizeType::QWord, Operand::Register(0)));
                    self.emit_expression(*expr.lhs)?;
                    self.instructions
                        .push(Instruction::Pop(SizeType::QWord, Operand::Register(2)));
                    self.instructions.push(Instruction::Memcpy(
                        lhs_type_size,
                        Operand::Register(0),
                        Operand::Register(2),
                    ));
                    Ok(SizeType::QWord)
                } else {
                    let rhs = self.emit_expression(*expr.rhs)?;
                    self.instructions
                        .push(Instruction::Push(rhs, Operand::Register(0)));
                    self.emit_expression(*expr.lhs)?;
                    self.instructions
                        .push(Instruction::Pop(rhs, Operand::Deref(0)));
                    Ok(SizeType::QWord)
                }
            }

            ExprBinaryOp::Equal => {
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                self.instructions.push(Instruction::Equal(
                    lhs,
                    Operand::Register(0),
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            ExprBinaryOp::NotEqual => {
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                self.instructions.push(Instruction::Equal(
                    lhs,
                    Operand::Register(0),
                    Operand::Register(2),
                    Operand::Register(0),
                ));
                self.instructions.push(Instruction::LogicalNot(
                    SizeType::DWord,
                    Operand::Register(0),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }

            ExprBinaryOp::LessThan => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                // register2 = lhs
                // register0 = rhs
                match type_ {
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::LessThanF(
                            rhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::LessThanI(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::LessThanU(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        }
                    }
                    _ => {
                        self.instructions.push(Instruction::LessThanU(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                }
                Ok(SizeType::DWord)
            }

            ExprBinaryOp::LessThanOrEqual => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                // register2 = lhs
                // register0 = rhs
                match type_ {
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::LessThanF(
                            rhs,
                            Operand::Register(0),
                            Operand::Register(2),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::LessThanI(
                                lhs,
                                Operand::Register(0),
                                Operand::Register(2),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::LessThanU(
                                lhs,
                                Operand::Register(0),
                                Operand::Register(2),
                                Operand::Register(0),
                            ));
                        }
                    }
                    _ => {
                        self.instructions.push(Instruction::LessThanU(
                            lhs,
                            Operand::Register(0),
                            Operand::Register(2),
                            Operand::Register(0),
                        ));
                    }
                }
                self.instructions.push(Instruction::LogicalNot(
                    SizeType::DWord,
                    Operand::Register(0),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }

            ExprBinaryOp::GreaterThan => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                // register2 = lhs
                // register0 = rhs
                match type_ {
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::LessThanF(
                            rhs,
                            Operand::Register(0),
                            Operand::Register(2),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::LessThanI(
                                lhs,
                                Operand::Register(0),
                                Operand::Register(2),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::LessThanU(
                                lhs,
                                Operand::Register(0),
                                Operand::Register(2),
                                Operand::Register(0),
                            ));
                        }
                    }
                    _ => {
                        self.instructions.push(Instruction::LessThanU(
                            lhs,
                            Operand::Register(0),
                            Operand::Register(2),
                            Operand::Register(0),
                        ));
                    }
                }
                Ok(SizeType::DWord)
            }

            ExprBinaryOp::GreaterThanOrEqual => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                // register2 = lhs
                // register0 = rhs
                match type_ {
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::LessThanF(
                            rhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::LessThanI(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::LessThanU(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        }
                    }
                    _ => {
                        self.instructions.push(Instruction::LessThanU(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                }
                self.instructions.push(Instruction::LogicalNot(
                    SizeType::DWord,
                    Operand::Register(0),
                    Operand::Register(0),
                ));
                Ok(SizeType::DWord)
            }
            ExprBinaryOp::Mod => {
                let is_signed = match expr.lhs.cv_type()?.type_ {
                    PrimitiveType::Integer(i) => i.is_signed(),
                    _ => unreachable!("emit_expression_binary: Mod"),
                };
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));

                if is_signed {
                    self.instructions.push(Instruction::ModI(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                        Operand::Register(0),
                    ));
                } else {
                    self.instructions.push(Instruction::ModU(
                        lhs,
                        Operand::Register(2),
                        Operand::Register(0),
                        Operand::Register(0),
                    ));
                }

                Ok(lhs)
            }

            ExprBinaryOp::Mul => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                match type_ {
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::MulI(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::MulU(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        }
                    }
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::MulF(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    _ => unreachable!("emit_expression_binary: Mul"),
                }
                Ok(lhs)
            }
            ExprBinaryOp::Div => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                self.emit_expression_derefed(*expr.rhs)?;
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                match type_ {
                    PrimitiveType::Integer(i) => {
                        if i.is_signed() {
                            self.instructions.push(Instruction::DivI(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        } else {
                            self.instructions.push(Instruction::DivU(
                                lhs,
                                Operand::Register(2),
                                Operand::Register(0),
                                Operand::Register(0),
                            ));
                        }
                    }
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::DivF(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    _ => unreachable!("emit_expression_binary: Div"),
                }
                Ok(lhs)
            }

            ExprBinaryOp::Add => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                match type_ {
                    PrimitiveType::Integer(_) => {
                        self.instructions.push(Instruction::AddI(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::AddF(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    _ => unreachable!("emit_expression_binary: Add"),
                }
                Ok(lhs)
            }

            ExprBinaryOp::Sub => {
                let type_ = expr.lhs.cv_type()?.type_;
                let lhs = self.emit_expression_derefed(*expr.lhs)?;
                self.instructions
                    .push(Instruction::Push(lhs, Operand::Register(0)));
                let rhs = self.emit_expression_derefed(*expr.rhs)?;
                debug_assert!(lhs == rhs);
                self.instructions
                    .push(Instruction::Pop(lhs, Operand::Register(2)));
                match type_ {
                    PrimitiveType::Integer(_) => {
                        self.instructions.push(Instruction::SubI(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    PrimitiveType::Float(_) => {
                        self.instructions.push(Instruction::SubF(
                            lhs,
                            Operand::Register(2),
                            Operand::Register(0),
                            Operand::Register(0),
                        ));
                    }
                    _ => unreachable!("emit_expression_binary: Sub"),
                }
                Ok(lhs)
            }

            _ => unimplemented!("emit_expression_binary: {:?}", expr.op),
        }
    }
    fn emit_expression_initializerlist(
        &mut self,
        expr: ast2::ExprInitializerList,
    ) -> Result<SizeType, CompileError> {
        unimplemented!("emit_expression_initializerlist")
    }
}
