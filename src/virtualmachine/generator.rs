use super::Instruction;
use super::LabelType;
use super::VirtualMachine;

use crate::ast2;
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

    pub fn process(
        &mut self,
        translation_unit: ast2::TranslationUnit,
    ) -> Result<VirtualMachine, CompileError> {
        for statement in translation_unit.statements {
            self.process_statement(statement)?;
        }

        let mut vm = VirtualMachine::new();
        vm.label_map = std::mem::take(&mut self.labels)
            .into_iter()
            .map(|x| x.unwrap())
            .collect();
        vm.instructions = std::mem::take(&mut self.instructions);
        Ok(vm)
    }

    pub fn process_statement(&mut self, statement: Statement) -> Result<(), CompileError> {
        match statement {
            Statement::None => {}
            Statement::Expression(stmt) => self.process_statement_expression(stmt),
            Statement::Labeled(stmt) => self.process_statement_labeled(stmt),
            Statement::Goto(stmt) => self.process_statement_goto(stmt),
            Statement::Compound(stmt) => self.process_statement_compound(stmt),
            Statement::If(stmt) => self.process_statement_if(stmt),
            Statement::Switch(stmt) => self.process_statement_switch(stmt),
            Statement::_Case(_) => unreachable!("_Case should not be visible to end-users"),
            Statement::_Default(_) => {
                unreachable!("_Default should not be visible to end-users")
            }
            Statement::Continue => self.process_statement_continue(),
            Statement::Break => self.process_statement_break(),
            Statement::Return(stmt) => self.process_statement_return(stmt),
            Statement::For(stmt) => self.process_statement_for(stmt),
            Statement::While(stmt) => self.process_statement_while(stmt),
            Statement::DoWhile(stmt) => self.process_statement_dowhile(stmt),
            Statement::VariableDeclaration(stmt) => {
                self.process_statement_variable_declaration(stmt)
            }
        }
        Ok(())
    }

    pub fn process_expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::I8(expr) => self.process_expression_i8(expr),
            Expression::I16(expr) => self.process_expression_i16(expr),
            Expression::I32(expr) => self.process_expression_i32(expr),
            Expression::I64(expr) => self.process_expression_i64(expr),
            Expression::U8(expr) => self.process_expression_u8(expr),
            Expression::U16(expr) => self.process_expression_u16(expr),
            Expression::U32(expr) => self.process_expression_u32(expr),
            Expression::U64(expr) => self.process_expression_u64(expr),
            Expression::F32(expr) => self.process_expression_f32(expr),
            Expression::F64(expr) => self.process_expression_f64(expr),
            Expression::String(expr) => self.process_expression_string(expr),
            Expression::Variable(expr) => self.process_expression_variable(expr),
            Expression::Conditional(expr) => self.process_expression_conditional(expr),
            Expression::Cast(expr) => self.process_expression_cast(expr),
            Expression::Member(expr) => self.process_expression_member(expr),
            Expression::Arrow(expr) => self.process_expression_arrow(expr),
            Expression::Paren(expr) => self.process_expression_paren(expr),
            Expression::Bracket(expr) => self.process_expression_bracket(expr),
            Expression::Unary(expr) => self.process_expression_unary(expr),
            Expression::Binary(expr) => self.process_expression_binary(expr),
            Expression::InitializerList(expr) => self.process_expression_initializerlist(expr),
        }
        Ok(())
    }
}

impl InstructionGenerator {
    fn process_statement_expression(&mut self, stmt: ast2::StmtExpression) {}
    fn process_statement_labeled(&mut self, stmt: ast2::StmtLabeled) {}
    fn process_statement_goto(&mut self, stmt: ast2::StmtGoto) {}
    fn process_statement_compound(&mut self, stmt: ast2::StmtCompound) {}
    fn process_statement_if(&mut self, stmt: ast2::StmtIf) {}
    fn process_statement_switch(&mut self, stmt: ast2::StmtSwitch) {}
    fn process_statement_continue(&mut self) {}
    fn process_statement_break(&mut self) {}
    fn process_statement_return(&mut self, stmt: ast2::StmtReturn) {}
    fn process_statement_for(&mut self, stmt: ast2::StmtFor) {}
    fn process_statement_while(&mut self, stmt: ast2::StmtWhile) {}
    fn process_statement_dowhile(&mut self, stmt: ast2::StmtDoWhile) {}
    fn process_statement_variable_declaration(&mut self, stmt: ast2::StmtVariableDeclaration) {}
}

impl InstructionGenerator {
    fn process_expression_i8(&mut self, expr: i8) {}
    fn process_expression_i16(&mut self, expr: i16) {}
    fn process_expression_i32(&mut self, expr: i32) {}
    fn process_expression_i64(&mut self, expr: i64) {}
    fn process_expression_u8(&mut self, expr: u8) {}
    fn process_expression_u16(&mut self, expr: u16) {}
    fn process_expression_u32(&mut self, expr: u32) {}
    fn process_expression_u64(&mut self, expr: u64) {}
    fn process_expression_f32(&mut self, expr: f32) {}
    fn process_expression_f64(&mut self, expr: f64) {}
    fn process_expression_string(&mut self, expr: String) {}
    fn process_expression_variable(&mut self, expr: VariableInfo) {}
    fn process_expression_conditional(&mut self, expr: ast2::ExprConditional) {}
    fn process_expression_cast(&mut self, expr: ast2::ExprCast) {}
    fn process_expression_member(&mut self, expr: ast2::ExprMember) {}
    fn process_expression_arrow(&mut self, expr: ast2::ExprMember) {}
    fn process_expression_paren(&mut self, expr: ast2::ExprParen) {}
    fn process_expression_bracket(&mut self, expr: ast2::ExprBracket) {}
    fn process_expression_unary(&mut self, expr: ast2::ExprUnary) {}
    fn process_expression_binary(&mut self, expr: ast2::ExprBinary) {}
    fn process_expression_initializerlist(&mut self, expr: ast2::ExprInitializerList) {}
}
