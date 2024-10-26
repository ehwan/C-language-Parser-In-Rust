use super::{expression::Expression, typename::TypeInfo};
use crate::virtualmachine::instruction::generation::FunctionInfo;
use crate::virtualmachine::instruction::generation::InstructionGenerator;
use crate::virtualmachine::instruction::operand::Operand;
use crate::virtualmachine::instruction::*;
use crate::virtualmachine::program::STACK_POINTER_BASE_REGISTER;
use crate::virtualmachine::program::STACK_POINTER_REGISTER;
use crate::virtualmachine::scope::FunctionScope;
use crate::virtualmachine::variable::VariableData;

#[derive(Debug, Clone)]
pub enum Statement {
    Null(StmtNull),
    Expression(StmtExpression),
    Labeled(StmtLabeled),
    Compound(StmtCompound),
    If(StmtIf),
    Switch(StmtSwitch),
    Case(StmtCase),
    Default(StmtDefault),
    Continue(StmtContinue),
    Break(StmtBreak),
    While(StmtWhile),
    DoWhile(StmtDoWhile),
    For(StmtFor),
    Goto(StmtGoto),
    Return(StmtReturn),
    StructDefinition(StmtStructDefinition),
    FunctionDeclaration(StmtFunctionDeclaration),
    Declaration(StmtDeclaration),
    Typedef(StmtTypedef),
    FunctionDefinition(StmtFunctionDefinition),
    TranslationUnit(TranslationUnit),
}

impl Statement {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        match self {
            Statement::Null(stmt) => stmt.emit(instructions),
            Statement::Expression(stmt) => stmt.emit(instructions),
            Statement::Labeled(stmt) => stmt.emit(instructions),
            Statement::Compound(stmt) => stmt.emit(instructions),
            Statement::If(stmt) => stmt.emit(instructions),
            Statement::Switch(stmt) => stmt.emit(instructions),
            Statement::Case(stmt) => stmt.emit(instructions),
            Statement::Default(stmt) => stmt.emit(instructions),
            Statement::Continue(stmt) => stmt.emit(instructions),
            Statement::Break(stmt) => stmt.emit(instructions),
            Statement::While(stmt) => stmt.emit(instructions),
            Statement::DoWhile(stmt) => stmt.emit(instructions),
            Statement::For(stmt) => stmt.emit(instructions),
            Statement::Goto(stmt) => stmt.emit(instructions),
            Statement::Return(stmt) => stmt.emit(instructions),
            Statement::StructDefinition(stmt) => stmt.emit(instructions),
            Statement::FunctionDeclaration(stmt) => stmt.emit(instructions),
            Statement::Declaration(stmt) => stmt.emit(instructions),
            Statement::Typedef(stmt) => stmt.emit(instructions),
            Statement::FunctionDefinition(stmt) => stmt.emit(instructions),
            Statement::TranslationUnit(stmt) => stmt.emit(instructions),
        }
    }
}

/// Statements that do nothing
#[derive(Debug, Clone)]
pub struct StmtNull;
impl StmtNull {
    pub fn emit(&self, _instructions: &mut InstructionGenerator) {}
}

/// for any expression ends with semicolon ';'
#[derive(Debug, Clone)]
pub struct StmtExpression {
    pub expression: Expression,
}
impl StmtExpression {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.expression.emit(instructions);
    }
}

/// label:
///    statement
#[derive(Debug, Clone)]
pub struct StmtLabeled {
    pub label: String,
    pub statement: Box<Statement>,
}
impl StmtLabeled {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.set_label(&self.label);
        self.statement.emit(instructions);
    }
}

/// { statements ... }
#[derive(Debug, Clone)]
pub struct StmtCompound {
    pub statements: Vec<Statement>,
}
impl StmtCompound {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push_scope();

        for statement in &self.statements {
            statement.emit(instructions);
        }

        instructions.pop_scope();
    }
}

/// if ( condition_expression ) then_statement else else_statement
/// no else if statement
#[derive(Debug, Clone)]
pub struct StmtIf {
    pub cond: Expression,
    pub then_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}
impl StmtIf {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let else_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        self.cond.emit(instructions);
        instructions.push(Instruction::JumpZero(JumpZero {
            label: else_label.clone(),
            operand_cond: Operand::Register(0),
        }));
        self.then_statement.emit(instructions);
        instructions.push(Instruction::Jump(Jump {
            label: end_label.clone(),
        }));
        instructions.set_label(&else_label);
        if let Some(else_statement) = &self.else_statement {
            else_statement.emit(instructions);
        }
        instructions.set_label(&end_label);
    }
}

/// switch ( target_expression ) body_statement
#[derive(Debug, Clone)]
pub struct StmtSwitch {
    pub target: Expression,
    pub statement: Box<Statement>,
}
impl StmtSwitch {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let end_label = instructions.get_unique_label();
        let default_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((default_label.clone(), end_label.clone()));

        // push target to stack
        self.target.emit(instructions);
        if self.target.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }
        // and push variable for 'if the pattern matched already?'
        instructions.push(Instruction::PushStack(PushStack {
            operand: Operand::Value(VariableData::UInt8(0)),
        }));

        // body
        self.statement.emit(instructions);
        // check if the pattern matched and default is defined
        if instructions.labels.get(&default_label).is_some() {
            // if not matched, set pattern matched to true and goto default
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -1),
                operand_to: Operand::Register(0),
            }));
            instructions.push(Instruction::JumpZero(JumpZero {
                label: default_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }

        // end label here, cleanup
        instructions.set_label(&end_label);
        // pop pattern-matched state and target from stack
        instructions.push(Instruction::SubAssign(SubAssign {
            lhs: Operand::Register(STACK_POINTER_REGISTER),
            rhs: Operand::Value(VariableData::UInt64(2)),
        }));

        instructions
            .label_stack
            .pop()
            .expect("Switch: label_stack is empty");
    }
}
/// case value: statement
#[derive(Debug, Clone)]
pub struct StmtCase {
    pub value: Expression,
    pub statement: Box<Statement>,
}
impl StmtCase {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let case_end_label = instructions.get_unique_label();
        let comparison_skip_label = instructions.get_unique_label();

        // copy state from stack
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -1),
            operand_to: Operand::Register(0),
        }));
        // if the pattern matched already, skip comparison
        instructions.push(Instruction::JumpNonZero(JumpNonZero {
            label: comparison_skip_label.clone(),
            operand_cond: Operand::Register(0),
        }));

        // comparison start here
        // evaluate value
        self.value.emit(instructions);
        // register1 = value
        if self.value.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            }));
        } else {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Register(0),
                operand_to: Operand::Register(1),
            }));
        }
        // register0 = target
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Derefed(STACK_POINTER_REGISTER, -2),
            operand_to: Operand::Register(0),
        }));
        // register2 = result of comparison
        instructions.push(Instruction::Equal(Equal {
            lhs: Operand::Register(0),
            rhs: Operand::Register(1),
            to: Operand::Register(2),
        }));
        instructions.push(Instruction::JumpZero(JumpZero {
            label: case_end_label.clone(),
            operand_cond: Operand::Register(2),
        }));

        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt8(1)),
            operand_to: Operand::Derefed(STACK_POINTER_REGISTER, -1),
        }));

        instructions.set_label(&comparison_skip_label);
        self.statement.emit(instructions);

        instructions.set_label(&case_end_label);
    }
}

/// default: statement
#[derive(Debug, Clone)]
pub struct StmtDefault {
    pub statement: Box<Statement>,
}
impl StmtDefault {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let default_end_label = instructions.get_unique_label();
        let (default_label, _) = instructions
            .label_stack
            .last()
            .expect("Default: label_stack is empty")
            .clone();
        // skip default statement
        instructions.push(Instruction::Jump(Jump {
            label: default_end_label.clone(),
        }));
        instructions.set_label(&default_label);
        // set pattern matched
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt8(1)),
            operand_to: Operand::Derefed(STACK_POINTER_REGISTER, -1),
        }));
        self.statement.emit(instructions);
        instructions.set_label(&default_end_label);
    }
}

/// continue;
#[derive(Debug, Clone)]
pub struct StmtContinue;
impl StmtContinue {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let continue_label = &instructions
            .label_stack
            .last()
            .expect("Continue: label_stack is empty")
            .0;
        instructions.push(Instruction::Jump(Jump {
            label: continue_label.clone(),
        }));
    }
}

/// break;
#[derive(Debug, Clone)]
pub struct StmtBreak;
impl StmtBreak {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let end_label = &instructions
            .label_stack
            .last()
            .expect("Break: label_stack is empty")
            .1;
        instructions.push(Instruction::Jump(Jump {
            label: end_label.clone(),
        }));
    }
}

/// while ( condition_expression ) statement
#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub cond: Expression,
    pub statement: Box<Statement>,
}
impl StmtWhile {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let start_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((start_label.clone(), end_label.clone()));

        instructions.set_label(&start_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference(instructions) {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }
        self.statement.emit(instructions);
        instructions.push(Instruction::Jump(Jump {
            label: start_label.clone(),
        }));
        instructions.set_label(&end_label);

        instructions
            .label_stack
            .pop()
            .expect("While: label_stack is empty");
    }
}

/// do statement while ( condition_expression );

#[derive(Debug, Clone)]
pub struct StmtDoWhile {
    pub cond: Expression,
    pub statement: Box<Statement>,
}
impl StmtDoWhile {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let start_label = instructions.get_unique_label();
        let continue_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        instructions
            .label_stack
            .push((continue_label.clone(), end_label.clone()));

        // start_label:
        //    do { body ... }
        // continue_label:
        //    while (cond);
        // end_label:

        instructions.set_label(&start_label);
        self.statement.emit(instructions);

        instructions.set_label(&continue_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference(instructions) {
            instructions.push(Instruction::JumpNonZero(JumpNonZero {
                label: start_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::JumpNonZero(JumpNonZero {
                label: start_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }
        instructions.set_label(&end_label);

        instructions
            .label_stack
            .pop()
            .expect("DoWhile: label_stack is empty");
    }
}

/// for ( init; cond; next ) statement
/// since init is expression, must declare variable before entering for loop
#[derive(Debug, Clone)]
pub struct StmtFor {
    pub init: Expression,
    pub cond: Expression,
    pub next: Option<Expression>,
    pub statement: Box<Statement>,
}
impl StmtFor {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let cond_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();
        let continue_label = instructions.get_unique_label();
        instructions
            .label_stack
            .push((continue_label.clone(), end_label.clone()));

        // init
        //   COND:
        // cond
        // body
        //   CONTINUE:
        // next
        // jump COND
        //   END:

        self.init.emit(instructions);
        instructions.set_label(&cond_label);
        self.cond.emit(instructions);
        if self.cond.is_return_reference(instructions) {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: end_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }
        self.statement.emit(instructions);
        instructions.set_label(&continue_label);
        if let Some(next) = &self.next {
            next.emit(instructions);
        }
        instructions.push(Instruction::Jump(Jump {
            label: cond_label.clone(),
        }));
        instructions.set_label(&end_label);

        instructions
            .label_stack
            .pop()
            .expect("For: label_stack is empty");
    }
}

/// goto label;
#[derive(Debug, Clone)]
pub struct StmtGoto {
    pub label: String,
}
impl StmtGoto {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::Jump(Jump {
            label: self.label.clone(),
        }));
    }
}

/// return; or return expression;
#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub expr: Option<Expression>,
}
impl StmtReturn {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        if let Some(expr) = &self.expr {
            expr.emit(instructions);
            // force return as value
            if expr.is_return_reference(instructions) {
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Derefed(0, 0),
                    operand_to: Operand::Register(0),
                }));
            }
        }
        instructions.push(Instruction::Return(Return {}));
    }
}

/// type definition of struct, union, enum
#[derive(Debug, Clone)]
pub struct StmtStructDefinition {
    pub typeinfo: TypeInfo,
}
impl StmtStructDefinition {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        match &self.typeinfo {
            TypeInfo::Struct(t) => {
                if t.name.is_none() {
                    println!("Anonymous struct in declaration statement; ignored it");
                } else {
                    let old = if instructions.scopes.is_empty() {
                        &mut instructions.global_scope
                    } else {
                        instructions.scopes.last_mut().unwrap()
                    }
                    .type_infos
                    .insert(
                        t.name.as_ref().unwrap().clone(),
                        TypeInfo::Struct(t.clone()),
                    );
                    if old.is_some() {
                        panic!("Struct {} already exists", t.name.as_ref().unwrap());
                    }
                }
            }
            _ => panic!("Invalid type for type declaration: {:?}", self.typeinfo),
        }
    }
}

/// return_type function_name ( params );
#[derive(Debug, Clone)]
pub struct StmtFunctionDeclaration {
    pub return_type: TypeInfo,
    pub name: String,
    pub params: Vec<TypeInfo>,
}
impl StmtFunctionDeclaration {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        // check if its already declared
        let old = instructions.functions.get(&self.name);
        if let Some(old) = old {
            // no need to check if the function is already defined,
            // since this statement is declaration statement

            // check parameter types are same
            let param_equal = old
                .params
                .iter()
                .map(|(_, type_)| type_)
                .eq(self.params.iter());
            if param_equal == false {
                panic!(
                    "Function {} is already declared with different parameter types",
                    &self.name
                );
            }

            // check return type is same
            if &old.return_type != &self.return_type {
                panic!(
                    "Function {} is already declared with different return type",
                    &self.name
                );
            }
        } else {
            // function is not declared
            let params: Vec<_> = self
                .params
                .iter()
                .map(|typeinfo| (None, typeinfo.clone()))
                .collect();
            let function_data = FunctionInfo {
                return_type: self.return_type.clone(),
                params,
                is_defined: false,
            };
            instructions
                .functions
                .insert(self.name.clone(), function_data);
        }
    }
}

/// typename var1, var2, var3, ...,;
/// var_i can be decorated with qualifiers; pointer, const ...
#[derive(Debug, Clone)]
pub struct StmtDeclaration {
    pub vars: Vec<(String, TypeInfo, Option<Expression>)>, // name, type, initializer
}
impl StmtDeclaration {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        for declaration in &self.vars {
            // variable declaration
            if let Some(initial_value) = &declaration.2 {
                // variable with initial value

                let var_type = instructions.get_true_typeinfo(&declaration.1);
                // check type
                match var_type.remove_const() {
                    TypeInfo::Function(_, _) => {
                        panic!(
                            "Function declaration cannot have initial value; something went wrong"
                        );
                    }
                    TypeInfo::Struct(sinfo) => {
                        // link name to stack
                        instructions.declare_variable(
                            &declaration.0,
                            &var_type,
                            sinfo.number_of_primitives(),
                        );

                        sinfo.emit_init(instructions, initial_value);
                    }
                    TypeInfo::Union(_uinfo) => {
                        panic!("Union declaration in declaration statement is not implemented");
                    }

                    // array
                    TypeInfo::Array(type_, size) => {
                        // initializer must be initializer list
                        let initial_value = match initial_value {
                            Expression::InitializerList(initial_value) => initial_value,
                            _ => unreachable!("Array initializer must be initializer list"),
                        };

                        let size = match size {
                            Some(size) => {
                                if initial_value.initializers.len() > size {
                                    panic!("Too many initializers for array");
                                }
                                size
                            }
                            None => initial_value.initializers.len(),
                        };
                        if size == 0 {
                            panic!("Array size must be greater than 0");
                        }

                        // link name to stack
                        instructions.declare_variable(
                            &declaration.0,
                            &TypeInfo::Array(type_.clone(), Some(size)),
                            size * type_.number_of_primitives(),
                        );

                        TypeInfo::Array(type_.clone(), Some(size))
                            .emit_init(instructions, declaration.2.as_ref().unwrap());
                    }

                    // primitive types + pointer
                    TypeInfo::UInt8
                    | TypeInfo::UInt16
                    | TypeInfo::UInt32
                    | TypeInfo::UInt64
                    | TypeInfo::Int8
                    | TypeInfo::Int16
                    | TypeInfo::Int32
                    | TypeInfo::Int64
                    | TypeInfo::Float32
                    | TypeInfo::Float64
                    | TypeInfo::Pointer(_) => {
                        // link name to stack
                        instructions.declare_variable(&declaration.0, &var_type, 1);

                        var_type.emit_init(instructions, initial_value);
                    }
                    _ => panic!(
                        "Invalid type for variable declaration: {:?}",
                        &declaration.1
                    ),
                }
            } else {
                // variable without initial value

                let var_type = instructions.get_true_typeinfo(&declaration.1);
                match var_type.remove_const() {
                    TypeInfo::Struct(sinfo) => {
                        let size = sinfo.number_of_primitives();
                        if size == 0 {
                            panic!("Struct size must be greater than 0");
                        }
                        // link name to stack
                        instructions.declare_variable(&declaration.0, &var_type, size);

                        sinfo.emit_default(instructions);
                    }
                    TypeInfo::Union(_uinfo) => {
                        panic!("Union declaration in declaration statement is not implemented");
                    }

                    TypeInfo::Array(type_, size) => {
                        let size =
                            size.expect("Array declaration without initializer must have size");
                        if size == 0 {
                            panic!("Array size must be greater than 0");
                        }

                        // link name to stack
                        instructions.declare_variable(
                            &declaration.0,
                            &TypeInfo::Array(type_.clone(), Some(size)),
                            size * type_.number_of_primitives(),
                        );

                        TypeInfo::Array(type_.clone(), Some(size)).emit_default(instructions);
                    }

                    // primitive types + pointer
                    TypeInfo::UInt8
                    | TypeInfo::UInt16
                    | TypeInfo::UInt32
                    | TypeInfo::UInt64
                    | TypeInfo::Int8
                    | TypeInfo::Int16
                    | TypeInfo::Int32
                    | TypeInfo::Int64
                    | TypeInfo::Float32
                    | TypeInfo::Float64
                    | TypeInfo::Pointer(_) => {
                        // link name to stack
                        instructions.declare_variable(&declaration.0, &var_type, 1);

                        // push default value to stack
                        var_type.emit_default(instructions);
                    }
                    _ => panic!(
                        "Invalid type for variable declaration: {:?}",
                        &declaration.1
                    ),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StmtTypedef {
    pub name: String,
    pub typeinfo: TypeInfo,
}
impl StmtTypedef {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let old = if instructions.scopes.is_empty() {
            &mut instructions.global_scope
        } else {
            instructions.scopes.last_mut().unwrap()
        }
        .type_infos
        .insert(self.name.clone(), self.typeinfo.clone());
        if old.is_some() {
            panic!("Type {} already exists", self.name);
        }
    }
}

#[derive(Debug, Clone)]
pub struct StmtFunctionDefinition {
    pub return_type: TypeInfo,
    pub name: String,
    pub params: Vec<(Option<String>, TypeInfo)>,
    pub body: Box<Statement>,
}
impl StmtFunctionDefinition {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        if instructions.function_scope.is_some() {
            panic!("nested function is not allowed");
        }

        let function_data = FunctionInfo {
            return_type: instructions.get_true_typeinfo(&self.return_type),
            params: self
                .params
                .iter()
                .map(|(name, type_)| (name.clone(), instructions.get_true_typeinfo(type_)))
                .collect(),
            is_defined: true,
        };
        let old = instructions
            .functions
            .insert(self.name.clone(), function_data);
        if let Some(old) = old {
            // check if old was declaration
            if old.is_defined {
                panic!("redefinition of function {}", &self.name);
            }

            // check parameter types are same
            if old
                .params
                .iter()
                .map(|(_, type_)| instructions.get_true_typeinfo(type_))
                .eq(self
                    .params
                    .iter()
                    .map(|(_, type_)| instructions.get_true_typeinfo(type_)))
                == false
            {
                panic!(
                    "Function {} is already declared with different parameter types",
                    &self.name
                );
            }

            // check return type is same
            if instructions.get_true_typeinfo(&old.return_type)
                != instructions.get_true_typeinfo(&self.return_type)
            {
                panic!(
                    "Function {} is already declared with different return type",
                    &self.name
                );
            }
        }
        instructions.set_label(&self.name);

        instructions.function_scope = Some(FunctionScope::new());
        instructions.push_scope();

        // argument initialization
        // function's arguments are pushed to stack before call ( and MUST BE )
        // ===== top of stack: return_address -> arg1 -> arg2 ... =====
        //                           ^ top of stack

        for (id, param) in self.params.iter().enumerate() {
            if param.0.is_some() {
                instructions.link_variable(
                    param.0.as_ref().unwrap(),
                    &instructions.get_true_typeinfo(&param.1),
                    -(id as isize) - 3,
                );
            }
        }

        // push base pointer
        instructions.push(Instruction::PushStack(PushStack {
            operand: Operand::Register(STACK_POINTER_BASE_REGISTER),
        }));
        // move base
        // rbp = rsp
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Register(STACK_POINTER_REGISTER),
            operand_to: Operand::Register(STACK_POINTER_BASE_REGISTER),
        }));
        // here, [rbp-1] is old base pointer
        // here, [rbp-2] is return address

        // body
        self.body.emit(instructions);

        // end of body
        // if return type is void, add return statement
        // else, add panic statement for missing return statement
        if instructions.get_true_typeinfo(&self.return_type) == TypeInfo::Void {
            // force add return statement
            let return_statement = StmtReturn { expr: None };
            return_statement.emit(instructions);
        } else {
            // panic
            instructions.push(Instruction::Panic(Panic {
                message: format!(
                    "Function {} must return a {:?} value",
                    &self.name, &self.return_type
                ),
            }));
        }

        instructions.pop_scope();
        instructions.function_scope = None;
    }
}

#[derive(Debug, Clone)]
pub struct TranslationUnit {
    pub statements: Vec<Statement>,
}
impl TranslationUnit {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        for statement in &self.statements {
            statement.emit(instructions);
        }

        // find main function
        let _main = instructions
            .functions
            .get("main")
            .expect("main function not found");

        let startaddress = instructions.instructions.len();
        instructions.start_address = startaddress;
        instructions.push(Instruction::Call(Call {
            label: "main".to_string(),
        }));
    }
}
