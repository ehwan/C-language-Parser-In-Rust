use super::variable::VariableData;
use crate::ast::typename::TypeInfo;
use std::cell::RefCell;
use std::rc::Rc;

pub mod binary;
pub mod expression;
pub mod unary;

pub trait Instruction: std::fmt::Debug {
    fn execute(&self, program: &mut crate::program::program::Program);
}

/// Do Nothing
#[derive(Debug)]
pub struct DefineLabel {
    pub label: String,
}
impl Instruction for DefineLabel {
    fn execute(&self, _program: &mut crate::program::program::Program) {
        // label is defined at emitting session
    }
}

// move constant to register
#[derive(Debug)]
pub struct Constant<const REGISTER: usize> {
    pub value: VariableData,
    pub info: TypeInfo,
}
impl<const REGISTER: usize> Instruction for Constant<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.registers[REGISTER] =
            Rc::new(RefCell::new((self.info.clone(), self.value.clone())));
    }
}

/// Pop one from stack and discard
#[derive(Debug)]
pub struct PopStack {}
impl Instruction for PopStack {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.pop_from_stack();
    }
}

/// Pop one from stack to register N
#[derive(Debug)]
pub struct PopStackTo<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for PopStackTo<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = program.pop_from_stack();
        program.registers[REGISTER] = var;
    }
}

/// Push register N to stack
#[derive(Debug)]
pub struct PushRegister<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for PushRegister<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.push_to_stack(program.registers[REGISTER].clone());
    }
}

/// copy register N to register M
#[derive(Debug)]
pub struct MoveRegister<const REGISTER_FROM: usize, const REGISTER_TO: usize> {}
impl<const REGISTER_FROM: usize, const REGISTER_TO: usize> Instruction
    for MoveRegister<REGISTER_FROM, REGISTER_TO>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.registers[REGISTER_TO] = program.registers[REGISTER_FROM].clone();
    }
}

/// Deep copy register N to register M
#[derive(Debug)]
pub struct DeepCopyRegister<const REGISTER_FROM: usize, const REGISTER_TO: usize> {}
impl<const REGISTER_FROM: usize, const REGISTER_TO: usize> Instruction
    for DeepCopyRegister<REGISTER_FROM, REGISTER_TO>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let typeinfo = program.registers[REGISTER_FROM].borrow().0.clone();
        let deep_cloned = program.registers[REGISTER_FROM].borrow().1.deep_clone();
        program.registers[REGISTER_TO] = Rc::new(RefCell::new((typeinfo, deep_cloned)));
    }
}

/// Set Current Address to Register N
#[derive(Debug)]
pub struct SetAddress<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for SetAddress<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = program.current_instruction as u64;
        program.registers[REGISTER] = Rc::new(RefCell::new((
            TypeInfo::UInt64,
            VariableData::UInt64(address),
        )));
    }
}

/// Get address of label
#[derive(Debug)]
pub struct GetLabelAddress<const REGISTER: usize> {
    pub label: String,
}
impl<const REGISTER: usize> Instruction for GetLabelAddress<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = *program.labels.get(&self.label).expect("Label not found");
        program.registers[REGISTER] = Rc::new(RefCell::new((
            TypeInfo::UInt64,
            VariableData::UInt64(address as u64),
        )));
    }
}

/// jump to address in Register N
#[derive(Debug)]
pub struct Jump<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Jump<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = program.registers[REGISTER].borrow().1.to_u64() as usize;
        program.current_instruction = address;
    }
}

/// Jump to address if register N is zero
#[derive(Debug)]
pub struct JumpZero<const ADDRESS_REGISTER: usize, const COND_REGISTER: usize> {}
impl<const ADDRESS_REGISTER: usize, const COND_REGISTER: usize> Instruction
    for JumpZero<ADDRESS_REGISTER, COND_REGISTER>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = program.registers[ADDRESS_REGISTER].borrow().1.to_u64() as usize;
        if program.registers[COND_REGISTER].borrow().1.to_u64() == 0 {
            program.current_instruction = address;
        }
    }
}

/// Jump to address if register N is not zero
#[derive(Debug)]
pub struct JumpNotZero<const ADDRESS_REGISTER: usize, const COND_REGISTER: usize> {}
impl<const ADDRESS_REGISTER: usize, const COND_REGISTER: usize> Instruction
    for JumpNotZero<ADDRESS_REGISTER, COND_REGISTER>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = program.registers[ADDRESS_REGISTER].borrow().1.to_u64() as usize;
        if program.registers[COND_REGISTER].borrow().1.to_u64() != 0 {
            program.current_instruction = address;
        }
    }
}

/// move variable to register
#[derive(Debug)]
pub struct GetVariable<const REGISTER: usize> {
    pub name: String,
}
impl<const REGISTER: usize> Instruction for GetVariable<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = program
            .search_variable(&self.name)
            .expect("Variable not found")
            .clone();
        program.registers[REGISTER] = var;
    }
}

/// make new variable with default value
#[derive(Debug)]
pub struct NewVariable {
    pub name: String,
    pub info: TypeInfo,
}
impl Instruction for NewVariable {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let old = program.cur_scope_mut().variables.insert(
            self.name.clone(),
            Rc::new(RefCell::new((
                self.info.clone(),
                VariableData::init_default(&self.info),
            ))),
        );
        if old.is_some() {
            panic!("Variable {} already exists", &self.name);
        }
    }
}

/// make new scope
#[derive(Debug)]
pub struct NewScope {}
impl Instruction for NewScope {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.new_scope();
    }
}

/// pop outermost scope
#[derive(Debug)]
pub struct PopScope {}
impl Instruction for PopScope {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.pop_scope();
    }
}

#[derive(Debug)]
pub struct DeclareStructure {
    pub info: crate::ast::typename::StructInfo,
}
impl Instruction for DeclareStructure {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let old = program.cur_scope_mut().type_infos.insert(
            self.info.name.clone().unwrap(),
            Rc::new(TypeInfo::Struct(self.info.clone())),
        );
        if old.is_some() {
            panic!("Struct {} already exists", self.info.name.clone().unwrap());
        }
    }
}

#[derive(Debug)]
pub struct DeclareUnion {
    pub info: crate::ast::typename::UnionInfo,
}
impl Instruction for DeclareUnion {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let old = program.cur_scope_mut().type_infos.insert(
            self.info.name.clone().unwrap(),
            Rc::new(TypeInfo::Union(self.info.clone())),
        );
        if old.is_some() {
            panic!("Union {} already exists", self.info.name.clone().unwrap());
        }
    }
}

#[derive(Debug)]
pub struct DeclareEnum {
    pub info: crate::ast::typename::EnumInfo,
}
impl Instruction for DeclareEnum {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let old = program.cur_scope_mut().type_infos.insert(
            self.info.name.clone().unwrap(),
            Rc::new(TypeInfo::Enum(self.info.clone())),
        );
        if old.is_some() {
            panic!("Enum {} already exists", self.info.name.clone().unwrap());
        }
    }
}

// print vars in stack
#[derive(Debug)]
pub struct Print {}
impl Instruction for Print {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let count = program.pop_from_stack().borrow().1.to_u64();

        for _ in 0..count {
            let var = program.pop_from_stack();
            println!("{:?}", var.borrow().1);
        }
    }
}

/// push current address and scope count to stack and jump
#[derive(Debug)]
pub struct Call<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Call<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        program.push_to_stack(Rc::new(RefCell::new((
            TypeInfo::UInt64,
            VariableData::UInt64(program.scopes.len() as u64),
        ))));
        let address = program.registers[REGISTER].borrow().1.to_u64() as usize;
        program.push_to_stack(Rc::new(RefCell::new((
            TypeInfo::UInt64,
            VariableData::UInt64(program.current_instruction as u64),
        ))));
        program.current_instruction = address;
    }
}

#[derive(Debug)]
pub struct Return {}
impl Instruction for Return {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let address = program.pop_from_stack().borrow().1.to_u64() as usize;
        let scope_count = program.pop_from_stack().borrow().1.to_u64() as usize;
        program.current_instruction = address;
        while program.scopes.len() > scope_count {
            program.pop_scope();
        }
    }
}
