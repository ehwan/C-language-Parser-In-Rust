use super::Instruction;

use crate::program::variable::VariableData;

use std::cell::RefCell;
use std::rc::Rc;

/// inc 1 to register N
#[derive(Debug)]
pub struct Increment<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Increment<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = program.registers[REGISTER].clone();
        let var = &mut var.borrow_mut().1;
        match var {
            VariableData::Int8(ref mut value) => *value += 1,
            VariableData::UInt8(ref mut value) => *value += 1,
            VariableData::Int16(ref mut value) => *value += 1,
            VariableData::UInt16(ref mut value) => *value += 1,
            VariableData::Int32(ref mut value) => *value += 1,
            VariableData::UInt32(ref mut value) => *value += 1,
            VariableData::Int64(ref mut value) => *value += 1,
            VariableData::UInt64(ref mut value) => *value += 1,
            _ => panic!("Invalid type for increment"),
        }
    }
}

/// dec 1 to register N
#[derive(Debug)]
pub struct Decrement<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Decrement<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = program.registers[REGISTER].clone();
        let var = &mut var.borrow_mut().1;
        match var {
            VariableData::Int8(ref mut value) => *value -= 1,
            VariableData::UInt8(ref mut value) => *value -= 1,
            VariableData::Int16(ref mut value) => *value -= 1,
            VariableData::UInt16(ref mut value) => *value -= 1,
            VariableData::Int32(ref mut value) => *value -= 1,
            VariableData::UInt32(ref mut value) => *value -= 1,
            VariableData::Int64(ref mut value) => *value -= 1,
            VariableData::UInt64(ref mut value) => *value -= 1,
            _ => panic!("Invalid type for decrement"),
        }
    }
}

/// cast register N to type
#[derive(Debug)]
pub struct Cast<const REGISTER_FROM: usize, const REGISTER_TO: usize> {
    pub info: crate::ast::typename::TypeInfo,
}
impl<const REGISTER_FROM: usize, const REGISTER_TO: usize> Instruction
    for Cast<REGISTER_FROM, REGISTER_TO>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let casted = program.registers[REGISTER_FROM]
            .borrow()
            .1
            .cast_to(&self.info);
        if let Some(casted) = casted {
            program.registers[REGISTER_TO] = Rc::new(RefCell::new((self.info.clone(), casted)));
        } else {
            panic!("Invalid cast");
        }
    }
}

/// neg register N
#[derive(Debug)]
pub struct Negate<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Negate<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = &mut program.registers[REGISTER].borrow_mut().1;
        match var {
            VariableData::UInt8(ref mut value) => *value = -(*value as i8) as u8,
            VariableData::UInt16(ref mut value) => *value = -(*value as i16) as u16,
            VariableData::UInt32(ref mut value) => *value = -(*value as i32) as u32,
            VariableData::UInt64(ref mut value) => *value = -(*value as i64) as u64,
            VariableData::Int8(ref mut value) => *value = -*value,
            VariableData::Int16(ref mut value) => *value = -*value,
            VariableData::Int32(ref mut value) => *value = -*value,
            VariableData::Int64(ref mut value) => *value = -*value,
            _ => panic!("Invalid type for negate"),
        }
    }
}

/// logical not register N
#[derive(Debug)]
pub struct LogicalNot<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for LogicalNot<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = &mut program.registers[REGISTER].borrow_mut().1;
        match var {
            VariableData::UInt8(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::UInt16(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::UInt32(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::UInt64(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::Int8(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::Int16(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::Int32(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            VariableData::Int64(ref mut value) => *value = if *value == 0 { 1 } else { 0 },
            _ => panic!("Invalid type for logical not"),
        }
    }
}

/// bitwise not register N
#[derive(Debug)]
pub struct BitwiseNot<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for BitwiseNot<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = &mut program.registers[REGISTER].borrow_mut().1;
        match var {
            VariableData::UInt8(ref mut value) => *value = !*value,
            VariableData::UInt16(ref mut value) => *value = !*value,
            VariableData::UInt32(ref mut value) => *value = !*value,
            VariableData::UInt64(ref mut value) => *value = !*value,
            VariableData::Int8(ref mut value) => *value = !*value,
            VariableData::Int16(ref mut value) => *value = !*value,
            VariableData::Int32(ref mut value) => *value = !*value,
            VariableData::Int64(ref mut value) => *value = !*value,
            _ => panic!("Invalid type for bitwise not"),
        }
    }
}

/// dereference register N
#[derive(Debug)]
pub struct Dereference<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for Dereference<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let ptr = program.registers[REGISTER].clone().borrow().1.clone();
        if let VariableData::Pointer(val) = ptr {
            program.registers[REGISTER] = val.clone();
        } else {
            panic!("Invalid type for dereference");
        }
    }
}

/// address of variable to register N
#[derive(Debug)]
pub struct AddressOf<const REGISTER: usize> {}
impl<const REGISTER: usize> Instruction for AddressOf<REGISTER> {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let var = program.registers[REGISTER].clone();
        let vartype = var.borrow().0.clone();
        let ptr = Rc::new(RefCell::new((
            crate::ast::typename::TypeInfo::Pointer(Box::new(vartype)),
            VariableData::Pointer(var),
        )));
        program.registers[REGISTER] = ptr;
    }
}
