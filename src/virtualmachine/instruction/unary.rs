use super::Instruction;

use super::operand::*;
use crate::ast::typename::TypeInfo;
use crate::virtualmachine::program::VirtualProgram;
use crate::virtualmachine::variable::VariableData;

/// inc 1 register N
#[derive(Debug)]
pub struct Increment {
    pub operand: Operand, // register that have value of stack index
}
impl Instruction for Increment {
    fn execute(&self, program: &mut VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
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
pub struct Decrement {
    pub operand: Operand, // register that have value of stack index
}
impl Instruction for Decrement {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
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
pub struct Cast {
    pub info: TypeInfo,
    pub operand_from: Operand,
    pub operand_to: Operand,
}
impl Instruction for Cast {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let rhs = get_operand_value(program, &self.operand_from);
        let casted = rhs.cast_to(&self.info).expect("Invalid cast");
        *get_operand_value_mut(program, &self.operand_to) = casted;
    }
}

/// neg register N
#[derive(Debug)]
pub struct Negate {
    pub operand: Operand,
}
impl Instruction for Negate {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
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
pub struct LogicalNot {
    pub operand: Operand,
}
impl Instruction for LogicalNot {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
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
pub struct BitwiseNot {
    pub operand: Operand,
}
impl Instruction for BitwiseNot {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
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

/*
/// dereference register N
#[derive(Debug)]
pub struct Dereference {
    pub register_address: usize, // register that have value of stack index
    pub register_to: usize,
}
impl Instruction for Dereference {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let ptr = &program.registers[self.register_address];
        if let VariableData::Pointer(ptr) = ptr {
            program.registers[self.register_to] = program.stack[*ptr].clone();
        } else if let VariableData::UInt64(ptr) = ptr {
            program.registers[self.register_to] = program.stack[*ptr as usize].clone();
        } else {
            panic!("Invalid type for dereference");
        }
    }
}

*/
