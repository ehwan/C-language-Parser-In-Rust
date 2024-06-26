use super::Instruction;

use super::operand::*;
use crate::ast::typename::TypeInfo;
use crate::virtualmachine::program::VirtualProgram;
use crate::virtualmachine::variable::VariableData;

/// cast register N to type
#[derive(Debug)]
pub struct Cast {
    pub info: TypeInfo,
    pub operand_from: Operand,
    pub operand_to: Operand,
}
impl Instruction for Cast {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs_casted = get_operand_value(program, &self.operand_from)
            .cast_to(&self.info)
            .expect(format!("Invalid cast: to {:?}", &self.info).as_str());
        *get_operand_value_mut(program, &self.operand_to) = rhs_casted;
    }
}

/// neg register N
#[derive(Debug)]
pub struct Negate {
    pub operand: Operand,
}
impl Instruction for Negate {
    fn execute(&self, program: &mut VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
        let res = match &var {
            VariableData::UInt8(value) => VariableData::Int8(-(*value as i8)),
            VariableData::UInt16(value) => VariableData::Int16(-(*value as i16)),
            VariableData::UInt32(value) => VariableData::Int32(-(*value as i32)),
            VariableData::UInt64(value) => VariableData::Int64(-(*value as i64)),
            VariableData::Int8(value) => VariableData::Int8(-*value),
            VariableData::Int16(value) => VariableData::Int16(-*value),
            VariableData::Int32(value) => VariableData::Int32(-*value),
            VariableData::Int64(value) => VariableData::Int64(-*value),
            VariableData::Float32(value) => VariableData::Float32(-*value),
            VariableData::Float64(value) => VariableData::Float64(-*value),
            _ => panic!("Invalid type for negate"),
        };
        *var = res;
    }
}

/// logical not register N
#[derive(Debug)]
pub struct LogicalNot {
    pub operand: Operand,
}
impl Instruction for LogicalNot {
    fn execute(&self, program: &mut VirtualProgram) {
        let var = get_operand_value_mut(program, &self.operand);
        let res = match &var {
            VariableData::UInt8(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt16(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt32(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt64(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int8(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int16(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int32(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int64(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            _ => panic!("Invalid type for logical not"),
        };
        *var = VariableData::UInt8(res as u8);
    }
}

/// bitwise not register N
#[derive(Debug)]
pub struct BitwiseNot {
    pub operand: Operand,
}
impl Instruction for BitwiseNot {
    fn execute(&self, program: &mut VirtualProgram) {
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
