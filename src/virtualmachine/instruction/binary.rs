use super::operand::*;
use super::Instruction;
use crate::ast::typename::TypeInfo;
use crate::virtualmachine::program::VirtualProgram;
use crate::virtualmachine::variable::VariableData;

#[derive(Debug)]
pub struct LessThan {
    pub lhs: Operand,
    pub rhs: Operand,
    pub to: Operand,
}
impl Instruction for LessThan {
    fn execute(&self, program: &mut VirtualProgram) {
        let ret: bool = {
            let lhs = get_operand_value(program, &self.lhs);
            let rhs = get_operand_value(program, &self.rhs);

            if lhs.is_signed_integer() {
                if rhs.is_signed_integer() {
                    let lhs = lhs.to_i64();
                    let rhs = rhs.to_i64();
                    lhs < rhs
                } else if rhs.is_unsigned_integer() {
                    let lhs = lhs.to_i64();
                    let rhs = rhs.to_u64();
                    if lhs < 0 {
                        true
                    } else {
                        (lhs as u64) < rhs
                    }
                } else if rhs.is_float() {
                    lhs.to_f64() < rhs.to_f64()
                } else {
                    panic!("Invalid type for less than");
                }
            } else if lhs.is_unsigned_integer() {
                if rhs.is_signed_integer() {
                    let lhs = lhs.to_u64();
                    let rhs = rhs.to_i64();
                    if rhs < 0 {
                        false
                    } else {
                        lhs < rhs as u64
                    }
                } else if rhs.is_unsigned_integer() {
                    let lhs = lhs.to_u64();
                    let rhs = rhs.to_u64();
                    lhs < rhs
                } else if rhs.is_float() {
                    lhs.to_f64() < rhs.to_f64()
                } else {
                    panic!("Invalid type for less than");
                }
            } else if lhs.is_float() {
                lhs.to_f64() < rhs.to_f64()
            } else {
                panic!("Invalid type for less than");
            }
        };
        *get_operand_value_mut(program, &self.to) = VariableData::UInt8(if ret { 1 } else { 0 });
    }
}

#[derive(Debug)]
pub struct Equal {
    pub lhs: Operand,
    pub rhs: Operand,
    pub to: Operand,
}
impl Instruction for Equal {
    fn execute(&self, program: &mut VirtualProgram) {
        let ret: bool = {
            let lhs = get_operand_value(program, &self.lhs);
            let rhs = get_operand_value(program, &self.rhs);

            if lhs.is_signed_integer() {
                let lhs = lhs.to_i64();
                if rhs.is_signed_integer() {
                    let rhs = rhs.to_i64();
                    lhs == rhs
                } else if rhs.is_unsigned_integer() {
                    let rhs = rhs.to_u64();
                    if lhs < 0 {
                        false
                    } else {
                        (lhs as u64) == rhs
                    }
                } else if rhs.is_float() {
                    panic!("floating point variables are not equal-comparable");
                } else {
                    panic!("Invalid type for equal");
                }
            } else if lhs.is_unsigned_integer() {
                let lhs = lhs.to_u64();
                if rhs.is_signed_integer() {
                    let rhs = rhs.to_i64();
                    if rhs < 0 {
                        false
                    } else {
                        lhs == rhs as u64
                    }
                } else if rhs.is_unsigned_integer() {
                    let rhs = rhs.to_u64();
                    lhs == rhs
                } else if rhs.is_float() {
                    panic!("floating point variables are not equal-comparable");
                } else {
                    panic!("Invalid type for equal");
                }
            } else if lhs.is_float() {
                panic!("floating point variables are not equal-comparable");
            } else {
                panic!("Invalid type for equal");
            }
        };
        *get_operand_value_mut(program, &self.to) = VariableData::UInt8(if ret { 1 } else { 0 });
    }
}

#[derive(Debug)]
pub struct AddAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for AddAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);
        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs += rhs as f32,
                VariableData::UInt16(rhs) => *lhs += rhs as f32,
                VariableData::UInt32(rhs) => *lhs += rhs as f32,
                VariableData::UInt64(rhs) => *lhs += rhs as f32,
                VariableData::Int8(rhs) => *lhs += rhs as f32,
                VariableData::Int16(rhs) => *lhs += rhs as f32,
                VariableData::Int32(rhs) => *lhs += rhs as f32,
                VariableData::Int64(rhs) => *lhs += rhs as f32,
                VariableData::Float32(rhs) => *lhs += rhs as f32,
                VariableData::Float64(rhs) => *lhs += rhs as f32,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs += rhs as f64,
                VariableData::UInt16(rhs) => *lhs += rhs as f64,
                VariableData::UInt32(rhs) => *lhs += rhs as f64,
                VariableData::UInt64(rhs) => *lhs += rhs as f64,
                VariableData::Int8(rhs) => *lhs += rhs as f64,
                VariableData::Int16(rhs) => *lhs += rhs as f64,
                VariableData::Int32(rhs) => *lhs += rhs as f64,
                VariableData::Int64(rhs) => *lhs += rhs as f64,
                VariableData::Float32(rhs) => *lhs += rhs as f64,
                VariableData::Float64(rhs) => *lhs += rhs as f64,
                _ => panic!("Invalid type for add assign"),
            },
        }
    }
}

#[derive(Debug)]
pub struct SubAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for SubAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);
        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs -= rhs as f32,
                VariableData::UInt16(rhs) => *lhs -= rhs as f32,
                VariableData::UInt32(rhs) => *lhs -= rhs as f32,
                VariableData::UInt64(rhs) => *lhs -= rhs as f32,
                VariableData::Int8(rhs) => *lhs -= rhs as f32,
                VariableData::Int16(rhs) => *lhs -= rhs as f32,
                VariableData::Int32(rhs) => *lhs -= rhs as f32,
                VariableData::Int64(rhs) => *lhs -= rhs as f32,
                VariableData::Float32(rhs) => *lhs -= rhs as f32,
                VariableData::Float64(rhs) => *lhs -= rhs as f32,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs -= rhs as f64,
                VariableData::UInt16(rhs) => *lhs -= rhs as f64,
                VariableData::UInt32(rhs) => *lhs -= rhs as f64,
                VariableData::UInt64(rhs) => *lhs -= rhs as f64,
                VariableData::Int8(rhs) => *lhs -= rhs as f64,
                VariableData::Int16(rhs) => *lhs -= rhs as f64,
                VariableData::Int32(rhs) => *lhs -= rhs as f64,
                VariableData::Int64(rhs) => *lhs -= rhs as f64,
                VariableData::Float32(rhs) => *lhs -= rhs as f64,
                VariableData::Float64(rhs) => *lhs -= rhs as f64,
                _ => panic!("Invalid type for sub assign"),
            },
        }
    }
}

#[derive(Debug)]
pub struct MulAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for MulAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs *= rhs as f32,
                VariableData::UInt16(rhs) => *lhs *= rhs as f32,
                VariableData::UInt32(rhs) => *lhs *= rhs as f32,
                VariableData::UInt64(rhs) => *lhs *= rhs as f32,
                VariableData::Int8(rhs) => *lhs *= rhs as f32,
                VariableData::Int16(rhs) => *lhs *= rhs as f32,
                VariableData::Int32(rhs) => *lhs *= rhs as f32,
                VariableData::Int64(rhs) => *lhs *= rhs as f32,
                VariableData::Float32(rhs) => *lhs *= rhs as f32,
                VariableData::Float64(rhs) => *lhs *= rhs as f32,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs *= rhs as f64,
                VariableData::UInt16(rhs) => *lhs *= rhs as f64,
                VariableData::UInt32(rhs) => *lhs *= rhs as f64,
                VariableData::UInt64(rhs) => *lhs *= rhs as f64,
                VariableData::Int8(rhs) => *lhs *= rhs as f64,
                VariableData::Int16(rhs) => *lhs *= rhs as f64,
                VariableData::Int32(rhs) => *lhs *= rhs as f64,
                VariableData::Int64(rhs) => *lhs *= rhs as f64,
                VariableData::Float32(rhs) => *lhs *= rhs as f64,
                VariableData::Float64(rhs) => *lhs *= rhs as f64,
                _ => panic!("Invalid type for mul assign"),
            },
        }
    }
}

#[derive(Debug)]
pub struct DivAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for DivAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs /= rhs as f32,
                VariableData::UInt16(rhs) => *lhs /= rhs as f32,
                VariableData::UInt32(rhs) => *lhs /= rhs as f32,
                VariableData::UInt64(rhs) => *lhs /= rhs as f32,
                VariableData::Int8(rhs) => *lhs /= rhs as f32,
                VariableData::Int16(rhs) => *lhs /= rhs as f32,
                VariableData::Int32(rhs) => *lhs /= rhs as f32,
                VariableData::Int64(rhs) => *lhs /= rhs as f32,
                VariableData::Float32(rhs) => *lhs /= rhs as f32,
                VariableData::Float64(rhs) => *lhs /= rhs as f32,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs /= rhs as f64,
                VariableData::UInt16(rhs) => *lhs /= rhs as f64,
                VariableData::UInt32(rhs) => *lhs /= rhs as f64,
                VariableData::UInt64(rhs) => *lhs /= rhs as f64,
                VariableData::Int8(rhs) => *lhs /= rhs as f64,
                VariableData::Int16(rhs) => *lhs /= rhs as f64,
                VariableData::Int32(rhs) => *lhs /= rhs as f64,
                VariableData::Int64(rhs) => *lhs /= rhs as f64,
                VariableData::Float32(rhs) => *lhs /= rhs as f64,
                VariableData::Float64(rhs) => *lhs /= rhs as f64,
                _ => panic!("Invalid type for div assign"),
            },
        }
    }
}

#[derive(Debug)]
pub struct ModAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for ModAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                _ => panic!("Invalid type for mod assign"),
            },
            _ => panic!("Invalid type for mod assign"),
        }
    }
}

#[derive(Debug)]
pub struct BitwiseAndAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for BitwiseAndAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u8,
                VariableData::UInt16(rhs) => *lhs &= rhs as u8,
                VariableData::UInt32(rhs) => *lhs &= rhs as u8,
                VariableData::UInt64(rhs) => *lhs &= rhs as u8,
                VariableData::Int8(rhs) => *lhs &= rhs as u8,
                VariableData::Int16(rhs) => *lhs &= rhs as u8,
                VariableData::Int32(rhs) => *lhs &= rhs as u8,
                VariableData::Int64(rhs) => *lhs &= rhs as u8,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u16,
                VariableData::UInt16(rhs) => *lhs &= rhs as u16,
                VariableData::UInt32(rhs) => *lhs &= rhs as u16,
                VariableData::UInt64(rhs) => *lhs &= rhs as u16,
                VariableData::Int8(rhs) => *lhs &= rhs as u16,
                VariableData::Int16(rhs) => *lhs &= rhs as u16,
                VariableData::Int32(rhs) => *lhs &= rhs as u16,
                VariableData::Int64(rhs) => *lhs &= rhs as u16,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u32,
                VariableData::UInt16(rhs) => *lhs &= rhs as u32,
                VariableData::UInt32(rhs) => *lhs &= rhs as u32,
                VariableData::UInt64(rhs) => *lhs &= rhs as u32,
                VariableData::Int8(rhs) => *lhs &= rhs as u32,
                VariableData::Int16(rhs) => *lhs &= rhs as u32,
                VariableData::Int32(rhs) => *lhs &= rhs as u32,
                VariableData::Int64(rhs) => *lhs &= rhs as u32,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u64,
                VariableData::UInt16(rhs) => *lhs &= rhs as u64,
                VariableData::UInt32(rhs) => *lhs &= rhs as u64,
                VariableData::UInt64(rhs) => *lhs &= rhs as u64,
                VariableData::Int8(rhs) => *lhs &= rhs as u64,
                VariableData::Int16(rhs) => *lhs &= rhs as u64,
                VariableData::Int32(rhs) => *lhs &= rhs as u64,
                VariableData::Int64(rhs) => *lhs &= rhs as u64,
                _ => panic!("Invalid type for bitwise and assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i8,
                VariableData::UInt16(rhs) => *lhs &= rhs as i8,
                VariableData::UInt32(rhs) => *lhs &= rhs as i8,
                VariableData::UInt64(rhs) => *lhs &= rhs as i8,
                VariableData::Int8(rhs) => *lhs &= rhs as i8,
                VariableData::Int16(rhs) => *lhs &= rhs as i8,
                VariableData::Int32(rhs) => *lhs &= rhs as i8,
                VariableData::Int64(rhs) => *lhs &= rhs as i8,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i16,
                VariableData::UInt16(rhs) => *lhs &= rhs as i16,
                VariableData::UInt32(rhs) => *lhs &= rhs as i16,
                VariableData::UInt64(rhs) => *lhs &= rhs as i16,
                VariableData::Int8(rhs) => *lhs &= rhs as i16,
                VariableData::Int16(rhs) => *lhs &= rhs as i16,
                VariableData::Int32(rhs) => *lhs &= rhs as i16,
                VariableData::Int64(rhs) => *lhs &= rhs as i16,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i32,
                VariableData::UInt16(rhs) => *lhs &= rhs as i32,
                VariableData::UInt32(rhs) => *lhs &= rhs as i32,
                VariableData::UInt64(rhs) => *lhs &= rhs as i32,
                VariableData::Int8(rhs) => *lhs &= rhs as i32,
                VariableData::Int16(rhs) => *lhs &= rhs as i32,
                VariableData::Int32(rhs) => *lhs &= rhs as i32,
                VariableData::Int64(rhs) => *lhs &= rhs as i32,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i64,
                VariableData::UInt16(rhs) => *lhs &= rhs as i64,
                VariableData::UInt32(rhs) => *lhs &= rhs as i64,
                VariableData::UInt64(rhs) => *lhs &= rhs as i64,
                VariableData::Int8(rhs) => *lhs &= rhs as i64,
                VariableData::Int16(rhs) => *lhs &= rhs as i64,
                VariableData::Int32(rhs) => *lhs &= rhs as i64,
                VariableData::Int64(rhs) => *lhs &= rhs as i64,
                _ => panic!("Invalid type for bitwise and assign"),
            },

            _ => panic!("Invalid type for bitwise and assign"),
        }
    }
}

#[derive(Debug)]
pub struct BitwiseOrAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for BitwiseOrAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u8,
                VariableData::UInt16(rhs) => *lhs |= rhs as u8,
                VariableData::UInt32(rhs) => *lhs |= rhs as u8,
                VariableData::UInt64(rhs) => *lhs |= rhs as u8,
                VariableData::Int8(rhs) => *lhs |= rhs as u8,
                VariableData::Int16(rhs) => *lhs |= rhs as u8,
                VariableData::Int32(rhs) => *lhs |= rhs as u8,
                VariableData::Int64(rhs) => *lhs |= rhs as u8,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u16,
                VariableData::UInt16(rhs) => *lhs |= rhs as u16,
                VariableData::UInt32(rhs) => *lhs |= rhs as u16,
                VariableData::UInt64(rhs) => *lhs |= rhs as u16,
                VariableData::Int8(rhs) => *lhs |= rhs as u16,
                VariableData::Int16(rhs) => *lhs |= rhs as u16,
                VariableData::Int32(rhs) => *lhs |= rhs as u16,
                VariableData::Int64(rhs) => *lhs |= rhs as u16,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u32,
                VariableData::UInt16(rhs) => *lhs |= rhs as u32,
                VariableData::UInt32(rhs) => *lhs |= rhs as u32,
                VariableData::UInt64(rhs) => *lhs |= rhs as u32,
                VariableData::Int8(rhs) => *lhs |= rhs as u32,
                VariableData::Int16(rhs) => *lhs |= rhs as u32,
                VariableData::Int32(rhs) => *lhs |= rhs as u32,
                VariableData::Int64(rhs) => *lhs |= rhs as u32,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u64,
                VariableData::UInt16(rhs) => *lhs |= rhs as u64,
                VariableData::UInt32(rhs) => *lhs |= rhs as u64,
                VariableData::UInt64(rhs) => *lhs |= rhs as u64,
                VariableData::Int8(rhs) => *lhs |= rhs as u64,
                VariableData::Int16(rhs) => *lhs |= rhs as u64,
                VariableData::Int32(rhs) => *lhs |= rhs as u64,
                VariableData::Int64(rhs) => *lhs |= rhs as u64,
                _ => panic!("Invalid type for bitwise or assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i8,
                VariableData::UInt16(rhs) => *lhs |= rhs as i8,
                VariableData::UInt32(rhs) => *lhs |= rhs as i8,
                VariableData::UInt64(rhs) => *lhs |= rhs as i8,
                VariableData::Int8(rhs) => *lhs |= rhs as i8,
                VariableData::Int16(rhs) => *lhs |= rhs as i8,
                VariableData::Int32(rhs) => *lhs |= rhs as i8,
                VariableData::Int64(rhs) => *lhs |= rhs as i8,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i16,
                VariableData::UInt16(rhs) => *lhs |= rhs as i16,
                VariableData::UInt32(rhs) => *lhs |= rhs as i16,
                VariableData::UInt64(rhs) => *lhs |= rhs as i16,
                VariableData::Int8(rhs) => *lhs |= rhs as i16,
                VariableData::Int16(rhs) => *lhs |= rhs as i16,
                VariableData::Int32(rhs) => *lhs |= rhs as i16,
                VariableData::Int64(rhs) => *lhs |= rhs as i16,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i32,
                VariableData::UInt16(rhs) => *lhs |= rhs as i32,
                VariableData::UInt32(rhs) => *lhs |= rhs as i32,
                VariableData::UInt64(rhs) => *lhs |= rhs as i32,
                VariableData::Int8(rhs) => *lhs |= rhs as i32,
                VariableData::Int16(rhs) => *lhs |= rhs as i32,
                VariableData::Int32(rhs) => *lhs |= rhs as i32,
                VariableData::Int64(rhs) => *lhs |= rhs as i32,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i64,
                VariableData::UInt16(rhs) => *lhs |= rhs as i64,
                VariableData::UInt32(rhs) => *lhs |= rhs as i64,
                VariableData::UInt64(rhs) => *lhs |= rhs as i64,
                VariableData::Int8(rhs) => *lhs |= rhs as i64,
                VariableData::Int16(rhs) => *lhs |= rhs as i64,
                VariableData::Int32(rhs) => *lhs |= rhs as i64,
                VariableData::Int64(rhs) => *lhs |= rhs as i64,
                _ => panic!("Invalid type for bitwise or assign"),
            },

            _ => panic!("Invalid type for bitwise or assign"),
        }
    }
}

#[derive(Debug)]
pub struct BitwiseXorAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for BitwiseXorAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u8,
                VariableData::Int8(rhs) => *lhs ^= rhs as u8,
                VariableData::Int16(rhs) => *lhs ^= rhs as u8,
                VariableData::Int32(rhs) => *lhs ^= rhs as u8,
                VariableData::Int64(rhs) => *lhs ^= rhs as u8,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u16,
                VariableData::Int8(rhs) => *lhs ^= rhs as u16,
                VariableData::Int16(rhs) => *lhs ^= rhs as u16,
                VariableData::Int32(rhs) => *lhs ^= rhs as u16,
                VariableData::Int64(rhs) => *lhs ^= rhs as u16,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u32,
                VariableData::Int8(rhs) => *lhs ^= rhs as u32,
                VariableData::Int16(rhs) => *lhs ^= rhs as u32,
                VariableData::Int32(rhs) => *lhs ^= rhs as u32,
                VariableData::Int64(rhs) => *lhs ^= rhs as u32,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u64,
                VariableData::Int8(rhs) => *lhs ^= rhs as u64,
                VariableData::Int16(rhs) => *lhs ^= rhs as u64,
                VariableData::Int32(rhs) => *lhs ^= rhs as u64,
                VariableData::Int64(rhs) => *lhs ^= rhs as u64,
                _ => panic!("Invalid type for bitwise xor assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i8,
                VariableData::Int8(rhs) => *lhs ^= rhs as i8,
                VariableData::Int16(rhs) => *lhs ^= rhs as i8,
                VariableData::Int32(rhs) => *lhs ^= rhs as i8,
                VariableData::Int64(rhs) => *lhs ^= rhs as i8,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i16,
                VariableData::Int8(rhs) => *lhs ^= rhs as i16,
                VariableData::Int16(rhs) => *lhs ^= rhs as i16,
                VariableData::Int32(rhs) => *lhs ^= rhs as i16,
                VariableData::Int64(rhs) => *lhs ^= rhs as i16,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i32,
                VariableData::Int8(rhs) => *lhs ^= rhs as i32,
                VariableData::Int16(rhs) => *lhs ^= rhs as i32,
                VariableData::Int32(rhs) => *lhs ^= rhs as i32,
                VariableData::Int64(rhs) => *lhs ^= rhs as i32,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i64,
                VariableData::Int8(rhs) => *lhs ^= rhs as i64,
                VariableData::Int16(rhs) => *lhs ^= rhs as i64,
                VariableData::Int32(rhs) => *lhs ^= rhs as i64,
                VariableData::Int64(rhs) => *lhs ^= rhs as i64,
                _ => panic!("Invalid type for bitwise xor assign"),
            },

            _ => panic!("Invalid type for bitwise xor assign"),
        }
    }
}

#[derive(Debug)]
pub struct ShiftLeftAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for ShiftLeftAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            _ => panic!("Invalid type for left shift assign"),
        }
    }
}

#[derive(Debug)]
pub struct ShiftRightAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for ShiftRightAssign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            _ => panic!("Invalid type for right shift assign"),
        }
    }
}

#[derive(Debug)]
pub struct Assign {
    pub lhs_type: TypeInfo,
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for Assign {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.rhs)
            .cast_to(&self.lhs_type)
            .expect(format!("Invalid cast to {:?}", &self.lhs_type).as_str());

        *get_operand_value_mut(program, &self.lhs) = rhs;
    }
}

#[derive(Debug)]
pub struct AssignStruct {
    pub count: usize,
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Instruction for AssignStruct {
    fn execute(&self, program: &mut VirtualProgram) {
        let lhs_address = get_operand_value(program, &self.lhs).to_u64() as usize;
        let rhs_address = get_operand_value(program, &self.rhs).to_u64() as usize;

        for i in 0..self.count {
            program.stack[lhs_address + i] = program.stack[rhs_address + i].clone();
        }
    }
}
