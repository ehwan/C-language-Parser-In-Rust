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
        let lc = lhs.clone();
        let rc = rhs.clone();
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    if *rhs < 0 {
                        *lhs -= (-*rhs) as u64;
                    } else {
                        *lhs += *rhs as u64;
                    }
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::Float32(ref mut lhs), VariableData::Float32(ref rhs)) => {
                    *lhs += *rhs;
                }
                (VariableData::Float64(ref mut lhs), VariableData::Float64(ref rhs)) => {
                    *lhs += *rhs;
                }
                _ => panic!("Invalid type for add assign : {:?} {:?}", lc, rc),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Float32(ref mut lhs), VariableData::Float32(ref rhs)) => {
                    *lhs -= *rhs;
                }
                (VariableData::Float64(ref mut lhs), VariableData::Float64(ref rhs)) => {
                    *lhs -= *rhs;
                }
                _ => panic!("Invalid type for sub assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Float32(ref mut lhs), VariableData::Float32(ref rhs)) => {
                    *lhs *= *rhs;
                }
                (VariableData::Float64(ref mut lhs), VariableData::Float64(ref rhs)) => {
                    *lhs *= *rhs;
                }
                _ => panic!("Invalid type for mul assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Float32(ref mut lhs), VariableData::Float32(ref rhs)) => {
                    *lhs /= *rhs;
                }
                (VariableData::Float64(ref mut lhs), VariableData::Float64(ref rhs)) => {
                    *lhs /= *rhs;
                }
                _ => panic!("Invalid type for div assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs %= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs %= *rhs;
                }
                _ => panic!("Invalid type for mod assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs &= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs &= *rhs;
                }
                _ => panic!("Invalid type for bitwise and assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs |= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs |= *rhs;
                }
                _ => panic!("Invalid type for bitwise or assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs ^= *rhs;
                }
                _ => panic!("Invalid type for bitwise xor assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs <<= *rhs;
                }
                _ => panic!("Invalid type for left shift assign"),
            };
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
        {
            // this `match` should be in emitting phase...
            match (lhs, rhs) {
                (VariableData::UInt8(ref mut lhs), VariableData::UInt8(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::UInt16(ref mut lhs), VariableData::UInt16(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::UInt32(ref mut lhs), VariableData::UInt32(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::UInt64(ref mut lhs), VariableData::UInt64(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::Int8(ref mut lhs), VariableData::Int8(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::Int16(ref mut lhs), VariableData::Int16(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::Int32(ref mut lhs), VariableData::Int32(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                (VariableData::Int64(ref mut lhs), VariableData::Int64(ref rhs)) => {
                    *lhs >>= *rhs;
                }
                _ => panic!("Invalid type for right shift assign"),
            };
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
        let rhs = get_operand_value(program, &self.rhs).clone();
        let rhs = rhs
            .cast_to(&self.lhs_type)
            .expect(format!("Invalid cast to {:?}", &self.lhs_type).as_str());

        *get_operand_value_mut(program, &self.lhs) = rhs;
    }
}
