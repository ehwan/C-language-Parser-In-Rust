use super::Instruction;

use crate::ast::typename::TypeInfo;
use crate::program::variable::VariableData;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Add<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Add<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Float32, VariableData::Float32(result))
            }
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => {
                let result = *lhs + *rhs;
                (TypeInfo::Float64, VariableData::Float64(result))
            }

            _ => panic!("Invalid type for add : {:?} + {:?}", lhs_type, rhs_type),
        };

        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct Sub<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Sub<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Float32, VariableData::Float32(result))
            }
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => {
                let result = *lhs - *rhs;
                (TypeInfo::Float64, VariableData::Float64(result))
            }
            _ => panic!("Invalid type for sub : {:?} + {:?}", lhs_type, rhs_type),
        };

        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct Mul<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Mul<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Float32, VariableData::Float32(result))
            }
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => {
                let result = *lhs * *rhs;
                (TypeInfo::Float64, VariableData::Float64(result))
            }
            _ => panic!("Invalid type for mul: {:?} + {:?}", lhs_type, rhs_type),
        };

        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct Div<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Div<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Float32, VariableData::Float32(result))
            }
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => {
                let result = *lhs / *rhs;
                (TypeInfo::Float64, VariableData::Float64(result))
            }
            _ => panic!("Invalid type for div: {:?} + {:?}", lhs_type, rhs_type),
        };

        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct Mod<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Mod<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs % *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!("Invalid type for mod: {:?} + {:?}", lhs_type, rhs_type),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct BitwiseAnd<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseAnd<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs & *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!(
                "Invalid type for bitwise and: {:?} + {:?}",
                lhs_type, rhs_type
            ),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct BitwiseOr<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseOr<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs | *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!(
                "Invalid type for bitwise or: {:?} + {:?}",
                lhs_type, rhs_type
            ),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct BitwiseXor<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseXor<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs ^ *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!(
                "Invalid type for bitwise xor: {:?} + {:?}",
                lhs_type, rhs_type
            ),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct ShiftLeft<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for ShiftLeft<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs << *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!(
                "Invalid type for shift left: {:?} + {:?}",
                lhs_type, rhs_type
            ),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct ShiftRight<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for ShiftRight<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_type = rhs_variable.borrow().0.clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        // this `match` should be in emitting phase...
        let (newtype, newval) = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::UInt8, VariableData::UInt8(result))
            }
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::UInt16, VariableData::UInt16(result))
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::UInt32, VariableData::UInt32(result))
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::UInt64, VariableData::UInt64(result))
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::Int8, VariableData::Int8(result))
            }
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::Int16, VariableData::Int16(result))
            }
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::Int32, VariableData::Int32(result))
            }
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => {
                let result = *lhs >> *rhs;
                (TypeInfo::Int64, VariableData::Int64(result))
            }
            _ => panic!(
                "Invalid type for shift right: {:?} + {:?}",
                lhs_type, rhs_type
            ),
        };
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct LogicalAnd<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for LogicalAnd<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs != 0 && *rhs != 0,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                *lhs != 0 && *rhs != 0
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                *lhs != 0 && *rhs != 0
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                *lhs != 0 && *rhs != 0
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs != 0 && *rhs != 0,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs != 0 && *rhs != 0,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs != 0 && *rhs != 0,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs != 0 && *rhs != 0,
            _ => panic!("Invalid type for logical and"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct LogicalOr<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for LogicalOr<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs != 0 || *rhs != 0,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => {
                *lhs != 0 || *rhs != 0
            }
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => {
                *lhs != 0 || *rhs != 0
            }
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => {
                *lhs != 0 || *rhs != 0
            }
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs != 0 || *rhs != 0,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs != 0 || *rhs != 0,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs != 0 || *rhs != 0,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs != 0 || *rhs != 0,
            _ => panic!("Invalid type for logical or"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct LessThan<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for LessThan<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs < *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs < *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs < *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs < *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs < *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs < *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs < *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs < *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs < *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs < *rhs,
            _ => panic!("Invalid type for less than"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct LessThanOrEqual<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for LessThanOrEqual<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs <= *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs <= *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs <= *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs <= *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs <= *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs <= *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs <= *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs <= *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs <= *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs <= *rhs,
            _ => panic!("Invalid type for less than or equal"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct GreaterThan<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for GreaterThan<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs > *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs > *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs > *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs > *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs > *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs > *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs > *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs > *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs > *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs > *rhs,
            _ => panic!("Invalid type for greater than"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct GreaterThanOrEqual<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for GreaterThanOrEqual<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs >= *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs >= *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs >= *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs >= *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs >= *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs >= *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs >= *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs >= *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs >= *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs >= *rhs,
            _ => panic!("Invalid type for greater than or equal"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct Equal<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Equal<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs == *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs == *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs == *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs == *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs == *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs == *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs == *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs == *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs == *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs == *rhs,
            _ => panic!("Invalid type for equal"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct NotEqual<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for NotEqual<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();

        let lhs = &lhs_variable.borrow_mut().1;
        let rhs = &rhs_variable.borrow_mut().1;

        let result = match (lhs, rhs) {
            (VariableData::UInt8(ref lhs), VariableData::UInt8(ref rhs)) => *lhs != *rhs,
            (VariableData::UInt16(ref lhs), VariableData::UInt16(ref rhs)) => *lhs != *rhs,
            (VariableData::UInt32(ref lhs), VariableData::UInt32(ref rhs)) => *lhs != *rhs,
            (VariableData::UInt64(ref lhs), VariableData::UInt64(ref rhs)) => *lhs != *rhs,
            (VariableData::Int8(ref lhs), VariableData::Int8(ref rhs)) => *lhs != *rhs,
            (VariableData::Int16(ref lhs), VariableData::Int16(ref rhs)) => *lhs != *rhs,
            (VariableData::Int32(ref lhs), VariableData::Int32(ref rhs)) => *lhs != *rhs,
            (VariableData::Int64(ref lhs), VariableData::Int64(ref rhs)) => *lhs != *rhs,
            (VariableData::Float32(ref lhs), VariableData::Float32(ref rhs)) => *lhs != *rhs,
            (VariableData::Float64(ref lhs), VariableData::Float64(ref rhs)) => *lhs != *rhs,
            _ => panic!("Invalid type for not equal"),
        };

        let newtype = TypeInfo::UInt8;
        let newval = VariableData::UInt8(if result { 1 } else { 0 });
        program.registers[0] = Rc::new(RefCell::new((newtype, newval)));
    }
}

#[derive(Debug)]
pub struct AddAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for AddAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for add assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct SubAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for SubAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for subtract assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct MulAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for MulAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for multiply assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct DivAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for DivAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for divide assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct ModAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for ModAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for modulo assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct BitwiseAndAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseAndAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for bitwise and assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct BitwiseOrAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseOrAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for bitwise or assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct BitwiseXorAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for BitwiseXorAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for bitwise xor assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct ShiftLeftAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for ShiftLeftAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for left shift assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct ShiftRightAssign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for ShiftRightAssign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        {
            let lhs_type = lhs_variable.borrow().0.clone();
            let rhs_type = rhs_variable.borrow().0.clone();
            let lhs = &mut lhs_variable.borrow_mut().1;
            let rhs = &rhs_variable.borrow_mut().1;

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
                _ => panic!(
                    "Invalid type for right shift assign: {:?} + {:?}",
                    lhs_type, rhs_type
                ),
            };
        }
        let lhs_type = lhs_variable.borrow().0.clone();
        let lhs = lhs_variable.borrow().1.deep_clone();
        program.registers[0] = Rc::new(RefCell::new((lhs_type, lhs)));
    }
}

#[derive(Debug)]
pub struct Assign<const REGISTER_LHS: usize, const REGISTER_RHS: usize> {}
impl<const REGISTER_LHS: usize, const REGISTER_RHS: usize> Instruction
    for Assign<REGISTER_LHS, REGISTER_RHS>
{
    fn execute(&self, program: &mut crate::program::program::Program) {
        let lhs_variable = program.registers[REGISTER_LHS].clone();
        let lhs_type = lhs_variable.borrow().0.clone();
        let rhs_variable = program.registers[REGISTER_RHS].clone();
        let rhs_type = rhs_variable.borrow().0.clone();

        if lhs_type == rhs_type {
            let rhs_cloned = rhs_variable.borrow().1.deep_clone();
            lhs_variable.borrow_mut().1 = rhs_cloned;
        } else {
            let rhs_cloned = rhs_variable.borrow().1.deep_clone();
            if let Some(rhs_casted) = rhs_cloned.cast_to(&lhs_type) {
                lhs_variable.borrow_mut().1 = rhs_casted;
            } else {
                panic!("Invalid type for assign: {:?} + {:?}", lhs_type, rhs_type);
            }
        }
    }
}
