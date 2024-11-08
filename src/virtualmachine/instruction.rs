use crate::ast2::Address;
use crate::ast2::{Float, Integer};

use super::LabelType;
use super::Operand;
use super::SizeType;

#[derive(Debug, Clone)]
pub enum Instruction {
    /// from -> to
    Move(SizeType, Operand, Operand),

    Jump(LabelType),
    JumpZero(SizeType, Operand, LabelType),
    JumpNotZero(SizeType, Operand, LabelType),

    Push(SizeType, Operand),
    Pop(SizeType, Operand),

    /// bytes, src, dst
    Memcpy(usize, Operand, Operand),

    /// float to float, ax, inplace
    F2F(Float, Float),
    /// float to int, ax, inplace
    F2I(Float, Integer),
    /// integer to float, ax, inplace
    I2F(Integer, Float),
    /// integer to integer, ax, inplace
    I2I(Integer, Integer),

    AddI(SizeType, Operand, Operand, Operand),
    AddF(SizeType, Operand, Operand, Operand),
    SubI(SizeType, Operand, Operand, Operand),
    SubF(SizeType, Operand, Operand, Operand),

    /// (src_size, src, dst)
    /// dst must Be int32 (dword)
    LogicalNot(SizeType, Operand, Operand),
    Neg(SizeType, Operand),
    BitNot(SizeType, Operand),

    BitAnd(SizeType, Operand, Operand),
    BitOr(SizeType, Operand, Operand),
    BitXor(SizeType, Operand, Operand),

    ShiftLeftI(SizeType, Operand, Operand),
    ShiftLeftU(SizeType, Operand, Operand),
    ShiftRightI(SizeType, Operand, Operand),
    ShiftRightU(SizeType, Operand, Operand),

    Equal(SizeType, Operand, Operand, Operand),
    LessThanI(SizeType, Operand, Operand, Operand),
    LessThanU(SizeType, Operand, Operand, Operand),
    LessThanF(SizeType, Operand, Operand, Operand),

    ModI(SizeType, Operand, Operand, Operand),
    ModU(SizeType, Operand, Operand, Operand),

    MulF(SizeType, Operand, Operand, Operand),
    MulI(SizeType, Operand, Operand, Operand),
    MulU(SizeType, Operand, Operand, Operand),

    DivF(SizeType, Operand, Operand, Operand),
    DivI(SizeType, Operand, Operand, Operand),
    DivU(SizeType, Operand, Operand, Operand),
}
