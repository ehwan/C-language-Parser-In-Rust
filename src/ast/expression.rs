use super::typename::TypeInfo;
use crate::program::instruction::binary::Assign;
use crate::program::instruction::{
    Constant, DeepCopyRegister, GetLabelAddress, GetVariable, Instruction, Jump, JumpZero,
    NewScope, PopScope, Print, PushRegister, SetAddress,
};
use crate::program::{program::Program, variable::VariableData};

use core::panic;
use std::borrow::Borrow;
use std::rc::Rc;
use std::{any::Any, cell::RefCell};

pub trait Expression: std::fmt::Debug + Any {
    /// push instruction that eval expression and store result in register0
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>);

    fn as_any(&self) -> &dyn Any;

    fn get_constant_i64(&self) -> Result<i64, String> {
        Err(format!("get_constant_i64 not implemented for {:?}", self))
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo;
}

// expression that always returns void
#[derive(Debug, Clone)]
pub struct VoidExpression {}
impl Expression for VoidExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Void,
            info: TypeInfo::Void,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Void
    }
}

#[derive(Debug, Clone)]
pub struct PrimaryIdentifier {
    pub name: String,
}
impl Expression for PrimaryIdentifier {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::GetVariable::<0> {
            name: self.name.clone(),
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        let var = program.search_variable(&self.name);
        if let Some(var) = var {
            var.as_ref().borrow().0.clone()
        } else {
            panic!("Variable {} not found", self.name);
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstantInteger {
    pub value: i32,
}
impl Expression for ConstantInteger {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Int32(self.value),
            info: TypeInfo::Int32,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Int32
    }
}
#[derive(Debug, Clone)]
pub struct ConstantUnsignedInteger {
    pub value: u32,
}
impl Expression for ConstantUnsignedInteger {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::UInt32(self.value),
            info: TypeInfo::UInt32,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt32
    }
}

#[derive(Debug, Clone)]
pub struct ConstantCharacter {
    pub value: i8,
}
impl Expression for ConstantCharacter {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Int8(self.value),
            info: TypeInfo::UInt8,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Int8
    }
}

#[derive(Debug, Clone)]
pub struct ConstantLong {
    pub value: i64,
}
impl Expression for ConstantLong {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Int64(self.value),
            info: TypeInfo::Int64,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Int64
    }
}
#[derive(Debug, Clone)]
pub struct ConstantUnsignedLong {
    pub value: u64,
}
impl Expression for ConstantUnsignedLong {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::UInt64(self.value),
            info: TypeInfo::UInt64,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
    }
}

#[derive(Debug, Clone)]
pub struct ConstantFloat {
    pub value: f32,
}
impl Expression for ConstantFloat {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Float32(self.value),
            info: TypeInfo::Float32,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Float32
    }
}

#[derive(Debug, Clone)]
pub struct ConstantDouble {
    pub value: f64,
}
impl Expression for ConstantDouble {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Float64(self.value),
            info: TypeInfo::Float64,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::Float64
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}
impl Expression for StringLiteral {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let mut bytes = self.value.as_bytes().to_vec();
        bytes.push(0);
        let bytes: Vec<_> = bytes
            .into_iter()
            .map(|b| Rc::new(RefCell::new((TypeInfo::UInt8, VariableData::UInt8(b)))))
            .collect();
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::Array(bytes),
            info: TypeInfo::Array(Box::new(TypeInfo::UInt8), Some(self.value.len() + 1)),
        }));
        panic!("StringLiteral.eval not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        // len+1 for null-terminator
        TypeInfo::Array(Box::new(TypeInfo::UInt8), Some(self.value.len() + 1))
    }
}

#[derive(Debug)]
pub struct PostBracket {
    pub src: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}
impl Expression for PostBracket {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::MoveRegister::<0, 1> {},
        ));
        self.index.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::expression::GetArrayElement {},
        ));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        if let TypeInfo::Array(t, _) = self.src.get_typeinfo(program) {
            *t
        } else {
            panic!("Bracket on non-array type");
        }
    }
}

#[derive(Debug)]
pub struct PostParen {
    pub src: Box<dyn Expression>,
    pub args: Vec<Box<dyn Expression>>,
}
// currently only for function call
impl Expression for PostParen {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let identifier = self
            .src
            .as_any()
            .downcast_ref::<PrimaryIdentifier>()
            .expect("FunctionCall must be called on `Identifier`");
        let name = identifier.name.clone();

        // check if it is a built-in function, print
        if &name == "print" {
            for arg in self.args.iter().rev() {
                arg.emit(program, instructions);
                instructions.push(Box::new(PushRegister::<0> {}));
            }
            instructions.push(Box::new(Constant::<0> {
                value: VariableData::UInt64(self.args.len() as u64),
                info: TypeInfo::UInt64,
            }));
            instructions.push(Box::new(PushRegister::<0> {}));
            instructions.push(Box::new(Print {}));
            return;
        } else {
            let funcdata = (*program.functions.get(&name).expect("Function not found")).clone();
            if funcdata.params.len() != self.args.len() {
                panic!(
                    "Function {} expects {} arguments, but {} were provided",
                    name,
                    funcdata.params.len(),
                    self.args.len()
                );
            }
            // push current address to stack
            instructions.push(Box::new(SetAddress::<0> {}));
            instructions.push(Box::new(PushRegister::<0> {}));

            // variable scope
            instructions.push(Box::new(NewScope {}));

            for i in 0..funcdata.params.len() {
                self.args[i].emit(program, instructions);

                // assign to named parameter
                if let (Some(name), typeinfo) = &funcdata.params[i] {
                    instructions.push(Box::new(crate::program::instruction::NewVariable {
                        name: name.clone(),
                        info: typeinfo.clone(),
                    }));
                    instructions.push(Box::new(GetVariable::<1> { name: name.clone() }));
                    instructions.push(Box::new(Assign::<1, 0> {}));
                }
            }

            // call function
            instructions.push(Box::new(GetLabelAddress::<1> {
                label: name.clone(),
            }));
            instructions.push(Box::new(Jump::<1> {}));

            // end of scope
            instructions.push(Box::new(PopScope {}));
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        panic!("FunctionCall::get_typeinfo not implemented");
    }
}

#[derive(Debug)]
pub struct PostMember {
    pub src: Box<dyn Expression>,
    pub member: String,
}
impl Expression for PostMember {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::expression::GetStructElement {
                name: self.member.clone(),
            },
        ));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        let src_type = self.src.get_typeinfo(program);
        if let TypeInfo::Struct(s) = src_type {
            let fields = if let Some(fields) = s.fields {
                fields
            } else {
                if let Some(name) = s.name {
                    let s = program
                        .search_typeinfo(&name)
                        .expect(format!("struct not found: {}", name).as_str());
                    if let TypeInfo::Struct(ref s) = s.as_ref() {
                        s.fields.clone().unwrap()
                    } else {
                        panic!("{} is not a struct", name);
                    }
                } else {
                    panic!("invalid struct type");
                }
            };
            fields
                .get(&self.member)
                .expect(format!("field not found: {}", self.member).as_str())
                .clone()
        } else if let TypeInfo::Union(s) = src_type {
            let fields = if let Some(fields) = s.fields {
                fields
            } else {
                if let Some(name) = s.name {
                    let s = program
                        .search_typeinfo(&name)
                        .expect(format!("union not found: {}", name).as_str());
                    if let TypeInfo::Union(ref s) = s.as_ref() {
                        s.fields.clone().unwrap()
                    } else {
                        panic!("{} is not a union", name);
                    }
                } else {
                    panic!("invalid union type");
                }
            };
            fields
                .get(&self.member)
                .expect(format!("field not found: {}", self.member).as_str())
                .clone()
        } else {
            panic!("Member on non-struct type");
        }
    }
}

#[derive(Debug)]
pub struct PostIncrement {
    pub src: Box<dyn Expression>,
}
impl Expression for PostIncrement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::MoveRegister::<0, 1> {},
        ));
        instructions.push(Box::new(DeepCopyRegister::<1, 0> {}));
        instructions.push(Box::new(
            crate::program::instruction::unary::Increment::<0> {},
        ));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        self.src.get_typeinfo(program)
    }
}

#[derive(Debug)]
pub struct PostDecrement {
    pub src: Box<dyn Expression>,
}
impl Expression for PostDecrement {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::MoveRegister::<0, 1> {},
        ));
        instructions.push(Box::new(DeepCopyRegister::<1, 0> {}));
        instructions.push(Box::new(
            crate::program::instruction::unary::Decrement::<0> {},
        ));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        self.src.get_typeinfo(program)
    }
}

#[derive(Debug)]
pub struct CastExpression {
    pub src: Box<dyn Expression>,
    pub typeinfo: TypeInfo,
}
impl Expression for CastExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(crate::program::instruction::unary::Cast::<0, 0> {
            info: self.typeinfo.clone(),
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        self.typeinfo.clone()
    }
}

#[derive(Debug)]
pub struct SizeofType {
    pub typeinfo: TypeInfo,
}
impl Expression for SizeofType {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::UInt64(self.typeinfo.sizeof() as u64),
            info: TypeInfo::UInt64,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.typeinfo.sizeof() as i64)
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
    }
}

#[derive(Debug)]
pub struct SizeofExpr {
    pub expr: Box<dyn Expression>,
}
impl Expression for SizeofExpr {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        let sizeof = self.expr.get_typeinfo(program).sizeof();
        instructions.push(Box::new(crate::program::instruction::Constant::<0> {
            value: crate::program::variable::VariableData::UInt64(sizeof as u64),
            info: TypeInfo::UInt64,
        }));
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        TypeInfo::UInt64
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    Dereference,
    AddressOf,
    Increment,
    Decrement,
}
#[derive(Debug)]
pub struct UnaryExpression {
    pub op: UnaryOperator,
    pub src: Box<dyn Expression>,
}
impl Expression for UnaryExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.src.emit(program, instructions);
        instructions.push(Box::new(
            crate::program::instruction::MoveRegister::<0, 1> {},
        ));
        instructions.push(Box::new(crate::program::instruction::DeepCopyRegister::<
            1,
            0,
        > {}));

        match self.op {
            UnaryOperator::Plus => {}
            UnaryOperator::Minus => {
                instructions.push(Box::new(crate::program::instruction::unary::Negate::<0> {}));
            }
            UnaryOperator::LogicalNot => {
                instructions.push(Box::new(crate::program::instruction::unary::LogicalNot::<
                    0,
                > {}));
            }
            UnaryOperator::BitwiseNot => {
                instructions.push(Box::new(crate::program::instruction::unary::BitwiseNot::<
                    0,
                > {}));
            }
            UnaryOperator::Dereference => {
                instructions.push(Box::new(crate::program::instruction::unary::Dereference::<
                    0,
                > {}));
            }
            UnaryOperator::AddressOf => {
                instructions.push(Box::new(
                    crate::program::instruction::unary::AddressOf::<0> {},
                ));
            }
            UnaryOperator::Increment => {
                instructions.push(Box::new(
                    crate::program::instruction::unary::Increment::<1> {},
                ));
            }
            UnaryOperator::Decrement => {
                instructions.push(Box::new(
                    crate::program::instruction::unary::Decrement::<1> {},
                ));
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        let srctype = self.src.get_typeinfo(program);
        match self.op {
            UnaryOperator::Plus | UnaryOperator::Minus => srctype,
            UnaryOperator::LogicalNot | UnaryOperator::BitwiseNot => srctype,
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(t) = self.src.get_typeinfo(program) {
                    *t
                } else {
                    panic!("Dereference on non-pointer type");
                }
            }
            UnaryOperator::AddressOf => TypeInfo::Pointer(Box::new(srctype)),
            UnaryOperator::Increment | UnaryOperator::Decrement => srctype,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    ShiftLeft,
    ShiftRight,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
}
#[derive(Debug)]
pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for BinaryExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.lhs.emit(program, instructions);
        // register0 = lhs

        instructions.push(Box::new(crate::program::instruction::PushRegister::<0> {}));
        // push register0 to stack

        self.rhs.emit(program, instructions);
        // register0 = rhs

        instructions.push(Box::new(crate::program::instruction::PopStackTo::<1> {}));
        // register1 = lhs

        match self.op {
            BinaryOperator::Add => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Add::<1, 0> {},
                ));
            }
            BinaryOperator::Sub => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Sub::<1, 0> {},
                ));
            }
            BinaryOperator::Mul => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Mul::<1, 0> {},
                ));
            }
            BinaryOperator::Div => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Div::<1, 0> {},
                ));
            }
            BinaryOperator::Mod => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Mod::<1, 0> {},
                ));
            }
            BinaryOperator::BitwiseAnd => {
                instructions.push(Box::new(crate::program::instruction::binary::BitwiseAnd::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::BitwiseOr => {
                instructions.push(Box::new(crate::program::instruction::binary::BitwiseOr::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::BitwiseXor => {
                instructions.push(Box::new(crate::program::instruction::binary::BitwiseXor::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::LogicalAnd => {
                instructions.push(Box::new(crate::program::instruction::binary::LogicalAnd::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::LogicalOr => {
                instructions.push(Box::new(crate::program::instruction::binary::LogicalOr::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::ShiftLeft => {
                instructions.push(Box::new(crate::program::instruction::binary::ShiftLeft::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::ShiftRight => {
                instructions.push(Box::new(crate::program::instruction::binary::ShiftRight::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::LessThan => {
                instructions.push(Box::new(crate::program::instruction::binary::LessThan::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::GreaterThan => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::GreaterThan::<1, 0> {},
                ));
            }
            BinaryOperator::LessThanOrEqual => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::LessThanOrEqual::<1, 0> {},
                ));
            }
            BinaryOperator::GreaterThanOrEqual => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::GreaterThanOrEqual::<1, 0> {},
                ));
            }
            BinaryOperator::Equal => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::Equal::<1, 0> {},
                ));
            }
            BinaryOperator::NotEqual => {
                instructions.push(Box::new(crate::program::instruction::binary::NotEqual::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::Assign => {
                instructions.push(Box::new(crate::program::instruction::binary::Assign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::AddAssign => {
                instructions.push(Box::new(crate::program::instruction::binary::AddAssign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::SubAssign => {
                instructions.push(Box::new(crate::program::instruction::binary::SubAssign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::MulAssign => {
                instructions.push(Box::new(crate::program::instruction::binary::MulAssign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::DivAssign => {
                instructions.push(Box::new(crate::program::instruction::binary::DivAssign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::ModAssign => {
                instructions.push(Box::new(crate::program::instruction::binary::ModAssign::<
                    1,
                    0,
                > {}));
            }
            BinaryOperator::BitwiseAndAssign => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::BitwiseAndAssign::<1, 0> {},
                ));
            }
            BinaryOperator::BitwiseOrAssign => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::BitwiseOrAssign::<1, 0> {},
                ));
            }
            BinaryOperator::BitwiseXorAssign => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::BitwiseXorAssign::<1, 0> {},
                ));
            }
            BinaryOperator::ShiftLeftAssign => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::ShiftLeftAssign::<1, 0> {},
                ));
            }
            BinaryOperator::ShiftRightAssign => {
                instructions.push(Box::new(
                    crate::program::instruction::binary::ShiftRightAssign::<1, 0> {},
                ));
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        match self.op {
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::BitwiseAnd
            | BinaryOperator::BitwiseOr
            | BinaryOperator::BitwiseXor
            | BinaryOperator::ShiftLeft
            | BinaryOperator::ShiftRight
            | BinaryOperator::LogicalAnd
            | BinaryOperator::LogicalOr => todo!("Binary operator {:?}", self.op),
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThanOrEqual
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual => TypeInfo::UInt32,
            BinaryOperator::Assign
            | BinaryOperator::AddAssign
            | BinaryOperator::SubAssign
            | BinaryOperator::MulAssign
            | BinaryOperator::DivAssign
            | BinaryOperator::ModAssign
            | BinaryOperator::BitwiseAndAssign
            | BinaryOperator::BitwiseOrAssign
            | BinaryOperator::BitwiseXorAssign
            | BinaryOperator::ShiftLeftAssign
            | BinaryOperator::ShiftRightAssign => self.lhs.get_typeinfo(program),
        }
    }
}

#[derive(Debug)]
pub struct PostArrow {
    pub src: Box<dyn Expression>,
    pub member: String,
}
impl Expression for PostArrow {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("PostArrow.eval not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        let src_type = self.src.get_typeinfo(program);
        if let TypeInfo::Pointer(t) = src_type {
            if let TypeInfo::Struct(s) = *t {
                let fields = if let Some(fields) = s.fields {
                    fields
                } else {
                    if let Some(name) = s.name {
                        let s = program
                            .search_typeinfo(&name)
                            .expect(format!("struct not found: {}", name).as_str());
                        if let TypeInfo::Struct(ref s) = s.as_ref() {
                            s.fields.clone().unwrap()
                        } else {
                            panic!("{} is not a struct", name);
                        }
                    } else {
                        panic!("invalid struct type");
                    }
                };
                fields
                    .get(&self.member)
                    .expect(format!("field not found: {}", self.member).as_str())
                    .clone()
            } else {
                panic!("Arrow on non-struct type");
            }
        } else {
            panic!("Arrow on non-pointer type");
        }
    }
}

#[derive(Debug)]
pub struct ConditionalExpression {
    pub cond: Box<dyn Expression>,
    pub then_expr: Box<dyn Expression>,
    pub else_expr: Box<dyn Expression>,
}
impl Expression for ConditionalExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.cond.emit(program, instructions);
        let else_label = program.get_unique_label();
        let end_label = program.get_unique_label();
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: else_label.clone(),
        }));
        instructions.push(Box::new(JumpZero::<1, 0> {}));
        self.then_expr.emit(program, instructions);
        instructions.push(Box::new(GetLabelAddress::<1> {
            label: end_label.clone(),
        }));
        instructions.push(Box::new(Jump::<1> {}));
        program.set_label(else_label, instructions);
        self.else_expr.emit(program, instructions);
        program.set_label(end_label.clone(), instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        self.then_expr.get_typeinfo(program)
    }
}

#[derive(Debug)]
pub struct CommaExpression {
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for CommaExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        self.lhs.emit(program, instructions);
        self.rhs.emit(program, instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        self.rhs.get_typeinfo(program)
    }
}

#[derive(Debug)]
pub struct InitializerListExpression {
    pub initializers: Vec<Box<dyn Expression>>,
}
impl Expression for InitializerListExpression {
    fn emit(&self, program: &mut Program, instructions: &mut Vec<Box<dyn Instruction>>) {
        panic!("InitializerListExpression.eval not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, program: &Program) -> TypeInfo {
        panic!("InitializerListExpression.get_typeinfo not implemented");
    }
}
