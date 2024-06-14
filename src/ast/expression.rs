use super::typename::TypeInfo;
use crate::virtualmachine::instruction::binary::*;
use crate::virtualmachine::instruction::generation::InstructionGenerator;
use crate::virtualmachine::instruction::generation::VariableOffset;
use crate::virtualmachine::instruction::operand::Operand;
use crate::virtualmachine::instruction::unary::*;
use crate::virtualmachine::instruction::*;
use crate::virtualmachine::program::STACK_POINTER_BASE_REGISTER;
use crate::virtualmachine::program::STACK_POINTER_REGISTER;
use crate::virtualmachine::program::TEXT_SIZE_REGISTER;
use crate::virtualmachine::variable::VariableData;

use core::panic;
use std::any::Any;

pub trait Expression: std::fmt::Debug + Any {
    /// push instruction that eval expression and store result in register0
    fn emit(&self, instructions: &mut InstructionGenerator);

    fn as_any(&self) -> &dyn Any;

    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool;

    fn get_constant_i64(&self) -> Result<i64, String> {
        Err(format!("get_constant_i64 not implemented for {:?}", self))
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo;
}

// expression that always returns true int32(1)
#[derive(Debug, Clone)]
pub struct VoidExpression {}
impl Expression for VoidExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Int32(1)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int32
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct PrimaryIdentifier {
    pub name: String,
}
// push pointer
impl Expression for PrimaryIdentifier {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let offset = instructions
            .search_variable(&self.name)
            .expect(format!("Variable {} not found", self.name).as_str())
            .1;
        match offset {
            VariableOffset::Global(absolute_offset) => {
                instructions.push(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt64(absolute_offset as u64)),
                    operand_to: Operand::Register(0),
                });
                instructions.push(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(TEXT_SIZE_REGISTER),
                });
            }
            VariableOffset::Local(relative_offset) => {
                instructions.push(MoveRegister {
                    operand_from: Operand::Register(STACK_POINTER_BASE_REGISTER),
                    operand_to: Operand::Register(0),
                });
                instructions.push(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Value(VariableData::Int64(relative_offset as i64)),
                });
            }
        }
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions) {
            TypeInfo::Array(_, _) => false,
            // TypeInfo::Struct(_) => false,
            _ => true,
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        instructions
            .search_variable(&self.name)
            .expect(format!("Variable {} not found", self.name).as_str())
            .0
            .clone()
    }
}
#[derive(Debug)]
pub struct PostMember {
    pub src: Box<dyn Expression>,
    pub member: String,
}
impl Expression for PostMember {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let member_offset = match self.src.get_typeinfo(instructions) {
            TypeInfo::Struct(mut sinfo) => {
                instructions.get_struct_definition(&mut sinfo);
                let mut member_offset: Option<usize> = None;
                for (_, name, offset) in sinfo.fields.as_ref().unwrap() {
                    if name == &self.member {
                        member_offset = Some(*offset);
                        break;
                    }
                }
                member_offset.expect(format!("Field {} not found", self.member).as_str())
            }
            _ => panic!("PostMember on non-struct type"),
        };
        self.src.emit(instructions);
        instructions.push(AddAssign {
            lhs: Operand::Register(0),
            rhs: Operand::Value(VariableData::UInt64(member_offset as u64)),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.src.get_typeinfo(instructions) {
            TypeInfo::Struct(mut sinfo) => {
                instructions.get_struct_definition(&mut sinfo);
                let mut member_type: Option<TypeInfo> = None;
                for (t, name, _) in sinfo.fields.as_ref().unwrap() {
                    if name == &self.member {
                        member_type = Some(t.clone());
                        break;
                    }
                }
                member_type.expect(format!("Field {} not found", self.member).as_str())
            }
            _ => panic!("PostMember on non-struct type"),
        }
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions) {
            TypeInfo::Array(_, _) => false,
            // TypeInfo::Struct(_) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstantInteger {
    pub value: i32,
}
impl Expression for ConstantInteger {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Int32(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int32
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}
#[derive(Debug, Clone)]
pub struct ConstantUnsignedInteger {
    pub value: u32,
}
impl Expression for ConstantUnsignedInteger {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt32(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt32
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantCharacter {
    pub value: i8,
}
impl Expression for ConstantCharacter {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Int8(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int8
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantLong {
    pub value: i64,
}
impl Expression for ConstantLong {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Int64(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int64
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}
#[derive(Debug, Clone)]
pub struct ConstantUnsignedLong {
    pub value: u64,
}
impl Expression for ConstantUnsignedLong {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantFloat {
    pub value: f32,
}
impl Expression for ConstantFloat {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Float32(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Float32
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ConstantDouble {
    pub value: f64,
}
impl Expression for ConstantDouble {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::Float64(self.value)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Float64
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}
impl Expression for StringLiteral {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        // store string to global text section
        let offset = instructions.text_section.len(); // <-- this will be absolute address

        let mut null_terminated = self.value.clone();
        null_terminated.push('\0');
        instructions
            .text_section
            .append(&mut null_terminated.as_bytes().to_vec());
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(offset as u64)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Pointer(Box::new(TypeInfo::Int8))
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PostBracket {
    pub src: Box<dyn Expression>,
    pub index: Box<dyn Expression>,
}
impl Expression for PostBracket {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        match self.src.get_typeinfo(instructions) {
            TypeInfo::Array(_, _) => {}
            TypeInfo::Pointer(_) => {}
            _ => panic!("Bracket is only available at array or pointer type"),
        }
        self.index.emit(instructions);
        if self.index.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }
        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        }
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });
        // instructions.push( AddAssign(
        //     Operand::Register(0),
        //     Operand::Register(1),
        // ));
        instructions.push(Bracket {
            operand_from: Operand::Register(0),
            operand_idx: Operand::Register(1),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.src.get_typeinfo(instructions) {
            TypeInfo::Array(t, _) => *t,
            TypeInfo::Pointer(t) => *t,
            _ => panic!("Bracket is only available at array or pointer type"),
        }
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct PostParen {
    pub src: Box<dyn Expression>,
    pub args: Vec<Box<dyn Expression>>,
}
// currently only for function call
impl Expression for PostParen {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let identifier = self
            .src
            .as_any()
            .downcast_ref::<PrimaryIdentifier>()
            .expect("FunctionCall must be called on `Identifier`");
        let name = identifier.name.clone();

        // check if it is a built-in function, print or print_str
        if &name == "print" {
            for arg in self.args.iter().rev() {
                arg.emit(instructions);
                if arg.is_return_reference(instructions) {
                    instructions.push(PushStack {
                        operand: Operand::Derefed(0, 0),
                    });
                } else {
                    instructions.push(PushStack {
                        operand: Operand::Register(0),
                    });
                }
            }
            instructions.push(PushStack {
                operand: Operand::Value(VariableData::UInt64(self.args.len() as u64)),
            });
            instructions.push(Print {});
        } else if &name == "print_str" {
            if self.args.len() != 1 {
                panic!(
                    "print_str expects 1 argument, but {} were provided",
                    self.args.len()
                );
            }
            self.args[0].emit(instructions);
            if self.args[0].is_return_reference(instructions) {
                instructions.push(MoveRegister {
                    operand_from: Operand::Derefed(0, 0),
                    operand_to: Operand::Register(0),
                });
            }
            instructions.push(PrintStr {
                str: Operand::Register(0),
            });
        } else {
            let funcdata = instructions
                .functions
                .get(&name)
                .expect(format!("Function not found : {}", &name).as_str())
                .clone();
            if funcdata.params.len() != self.args.len() {
                panic!(
                    "Function {} expects {} arguments, but {} were provided",
                    name,
                    funcdata.params.len(),
                    self.args.len()
                );
            }

            // push arguments to stack
            for param in self.args.iter().rev() {
                param.emit(instructions);
                if param.is_return_reference(instructions) {
                    instructions.push(PushStack {
                        operand: Operand::Derefed(0, 0),
                    });
                } else {
                    instructions.push(PushStack {
                        operand: Operand::Register(0),
                    });
                }
            }

            // call function
            instructions.push(Call {
                label: name.clone(),
            });

            // pop arguments from stack
            instructions.push(SubAssign {
                lhs: Operand::Register(STACK_POINTER_REGISTER),
                rhs: Operand::Value(VariableData::UInt64(funcdata.params.len() as u64)),
            });
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let identifier = self
            .src
            .as_any()
            .downcast_ref::<PrimaryIdentifier>()
            .expect("FunctionCall must be called on `Identifier`");
        let name = identifier.name.clone();
        let funcdata = &instructions
            .functions
            .get(&name)
            .expect(format!("Function not found : {}", &name).as_str());
        funcdata.return_type.clone()
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PostIncrement {
    pub src: Box<dyn Expression>,
}
impl Expression for PostIncrement {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        if self.src.is_return_reference(instructions) == false {
            panic!("PostIncrement on non-lhs");
        }
        self.src.emit(instructions);
        instructions.push(MoveRegister {
            operand_from: Operand::Register(0),
            operand_to: Operand::Register(1),
        });
        instructions.push(MoveRegister {
            operand_from: Operand::Derefed(1, 0),
            operand_to: Operand::Register(0),
        });
        if let TypeInfo::Pointer(t) = self.src.get_typeinfo(instructions) {
            let move_size = t.number_of_primitives();
            instructions.push(AddAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
            });
        } else {
            instructions.push(AddAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt8(1)),
            });
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.src.get_typeinfo(instructions)
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PostDecrement {
    pub src: Box<dyn Expression>,
}
impl Expression for PostDecrement {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        if self.src.is_return_reference(instructions) == false {
            panic!("PostDecrement on non-lhs");
        }
        self.src.emit(instructions);
        instructions.push(MoveRegister {
            operand_from: Operand::Register(0),
            operand_to: Operand::Register(1),
        });
        instructions.push(MoveRegister {
            operand_from: Operand::Derefed(1, 0),
            operand_to: Operand::Register(0),
        });

        if let TypeInfo::Pointer(t) = self.src.get_typeinfo(instructions) {
            let move_size = t.number_of_primitives();
            instructions.push(SubAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
            });
        } else {
            instructions.push(SubAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt8(1)),
            });
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.src.get_typeinfo(instructions)
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct CastExpression {
    pub src: Box<dyn Expression>,
    pub typeinfo: TypeInfo,
}
impl Expression for CastExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(Cast {
                info: self.typeinfo.clone(),
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        } else {
            instructions.push(Cast {
                info: self.typeinfo.clone(),
                operand_from: Operand::Register(0),
                operand_to: Operand::Register(0),
            });
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        self.typeinfo.clone()
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct SizeofType {
    pub typeinfo: TypeInfo,
}
impl Expression for SizeofType {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(self.typeinfo.sizeof() as u64)),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.typeinfo.sizeof() as i64)
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct SizeofExpr {
    pub expr: Box<dyn Expression>,
}
impl Expression for SizeofExpr {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(
                self.expr.get_typeinfo(instructions).sizeof() as u64,
            )),
            operand_to: Operand::Register(0),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.src.emit(instructions);

        match self.op {
            UnaryOperator::Plus => {
                match self.src.get_typeinfo(instructions) {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64
                    | TypeInfo::Float32
                    | TypeInfo::Float64
                    | TypeInfo::Pointer(_)
                    | TypeInfo::Array(_, _) => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        }
                    }
                    _ => panic!(
                        "Unary Plus not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Minus => match self.src.get_typeinfo(instructions) {
                TypeInfo::Int8
                | TypeInfo::UInt8
                | TypeInfo::Int16
                | TypeInfo::UInt16
                | TypeInfo::Int32
                | TypeInfo::UInt32
                | TypeInfo::Int64
                | TypeInfo::UInt64
                | TypeInfo::Float32
                | TypeInfo::Float64
                | TypeInfo::Pointer(_)
                | TypeInfo::Array(_, _) => {
                    if self.src.is_return_reference(instructions) {
                        instructions.push(MoveRegister {
                            operand_from: Operand::Derefed(0, 0),
                            operand_to: Operand::Register(0),
                        });
                    }
                    instructions.push(Negate {
                        operand: Operand::Register(0),
                    });
                }
                _ => panic!(
                    "Unary Minus not implemented for {:?}",
                    self.src.get_typeinfo(instructions)
                ),
            },
            UnaryOperator::LogicalNot => {
                match self.src.get_typeinfo(instructions) {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64
                    | TypeInfo::Pointer(_)
                    | TypeInfo::Array(_, _) => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        }
                        instructions.push(LogicalNot {
                            operand: Operand::Register(0),
                        });
                    }
                    _ => panic!(
                        "Unary LogicalNot not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::BitwiseNot => {
                match self.src.get_typeinfo(instructions) {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64
                    | TypeInfo::Pointer(_)
                    | TypeInfo::Array(_, _) => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        }
                        instructions.push(BitwiseNot {
                            operand: Operand::Register(0),
                        });
                    }
                    _ => panic!(
                        "Unary BitwiseNot not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(_) = self.src.get_typeinfo(instructions) {
                    if self.src.is_return_reference(instructions) {
                        instructions.push(MoveRegister {
                            operand_from: Operand::Derefed(0, 0),
                            operand_to: Operand::Register(0),
                        });
                    } else {
                        instructions.push(MoveRegister {
                            operand_from: Operand::Register(0),
                            operand_to: Operand::Register(0),
                        });
                    }
                } else {
                    panic!(
                        "Dereference on non-pointer type :{:?}",
                        self.src.get_typeinfo(instructions)
                    );
                }
            }
            UnaryOperator::AddressOf => {
                if self.src.is_return_reference(instructions) == false {
                    panic!("AddressOf on non-reference");
                }
            }
            UnaryOperator::Increment => {
                match self.src.get_typeinfo(instructions) {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64 => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(AddAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt8(1)),
                            });
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        } else {
                            panic!("Increment on non-reference");
                        }
                    }
                    TypeInfo::Pointer(t) => {
                        let move_size = t.number_of_primitives();
                        if self.src.is_return_reference(instructions) {
                            instructions.push(AddAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                            });
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        } else {
                            panic!("Increment on non-reference");
                        }
                    }
                    _ => panic!(
                        "Unary Increment not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Decrement => {
                match self.src.get_typeinfo(instructions) {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64 => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(SubAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt8(1)),
                            });
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        } else {
                            panic!("Decrement on non-reference");
                        }
                    }
                    TypeInfo::Pointer(t) => {
                        let move_size = t.number_of_primitives();
                        if self.src.is_return_reference(instructions) {
                            instructions.push(SubAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                            });
                            instructions.push(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            });
                        } else {
                            panic!("Decrement on non-reference");
                        }
                    }
                    _ => panic!(
                        "Unary Decrement not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let srctype = self.src.get_typeinfo(instructions);
        match self.op {
            UnaryOperator::Plus => srctype,
            UnaryOperator::Minus => match srctype {
                TypeInfo::Int8 => TypeInfo::Int8,
                TypeInfo::UInt8 => TypeInfo::Int8,
                TypeInfo::Int16 => TypeInfo::Int16,
                TypeInfo::UInt16 => TypeInfo::Int16,
                TypeInfo::Int32 => TypeInfo::Int32,
                TypeInfo::UInt32 => TypeInfo::Int32,
                TypeInfo::Int64 => TypeInfo::Int64,
                TypeInfo::UInt64 => TypeInfo::Int64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                TypeInfo::Pointer(t) => TypeInfo::Pointer(t),
                TypeInfo::Array(t, _) => TypeInfo::Pointer(t),
                _ => panic!("Unary Minus not implemented for {:?}", srctype),
            },
            UnaryOperator::LogicalNot => TypeInfo::UInt8,
            UnaryOperator::BitwiseNot => match srctype {
                TypeInfo::Int8 => TypeInfo::Int8,
                TypeInfo::UInt8 => TypeInfo::Int8,
                TypeInfo::Int16 => TypeInfo::Int16,
                TypeInfo::UInt16 => TypeInfo::Int16,
                TypeInfo::Int32 => TypeInfo::Int32,
                TypeInfo::UInt32 => TypeInfo::Int32,
                TypeInfo::Int64 => TypeInfo::Int64,
                TypeInfo::UInt64 => TypeInfo::Int64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                TypeInfo::Pointer(t) => TypeInfo::Pointer(t),
                TypeInfo::Array(t, _) => TypeInfo::Pointer(t),
                _ => panic!("BitwiseNot not implemented for {:?}", srctype),
            },
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(t) = self.src.get_typeinfo(instructions) {
                    *t
                } else {
                    panic!("Dereference on non-pointer type");
                }
            }
            UnaryOperator::AddressOf => {
                if self.src.is_return_reference(instructions) == false {
                    panic!("AddressOf on non-reference");
                }
                TypeInfo::Pointer(Box::new(srctype))
            }
            UnaryOperator::Increment | UnaryOperator::Decrement => srctype,
        }
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        match self.op {
            UnaryOperator::Dereference => true,
            _ => false,
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

/// for logical and, or
#[derive(Debug)]
pub struct LogicalBinaryExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for LogicalBinaryExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        }
        match self.op {
            BinaryOperator::LogicalAnd => {
                let zero_label = instructions.get_unique_label();
                let end_label = instructions.get_unique_label();
                instructions.push(JumpZero {
                    operand_cond: Operand::Register(0),
                    label: zero_label.clone(),
                });

                // lhs is true here
                // eval rhs
                self.rhs.emit(instructions);
                if self.rhs.is_return_reference(instructions) {
                    instructions.push(MoveRegister {
                        operand_from: Operand::Derefed(0, 0),
                        operand_to: Operand::Register(0),
                    });
                }
                instructions.push(JumpZero {
                    label: zero_label.clone(),
                    operand_cond: Operand::Register(0),
                });
                instructions.push(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(1)),
                    operand_to: Operand::Register(0),
                });
                instructions.push(Jump {
                    label: end_label.clone(),
                });

                instructions.set_label(&zero_label);
                instructions.push(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(0)),
                    operand_to: Operand::Register(0),
                });
                instructions.set_label(&end_label);
            }
            BinaryOperator::LogicalOr => {
                let one_label = instructions.get_unique_label();
                let end_label = instructions.get_unique_label();
                instructions.push(JumpNonZero {
                    operand_cond: Operand::Register(0),
                    label: one_label.clone(),
                });

                // lhs is false here
                // eval rhs
                self.rhs.emit(instructions);
                if self.rhs.is_return_reference(instructions) {
                    instructions.push(MoveRegister {
                        operand_from: Operand::Derefed(0, 0),
                        operand_to: Operand::Register(0),
                    });
                }
                instructions.push(JumpNonZero {
                    label: one_label.clone(),
                    operand_cond: Operand::Register(0),
                });
                instructions.push(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(0)),
                    operand_to: Operand::Register(0),
                });
                instructions.push(Jump {
                    label: end_label.clone(),
                });

                instructions.set_label(&one_label);
                instructions.push(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(1)),
                    operand_to: Operand::Register(0),
                });
                instructions.set_label(&end_label);
            }
            _ => {
                panic!(
                    "Invalid operator for LogicalBinaryExpression: {:?}",
                    self.op
                );
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        TypeInfo::UInt8
    }
}

#[derive(Debug)]
pub struct ComparisonExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for ComparisonExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-numeric type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-numeric type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        // eval lhs and push to stack
        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }

        // eval rhs
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        }

        // pop lhs from stack
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });

        let lhs_register = Operand::Register(1);
        let rhs_register = Operand::Register(0);

        match self.op {
            BinaryOperator::LessThan => {
                // lhs < rhs
                instructions.push(LessThan {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                });
            }
            BinaryOperator::GreaterThan => {
                // lhs > rhs
                instructions.push(LessThan {
                    lhs: rhs_register,
                    rhs: lhs_register,
                    to: Operand::Register(0),
                });
            }
            BinaryOperator::LessThanOrEqual => {
                // lhs <= rhs
                // !( lhs > rhs )
                instructions.push(LessThan {
                    lhs: rhs_register,
                    rhs: lhs_register,
                    to: Operand::Register(0),
                });
                instructions.push(LogicalNot {
                    operand: Operand::Register(0),
                });
            }
            BinaryOperator::GreaterThanOrEqual => {
                // lhs >= rhs
                // !( lhs < rhs )
                instructions.push(LessThan {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                });
                instructions.push(LogicalNot {
                    operand: Operand::Register(0),
                });
            }
            BinaryOperator::Equal => {
                // lhs == rhs
                instructions.push(Equal {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                });
            }
            BinaryOperator::NotEqual => {
                // !(lhs == rhs )
                instructions.push(Equal {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                });
                instructions.push(LogicalNot {
                    operand: Operand::Register(0),
                });
            }
            _ => panic!("Invalid operator for ComparisonExpression: {:?}", self.op),
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-int type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions) {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-int type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        TypeInfo::UInt8
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}
// lhs = rhs
#[derive(Debug)]
pub struct AssignExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for AssignExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        // check if it is non-numeric <-> non-numeric assignment
        if let TypeInfo::Struct(mut lhs_type) = self.lhs.get_typeinfo(instructions) {
            instructions.get_struct_definition(&mut lhs_type);
            if let TypeInfo::Struct(mut rhs_type) = self.rhs.get_typeinfo(instructions) {
                instructions.get_struct_definition(&mut rhs_type);
                if &lhs_type != &rhs_type {
                    panic!(
                        "Struct assignment with different types: {:?} = {:?}",
                        lhs_type, rhs_type
                    );
                }
                // struct = struct assignment
                self.rhs.emit(instructions);
                instructions.push(PushStack {
                    operand: Operand::Register(0),
                });
                self.lhs.emit(instructions);
                instructions.push(PopStack {
                    operand: Operand::Register(1),
                });

                let count = lhs_type.number_of_primitives();
                instructions.push(AssignStruct {
                    count,
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            } else {
                // struct = other assignment; panic
                panic!(
                    "Struct assignment with non-struct type: {:?} = {:?}",
                    lhs_type,
                    self.rhs.get_typeinfo(instructions)
                );
            }
        } else {
            // normal assignment

            if self.lhs.is_return_reference(instructions) == false {
                panic!("{:?} on non-reference", self.op);
            }
            self.rhs.emit(instructions);
            if self.rhs.is_return_reference(instructions) {
                instructions.push(PushStack {
                    operand: Operand::Derefed(0, 0),
                });
            } else {
                instructions.push(PushStack {
                    operand: Operand::Register(0),
                });
            }
            self.lhs.emit(instructions);
            instructions.push(PopStack {
                operand: Operand::Register(1),
            });
            instructions.push(Assign {
                lhs_type: self.lhs.get_typeinfo(instructions),
                lhs: Operand::Derefed(0, 0),
                rhs: Operand::Register(1),
            });
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.lhs.is_return_reference(instructions)
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}

// += -= *= /= %=
// <<= >>= &= |= ^=
#[derive(Debug)]
pub struct AssignOpExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for AssignOpExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        if self.lhs.is_return_reference(instructions) == false {
            panic!("{:?} on non-reference", self.op);
        }
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }
        self.lhs.emit(instructions);
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });
        match self.op {
            BinaryOperator::AddAssign => {
                if let TypeInfo::Pointer(t) = self.lhs.get_typeinfo(instructions) {
                    let move_size = t.number_of_primitives();
                    instructions.push(MoveRegister {
                        operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                        operand_to: Operand::Register(2),
                    });
                    instructions.push(MulAssign {
                        lhs: Operand::Register(2),
                        rhs: Operand::Register(1),
                    });
                    instructions.push(AddAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(2),
                    });
                } else {
                    instructions.push(AddAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(1),
                    });
                }
            }
            BinaryOperator::SubAssign => {
                if let TypeInfo::Pointer(t) = self.lhs.get_typeinfo(instructions) {
                    let move_size = t.number_of_primitives();
                    instructions.push(MoveRegister {
                        operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                        operand_to: Operand::Register(2),
                    });
                    instructions.push(MulAssign {
                        lhs: Operand::Register(2),
                        rhs: Operand::Register(1),
                    });
                    instructions.push(SubAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(2),
                    });
                } else {
                    instructions.push(SubAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(1),
                    });
                }
            }
            BinaryOperator::MulAssign => {
                instructions.push(MulAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::DivAssign => {
                instructions.push(DivAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::ModAssign => {
                instructions.push(ModAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::ShiftLeftAssign => {
                instructions.push(ShiftLeftAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::ShiftRightAssign => {
                instructions.push(ShiftRightAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::BitwiseAndAssign => {
                instructions.push(BitwiseAndAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::BitwiseOrAssign => {
                instructions.push(BitwiseOrAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::BitwiseXorAssign => {
                instructions.push(BitwiseXorAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                });
            }
            _ => panic!("Invalid operator for AssignExpression: {:?}", self.op),
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.lhs.is_return_reference(instructions)
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}

#[derive(Debug)]
pub struct AdditiveExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for AdditiveExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        // for type-check panic
        self.get_typeinfo(instructions);

        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            });
        }
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });

        // pointer arithmetic
        if let TypeInfo::Pointer(t) = self.lhs.get_typeinfo(instructions) {
            let move_size = t.number_of_primitives();
            instructions.push(MoveRegister {
                operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                operand_to: Operand::Register(2),
            });
            instructions.push(MulAssign {
                lhs: Operand::Register(2),
                rhs: Operand::Register(1),
            });

            match self.op {
                BinaryOperator::Add => {
                    instructions.push(AddAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    });
                }
                BinaryOperator::Sub => {
                    instructions.push(SubAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    });
                }
                _ => panic!("Invalid operator for AdditiveExpression: {:?}", self.op),
            }
        } else {
            match self.op {
                BinaryOperator::Add => {
                    instructions.push(AddAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(1),
                    });
                }
                BinaryOperator::Sub => {
                    instructions.push(SubAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(1),
                    });
                }
                _ => panic!("Invalid operator for AdditiveExpression: {:?}", self.op),
            }
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        false
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        // choose bigger-precision type
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8 | TypeInfo::Int8 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float32 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float32,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float64 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float64,
                TypeInfo::Float32 => TypeInfo::Float64,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Pointer(t) => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Pointer(t),
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Pointer(t),
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Pointer(t),
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Pointer(t),
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}
#[derive(Debug)]
pub struct MultiplicativeExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for MultiplicativeExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            });
        }
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });

        match self.op {
            BinaryOperator::Mul => {
                instructions.push(MulAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::Div => {
                instructions.push(DivAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::Mod => {
                instructions.push(ModAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            _ => panic!(
                "Invalid operator for MultiplicativeExpression: {:?}",
                self.op
            ),
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        false
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        // choose bigger-precision type
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8 | TypeInfo::Int8 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float32 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float32,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float64 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float64,
                TypeInfo::Float32 => TypeInfo::Float64,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}
#[derive(Debug)]
pub struct ShiftExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for ShiftExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        }

        instructions.push(PopStack {
            operand: Operand::Register(1),
        });

        match self.op {
            BinaryOperator::ShiftLeft => {
                instructions.push(ShiftLeftAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::ShiftRight => {
                instructions.push(ShiftRightAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            _ => panic!("Invalid operator for ShiftExpression: {:?}", self.op),
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}
#[derive(Debug)]
pub struct BitwiseExpression {
    pub op: BinaryOperator,
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for BitwiseExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(PushStack {
                operand: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(PushStack {
                operand: Operand::Register(0),
            });
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            });
        }
        instructions.push(PopStack {
            operand: Operand::Register(1),
        });

        match self.op {
            BinaryOperator::BitwiseAnd => {
                instructions.push(BitwiseAndAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::BitwiseOr => {
                instructions.push(BitwiseOrAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            BinaryOperator::BitwiseXor => {
                instructions.push(BitwiseXorAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                });
            }
            _ => panic!("Invalid operator for BitwiseExpression: {:?}", self.op),
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        // choose bigger-precision type
        match self.lhs.get_typeinfo(instructions) {
            TypeInfo::UInt8 | TypeInfo::Int8 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match self.rhs.get_typeinfo(instructions) {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}

#[derive(Debug)]
pub struct PostArrow {
    pub src: Box<dyn Expression>,
    pub member: String,
}
impl Expression for PostArrow {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let member_offset = match self.src.get_typeinfo(instructions) {
            TypeInfo::Pointer(t) => match t.as_ref() {
                TypeInfo::Struct(sinfo) => {
                    let mut sinfo = sinfo.clone();
                    instructions.get_struct_definition(&mut sinfo);
                    let mut member_offset: Option<usize> = None;
                    for (_, name, offset) in sinfo.fields.as_ref().unwrap() {
                        if name == &self.member {
                            member_offset = Some(*offset);
                            break;
                        }
                    }
                    member_offset.expect(format!("member not found: {:?}", self.member).as_str())
                }
                _ => panic!("-> operator on non-struct type: {:?}", t),
            },
            _ => panic!(
                "-> operator on non-pointer type: {:?}",
                self.src.get_typeinfo(instructions)
            ),
        };

        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            });
        }
        instructions.push(AddAssign {
            lhs: Operand::Register(0),
            rhs: Operand::Value(VariableData::UInt64(member_offset as u64)),
        });
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.src.get_typeinfo(instructions) {
            TypeInfo::Pointer(t) => match t.as_ref() {
                TypeInfo::Struct(sinfo) => {
                    let mut sinfo = sinfo.clone();
                    instructions.get_struct_definition(&mut sinfo);
                    let mut member_type: Option<TypeInfo> = None;
                    for (type_, name, _) in sinfo.fields.as_ref().unwrap() {
                        if name == &self.member {
                            member_type = Some(type_.clone());
                            break;
                        }
                    }
                    member_type.expect(format!("member not found: {:?}", self.member).as_str())
                }
                _ => panic!("-> operator on non-struct type: {:?}", t),
            },
            _ => panic!(
                "-> operator on non-pointer type: {:?}",
                self.src.get_typeinfo(instructions)
            ),
        }
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions) {
            TypeInfo::Array(_, _) => false,
            // TypeInfo::Struct(_) => false,
            _ => true,
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
    fn emit(&self, instructions: &mut InstructionGenerator) {
        let else_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        self.cond.emit(instructions);
        if self.cond.is_return_reference(instructions) {
            instructions.push(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            });
        } else {
            instructions.push(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Register(0),
            });
        }

        self.then_expr.emit(instructions);
        if self.then_expr.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            });
            instructions.push(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            });
        }
        instructions.push(Jump {
            label: end_label.clone(),
        });
        instructions.set_label(&else_label);
        self.else_expr.emit(instructions);
        if self.else_expr.is_return_reference(instructions) {
            instructions.push(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            });
            instructions.push(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            });
        }
        instructions.set_label(&end_label);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.then_expr.get_typeinfo(instructions)
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct CommaExpression {
    pub lhs: Box<dyn Expression>,
    pub rhs: Box<dyn Expression>,
}
impl Expression for CommaExpression {
    fn emit(&self, instructions: &mut InstructionGenerator) {
        self.lhs.emit(instructions);
        self.rhs.emit(instructions);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.rhs.is_return_reference(instructions)
    }
    fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.rhs.get_typeinfo(instructions)
    }
}

#[derive(Debug)]
pub struct InitializerListExpression {
    pub initializers: Vec<Box<dyn Expression>>,
}
impl Expression for InitializerListExpression {
    fn emit(&self, _instructions: &mut InstructionGenerator) {
        panic!("InitializerListExpression.eval not implemented");
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        panic!("InitializerListExpression.get_typeinfo not implemented");
    }
    fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        panic!("InitializerListExpression.is_return_reference not implemented");
    }
}
