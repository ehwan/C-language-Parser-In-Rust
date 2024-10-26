use std::collections::HashMap;

use generation::InstructionGenerator;

use crate::virtualmachine::instruction::operand::Operand;
use crate::virtualmachine::{
    instruction::*, program::STACK_POINTER_REGISTER, variable::VariableData,
};

use super::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Struct(StructInfo),
    Union(UnionInfo),
    Enum(EnumInfo),
    Pointer(Box<TypeInfo>),
    Array(Box<TypeInfo>, Option<usize>),
    Function(Box<TypeInfo>, Vec<TypeInfo>),
    Const(Box<TypeInfo>),

    // for typedef
    // temporary store the name of the type; will be replaced by the actual type in emitting
    Identifier(String),
}
impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo::Int32
    }
}
impl TypeInfo {
    pub fn sizeof(&self) -> usize {
        match self {
            TypeInfo::Void => panic!("sizeof(void) is invalid"),
            TypeInfo::Int8 => 1,
            TypeInfo::Int16 => 2,
            TypeInfo::Int32 => 4,
            TypeInfo::Int64 => 8,
            TypeInfo::UInt8 => 1,
            TypeInfo::UInt16 => 2,
            TypeInfo::UInt32 => 4,
            TypeInfo::UInt64 => 8,
            TypeInfo::Float32 => 4,
            TypeInfo::Float64 => 8,
            TypeInfo::Struct(info) => info.sizeof(),
            TypeInfo::Union(info) => info.sizeof(),
            TypeInfo::Enum(_) => 8,
            TypeInfo::Pointer(_) => 8,
            TypeInfo::Array(info, size) => {
                info.sizeof() * size.expect("sizeof: Array size is not defined")
            }
            TypeInfo::Function(_, _) => panic!("sizeof(function) is invalid"),
            TypeInfo::Identifier(_) => panic!("sizeof(identifier) is invalid"),
            TypeInfo::Const(t) => t.sizeof(),
        }
    }
    pub fn number_of_primitives(&self) -> usize {
        match self {
            TypeInfo::Void => 0,
            TypeInfo::Int8 | TypeInfo::Int16 | TypeInfo::Int32 | TypeInfo::Int64 => 1,
            TypeInfo::UInt8 | TypeInfo::UInt16 | TypeInfo::UInt32 | TypeInfo::UInt64 => 1,
            TypeInfo::Float32 | TypeInfo::Float64 => 1,
            TypeInfo::Struct(structinfo) => structinfo.number_of_primitives(),
            TypeInfo::Pointer(_) => 1,
            TypeInfo::Array(info, Some(size)) => info.number_of_primitives() * size,
            TypeInfo::Const(t) => t.number_of_primitives(),
            _ => panic!("number_of_primitives: unsupported type: {:?}", self),
        }
    }
    // push default value to the stack
    pub fn emit_default(&self, instructions: &mut InstructionGenerator) {
        match self {
            TypeInfo::UInt8 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt8(0)),
            })),
            TypeInfo::Int8 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Int8(0)),
            })),

            TypeInfo::UInt16 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt16(0)),
            })),
            TypeInfo::Int16 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Int16(0)),
            })),

            TypeInfo::UInt32 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt32(0)),
            })),
            TypeInfo::Int32 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Int32(0)),
            })),

            TypeInfo::UInt64 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt64(0)),
            })),
            TypeInfo::Int64 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Int64(0)),
            })),

            TypeInfo::Float32 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Float32(0.0)),
            })),

            TypeInfo::Float64 => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::Float64(0.0)),
            })),

            TypeInfo::Pointer(_) => instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt64(0)),
            })),

            TypeInfo::Array(t, Some(n)) => {
                for _ in 0..*n {
                    t.emit_default(instructions);
                }
            }

            TypeInfo::Struct(info) => info.emit_default(instructions),

            TypeInfo::Const(t) => t.emit_default(instructions),

            _ => panic!("emit_default: unsupported type: {:?}", self),
        }
    }

    // initialize this type with the given initializer
    // and push to stack
    pub fn emit_init(&self, instructions: &mut InstructionGenerator, initializer: &Expression) {
        match self {
            TypeInfo::Array(t, Some(n)) => {
                let initializer = match initializer {
                    Expression::InitializerList(initializer) => initializer,
                    _ => {
                        unreachable!(
                            "TypeInfo::emit_init: initializer is not InitializerListExpression"
                        )
                    }
                };

                if initializer.initializers.len() > *n {
                    panic!(
                        "Array initialization overflow: expected {}, got {}",
                        n,
                        initializer.initializers.len()
                    );
                }

                for i in 0..initializer.initializers.len() {
                    t.emit_init(instructions, &initializer.initializers[i]);
                }

                let remaining = *n - initializer.initializers.len();
                for _ in 0..remaining {
                    t.emit_default(instructions);
                }
            }

            TypeInfo::Struct(info) => info.emit_init(instructions, &initializer),

            TypeInfo::Const(t) => t.emit_init(instructions, &initializer),

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
            | TypeInfo::Pointer(_) => {
                // check if it is initializer list
                match initializer {
                    Expression::InitializerList(initializer) => {
                        if initializer.initializers.len() != 1 {
                            panic!(
                            "TypeInfo::emit_init: initializer length mismatch: expected 1, got {}",
                            initializer.initializers.len()
                        );
                        }
                        self.emit_init(instructions, &initializer.initializers[0]);
                    }
                    _ => {
                        // register0 = initial value
                        initializer.emit(instructions);

                        // register1 = (type-casting) register0
                        if initializer.is_return_reference(instructions) {
                            instructions.push(Instruction::Assign(Assign {
                                lhs_type: self.clone(),
                                lhs: Operand::Register(1),
                                rhs: Operand::Derefed(0, 0),
                            }));
                        } else {
                            instructions.push(Instruction::Assign(Assign {
                                lhs_type: self.clone(),
                                lhs: Operand::Register(1),
                                rhs: Operand::Register(0),
                            }));
                        }
                        // push register1 to stack
                        instructions.push(Instruction::PushStack(PushStack {
                            operand: Operand::Register(1),
                        }));
                    }
                }
            }
            _ => panic!("emit_init: unsupported type: {:?}", self),
        }
    }

    // remove outermost const recursively
    pub fn remove_const(&self) -> TypeInfo {
        match self {
            TypeInfo::Const(t) => t.remove_const(),
            _ => self.clone(),
        }
    }

    pub fn add_const(&self) -> TypeInfo {
        match self {
            TypeInfo::Const(_) => self.clone(),
            _ => TypeInfo::Const(Box::new(self.clone())),
        }
    }
    pub fn is_const(&self) -> bool {
        match self {
            TypeInfo::Const(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInfo {
    pub name: Option<String>,
    pub fields: Option<Vec<(TypeInfo, String, usize)>>,
}
impl StructInfo {
    pub fn sizeof(&self) -> usize {
        let mut size: usize = 0;
        for (t, _, _) in self.fields.as_ref().unwrap() {
            size += t.sizeof();
        }
        size
    }
    pub fn number_of_primitives(&self) -> usize {
        let mut count: usize = 0;
        for (t, _, _) in self.fields.as_ref().unwrap() {
            count += t.number_of_primitives();
        }
        count
    }
    pub fn emit_default(&self, instructions: &mut InstructionGenerator) {
        for (t, _, _) in self.fields.as_ref().unwrap() {
            t.emit_default(instructions);
        }
    }
    pub fn emit_init(&self, instructions: &mut InstructionGenerator, initializer: &Expression) {
        match initializer {
            Expression::InitializerList(initializer) => {
                // struct init with initializer

                if initializer.initializers.len() != self.fields.as_ref().unwrap().len() {
                    panic!(
                        "StructInfo::emit_init: initializer length mismatch: expected {}, got {}",
                        self.fields.as_ref().unwrap().len(),
                        initializer.initializers.len()
                    );
                }

                for i in 0..initializer.initializers.len() {
                    let (t, _, _) = &self.fields.as_ref().unwrap()[i];
                    instructions
                        .get_true_typeinfo(t)
                        .emit_init(instructions, &initializer.initializers[i]);
                }
            }
            _ => {
                // struct init with other struct
                if let TypeInfo::Struct(rhs_type) = initializer.get_typeinfo(instructions) {
                    if self != &rhs_type {
                        panic!("struct init: type mismatch1");
                    }

                    let primitive_count = rhs_type.number_of_primitives();

                    initializer.emit(instructions);
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Register(0),
                        operand_to: Operand::Register(1),
                    }));

                    // start address of new struct
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Register(STACK_POINTER_REGISTER),
                        operand_to: Operand::Register(0),
                    }));
                    // alloc stack
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Register(STACK_POINTER_REGISTER),
                        rhs: Operand::Value(VariableData::UInt64(primitive_count as u64)),
                    }));

                    instructions.push(Instruction::AssignStruct(AssignStruct {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(1),
                        count: primitive_count,
                    }));
                } else {
                    panic!("struct init: type mismatch2");
                }
            }
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct UnionInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, TypeInfo>>,
}
impl UnionInfo {
    pub fn sizeof(&self) -> usize {
        let mut size: usize = 0;
        for (_, field) in self.fields.as_ref().unwrap() {
            size = size.max(field.sizeof());
        }
        size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, i64>>,
}
