use super::variable::VariableData;

use crate::virtualmachine::instruction::generation::InstructionGenerator;
use std::collections::HashMap;

/// Virtual Program
/// have stack, registers, labels
pub struct VirtualProgram {
    pub label_map: HashMap<String, usize>,
    pub stack: Vec<VariableData>,
    /// size of text section
    /// because we use front of stack for text section, we need offset for stack pointer
    pub text_size: usize,

    /// each register is for single primitive type
    /// rax, rbx, rcx, rdx, rtx, rbp, rsp
    /// last tree registers are for
    /// text section size, stack pointer and base pointer
    pub registers: [VariableData; 7],

    pub current_instruction: usize,
}

pub const STACK_SIZE: usize = 10240; // stack size
pub const STACK_POINTER_REGISTER: usize = 6; // index of register for use as stack pointer (rsp)
pub const STACK_POINTER_BASE_REGISTER: usize = 5; // index of register for use as stack base pointer (rbp)
pub const TEXT_SIZE_REGISTER: usize = 4; // index of register for use as text section size (rtx)
impl VirtualProgram {
    pub fn new() -> VirtualProgram {
        let mut ret = VirtualProgram {
            label_map: HashMap::new(),
            stack: Vec::new(),
            text_size: 0,

            registers: [
                VariableData::UInt64(0),
                VariableData::UInt64(0),
                VariableData::UInt64(0),
                VariableData::UInt64(0),
                VariableData::UInt64(0), // rtx
                VariableData::UInt64(0), // rpb
                VariableData::UInt64(0), // rsp
            ],

            current_instruction: 0,
        };
        // pre allocate stack
        ret.stack.resize(STACK_SIZE, VariableData::UInt64(0));
        ret
    }

    pub fn execute(&mut self, instructions: &InstructionGenerator) {
        self.label_map = instructions.labels.clone();
        self.current_instruction = instructions.start_address;

        // set base pointer and stack pointer
        self.registers[TEXT_SIZE_REGISTER] =
            VariableData::UInt64(instructions.text_section.len() as u64);
        self.registers[STACK_POINTER_REGISTER] =
            VariableData::UInt64(instructions.text_section.len() as u64);
        self.registers[STACK_POINTER_BASE_REGISTER] =
            VariableData::UInt64(instructions.text_section.len() as u64);

        // copy text data to front of the stack
        for (i, ch) in instructions.text_section.iter().enumerate() {
            self.stack[i] = VariableData::Int8(*ch as i8);
        }

        while self.current_instruction < instructions.instructions.len() {
            instructions.instructions[self.current_instruction].execute(self);

            self.current_instruction += 1;
        }
    }
}
