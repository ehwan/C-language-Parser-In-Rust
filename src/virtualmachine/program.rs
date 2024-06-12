use super::variable::VariableData;

use crate::virtualmachine::instruction::generation::InstructionGenerator;
use std::collections::HashMap;

/// Virtual Program
/// have stack, registers, labels
pub struct VirtualProgram {
    pub label_map: HashMap<String, usize>,
    pub stack: Vec<VariableData>,

    /// each register is for single primitive type
    /// last two registers are for stack pointer and base pointer
    pub registers: [VariableData; 6],

    pub current_instruction: usize,
}

pub const STACK_SIZE: usize = 10240; // stack size
pub const STACK_POINTER_REGISTER: usize = 5; // index of register for use as stack pointer (rsp)
pub const STACK_POINTER_BASE_REGISTER: usize = 4; // index of register for use as stack base pointer (rbp)
impl VirtualProgram {
    pub fn new() -> VirtualProgram {
        let mut ret = VirtualProgram {
            label_map: HashMap::new(),
            stack: Vec::new(),

            registers: [
                VariableData::UInt64(0),
                VariableData::UInt64(0),
                VariableData::UInt64(0),
                VariableData::UInt64(0),
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

        while self.current_instruction < instructions.instructions.len() {
            instructions.instructions[self.current_instruction].execute(self);

            self.current_instruction += 1;
        }
    }
}
