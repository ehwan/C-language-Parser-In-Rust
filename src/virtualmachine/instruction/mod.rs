pub mod binary;
pub mod generation;
pub mod operand;
pub mod unary;

use super::program::{STACK_POINTER_BASE_REGISTER, STACK_POINTER_REGISTER, STACK_SIZE};
use super::variable::VariableData;
use crate::virtualmachine::program::VirtualProgram;
use operand::*;

pub trait Instruction: std::fmt::Debug {
    fn execute(&self, program: &mut VirtualProgram);
}

/// Do Nothing; for debugging
#[derive(Debug)]
pub struct DefineLabel {
    pub label: String,
}
impl Instruction for DefineLabel {
    fn execute(&self, _program: &mut VirtualProgram) {
        // label is defined at emitting session
    }
}

/// copy register N to register M
#[derive(Debug)]
pub struct MoveRegister {
    pub operand_from: Operand,
    pub operand_to: Operand,
}
impl Instruction for MoveRegister {
    fn execute(&self, program: &mut VirtualProgram) {
        let rhs = get_operand_value(program, &self.operand_from).clone();
        let lhs = get_operand_value_mut(program, &self.operand_to);
        *lhs = rhs;
    }
}
#[derive(Debug)]
pub struct PushStack {
    pub operand: Operand,
}
impl Instruction for PushStack {
    fn execute(&self, program: &mut VirtualProgram) {
        // check if operand is stack pointer
        if let Operand::Register(reg_id) = &self.operand {
            if reg_id == &STACK_POINTER_REGISTER {
                panic!("Cannot push RSP");
            }
        }
        let value = get_operand_value(program, &self.operand).clone();
        let stack_index = program.registers[STACK_POINTER_REGISTER].to_u64() as usize;

        // overflow check
        if stack_index == STACK_SIZE {
            panic!("PushStack: stack overflow");
        }

        program.stack[stack_index] = value;

        // inc stack pointer
        if let VariableData::UInt64(ref mut value) = program.registers[STACK_POINTER_REGISTER] {
            *value += 1;
        } else {
            panic!("PushStack: stack pointer is not UInt64");
        }
    }
}
#[derive(Debug)]
pub struct PopStack {
    pub operand: Operand,
}
impl Instruction for PopStack {
    fn execute(&self, program: &mut VirtualProgram) {
        // check if operand is stack pointer
        if let Operand::Register(reg_id) = &self.operand {
            if reg_id == &STACK_POINTER_REGISTER {
                panic!("Cannot Pop to RSP");
            }
        }
        let stack_index = if let VariableData::UInt64(ref mut value) =
            program.registers[STACK_POINTER_REGISTER]
        {
            *value -= 1;
            *value
        } else {
            panic!("PushStack: stack pointer is not UInt64");
        };
        *get_operand_value_mut(program, &self.operand) =
            program.stack[stack_index as usize].clone();
    }
}

/// jump to address in Register N
#[derive(Debug)]
pub struct Jump {
    pub label: String,
}
impl Instruction for Jump {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let address = program
            .label_map
            .get(&self.label)
            .expect(format!("Jump: label not found: {}", self.label).as_str())
            .clone();
        program.current_instruction = address;
    }
}

/// Jump to address if register N is zero
#[derive(Debug)]
pub struct JumpZero {
    pub label: String,
    pub operand_cond: Operand,
}
impl Instruction for JumpZero {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let address = program
            .label_map
            .get(&self.label)
            .expect(format!("JumpZero: label not found: {}", self.label).as_str())
            .clone();
        let cond = get_operand_value(program, &self.operand_cond).to_u64();
        if cond == 0 {
            program.current_instruction = address;
        }
    }
}

/// Jump to address if register N is not zero
#[derive(Debug)]
pub struct JumpNonZero {
    pub label: String,
    pub operand_cond: Operand,
}
impl Instruction for JumpNonZero {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let address = program
            .label_map
            .get(&self.label)
            .expect(format!("JumpNonZero: label not found: {}", self.label).as_str())
            .clone();
        let cond = get_operand_value(program, &self.operand_cond).to_u64();
        if cond != 0 {
            program.current_instruction = address;
        }
    }
}

/// print vars in stack
#[derive(Debug)]
pub struct Print {}
impl Instruction for Print {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        PopStack {
            operand: Operand::Register(1),
        }
        .execute(program);

        let var_count = get_operand_value(program, &Operand::Register(1)).to_u64();
        print!("Print: ");
        for _ in 0..var_count {
            PopStack {
                operand: Operand::Register(1),
            }
            .execute(program);
            let var = get_operand_value(program, &Operand::Register(1));
            print!("{:?}, ", var);
        }
        println!("");
    }
}

/// instruction that panic
#[derive(Debug)]
pub struct Panic {
    pub message: String,
}
impl Instruction for Panic {
    fn execute(&self, _program: &mut crate::virtualmachine::program::VirtualProgram) {
        panic!("Panic: {}", self.message);
    }
}

/// push current address and scope count to stack and jump
#[derive(Debug)]
pub struct Call {
    pub label: String,
}
impl Instruction for Call {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let move_to_address = *program
            .label_map
            .get(&self.label)
            .expect(format!("Call: label not found: {}", self.label).as_str());
        // let move_to_address = get_operand_value(program, &self.address).to_u64() as usize;
        let current_address = program.current_instruction;

        // push current address to stack
        PushStack {
            operand: Operand::Value(VariableData::UInt64(current_address as u64)),
        }
        .execute(program);

        // jump to address
        program.current_instruction = move_to_address;
    }
}

#[derive(Debug)]
pub struct Return {}
impl Instruction for Return {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        // register0 is beging used by returned value

        // current base pointer is old stack pointer
        let cur_base_pointer = program.registers[STACK_POINTER_BASE_REGISTER].to_u64() as usize;

        let old_base_pointer = program.stack[cur_base_pointer - 1].to_u64() as usize;
        let return_address = program.stack[cur_base_pointer - 2].to_u64() as usize;

        program.registers[STACK_POINTER_REGISTER] =
            VariableData::UInt64(cur_base_pointer as u64 - 2);
        program.registers[STACK_POINTER_BASE_REGISTER] =
            VariableData::UInt64(old_base_pointer as u64);
        program.current_instruction = return_address;
    }
}

#[derive(Debug)]
pub struct PrintStr {
    pub str: Operand, // null terminated string
}
impl Instruction for PrintStr {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualProgram) {
        let mut address = get_operand_value(program, &self.str).to_u64() as usize;

        let mut chars = Vec::new();

        while program.stack[address].to_u64() != 0 {
            let ch = program.stack[address].to_u64() as u8;
            chars.push(ch);
            address += 1;
        }
        let string = String::from_utf8(chars).unwrap();
        println!("{:?}", string);
    }
}
