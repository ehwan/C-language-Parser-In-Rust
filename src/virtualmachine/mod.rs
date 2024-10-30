mod generator;
mod instruction;
mod operand;
mod vm;

pub type LabelType = usize;

pub use instruction::Instruction;

pub use generator::InstructionGenerator;
pub use vm::VirtualMachine;
