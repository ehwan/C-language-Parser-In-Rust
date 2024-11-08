mod generator;
mod instruction;
mod operand;
mod vm;

pub type LabelType = usize;

pub use generator::InstructionGenerator;
pub use instruction::Instruction;
pub use operand::Operand;
pub use operand::SizeType;
pub use vm::VirtualMachine;
