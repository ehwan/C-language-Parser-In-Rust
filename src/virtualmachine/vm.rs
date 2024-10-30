use super::Instruction;

/// Virtual Program
/// have stack, registers, labels
pub struct VirtualMachine {
    pub(crate) label_map: Vec<usize>,

    pub(crate) stack: Vec<u8>,

    /// each register is for single primitive type
    /// rax, rbx, rcx, rdx, rtx, rbp, rsp
    /// last tree registers are for
    /// text section size, stack pointer and base pointer
    pub(crate) registers: [u64; 7],

    pub(crate) current_instruction: usize,
    pub(crate) instructions: Vec<Instruction>,
}

pub const STACK_SIZE: usize = 10240; // stack size
pub const STACK_POINTER_REGISTER: usize = 6; // index of register for use as stack pointer (rsp)
pub const STACK_POINTER_BASE_REGISTER: usize = 5; // index of register for use as stack base pointer (rbp)
pub const TEXT_SIZE_REGISTER: usize = 4; // index of register for use as text section size (rtx)
pub const RAX: usize = 0; // index of register for use as rax
pub const RBX: usize = 1; // index of register for use as rax
pub const RCX: usize = 2; // index of register for use as rax
pub const RDX: usize = 3; // index of register for use as rax
impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            label_map: Vec::new(),
            stack: Vec::new(),

            registers: Default::default(),

            current_instruction: 0,
            instructions: Vec::new(),
        }
    }
}
