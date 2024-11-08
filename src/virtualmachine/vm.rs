use crate::ast2::Address;
use crate::ast2::Float;
use crate::ast2::Integer;

use super::Instruction;
use super::Operand;
use super::SizeType;

#[derive(Debug, Clone, Copy)]
pub struct RegisterData {
    pub bytes: u64,
}
impl Default for RegisterData {
    fn default() -> Self {
        RegisterData { bytes: 0 }
    }
}
impl RegisterData {}

/// Virtual Program
/// have stack, registers, labels
pub struct VirtualMachine {
    pub(crate) label_map: Vec<usize>,
    pub(crate) text: Vec<u8>,
    /// address of i'th function
    pub(crate) functions: Vec<usize>,

    pub(crate) stack: Vec<u8>,
    pub(crate) instructions: Vec<Instruction>,

    pub(crate) current_instruction: usize,

    pub(crate) registers: [RegisterData; 7],
}

pub(crate) const REGISTER_SP: usize = 6; // index of register for use as stack pointer (rsp)
pub(crate) const REGISTER_BP: usize = 5; // index of register for use as stack base pointer (rbp)

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            label_map: Vec::new(),
            stack: Vec::new(),
            functions: Vec::new(),

            registers: Default::default(),

            current_instruction: 0,
            instructions: Vec::new(),
            text: Vec::new(),
        }
    }

    fn get_bp(&self) -> u64 {
        self.registers[REGISTER_BP].bytes
    }

    unsafe fn get_as<T: Copy>(buf: &[u8], address: usize) -> T {
        *((&buf[address] as *const u8) as *const T)
    }
    unsafe fn set_as<T: Copy>(buf: &mut [u8], address: usize, value: T) {
        *((&mut buf[address] as *mut u8) as *mut T) = value;
    }

    fn get_byte(&self, operand: Operand) -> u8 {
        match operand {
            Operand::Constant(value) => value as u8,
            Operand::Register(register) => self.registers[register].bytes as u8,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => self.stack[g],
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        self.stack[address]
                    }
                    Address::Function(_) => {
                        panic!("Function address in get_byte")
                    }
                    Address::Text(t) => self.text[t],
                }
            }
        }
    }
    fn get_word(&self, operand: Operand) -> u16 {
        match operand {
            Operand::Constant(value) => value as u16,
            Operand::Register(register) => self.registers[register].bytes as u16,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::get_as::<u16>(&self.stack, g) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::get_as::<u16>(&self.stack, address) }
                    }
                    Address::Function(_) => {
                        panic!("Function address in get_byte")
                    }
                    Address::Text(t) => unsafe { Self::get_as::<u16>(&self.text, t) },
                }
            }
        }
    }
    fn get_dword(&self, operand: Operand) -> u32 {
        match operand {
            Operand::Constant(value) => value as u32,
            Operand::Register(register) => self.registers[register].bytes as u32,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::get_as::<u32>(&self.stack, g) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::get_as::<u32>(&self.stack, address) }
                    }
                    Address::Function(_) => {
                        panic!("Function address in get_dword")
                    }
                    Address::Text(t) => unsafe { Self::get_as::<u32>(&self.text, t) },
                }
            }
        }
    }
    fn get_qword(&self, operand: Operand) -> u64 {
        match operand {
            Operand::Constant(value) => value as u64,
            Operand::Register(register) => self.registers[register].bytes as u64,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::get_as::<u64>(&self.stack, g) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::get_as::<u64>(&self.stack, address) }
                    }
                    Address::Function(f) => self.functions[f] as u64,
                    Address::Text(t) => unsafe { Self::get_as::<u64>(&self.text, t) },
                }
            }
        }
    }
    fn set_byte(&mut self, operand: Operand, value: u8) {
        match operand {
            Operand::Register(register) => self.registers[register].bytes = value as u64,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => self.stack[g] = value,
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        self.stack[address] = value;
                    }
                    Address::Function(_) => {
                        panic!("Function address in set_byte")
                    }
                    Address::Text(t) => self.text[t] = value,
                }
            }
            Operand::Constant(_) => {
                unreachable!("Cannot set value to constant")
            }
        }
    }
    fn set_word(&mut self, operand: Operand, value: u16) {
        match operand {
            Operand::Register(register) => self.registers[register].bytes = value as u64,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::set_as::<u16>(&mut self.stack, g, value) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::set_as::<u16>(&mut self.stack, address, value) }
                    }
                    Address::Function(_) => {
                        unreachable!("Function address in get_byte")
                    }
                    Address::Text(_) => unreachable!("Text address in set_word"),
                }
            }
            Operand::Constant(_) => {
                unreachable!("Cannot set value to constant")
            }
        }
    }
    fn set_dword(&mut self, operand: Operand, value: u32) {
        match operand {
            Operand::Register(register) => self.registers[register].bytes = value as u64,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::set_as::<u32>(&mut self.stack, g, value) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::set_as::<u32>(&mut self.stack, address, value) }
                    }
                    Address::Function(_) => {
                        unreachable!("Function address in get_byte")
                    }
                    Address::Text(_) => unreachable!("Text address in set_dword"),
                }
            }
            Operand::Constant(_) => {
                unreachable!("Cannot set value to constant")
            }
        }
    }
    fn set_qword(&mut self, operand: Operand, value: u64) {
        match operand {
            Operand::Register(register) => self.registers[register].bytes = value as u64,
            Operand::Deref(register) => {
                let address = self.registers[register].bytes;
                match Address::from_u64(address) {
                    Address::Global(g) => unsafe { Self::set_as::<u64>(&mut self.stack, g, value) },
                    Address::Local(l) => {
                        let address = self.get_bp() as usize + l;
                        unsafe { Self::set_as::<u64>(&mut self.stack, address, value) }
                    }
                    Address::Function(_) => {
                        unreachable!("Function address in set_qword")
                    }
                    Address::Text(_) => unreachable!("Text address in set_qword"),
                }
            }
            Operand::Constant(_) => {
                unreachable!("Cannot set value to constant")
            }
        }
    }

    pub fn cycle(&mut self) {
        let instruction = self.instructions[self.current_instruction].clone();
        self.current_instruction += 1;

        match instruction {
            Instruction::Move(size, from, to) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(from);
                    self.set_byte(to, value);
                }
                SizeType::Word => {
                    let value = self.get_word(from);
                    self.set_word(to, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(from);
                    self.set_dword(to, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(from);
                    self.set_qword(to, value);
                }
            },

            Instruction::Jump(label) => {
                self.current_instruction = self.label_map[label];
            }

            Instruction::JumpZero(size, operand, label) => match size {
                SizeType::Byte => {
                    if self.get_byte(operand) == 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::Word => {
                    if self.get_word(operand) == 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::DWord => {
                    if self.get_dword(operand) == 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::QWord => {
                    if self.get_qword(operand) == 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
            },

            Instruction::JumpNotZero(size, operand, label) => match size {
                SizeType::Byte => {
                    if self.get_byte(operand) != 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::Word => {
                    if self.get_word(operand) != 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::DWord => {
                    if self.get_dword(operand) != 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
                SizeType::QWord => {
                    if self.get_qword(operand) != 0 {
                        self.current_instruction = self.label_map[label];
                    }
                }
            },
            Instruction::Push(size, src) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(src);
                    self.registers[REGISTER_SP].bytes -= 1;
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    self.stack[absolute_index as usize] = value;
                }
                SizeType::Word => {
                    let value = self.get_word(src);
                    self.registers[REGISTER_SP].bytes -= 2;
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    unsafe { Self::set_as::<u16>(&mut self.stack, absolute_index as usize, value) }
                }
                SizeType::DWord => {
                    let value = self.get_dword(src);
                    self.registers[REGISTER_SP].bytes -= 4;
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    unsafe { Self::set_as::<u32>(&mut self.stack, absolute_index as usize, value) }
                }
                SizeType::QWord => {
                    let value = self.get_qword(src);
                    self.registers[REGISTER_SP].bytes -= 8;
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    unsafe { Self::set_as::<u64>(&mut self.stack, absolute_index as usize, value) }
                }
            },
            Instruction::Pop(size, dst) => match size {
                SizeType::Byte => {
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    let value = self.stack[absolute_index as usize];
                    self.registers[REGISTER_SP].bytes += 1;
                    self.set_byte(dst, value);
                }
                SizeType::Word => {
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    let value =
                        unsafe { Self::get_as::<u16>(&self.stack, absolute_index as usize) };
                    self.registers[REGISTER_SP].bytes += 2;
                    self.set_word(dst, value);
                }
                SizeType::DWord => {
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    let value =
                        unsafe { Self::get_as::<u32>(&self.stack, absolute_index as usize) };
                    self.registers[REGISTER_SP].bytes += 4;
                    self.set_dword(dst, value);
                }
                SizeType::QWord => {
                    let absolute_index = self.registers[REGISTER_SP].bytes;
                    let value =
                        unsafe { Self::get_as::<u64>(&self.stack, absolute_index as usize) };
                    self.registers[REGISTER_SP].bytes += 8;
                    self.set_qword(dst, value);
                }
            },

            Instruction::Memcpy(bytes, src, dst) => {
                let src_addr = match Address::from_u64(self.get_qword(src)) {
                    Address::Global(g) => g,
                    Address::Local(l) => self.get_bp() as usize + l,
                    _ => panic!("Invalid address for memcpy"),
                };
                let dst_addr = match Address::from_u64(self.get_qword(dst)) {
                    Address::Global(g) => g,
                    Address::Local(l) => self.get_bp() as usize + l,
                    _ => panic!("Invalid address for memcpy"),
                };

                for i in 0..bytes {
                    self.stack[dst_addr + i] = self.stack[src_addr + i];
                }
            }

            Instruction::F2F(from, to) => match (from, to) {
                (Float::Float32, Float::Float64) => {
                    let bits = self.get_dword(Operand::Register(0));
                    let value = f32::from_bits(bits) as f64;
                    self.set_qword(Operand::Register(0), value.to_bits());
                }
                (Float::Float64, Float::Float32) => {
                    let bits = self.get_qword(Operand::Register(0));
                    let value = f64::from_bits(bits) as f32;
                    self.set_dword(Operand::Register(0), value.to_bits());
                }
                _ => {}
            },
            Instruction::F2I(from, to) => {
                let ivalue = match from {
                    Float::Float32 => {
                        let bits = self.get_dword(Operand::Register(0));
                        f32::from_bits(bits) as i64
                    }
                    Float::Float64 => {
                        let bits = self.get_qword(Operand::Register(0));
                        f64::from_bits(bits) as i64
                    }
                };
                match to {
                    Integer::UInt8 | Integer::Int8 => {
                        self.set_byte(Operand::Register(0), ivalue as u8);
                    }
                    Integer::UInt16 | Integer::Int16 => {
                        self.set_word(Operand::Register(0), ivalue as u16);
                    }
                    Integer::UInt32 | Integer::Int32 => {
                        self.set_dword(Operand::Register(0), ivalue as u32);
                    }
                    Integer::UInt64 | Integer::Int64 => {
                        self.set_qword(Operand::Register(0), ivalue as u64);
                    }
                }
            }
            Instruction::I2F(from, to) => match from {
                Integer::UInt8 => {
                    let value = self.get_byte(Operand::Register(0));
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::Int8 => {
                    let value = self.get_byte(Operand::Register(0)) as i8;
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::UInt16 => {
                    let value = self.get_word(Operand::Register(0));
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::Int16 => {
                    let value = self.get_word(Operand::Register(0)) as i16;
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::UInt32 => {
                    let value = self.get_dword(Operand::Register(0));
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::Int32 => {
                    let value = self.get_dword(Operand::Register(0)) as i32;
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::UInt64 => {
                    let value = self.get_qword(Operand::Register(0));
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
                Integer::Int64 => {
                    let value = self.get_qword(Operand::Register(0)) as i64;
                    match to {
                        Float::Float32 => {
                            let fvalue = value as f32;
                            self.set_dword(Operand::Register(0), fvalue.to_bits());
                        }
                        Float::Float64 => {
                            let fvalue = value as f64;
                            self.set_qword(Operand::Register(0), fvalue.to_bits());
                        }
                    }
                }
            },

            Instruction::I2I(from, to) => {
                let value = match from {
                    Integer::UInt8 => self.get_byte(Operand::Register(0)) as u64,
                    Integer::Int8 => self.get_byte(Operand::Register(0)) as i8 as i64 as u64,
                    Integer::UInt16 => self.get_word(Operand::Register(0)) as u64,
                    Integer::Int16 => self.get_word(Operand::Register(0)) as i16 as i64 as u64,
                    Integer::UInt32 => self.get_dword(Operand::Register(0)) as u64,
                    Integer::Int32 => self.get_dword(Operand::Register(0)) as i32 as i64 as u64,
                    Integer::UInt64 => self.get_qword(Operand::Register(0)),
                    Integer::Int64 => self.get_qword(Operand::Register(0)) as i64 as u64,
                };

                match to {
                    Integer::UInt8 => self.set_byte(Operand::Register(0), value as u8),
                    Integer::Int8 => self.set_byte(Operand::Register(0), value as i8 as u8),
                    Integer::UInt16 => self.set_word(Operand::Register(0), value as u16),
                    Integer::Int16 => self.set_word(Operand::Register(0), value as i16 as u16),
                    Integer::UInt32 => self.set_dword(Operand::Register(0), value as u32),
                    Integer::Int32 => self.set_dword(Operand::Register(0), value as i32 as u32),
                    Integer::UInt64 => self.set_qword(Operand::Register(0), value),
                    Integer::Int64 => self.set_qword(Operand::Register(0), value as i64 as u64),
                }
            }

            Instruction::AddI(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(lhs).wrapping_add(self.get_byte(rhs));
                    self.set_byte(out, value);
                }
                SizeType::Word => {
                    let value = self.get_word(lhs).wrapping_add(self.get_word(rhs));
                    self.set_word(out, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(lhs).wrapping_add(self.get_dword(rhs));
                    self.set_dword(out, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(lhs).wrapping_add(self.get_qword(rhs));
                    self.set_qword(out, value);
                }
            },
            Instruction::AddF(size, lhs, rhs, out) => match size {
                SizeType::DWord => {
                    let value =
                        f32::from_bits(self.get_dword(lhs)) + f32::from_bits(self.get_dword(rhs));
                    self.set_dword(out, value.to_bits());
                }
                SizeType::QWord => {
                    let value =
                        f64::from_bits(self.get_qword(lhs)) + f64::from_bits(self.get_qword(rhs));
                    self.set_qword(out, value.to_bits());
                }
                _ => unreachable!("Invalid size for AddF"),
            },

            Instruction::SubI(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(lhs).wrapping_sub(self.get_byte(rhs));
                    self.set_byte(out, value);
                }
                SizeType::Word => {
                    let value = self.get_word(lhs).wrapping_sub(self.get_word(rhs));
                    self.set_word(out, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(lhs).wrapping_sub(self.get_dword(rhs));
                    self.set_dword(out, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(lhs).wrapping_sub(self.get_qword(rhs));
                    self.set_qword(out, value);
                }
            },
            Instruction::SubF(size, lhs, rhs, out) => match size {
                SizeType::DWord => {
                    let value =
                        f32::from_bits(self.get_dword(lhs)) - f32::from_bits(self.get_dword(rhs));
                    self.set_dword(out, value.to_bits());
                }
                SizeType::QWord => {
                    let value =
                        f64::from_bits(self.get_qword(lhs)) - f64::from_bits(self.get_qword(rhs));
                    self.set_qword(out, value.to_bits());
                }
                _ => unreachable!("Invalid size for AddF"),
            },

            Instruction::LogicalNot(size, src, dst) => {
                let iszero = match size {
                    SizeType::Byte => self.get_byte(src) == 0,
                    SizeType::Word => self.get_word(src) == 0,
                    SizeType::DWord => self.get_dword(src) == 0,
                    SizeType::QWord => self.get_qword(src) == 0,
                };
                let value = if iszero { 1 } else { 0 };
                self.set_dword(dst, value);
            }
            Instruction::BitNot(size, src) => match size {
                SizeType::Byte => {
                    let value = !self.get_byte(src);
                    self.set_byte(src, value);
                }
                SizeType::Word => {
                    let value = !self.get_word(src);
                    self.set_word(src, value);
                }
                SizeType::DWord => {
                    let value = !self.get_dword(src);
                    self.set_dword(src, value);
                }
                SizeType::QWord => {
                    let value = !self.get_qword(src);
                    self.set_qword(src, value);
                }
            },
            Instruction::Neg(size, src) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(src).wrapping_neg();
                    self.set_byte(src, value);
                }
                SizeType::Word => {
                    let value = self.get_word(src).wrapping_neg();
                    self.set_word(src, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(src).wrapping_neg();
                    self.set_dword(src, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(src).wrapping_neg();
                    self.set_qword(src, value);
                }
            },

            Instruction::BitAnd(size, lhs, rhs) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(lhs) & self.get_byte(rhs);
                    self.set_byte(lhs, value);
                }
                SizeType::Word => {
                    let value = self.get_word(lhs) & self.get_word(rhs);
                    self.set_word(lhs, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(lhs) & self.get_dword(rhs);
                    self.set_dword(lhs, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(lhs) & self.get_qword(rhs);
                    self.set_qword(lhs, value);
                }
            },

            Instruction::BitOr(size, lhs, rhs) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(lhs) | self.get_byte(rhs);
                    self.set_byte(lhs, value);
                }
                SizeType::Word => {
                    let value = self.get_word(lhs) | self.get_word(rhs);
                    self.set_word(lhs, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(lhs) | self.get_dword(rhs);
                    self.set_dword(lhs, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(lhs) | self.get_qword(rhs);
                    self.set_qword(lhs, value);
                }
            },
            Instruction::BitXor(size, lhs, rhs) => match size {
                SizeType::Byte => {
                    let value = self.get_byte(lhs) ^ self.get_byte(rhs);
                    self.set_byte(lhs, value);
                }
                SizeType::Word => {
                    let value = self.get_word(lhs) ^ self.get_word(rhs);
                    self.set_word(lhs, value);
                }
                SizeType::DWord => {
                    let value = self.get_dword(lhs) ^ self.get_dword(rhs);
                    self.set_dword(lhs, value);
                }
                SizeType::QWord => {
                    let value = self.get_qword(lhs) ^ self.get_qword(rhs);
                    self.set_qword(lhs, value);
                }
            },

            Instruction::ShiftLeftI(size, src, bits) => match size {
                SizeType::Byte => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_byte(src) as i8).wrapping_shl(rhs);
                    self.set_byte(src, value as u8);
                }
                SizeType::Word => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_word(src) as i16).wrapping_shl(rhs);
                    self.set_word(src, value as u16);
                }
                SizeType::DWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_dword(src) as i32).wrapping_shl(rhs);
                    self.set_dword(src, value as u32);
                }
                SizeType::QWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_qword(src) as i64).wrapping_shl(rhs);
                    self.set_qword(src, value as u64);
                }
            },
            Instruction::ShiftLeftU(size, src, bits) => match size {
                SizeType::Byte => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_byte(src).wrapping_shl(rhs);
                    self.set_byte(src, value);
                }
                SizeType::Word => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_word(src).wrapping_shl(rhs);
                    self.set_word(src, value);
                }
                SizeType::DWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_dword(src).wrapping_shl(rhs);
                    self.set_dword(src, value);
                }
                SizeType::QWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_qword(src).wrapping_shl(rhs);
                    self.set_qword(src, value);
                }
            },

            Instruction::ShiftRightI(size, src, bits) => match size {
                SizeType::Byte => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_byte(src) as i8).wrapping_shr(rhs);
                    self.set_byte(src, value as u8);
                }
                SizeType::Word => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_word(src) as i16).wrapping_shr(rhs);
                    self.set_word(src, value as u16);
                }
                SizeType::DWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_dword(src) as i32).wrapping_shr(rhs);
                    self.set_dword(src, value as u32);
                }
                SizeType::QWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = (self.get_qword(src) as i64).wrapping_shr(rhs);
                    self.set_qword(src, value as u64);
                }
            },

            Instruction::ShiftRightU(size, src, bits) => match size {
                SizeType::Byte => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_byte(src).wrapping_shr(rhs);
                    self.set_byte(src, value);
                }
                SizeType::Word => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_word(src).wrapping_shr(rhs);
                    self.set_word(src, value);
                }
                SizeType::DWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_dword(src).wrapping_shr(rhs);
                    self.set_dword(src, value);
                }
                SizeType::QWord => {
                    let rhs = self.get_byte(bits) as u32;
                    let value = self.get_qword(src).wrapping_shr(rhs);
                    self.set_qword(src, value);
                }
            },
            Instruction::Equal(size, lhs, rhs, out) => {
                let value = match size {
                    SizeType::Byte => self.get_byte(lhs) == self.get_byte(rhs),
                    SizeType::Word => self.get_word(lhs) == self.get_word(rhs),
                    SizeType::DWord => self.get_dword(lhs) == self.get_dword(rhs),
                    SizeType::QWord => self.get_qword(lhs) == self.get_qword(rhs),
                };
                self.set_dword(out, if value { 1 } else { 0 });
            }

            Instruction::LessThanI(size, lhs, rhs, out) => {
                let value = match size {
                    SizeType::Byte => (self.get_byte(lhs) as i8) < (self.get_byte(rhs) as i8),
                    SizeType::Word => (self.get_word(lhs) as i16) < (self.get_word(rhs) as i16),
                    SizeType::DWord => (self.get_dword(lhs) as i32) < (self.get_dword(rhs) as i32),
                    SizeType::QWord => (self.get_qword(lhs) as i64) < (self.get_qword(rhs) as i64),
                };
                self.set_dword(out, if value { 1 } else { 0 });
            }
            Instruction::LessThanU(size, lhs, rhs, out) => {
                let value = match size {
                    SizeType::Byte => self.get_byte(lhs) < self.get_byte(rhs),
                    SizeType::Word => self.get_word(lhs) < self.get_word(rhs),
                    SizeType::DWord => self.get_dword(lhs) < self.get_dword(rhs),
                    SizeType::QWord => self.get_qword(lhs) < self.get_qword(rhs),
                };
                self.set_dword(out, if value { 1 } else { 0 });
            }
            Instruction::LessThanF(size, lhs, rhs, out) => {
                let value = match size {
                    SizeType::DWord => {
                        f32::from_bits(self.get_dword(lhs)) < f32::from_bits(self.get_dword(rhs))
                    }
                    SizeType::QWord => {
                        f64::from_bits(self.get_qword(lhs)) < f64::from_bits(self.get_qword(rhs))
                    }
                    _ => panic!("Invalid size for LessThanF : {:?}", size),
                };
                self.set_dword(out, if value { 1 } else { 0 });
            }

            Instruction::ModI(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs) as i8;
                    let rhs = self.get_byte(rhs) as i8;
                    self.set_byte(out, (lhs % rhs) as u8);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs) as i16;
                    let rhs = self.get_word(rhs) as i16;
                    self.set_word(out, (lhs % rhs) as u16);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs) as i32;
                    let rhs = self.get_dword(rhs) as i32;
                    self.set_dword(out, (lhs % rhs) as u32);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs) as i64;
                    let rhs = self.get_qword(rhs) as i64;
                    self.set_qword(out, (lhs % rhs) as u64);
                }
            },

            Instruction::ModU(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs);
                    let rhs = self.get_byte(rhs);
                    self.set_byte(out, lhs % rhs);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs);
                    let rhs = self.get_word(rhs);
                    self.set_word(out, lhs % rhs);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs);
                    let rhs = self.get_dword(rhs);
                    self.set_dword(out, lhs % rhs);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs);
                    let rhs = self.get_qword(rhs);
                    self.set_qword(out, lhs % rhs);
                }
            },

            Instruction::DivU(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs);
                    let rhs = self.get_byte(rhs);
                    self.set_byte(out, lhs / rhs);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs);
                    let rhs = self.get_word(rhs);
                    self.set_word(out, lhs / rhs);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs);
                    let rhs = self.get_dword(rhs);
                    self.set_dword(out, lhs / rhs);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs);
                    let rhs = self.get_qword(rhs);
                    self.set_qword(out, lhs / rhs);
                }
            },

            Instruction::DivI(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs) as i8;
                    let rhs = self.get_byte(rhs) as i8;
                    self.set_byte(out, (lhs / rhs) as u8);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs) as i16;
                    let rhs = self.get_word(rhs) as i16;
                    self.set_word(out, (lhs / rhs) as u16);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs) as i32;
                    let rhs = self.get_dword(rhs) as i32;
                    self.set_dword(out, (lhs / rhs) as u32);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs) as i64;
                    let rhs = self.get_qword(rhs) as i64;
                    self.set_qword(out, (lhs / rhs) as u64);
                }
            },

            Instruction::DivF(size, lhs, rhs, out) => match size {
                SizeType::DWord => {
                    let lhs = f32::from_bits(self.get_dword(lhs));
                    let rhs = f32::from_bits(self.get_dword(rhs));
                    self.set_dword(out, (lhs / rhs).to_bits());
                }
                SizeType::QWord => {
                    let lhs = f64::from_bits(self.get_qword(lhs));
                    let rhs = f64::from_bits(self.get_qword(rhs));
                    self.set_qword(out, (lhs / rhs).to_bits());
                }
                _ => panic!("Invalid size for DivF : {:?}", size),
            },

            Instruction::MulI(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs) as i8;
                    let rhs = self.get_byte(rhs) as i8;
                    self.set_byte(out, (lhs * rhs) as u8);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs) as i16;
                    let rhs = self.get_word(rhs) as i16;
                    self.set_word(out, (lhs * rhs) as u16);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs) as i32;
                    let rhs = self.get_dword(rhs) as i32;
                    self.set_dword(out, (lhs * rhs) as u32);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs) as i64;
                    let rhs = self.get_qword(rhs) as i64;
                    self.set_qword(out, (lhs * rhs) as u64);
                }
            },

            Instruction::MulU(size, lhs, rhs, out) => match size {
                SizeType::Byte => {
                    let lhs = self.get_byte(lhs);
                    let rhs = self.get_byte(rhs);
                    self.set_byte(out, lhs * rhs);
                }
                SizeType::Word => {
                    let lhs = self.get_word(lhs);
                    let rhs = self.get_word(rhs);
                    self.set_word(out, lhs * rhs);
                }
                SizeType::DWord => {
                    let lhs = self.get_dword(lhs);
                    let rhs = self.get_dword(rhs);
                    self.set_dword(out, lhs * rhs);
                }
                SizeType::QWord => {
                    let lhs = self.get_qword(lhs);
                    let rhs = self.get_qword(rhs);
                    self.set_qword(out, lhs * rhs);
                }
            },

            Instruction::MulF(size, lhs, rhs, out) => match size {
                SizeType::DWord => {
                    let lhs = f32::from_bits(self.get_dword(lhs));
                    let rhs = f32::from_bits(self.get_dword(rhs));
                    self.set_dword(out, (lhs * rhs).to_bits());
                }
                SizeType::QWord => {
                    let lhs = f64::from_bits(self.get_qword(lhs));
                    let rhs = f64::from_bits(self.get_qword(rhs));
                    self.set_qword(out, (lhs * rhs).to_bits());
                }
                _ => panic!("Invalid size for MulF : {:?}", size),
            },

            _ => unimplemented!("Instruction not implemented: {:?}", instruction),
        }
    }
}
