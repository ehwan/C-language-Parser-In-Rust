use super::program::{STACK_POINTER_BASE_REGISTER, STACK_POINTER_REGISTER, STACK_SIZE};

#[derive(Debug, Clone)]
pub enum Instruction {
    MoveRegister(MoveRegister),
    PushStack(PushStack),
    PopStack(PopStack),
    Jump(Jump),
    JumpZero(JumpZero),
    JumpNonZero(JumpNonZero),
    Print(Print),
    Panic(Panic),
    Call(Call),
    Return(Return),
    PrintStr(PrintStr),

    Cast(Cast),
    Negate(Negate),
    LogicalNot(LogicalNot),
    BitwiseNot(BitwiseNot),
    LessThan(LessThan),
    Equal(Equal),
    AddAssign(AddAssign),
    SubAssign(SubAssign),
    MulAssign(MulAssign),
    DivAssign(DivAssign),
    ModAssign(ModAssign),
    BitwiseAndAssign(BitwiseAndAssign),
    BitwiseOrAssign(BitwiseOrAssign),
    BitwiseXorAssign(BitwiseXorAssign),
    ShiftLeftAssign(ShiftLeftAssign),
    ShiftRightAssign(ShiftRightAssign),
    Assign(Assign),
    AssignStruct(AssignStruct),
}

impl Instruction {
    pub fn execute(&self, program: &mut VirtualMachine) {
        match self {
            Instruction::DefineLabel(inst) => inst.execute(program),
            Instruction::MoveRegister(inst) => inst.execute(program),
            Instruction::PushStack(inst) => inst.execute(program),
            Instruction::PopStack(inst) => inst.execute(program),
            Instruction::Jump(inst) => inst.execute(program),
            Instruction::JumpZero(inst) => inst.execute(program),
            Instruction::JumpNonZero(inst) => inst.execute(program),
            Instruction::Print(inst) => inst.execute(program),
            Instruction::Panic(inst) => inst.execute(program),
            Instruction::Call(inst) => inst.execute(program),
            Instruction::Return(inst) => inst.execute(program),
            Instruction::PrintStr(inst) => inst.execute(program),
            Instruction::Cast(inst) => inst.execute(program),
            Instruction::Negate(inst) => inst.execute(program),
            Instruction::LogicalNot(inst) => inst.execute(program),
            Instruction::BitwiseNot(inst) => inst.execute(program),
            Instruction::LessThan(inst) => inst.execute(program),
            Instruction::Equal(inst) => inst.execute(program),
            Instruction::AddAssign(inst) => inst.execute(program),
            Instruction::SubAssign(inst) => inst.execute(program),
            Instruction::MulAssign(inst) => inst.execute(program),
            Instruction::DivAssign(inst) => inst.execute(program),
            Instruction::ModAssign(inst) => inst.execute(program),
            Instruction::BitwiseAndAssign(inst) => inst.execute(program),
            Instruction::BitwiseOrAssign(inst) => inst.execute(program),
            Instruction::BitwiseXorAssign(inst) => inst.execute(program),
            Instruction::ShiftLeftAssign(inst) => inst.execute(program),
            Instruction::ShiftRightAssign(inst) => inst.execute(program),
            Instruction::Assign(inst) => inst.execute(program),
            Instruction::AssignStruct(inst) => inst.execute(program),
        }
    }
}

/// Do Nothing; for debugging
#[derive(Debug, Clone)]
pub struct DefineLabel {
    pub label: String,
}
impl DefineLabel {
    fn execute(&self, _program: &mut VirtualMachine) {
        // label is defined at emitting session
    }
}

/// copy register N to register M
#[derive(Debug, Clone)]
pub struct MoveRegister {
    pub operand_from: Operand,
    pub operand_to: Operand,
}
impl MoveRegister {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.operand_from).clone();
        let lhs = get_operand_value_mut(program, &self.operand_to);
        *lhs = rhs;
    }
}
#[derive(Debug, Clone)]
pub struct PushStack {
    pub operand: Operand,
}
impl PushStack {
    fn execute(&self, program: &mut VirtualMachine) {
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
#[derive(Debug, Clone)]
pub struct PopStack {
    pub operand: Operand,
}
impl PopStack {
    fn execute(&self, program: &mut VirtualMachine) {
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
#[derive(Debug, Clone)]
pub struct Jump {
    pub label: String,
}
impl Jump {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
        let address = program
            .label_map
            .get(&self.label)
            .expect(format!("Jump: label not found: {}", self.label).as_str())
            .clone();
        program.current_instruction = address;
    }
}

/// Jump to address if register N is zero
#[derive(Debug, Clone)]
pub struct JumpZero {
    pub label: String,
    pub operand_cond: Operand,
}
impl JumpZero {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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
#[derive(Debug, Clone)]
pub struct JumpNonZero {
    pub label: String,
    pub operand_cond: Operand,
}
impl JumpNonZero {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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
#[derive(Debug, Clone)]
pub struct Print {}
impl Print {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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
#[derive(Debug, Clone)]
pub struct Panic {
    pub message: String,
}
impl Panic {
    fn execute(&self, _program: &mut crate::virtualmachine::program::VirtualMachine) {
        panic!("Panic: {}", self.message);
    }
}

/// push current address and scope count to stack and jump
#[derive(Debug, Clone)]
pub struct Call {
    pub label: String,
}
impl Call {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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

#[derive(Debug, Clone)]
pub struct Return {}
impl Return {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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

#[derive(Debug, Clone)]
pub struct PrintStr {
    pub str: Operand, // null terminated string
}
impl PrintStr {
    fn execute(&self, program: &mut crate::virtualmachine::program::VirtualMachine) {
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

/// cast register N to type
#[derive(Debug, Clone)]
pub struct Cast {
    pub info: TypeInfo,
    pub operand_from: Operand,
    pub operand_to: Operand,
}
impl Cast {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs_casted = get_operand_value(program, &self.operand_from)
            .cast_to(&self.info)
            .expect(format!("Invalid cast: to {:?}", &self.info).as_str());
        *get_operand_value_mut(program, &self.operand_to) = rhs_casted;
    }
}

/// neg register N
#[derive(Debug, Clone)]
pub struct Negate {
    pub operand: Operand,
}
impl Negate {
    fn execute(&self, program: &mut VirtualMachine) {
        let var = get_operand_value_mut(program, &self.operand);
        let res = match &var {
            VariableData::UInt8(value) => VariableData::Int8(-(*value as i8)),
            VariableData::UInt16(value) => VariableData::Int16(-(*value as i16)),
            VariableData::UInt32(value) => VariableData::Int32(-(*value as i32)),
            VariableData::UInt64(value) => VariableData::Int64(-(*value as i64)),
            VariableData::Int8(value) => VariableData::Int8(-*value),
            VariableData::Int16(value) => VariableData::Int16(-*value),
            VariableData::Int32(value) => VariableData::Int32(-*value),
            VariableData::Int64(value) => VariableData::Int64(-*value),
            VariableData::Float32(value) => VariableData::Float32(-*value),
            VariableData::Float64(value) => VariableData::Float64(-*value),
        };
        *var = res;
    }
}

/// logical not register N
#[derive(Debug, Clone)]
pub struct LogicalNot {
    pub operand: Operand,
}
impl LogicalNot {
    fn execute(&self, program: &mut VirtualMachine) {
        let var = get_operand_value_mut(program, &self.operand);
        let res = match &var {
            VariableData::UInt8(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt16(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt32(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::UInt64(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int8(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int16(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int32(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            VariableData::Int64(value) => {
                if *value == 0 {
                    1
                } else {
                    0
                }
            }
            _ => panic!("Invalid type for logical not"),
        };
        *var = VariableData::UInt8(res as u8);
    }
}

/// bitwise not register N
#[derive(Debug, Clone)]
pub struct BitwiseNot {
    pub operand: Operand,
}
impl BitwiseNot {
    fn execute(&self, program: &mut VirtualMachine) {
        let var = get_operand_value_mut(program, &self.operand);
        match var {
            VariableData::UInt8(ref mut value) => *value = !*value,
            VariableData::UInt16(ref mut value) => *value = !*value,
            VariableData::UInt32(ref mut value) => *value = !*value,
            VariableData::UInt64(ref mut value) => *value = !*value,
            VariableData::Int8(ref mut value) => *value = !*value,
            VariableData::Int16(ref mut value) => *value = !*value,
            VariableData::Int32(ref mut value) => *value = !*value,
            VariableData::Int64(ref mut value) => *value = !*value,
            _ => panic!("Invalid type for bitwise not"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LessThan {
    pub lhs: Operand,
    pub rhs: Operand,
    pub to: Operand,
}
impl LessThan {
    fn execute(&self, program: &mut VirtualMachine) {
        let ret: bool = {
            let lhs = get_operand_value(program, &self.lhs);
            let rhs = get_operand_value(program, &self.rhs);

            if lhs.is_signed_integer() {
                if rhs.is_signed_integer() {
                    let lhs = lhs.to_i64();
                    let rhs = rhs.to_i64();
                    lhs < rhs
                } else if rhs.is_unsigned_integer() {
                    let lhs = lhs.to_i64();
                    let rhs = rhs.to_u64();
                    if lhs < 0 {
                        true
                    } else {
                        (lhs as u64) < rhs
                    }
                } else if rhs.is_float() {
                    lhs.to_f64() < rhs.to_f64()
                } else {
                    panic!("Invalid type for less than");
                }
            } else if lhs.is_unsigned_integer() {
                if rhs.is_signed_integer() {
                    let lhs = lhs.to_u64();
                    let rhs = rhs.to_i64();
                    if rhs < 0 {
                        false
                    } else {
                        lhs < rhs as u64
                    }
                } else if rhs.is_unsigned_integer() {
                    let lhs = lhs.to_u64();
                    let rhs = rhs.to_u64();
                    lhs < rhs
                } else if rhs.is_float() {
                    lhs.to_f64() < rhs.to_f64()
                } else {
                    panic!("Invalid type for less than");
                }
            } else if lhs.is_float() {
                lhs.to_f64() < rhs.to_f64()
            } else {
                panic!("Invalid type for less than");
            }
        };
        *get_operand_value_mut(program, &self.to) = VariableData::UInt8(if ret { 1 } else { 0 });
    }
}

#[derive(Debug, Clone)]
pub struct Equal {
    pub lhs: Operand,
    pub rhs: Operand,
    pub to: Operand,
}
impl Equal {
    fn execute(&self, program: &mut VirtualMachine) {
        let ret: bool = {
            let lhs = get_operand_value(program, &self.lhs);
            let rhs = get_operand_value(program, &self.rhs);

            if lhs.is_signed_integer() {
                let lhs = lhs.to_i64();
                if rhs.is_signed_integer() {
                    let rhs = rhs.to_i64();
                    lhs == rhs
                } else if rhs.is_unsigned_integer() {
                    let rhs = rhs.to_u64();
                    if lhs < 0 {
                        false
                    } else {
                        (lhs as u64) == rhs
                    }
                } else if rhs.is_float() {
                    panic!("floating point variables are not equal-comparable");
                } else {
                    panic!("Invalid type for equal");
                }
            } else if lhs.is_unsigned_integer() {
                let lhs = lhs.to_u64();
                if rhs.is_signed_integer() {
                    let rhs = rhs.to_i64();
                    if rhs < 0 {
                        false
                    } else {
                        lhs == rhs as u64
                    }
                } else if rhs.is_unsigned_integer() {
                    let rhs = rhs.to_u64();
                    lhs == rhs
                } else if rhs.is_float() {
                    panic!("floating point variables are not equal-comparable");
                } else {
                    panic!("Invalid type for equal");
                }
            } else if lhs.is_float() {
                panic!("floating point variables are not equal-comparable");
            } else {
                panic!("Invalid type for equal");
            }
        };
        *get_operand_value_mut(program, &self.to) = VariableData::UInt8(if ret { 1 } else { 0 });
    }
}

#[derive(Debug, Clone)]
pub struct AddAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl AddAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);
        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i8).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i16).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i32).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add_signed(rhs as i64).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i8).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i16).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i32).0,
                _ => panic!("Invalid type for add assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_add_unsigned(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_add(rhs as i64).0,
                _ => panic!("Invalid type for add assign"),
            },

            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs += rhs as f32,
                VariableData::UInt16(rhs) => *lhs += rhs as f32,
                VariableData::UInt32(rhs) => *lhs += rhs as f32,
                VariableData::UInt64(rhs) => *lhs += rhs as f32,
                VariableData::Int8(rhs) => *lhs += rhs as f32,
                VariableData::Int16(rhs) => *lhs += rhs as f32,
                VariableData::Int32(rhs) => *lhs += rhs as f32,
                VariableData::Int64(rhs) => *lhs += rhs as f32,
                VariableData::Float32(rhs) => *lhs += rhs as f32,
                VariableData::Float64(rhs) => *lhs += rhs as f32,
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs += rhs as f64,
                VariableData::UInt16(rhs) => *lhs += rhs as f64,
                VariableData::UInt32(rhs) => *lhs += rhs as f64,
                VariableData::UInt64(rhs) => *lhs += rhs as f64,
                VariableData::Int8(rhs) => *lhs += rhs as f64,
                VariableData::Int16(rhs) => *lhs += rhs as f64,
                VariableData::Int32(rhs) => *lhs += rhs as f64,
                VariableData::Int64(rhs) => *lhs += rhs as f64,
                VariableData::Float32(rhs) => *lhs += rhs as f64,
                VariableData::Float64(rhs) => *lhs += rhs as f64,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl SubAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);
        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u8).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u16).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u32).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as u64).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i8).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i16).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i32).0,
                _ => panic!("Invalid type for sub assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_sub_unsigned(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_sub(rhs as i64).0,
                _ => panic!("Invalid type for sub assign"),
            },

            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs -= rhs as f32,
                VariableData::UInt16(rhs) => *lhs -= rhs as f32,
                VariableData::UInt32(rhs) => *lhs -= rhs as f32,
                VariableData::UInt64(rhs) => *lhs -= rhs as f32,
                VariableData::Int8(rhs) => *lhs -= rhs as f32,
                VariableData::Int16(rhs) => *lhs -= rhs as f32,
                VariableData::Int32(rhs) => *lhs -= rhs as f32,
                VariableData::Int64(rhs) => *lhs -= rhs as f32,
                VariableData::Float32(rhs) => *lhs -= rhs as f32,
                VariableData::Float64(rhs) => *lhs -= rhs as f32,
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs -= rhs as f64,
                VariableData::UInt16(rhs) => *lhs -= rhs as f64,
                VariableData::UInt32(rhs) => *lhs -= rhs as f64,
                VariableData::UInt64(rhs) => *lhs -= rhs as f64,
                VariableData::Int8(rhs) => *lhs -= rhs as f64,
                VariableData::Int16(rhs) => *lhs -= rhs as f64,
                VariableData::Int32(rhs) => *lhs -= rhs as f64,
                VariableData::Int64(rhs) => *lhs -= rhs as f64,
                VariableData::Float32(rhs) => *lhs -= rhs as f64,
                VariableData::Float64(rhs) => *lhs -= rhs as f64,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct MulAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl MulAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u8).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u16).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u32).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as u64).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i8).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i16).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i32).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_mul(rhs as i64).0,
                _ => panic!("Invalid type for mul assign"),
            },
            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs *= rhs as f32,
                VariableData::UInt16(rhs) => *lhs *= rhs as f32,
                VariableData::UInt32(rhs) => *lhs *= rhs as f32,
                VariableData::UInt64(rhs) => *lhs *= rhs as f32,
                VariableData::Int8(rhs) => *lhs *= rhs as f32,
                VariableData::Int16(rhs) => *lhs *= rhs as f32,
                VariableData::Int32(rhs) => *lhs *= rhs as f32,
                VariableData::Int64(rhs) => *lhs *= rhs as f32,
                VariableData::Float32(rhs) => *lhs *= rhs as f32,
                VariableData::Float64(rhs) => *lhs *= rhs as f32,
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs *= rhs as f64,
                VariableData::UInt16(rhs) => *lhs *= rhs as f64,
                VariableData::UInt32(rhs) => *lhs *= rhs as f64,
                VariableData::UInt64(rhs) => *lhs *= rhs as f64,
                VariableData::Int8(rhs) => *lhs *= rhs as f64,
                VariableData::Int16(rhs) => *lhs *= rhs as f64,
                VariableData::Int32(rhs) => *lhs *= rhs as f64,
                VariableData::Int64(rhs) => *lhs *= rhs as f64,
                VariableData::Float32(rhs) => *lhs *= rhs as f64,
                VariableData::Float64(rhs) => *lhs *= rhs as f64,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct DivAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl DivAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u8).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u16).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u32).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as u64).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i8).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i16).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i32).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_div(rhs as i64).0,
                _ => panic!("Invalid type for div assign"),
            },
            VariableData::Float32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs /= rhs as f32,
                VariableData::UInt16(rhs) => *lhs /= rhs as f32,
                VariableData::UInt32(rhs) => *lhs /= rhs as f32,
                VariableData::UInt64(rhs) => *lhs /= rhs as f32,
                VariableData::Int8(rhs) => *lhs /= rhs as f32,
                VariableData::Int16(rhs) => *lhs /= rhs as f32,
                VariableData::Int32(rhs) => *lhs /= rhs as f32,
                VariableData::Int64(rhs) => *lhs /= rhs as f32,
                VariableData::Float32(rhs) => *lhs /= rhs as f32,
                VariableData::Float64(rhs) => *lhs /= rhs as f32,
            },
            VariableData::Float64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs /= rhs as f64,
                VariableData::UInt16(rhs) => *lhs /= rhs as f64,
                VariableData::UInt32(rhs) => *lhs /= rhs as f64,
                VariableData::UInt64(rhs) => *lhs /= rhs as f64,
                VariableData::Int8(rhs) => *lhs /= rhs as f64,
                VariableData::Int16(rhs) => *lhs /= rhs as f64,
                VariableData::Int32(rhs) => *lhs /= rhs as f64,
                VariableData::Int64(rhs) => *lhs /= rhs as f64,
                VariableData::Float32(rhs) => *lhs /= rhs as f64,
                VariableData::Float64(rhs) => *lhs /= rhs as f64,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl ModAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u8).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u16).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u32).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as u64).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i8).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i16).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i32).0,
                _ => panic!("Invalid type for mod assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_rem(rhs as i64).0,
                _ => panic!("Invalid type for mod assign"),
            },
            _ => panic!("Invalid type for mod assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BitwiseAndAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl BitwiseAndAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u8,
                VariableData::UInt16(rhs) => *lhs &= rhs as u8,
                VariableData::UInt32(rhs) => *lhs &= rhs as u8,
                VariableData::UInt64(rhs) => *lhs &= rhs as u8,
                VariableData::Int8(rhs) => *lhs &= rhs as u8,
                VariableData::Int16(rhs) => *lhs &= rhs as u8,
                VariableData::Int32(rhs) => *lhs &= rhs as u8,
                VariableData::Int64(rhs) => *lhs &= rhs as u8,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u16,
                VariableData::UInt16(rhs) => *lhs &= rhs as u16,
                VariableData::UInt32(rhs) => *lhs &= rhs as u16,
                VariableData::UInt64(rhs) => *lhs &= rhs as u16,
                VariableData::Int8(rhs) => *lhs &= rhs as u16,
                VariableData::Int16(rhs) => *lhs &= rhs as u16,
                VariableData::Int32(rhs) => *lhs &= rhs as u16,
                VariableData::Int64(rhs) => *lhs &= rhs as u16,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u32,
                VariableData::UInt16(rhs) => *lhs &= rhs as u32,
                VariableData::UInt32(rhs) => *lhs &= rhs as u32,
                VariableData::UInt64(rhs) => *lhs &= rhs as u32,
                VariableData::Int8(rhs) => *lhs &= rhs as u32,
                VariableData::Int16(rhs) => *lhs &= rhs as u32,
                VariableData::Int32(rhs) => *lhs &= rhs as u32,
                VariableData::Int64(rhs) => *lhs &= rhs as u32,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as u64,
                VariableData::UInt16(rhs) => *lhs &= rhs as u64,
                VariableData::UInt32(rhs) => *lhs &= rhs as u64,
                VariableData::UInt64(rhs) => *lhs &= rhs as u64,
                VariableData::Int8(rhs) => *lhs &= rhs as u64,
                VariableData::Int16(rhs) => *lhs &= rhs as u64,
                VariableData::Int32(rhs) => *lhs &= rhs as u64,
                VariableData::Int64(rhs) => *lhs &= rhs as u64,
                _ => panic!("Invalid type for bitwise and assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i8,
                VariableData::UInt16(rhs) => *lhs &= rhs as i8,
                VariableData::UInt32(rhs) => *lhs &= rhs as i8,
                VariableData::UInt64(rhs) => *lhs &= rhs as i8,
                VariableData::Int8(rhs) => *lhs &= rhs as i8,
                VariableData::Int16(rhs) => *lhs &= rhs as i8,
                VariableData::Int32(rhs) => *lhs &= rhs as i8,
                VariableData::Int64(rhs) => *lhs &= rhs as i8,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i16,
                VariableData::UInt16(rhs) => *lhs &= rhs as i16,
                VariableData::UInt32(rhs) => *lhs &= rhs as i16,
                VariableData::UInt64(rhs) => *lhs &= rhs as i16,
                VariableData::Int8(rhs) => *lhs &= rhs as i16,
                VariableData::Int16(rhs) => *lhs &= rhs as i16,
                VariableData::Int32(rhs) => *lhs &= rhs as i16,
                VariableData::Int64(rhs) => *lhs &= rhs as i16,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i32,
                VariableData::UInt16(rhs) => *lhs &= rhs as i32,
                VariableData::UInt32(rhs) => *lhs &= rhs as i32,
                VariableData::UInt64(rhs) => *lhs &= rhs as i32,
                VariableData::Int8(rhs) => *lhs &= rhs as i32,
                VariableData::Int16(rhs) => *lhs &= rhs as i32,
                VariableData::Int32(rhs) => *lhs &= rhs as i32,
                VariableData::Int64(rhs) => *lhs &= rhs as i32,
                _ => panic!("Invalid type for bitwise and assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs &= rhs as i64,
                VariableData::UInt16(rhs) => *lhs &= rhs as i64,
                VariableData::UInt32(rhs) => *lhs &= rhs as i64,
                VariableData::UInt64(rhs) => *lhs &= rhs as i64,
                VariableData::Int8(rhs) => *lhs &= rhs as i64,
                VariableData::Int16(rhs) => *lhs &= rhs as i64,
                VariableData::Int32(rhs) => *lhs &= rhs as i64,
                VariableData::Int64(rhs) => *lhs &= rhs as i64,
                _ => panic!("Invalid type for bitwise and assign"),
            },

            _ => panic!("Invalid type for bitwise and assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BitwiseOrAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl BitwiseOrAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u8,
                VariableData::UInt16(rhs) => *lhs |= rhs as u8,
                VariableData::UInt32(rhs) => *lhs |= rhs as u8,
                VariableData::UInt64(rhs) => *lhs |= rhs as u8,
                VariableData::Int8(rhs) => *lhs |= rhs as u8,
                VariableData::Int16(rhs) => *lhs |= rhs as u8,
                VariableData::Int32(rhs) => *lhs |= rhs as u8,
                VariableData::Int64(rhs) => *lhs |= rhs as u8,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u16,
                VariableData::UInt16(rhs) => *lhs |= rhs as u16,
                VariableData::UInt32(rhs) => *lhs |= rhs as u16,
                VariableData::UInt64(rhs) => *lhs |= rhs as u16,
                VariableData::Int8(rhs) => *lhs |= rhs as u16,
                VariableData::Int16(rhs) => *lhs |= rhs as u16,
                VariableData::Int32(rhs) => *lhs |= rhs as u16,
                VariableData::Int64(rhs) => *lhs |= rhs as u16,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u32,
                VariableData::UInt16(rhs) => *lhs |= rhs as u32,
                VariableData::UInt32(rhs) => *lhs |= rhs as u32,
                VariableData::UInt64(rhs) => *lhs |= rhs as u32,
                VariableData::Int8(rhs) => *lhs |= rhs as u32,
                VariableData::Int16(rhs) => *lhs |= rhs as u32,
                VariableData::Int32(rhs) => *lhs |= rhs as u32,
                VariableData::Int64(rhs) => *lhs |= rhs as u32,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as u64,
                VariableData::UInt16(rhs) => *lhs |= rhs as u64,
                VariableData::UInt32(rhs) => *lhs |= rhs as u64,
                VariableData::UInt64(rhs) => *lhs |= rhs as u64,
                VariableData::Int8(rhs) => *lhs |= rhs as u64,
                VariableData::Int16(rhs) => *lhs |= rhs as u64,
                VariableData::Int32(rhs) => *lhs |= rhs as u64,
                VariableData::Int64(rhs) => *lhs |= rhs as u64,
                _ => panic!("Invalid type for bitwise or assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i8,
                VariableData::UInt16(rhs) => *lhs |= rhs as i8,
                VariableData::UInt32(rhs) => *lhs |= rhs as i8,
                VariableData::UInt64(rhs) => *lhs |= rhs as i8,
                VariableData::Int8(rhs) => *lhs |= rhs as i8,
                VariableData::Int16(rhs) => *lhs |= rhs as i8,
                VariableData::Int32(rhs) => *lhs |= rhs as i8,
                VariableData::Int64(rhs) => *lhs |= rhs as i8,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i16,
                VariableData::UInt16(rhs) => *lhs |= rhs as i16,
                VariableData::UInt32(rhs) => *lhs |= rhs as i16,
                VariableData::UInt64(rhs) => *lhs |= rhs as i16,
                VariableData::Int8(rhs) => *lhs |= rhs as i16,
                VariableData::Int16(rhs) => *lhs |= rhs as i16,
                VariableData::Int32(rhs) => *lhs |= rhs as i16,
                VariableData::Int64(rhs) => *lhs |= rhs as i16,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i32,
                VariableData::UInt16(rhs) => *lhs |= rhs as i32,
                VariableData::UInt32(rhs) => *lhs |= rhs as i32,
                VariableData::UInt64(rhs) => *lhs |= rhs as i32,
                VariableData::Int8(rhs) => *lhs |= rhs as i32,
                VariableData::Int16(rhs) => *lhs |= rhs as i32,
                VariableData::Int32(rhs) => *lhs |= rhs as i32,
                VariableData::Int64(rhs) => *lhs |= rhs as i32,
                _ => panic!("Invalid type for bitwise or assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs |= rhs as i64,
                VariableData::UInt16(rhs) => *lhs |= rhs as i64,
                VariableData::UInt32(rhs) => *lhs |= rhs as i64,
                VariableData::UInt64(rhs) => *lhs |= rhs as i64,
                VariableData::Int8(rhs) => *lhs |= rhs as i64,
                VariableData::Int16(rhs) => *lhs |= rhs as i64,
                VariableData::Int32(rhs) => *lhs |= rhs as i64,
                VariableData::Int64(rhs) => *lhs |= rhs as i64,
                _ => panic!("Invalid type for bitwise or assign"),
            },

            _ => panic!("Invalid type for bitwise or assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BitwiseXorAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl BitwiseXorAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u8,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u8,
                VariableData::Int8(rhs) => *lhs ^= rhs as u8,
                VariableData::Int16(rhs) => *lhs ^= rhs as u8,
                VariableData::Int32(rhs) => *lhs ^= rhs as u8,
                VariableData::Int64(rhs) => *lhs ^= rhs as u8,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u16,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u16,
                VariableData::Int8(rhs) => *lhs ^= rhs as u16,
                VariableData::Int16(rhs) => *lhs ^= rhs as u16,
                VariableData::Int32(rhs) => *lhs ^= rhs as u16,
                VariableData::Int64(rhs) => *lhs ^= rhs as u16,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u32,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u32,
                VariableData::Int8(rhs) => *lhs ^= rhs as u32,
                VariableData::Int16(rhs) => *lhs ^= rhs as u32,
                VariableData::Int32(rhs) => *lhs ^= rhs as u32,
                VariableData::Int64(rhs) => *lhs ^= rhs as u32,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt16(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt32(rhs) => *lhs ^= rhs as u64,
                VariableData::UInt64(rhs) => *lhs ^= rhs as u64,
                VariableData::Int8(rhs) => *lhs ^= rhs as u64,
                VariableData::Int16(rhs) => *lhs ^= rhs as u64,
                VariableData::Int32(rhs) => *lhs ^= rhs as u64,
                VariableData::Int64(rhs) => *lhs ^= rhs as u64,
                _ => panic!("Invalid type for bitwise xor assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i8,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i8,
                VariableData::Int8(rhs) => *lhs ^= rhs as i8,
                VariableData::Int16(rhs) => *lhs ^= rhs as i8,
                VariableData::Int32(rhs) => *lhs ^= rhs as i8,
                VariableData::Int64(rhs) => *lhs ^= rhs as i8,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i16,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i16,
                VariableData::Int8(rhs) => *lhs ^= rhs as i16,
                VariableData::Int16(rhs) => *lhs ^= rhs as i16,
                VariableData::Int32(rhs) => *lhs ^= rhs as i16,
                VariableData::Int64(rhs) => *lhs ^= rhs as i16,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i32,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i32,
                VariableData::Int8(rhs) => *lhs ^= rhs as i32,
                VariableData::Int16(rhs) => *lhs ^= rhs as i32,
                VariableData::Int32(rhs) => *lhs ^= rhs as i32,
                VariableData::Int64(rhs) => *lhs ^= rhs as i32,
                _ => panic!("Invalid type for bitwise xor assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt16(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt32(rhs) => *lhs ^= rhs as i64,
                VariableData::UInt64(rhs) => *lhs ^= rhs as i64,
                VariableData::Int8(rhs) => *lhs ^= rhs as i64,
                VariableData::Int16(rhs) => *lhs ^= rhs as i64,
                VariableData::Int32(rhs) => *lhs ^= rhs as i64,
                VariableData::Int64(rhs) => *lhs ^= rhs as i64,
                _ => panic!("Invalid type for bitwise xor assign"),
            },

            _ => panic!("Invalid type for bitwise xor assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ShiftLeftAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl ShiftLeftAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shl(rhs as u32).0,
                _ => panic!("Invalid type for left shift assign"),
            },
            _ => panic!("Invalid type for left shift assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ShiftRightAssign {
    pub lhs: Operand,
    pub rhs: Operand,
}
impl ShiftRightAssign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs).clone();
        let lhs = get_operand_value_mut(program, &self.lhs);

        match lhs {
            VariableData::UInt8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::UInt64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },

            VariableData::Int8(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int16(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int32(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            VariableData::Int64(ref mut lhs) => match rhs {
                VariableData::UInt8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::UInt64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int8(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int16(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int32(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                VariableData::Int64(rhs) => *lhs = lhs.overflowing_shr(rhs as u32).0,
                _ => panic!("Invalid type for right shift assign"),
            },
            _ => panic!("Invalid type for right shift assign"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs_type: TypeInfo,
    pub lhs: Operand,
    pub rhs: Operand,
}
impl Assign {
    fn execute(&self, program: &mut VirtualMachine) {
        let rhs = get_operand_value(program, &self.rhs)
            .cast_to(&self.lhs_type)
            .expect(format!("Invalid cast to {:?}", &self.lhs_type).as_str());

        *get_operand_value_mut(program, &self.lhs) = rhs;
    }
}

#[derive(Debug, Clone)]
pub struct AssignStruct {
    pub count: usize,
    pub lhs: Operand,
    pub rhs: Operand,
}
impl AssignStruct {
    fn execute(&self, program: &mut VirtualMachine) {
        let lhs_address = get_operand_value(program, &self.lhs).to_u64() as usize;
        let rhs_address = get_operand_value(program, &self.rhs).to_u64() as usize;

        for i in 0..self.count {
            program.stack[lhs_address + i] = program.stack[rhs_address + i].clone();
        }
    }
}
