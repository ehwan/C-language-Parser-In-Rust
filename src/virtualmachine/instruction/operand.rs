use crate::virtualmachine::program::VirtualProgram;
use crate::virtualmachine::variable::VariableData;

/// Type for Operand
/// Derefed: [rax]
/// Register: rax
/// Value: constant value
#[derive(Debug, Clone)]
pub enum Operand {
    Derefed(usize),  // number of register
    Register(usize), // number of register
    Value(VariableData),
}
/// helper function
pub(crate) fn get_operand_value<'a>(
    // helper function
    program: &'a VirtualProgram,
    operand: &'a Operand,
) -> &'a VariableData {
    match operand {
        Operand::Derefed(register) => {
            &program.stack[program.registers[*register].to_u64() as usize]
        }
        Operand::Register(register) => &program.registers[*register],
        Operand::Value(val) => val,
    }
}
/// helper function
pub(crate) fn get_operand_value_mut<'a>(
    // helper function
    program: &'a mut VirtualProgram,
    operand: &'a Operand,
) -> &'a mut VariableData {
    match operand {
        Operand::Derefed(register) => {
            &mut program.stack[program.registers[*register].to_u64() as usize]
        }
        Operand::Register(register) => &mut program.registers[*register],
        Operand::Value(_) => {
            panic!("get_operand_value_mut: cannot get mutable reference from value")
        }
    }
}
