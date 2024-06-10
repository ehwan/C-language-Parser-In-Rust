use super::Instruction;

use crate::program::variable::VariableData;

#[derive(Debug)]
pub struct GetArrayElement {}
impl Instruction for GetArrayElement {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let src = program.registers[1].borrow().1.clone();
        let index = program.registers[0].borrow().1.to_u64();

        if let VariableData::Array(arrs) = src {
            let element = arrs[index as usize].clone();
            program.registers[0] = element;
        } else {
            panic!("GetArrayElement: src is not an array");
        }
    }
}

#[derive(Debug)]
pub struct GetStructElement {
    pub name: String,
}
impl Instruction for GetStructElement {
    fn execute(&self, program: &mut crate::program::program::Program) {
        let src = program.registers[0].borrow().1.clone();

        if let VariableData::Struct(structs) = src {
            let element = structs
                .fields
                .get(&self.name)
                .expect(format!("GetStructElement: field {} not found", self.name).as_str())
                .clone();
            program.registers[0] = element;
        } else {
            panic!("GetStructElement: src is not a struct");
        }
    }
}
