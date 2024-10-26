use super::typename::TypeInfo;
use crate::virtualmachine::instruction::generation::InstructionGenerator;
use crate::virtualmachine::instruction::generation::VariableOffset;
use crate::virtualmachine::instruction::operand::Operand;
use crate::virtualmachine::instruction::*;
use crate::virtualmachine::program::STACK_POINTER_BASE_REGISTER;
use crate::virtualmachine::program::STACK_POINTER_REGISTER;
use crate::virtualmachine::program::TEXT_SIZE_REGISTER;
use crate::virtualmachine::variable::VariableData;

#[derive(Debug, Clone)]
pub enum Expression {
    Void(ExprVoid),
    PrimaryIdentifier(ExprPrimaryIdentifier),
    PostMember(ExprPostMember),
    ConstantInteger(ExprConstantInteger),
    ConstantUnsignedInteger(ExprConstantUnsignedInteger),
    ConstantCharacter(ExprConstantCharacter),
    ConstantLong(ExprConstantLong),
    ConstantUnsignedLong(ExprConstantUnsignedLong),
    ConstantFloat(ExprConstantFloat),
    ConstantDouble(ExprConstantDouble),
    StringLiteral(ExprString),
    PostBracket(ExprPostBracket),
    PostParen(ExprPostParen),
    PostIncrement(ExprPostIncrement),
    PostDecrement(ExprPostDecrement),
    Cast(ExprCast),
    SizeofType(ExprSizeOfType),
    SizeofExpr(ExprSizeOfExpr),
    Unary(ExprUnary),
    LogicalBinary(ExprLogicalBinary),
    Comparison(ExprComparison),
    Assign(ExprAssign),
    AssignOp(ExprAssignOp),

    Additive(ExprAdditive),
    Multiplicative(ExprMultiplicative),
    Shift(ExprShift),
    Bitwise(ExprBitwise),
    PostArrow(ExprPostArrow),
    Conditional(ExprConditional),
    Comma(ExprComma),
    InitializerList(ExprInitializerList),
}

impl Expression {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        match self {
            Expression::Void(expr) => expr.emit(instructions),
            Expression::PrimaryIdentifier(expr) => expr.emit(instructions),
            Expression::PostMember(expr) => expr.emit(instructions),
            Expression::ConstantInteger(expr) => expr.emit(instructions),
            Expression::ConstantUnsignedInteger(expr) => expr.emit(instructions),
            Expression::ConstantCharacter(expr) => expr.emit(instructions),
            Expression::ConstantLong(expr) => expr.emit(instructions),
            Expression::ConstantUnsignedLong(expr) => expr.emit(instructions),
            Expression::ConstantFloat(expr) => expr.emit(instructions),
            Expression::ConstantDouble(expr) => expr.emit(instructions),
            Expression::StringLiteral(expr) => expr.emit(instructions),
            Expression::PostBracket(expr) => expr.emit(instructions),
            Expression::PostParen(expr) => expr.emit(instructions),
            Expression::PostIncrement(expr) => expr.emit(instructions),
            Expression::PostDecrement(expr) => expr.emit(instructions),
            Expression::Cast(expr) => expr.emit(instructions),
            Expression::SizeofType(expr) => expr.emit(instructions),
            Expression::SizeofExpr(expr) => expr.emit(instructions),
            Expression::Unary(expr) => expr.emit(instructions),
            Expression::LogicalBinary(expr) => expr.emit(instructions),
            Expression::Comparison(expr) => expr.emit(instructions),
            Expression::Assign(expr) => expr.emit(instructions),
            Expression::AssignOp(expr) => expr.emit(instructions),
            Expression::Additive(expr) => expr.emit(instructions),
            Expression::Multiplicative(expr) => expr.emit(instructions),
            Expression::Shift(expr) => expr.emit(instructions),
            Expression::Bitwise(expr) => expr.emit(instructions),
            Expression::PostArrow(expr) => expr.emit(instructions),
            Expression::Conditional(expr) => expr.emit(instructions),
            Expression::Comma(expr) => expr.emit(instructions),
            Expression::InitializerList(expr) => expr.emit(instructions),
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self {
            Expression::Void(expr) => expr.get_typeinfo(instructions),
            Expression::PrimaryIdentifier(expr) => expr.get_typeinfo(instructions),
            Expression::PostMember(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantInteger(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantUnsignedInteger(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantCharacter(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantLong(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantUnsignedLong(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantFloat(expr) => expr.get_typeinfo(instructions),
            Expression::ConstantDouble(expr) => expr.get_typeinfo(instructions),
            Expression::StringLiteral(expr) => expr.get_typeinfo(instructions),
            Expression::PostBracket(expr) => expr.get_typeinfo(instructions),
            Expression::PostParen(expr) => expr.get_typeinfo(instructions),
            Expression::PostIncrement(expr) => expr.get_typeinfo(instructions),
            Expression::PostDecrement(expr) => expr.get_typeinfo(instructions),
            Expression::Cast(expr) => expr.get_typeinfo(instructions),
            Expression::SizeofType(expr) => expr.get_typeinfo(instructions),
            Expression::SizeofExpr(expr) => expr.get_typeinfo(instructions),
            Expression::Unary(expr) => expr.get_typeinfo(instructions),
            Expression::LogicalBinary(expr) => expr.get_typeinfo(instructions),
            Expression::Comparison(expr) => expr.get_typeinfo(instructions),
            Expression::Assign(expr) => expr.get_typeinfo(instructions),
            Expression::AssignOp(expr) => expr.get_typeinfo(instructions),
            Expression::Additive(expr) => expr.get_typeinfo(instructions),
            Expression::Multiplicative(expr) => expr.get_typeinfo(instructions),
            Expression::Shift(expr) => expr.get_typeinfo(instructions),
            Expression::Bitwise(expr) => expr.get_typeinfo(instructions),
            Expression::PostArrow(expr) => expr.get_typeinfo(instructions),
            Expression::Conditional(expr) => expr.get_typeinfo(instructions),
            Expression::Comma(expr) => expr.get_typeinfo(instructions),
            Expression::InitializerList(expr) => expr.get_typeinfo(instructions),
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self {
            Expression::Void(expr) => expr.is_return_reference(instructions),
            Expression::PrimaryIdentifier(expr) => expr.is_return_reference(instructions),
            Expression::PostMember(expr) => expr.is_return_reference(instructions),
            Expression::ConstantInteger(expr) => expr.is_return_reference(instructions),
            Expression::ConstantUnsignedInteger(expr) => expr.is_return_reference(instructions),
            Expression::ConstantCharacter(expr) => expr.is_return_reference(instructions),
            Expression::ConstantLong(expr) => expr.is_return_reference(instructions),
            Expression::ConstantUnsignedLong(expr) => expr.is_return_reference(instructions),
            Expression::ConstantFloat(expr) => expr.is_return_reference(instructions),
            Expression::ConstantDouble(expr) => expr.is_return_reference(instructions),
            Expression::StringLiteral(expr) => expr.is_return_reference(instructions),
            Expression::PostBracket(expr) => expr.is_return_reference(instructions),
            Expression::PostParen(expr) => expr.is_return_reference(instructions),
            Expression::PostIncrement(expr) => expr.is_return_reference(instructions),
            Expression::PostDecrement(expr) => expr.is_return_reference(instructions),
            Expression::Cast(expr) => expr.is_return_reference(instructions),
            Expression::SizeofType(expr) => expr.is_return_reference(instructions),
            Expression::SizeofExpr(expr) => expr.is_return_reference(instructions),
            Expression::Unary(expr) => expr.is_return_reference(instructions),
            Expression::LogicalBinary(expr) => expr.is_return_reference(instructions),
            Expression::Comparison(expr) => expr.is_return_reference(instructions),
            Expression::Assign(expr) => expr.is_return_reference(instructions),
            Expression::AssignOp(expr) => expr.is_return_reference(instructions),
            Expression::Additive(expr) => expr.is_return_reference(instructions),
            Expression::Multiplicative(expr) => expr.is_return_reference(instructions),
            Expression::Shift(expr) => expr.is_return_reference(instructions),
            Expression::Bitwise(expr) => expr.is_return_reference(instructions),
            Expression::PostArrow(expr) => expr.is_return_reference(instructions),
            Expression::Conditional(expr) => expr.is_return_reference(instructions),
            Expression::Comma(expr) => expr.is_return_reference(instructions),
            Expression::InitializerList(expr) => expr.is_return_reference(instructions),
        }
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        match self {
            Expression::ConstantInteger(expr) => expr.get_constant_i64(),
            Expression::ConstantUnsignedInteger(expr) => expr.get_constant_i64(),
            Expression::ConstantCharacter(expr) => expr.get_constant_i64(),
            Expression::ConstantLong(expr) => expr.get_constant_i64(),
            Expression::ConstantUnsignedLong(expr) => expr.get_constant_i64(),
            Expression::SizeofType(expr) => expr.get_constant_i64(),
            _ => Err("Not a constant expression".to_string()),
        }
    }
}

/// expression that always returns true int32(1)
#[derive(Debug, Clone)]
pub struct ExprVoid {}
impl ExprVoid {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Int32(1)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int32
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// for variable
#[derive(Debug, Clone)]
pub struct ExprPrimaryIdentifier {
    pub name: String,
}
// push pointer
impl ExprPrimaryIdentifier {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let offset = instructions
            .search_variable(&self.name)
            .expect(format!("Variable {} not found", self.name).as_str())
            .1;
        match offset {
            VariableOffset::Global(absolute_offset) => {
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt64(absolute_offset as u64)),
                    operand_to: Operand::Register(0),
                }));
                instructions.push(Instruction::AddAssign(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(TEXT_SIZE_REGISTER),
                }));
            }
            VariableOffset::Local(relative_offset) => {
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Register(STACK_POINTER_BASE_REGISTER),
                    operand_to: Operand::Register(0),
                }));
                instructions.push(Instruction::AddAssign(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Value(VariableData::Int64(relative_offset as i64)),
                }));
            }
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions).remove_const() {
            TypeInfo::Array(_, _) => false,
            _ => true,
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        instructions
            .search_variable(&self.name)
            .expect(format!("Variable {} not found", self.name).as_str())
            .0
    }
}

/// for struct member access
#[derive(Debug, Clone)]
pub struct ExprPostMember {
    pub src: Box<Expression>,
    pub member: String,
}
impl ExprPostMember {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let member_offset = match self.src.get_typeinfo(instructions).remove_const() {
            TypeInfo::Struct(sinfo) => {
                let mut member_offset: Option<usize> = None;
                for (_, name, offset) in sinfo.fields.as_ref().unwrap() {
                    if name == &self.member {
                        member_offset = Some(*offset);
                        break;
                    }
                }
                member_offset.expect(format!("Field {} not found", self.member).as_str())
            }
            _ => panic!("PostMember on non-struct type"),
        };
        self.src.emit(instructions);
        instructions.push(Instruction::AddAssign(AddAssign {
            lhs: Operand::Register(0),
            rhs: Operand::Value(VariableData::UInt64(member_offset as u64)),
        }));
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let src_type = self.src.get_typeinfo(instructions);
        match src_type.remove_const() {
            TypeInfo::Struct(sinfo) => {
                let mut member_type: Option<TypeInfo> = None;
                for (t, name, _) in sinfo.fields.as_ref().unwrap() {
                    if name == &self.member {
                        member_type = Some(t.clone());
                        break;
                    }
                }
                let member_type =
                    member_type.expect(format!("Field {} not found", self.member).as_str());
                if src_type.is_const() {
                    member_type.add_const()
                } else {
                    member_type
                }
            }
            _ => panic!("PostMember on non-struct type"),
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions).remove_const() {
            TypeInfo::Array(_, _) => false,
            _ => true,
        }
    }
}

/// constant integer
#[derive(Debug, Clone)]
pub struct ExprConstantInteger {
    pub value: i32,
}
impl ExprConstantInteger {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Int32(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int32
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// constant unsigned integer
#[derive(Debug, Clone)]
pub struct ExprConstantUnsignedInteger {
    pub value: u32,
}
impl ExprConstantUnsignedInteger {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt32(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt32
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// 'c' character
#[derive(Debug, Clone)]
pub struct ExprConstantCharacter {
    pub value: i8,
}
impl ExprConstantCharacter {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Int8(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int8
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// constant long
#[derive(Debug, Clone)]
pub struct ExprConstantLong {
    pub value: i64,
}
impl ExprConstantLong {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Int64(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Int64
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// constant unsigned long
#[derive(Debug, Clone)]
pub struct ExprConstantUnsignedLong {
    pub value: u64,
}
impl ExprConstantUnsignedLong {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.value as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// constant float
#[derive(Debug, Clone)]
pub struct ExprConstantFloat {
    pub value: f32,
}
impl ExprConstantFloat {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Float32(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Float32
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// constant double
#[derive(Debug, Clone)]
pub struct ExprConstantDouble {
    pub value: f64,
}
impl ExprConstantDouble {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::Float64(self.value)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Float64
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// for string literal
#[derive(Debug, Clone)]
pub struct ExprString {
    pub value: String,
}
impl ExprString {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        // store string to global text section
        let offset = instructions.text_section.len(); // <-- this will be absolute address

        let mut null_terminated = self.value.clone();
        null_terminated.push('\0');
        instructions
            .text_section
            .append(&mut null_terminated.as_bytes().to_vec());

        // put its address to register0
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(offset as u64)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::Pointer(Box::new(TypeInfo::Const(Box::new(TypeInfo::Int8))))
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// src[index]
#[derive(Debug, Clone)]
pub struct ExprPostBracket {
    pub src: Box<Expression>,
    pub index: Box<Expression>,
}
impl ExprPostBracket {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        match self.src.get_typeinfo(instructions).remove_const() {
            TypeInfo::Array(_, _) => {}
            TypeInfo::Pointer(_) => {}
            _ => panic!("Bracket is only available at array or pointer type"),
        }
        self.index.emit(instructions);
        if self.index.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }
        // register0 = src
        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        }
        // register1 = index
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        match self.src.get_typeinfo(instructions).remove_const() {
            TypeInfo::Array(t, _) => {
                let move_size = t.number_of_primitives();
                instructions.push(Instruction::MulAssign(MulAssign {
                    lhs: Operand::Register(1),
                    rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                }));
                instructions.push(Instruction::AddAssign(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            TypeInfo::Pointer(t) => {
                let move_size = t.number_of_primitives();
                instructions.push(Instruction::MulAssign(MulAssign {
                    lhs: Operand::Register(1),
                    rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                }));
                instructions.push(Instruction::AddAssign(AddAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            _ => panic!("Bracket is only available at array or pointer type"),
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let src_type = self.src.get_typeinfo(instructions);
        match src_type.remove_const() {
            TypeInfo::Array(t, _) => {
                if src_type.is_const() {
                    t.add_const()
                } else {
                    *t
                }
            }
            TypeInfo::Pointer(t) => *t,
            _ => panic!("Bracket is only available at array or pointer type"),
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        true
    }
}

/// src( args... )
#[derive(Debug, Clone)]
pub struct ExprPostParen {
    pub src: Box<Expression>,
    pub args: Vec<Expression>,
}
// currently only for function call
impl ExprPostParen {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let identifier = match &*self.src {
            Expression::PrimaryIdentifier(identifier) => identifier,
            _ => panic!("FunctionCall must be called on `Identifier`"),
        };
        let name = identifier.name.clone();

        // check if it is a built-in function, print or print_str
        if &name == "print" {
            for arg in self.args.iter().rev() {
                arg.emit(instructions);
                if arg.is_return_reference(instructions) {
                    instructions.push(Instruction::PushStack(PushStack {
                        operand: Operand::Derefed(0, 0),
                    }));
                } else {
                    instructions.push(Instruction::PushStack(PushStack {
                        operand: Operand::Register(0),
                    }));
                }
            }
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Value(VariableData::UInt64(self.args.len() as u64)),
            }));
            instructions.push(Instruction::Print(Print {}));
        } else if &name == "print_str" {
            if self.args.len() != 1 {
                panic!(
                    "print_str expects 1 argument, but {} were provided",
                    self.args.len()
                );
            }
            self.args[0].emit(instructions);
            if self.args[0].is_return_reference(instructions) {
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Derefed(0, 0),
                    operand_to: Operand::Register(0),
                }));
            }
            instructions.push(Instruction::PrintStr(PrintStr {
                str: Operand::Register(0),
            }));
        } else {
            let funcdata = instructions
                .functions
                .get(&name)
                .expect(format!("Function not found : {}", &name).as_str())
                .clone();
            if funcdata.params.len() != self.args.len() {
                panic!(
                    "Function {} expects {} arguments, but {} were provided",
                    name,
                    funcdata.params.len(),
                    self.args.len()
                );
            }

            // push arguments to stack
            for param in self.args.iter().rev() {
                param.emit(instructions);
                if param.is_return_reference(instructions) {
                    instructions.push(Instruction::PushStack(PushStack {
                        operand: Operand::Derefed(0, 0),
                    }));
                } else {
                    instructions.push(Instruction::PushStack(PushStack {
                        operand: Operand::Register(0),
                    }));
                }
            }

            // call function
            instructions.push(Instruction::Call(Call {
                label: name.clone(),
            }));

            // pop arguments from stack
            instructions.push(Instruction::SubAssign(SubAssign {
                lhs: Operand::Register(STACK_POINTER_REGISTER),
                rhs: Operand::Value(VariableData::UInt64(funcdata.params.len() as u64)),
            }));
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let identifier = match &*self.src {
            Expression::PrimaryIdentifier(identifier) => identifier,
            _ => panic!("FunctionCall must be called on `Identifier`"),
        };
        let name = identifier.name.clone();
        let funcdata = &instructions
            .functions
            .get(&name)
            .expect(format!("Function not found : {}", &name).as_str());
        instructions
            .get_true_typeinfo(&funcdata.return_type)
            .clone()
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// src++
#[derive(Debug, Clone)]
pub struct ExprPostIncrement {
    pub src: Box<Expression>,
}
impl ExprPostIncrement {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        if self.src.is_return_reference(instructions) == false {
            panic!("PostIncrement on non-lhs");
        }
        if self.src.get_typeinfo(instructions).is_const() {
            panic!("PostIncrement on const variable");
        }
        self.src.emit(instructions);
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Register(0),
            operand_to: Operand::Register(1),
        }));
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Derefed(1, 0),
            operand_to: Operand::Register(0),
        }));
        if let TypeInfo::Pointer(t) = self.src.get_typeinfo(instructions) {
            let move_size = t.number_of_primitives();
            instructions.push(Instruction::AddAssign(AddAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
            }));
        } else {
            instructions.push(Instruction::AddAssign(AddAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt8(1)),
            }));
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.src.get_typeinfo(instructions)
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// src--
#[derive(Debug, Clone)]
pub struct ExprPostDecrement {
    pub src: Box<Expression>,
}
impl ExprPostDecrement {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        if self.src.is_return_reference(instructions) == false {
            panic!("PostDecrement on non-lhs");
        }
        if self.src.get_typeinfo(instructions).is_const() {
            panic!("PostDecrement on const variable");
        }
        self.src.emit(instructions);
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Register(0),
            operand_to: Operand::Register(1),
        }));
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Derefed(1, 0),
            operand_to: Operand::Register(0),
        }));

        if let TypeInfo::Pointer(t) = self.src.get_typeinfo(instructions) {
            let move_size = t.number_of_primitives();
            instructions.push(Instruction::SubAssign(SubAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
            }));
        } else {
            instructions.push(Instruction::SubAssign(SubAssign {
                lhs: Operand::Derefed(1, 0),
                rhs: Operand::Value(VariableData::UInt8(1)),
            }));
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.src.get_typeinfo(instructions)
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// (typeinfo)src
#[derive(Debug, Clone)]
pub struct ExprCast {
    pub src: Box<Expression>,
    pub typeinfo: TypeInfo,
}
impl ExprCast {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(Instruction::Cast(Cast {
                info: instructions
                    .get_true_typeinfo(&self.typeinfo)
                    .remove_const(),
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        } else {
            instructions.push(Instruction::Cast(Cast {
                info: instructions
                    .get_true_typeinfo(&self.typeinfo)
                    .remove_const(),
                operand_from: Operand::Register(0),
                operand_to: Operand::Register(0),
            }));
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        instructions
            .get_true_typeinfo(&self.typeinfo)
            .remove_const()
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// sizeof(typeinfo)
#[derive(Debug, Clone)]
pub struct ExprSizeOfType {
    pub typeinfo: TypeInfo,
}
impl ExprSizeOfType {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(self.typeinfo.sizeof() as u64)),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_constant_i64(&self) -> Result<i64, String> {
        Ok(self.typeinfo.sizeof() as i64)
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

/// sizeof(expr)
#[derive(Debug, Clone)]
pub struct ExprSizeOfExpr {
    pub expr: Box<Expression>,
}
impl ExprSizeOfExpr {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        instructions.push(Instruction::MoveRegister(MoveRegister {
            operand_from: Operand::Value(VariableData::UInt64(
                self.expr.get_typeinfo(instructions).sizeof() as u64,
            )),
            operand_to: Operand::Register(0),
        }));
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        TypeInfo::UInt64
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    Dereference,
    AddressOf,
    Increment,
    Decrement,
}
#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: UnaryOperator,
    pub src: Box<Expression>,
}
impl ExprUnary {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.src.emit(instructions);

        let src_type = self.src.get_typeinfo(instructions);

        match self.op {
            UnaryOperator::Plus => {
                match src_type.remove_const() {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64
                    | TypeInfo::Float32
                    | TypeInfo::Float64
                    | TypeInfo::Pointer(_)
                    | TypeInfo::Array(_, _) => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        }
                    }
                    _ => panic!(
                        "Unary Plus not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Minus => match src_type.remove_const() {
                TypeInfo::Int8
                | TypeInfo::UInt8
                | TypeInfo::Int16
                | TypeInfo::UInt16
                | TypeInfo::Int32
                | TypeInfo::UInt32
                | TypeInfo::Int64
                | TypeInfo::UInt64
                | TypeInfo::Float32
                | TypeInfo::Float64
                | TypeInfo::Pointer(_)
                | TypeInfo::Array(_, _) => {
                    if self.src.is_return_reference(instructions) {
                        instructions.push(Instruction::MoveRegister(MoveRegister {
                            operand_from: Operand::Derefed(0, 0),
                            operand_to: Operand::Register(0),
                        }));
                    }
                    instructions.push(Instruction::Negate(Negate {
                        operand: Operand::Register(0),
                    }));
                }
                _ => panic!(
                    "Unary Minus not implemented for {:?}",
                    self.src.get_typeinfo(instructions)
                ),
            },
            UnaryOperator::LogicalNot => {
                match src_type.remove_const() {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64
                    | TypeInfo::Pointer(_)
                    | TypeInfo::Array(_, _) => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        }
                        instructions.push(Instruction::LogicalNot(LogicalNot {
                            operand: Operand::Register(0),
                        }));
                    }
                    _ => panic!(
                        "Unary LogicalNot not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::BitwiseNot => {
                match src_type.remove_const() {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64 => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        }
                        instructions.push(Instruction::BitwiseNot(BitwiseNot {
                            operand: Operand::Register(0),
                        }));
                    }
                    _ => panic!(
                        "Unary BitwiseNot not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(_) = src_type.remove_const() {
                    if self.src.is_return_reference(instructions) {
                        instructions.push(Instruction::MoveRegister(MoveRegister {
                            operand_from: Operand::Derefed(0, 0),
                            operand_to: Operand::Register(0),
                        }));
                    } else {
                        instructions.push(Instruction::MoveRegister(MoveRegister {
                            operand_from: Operand::Register(0),
                            operand_to: Operand::Register(0),
                        }));
                    }
                } else {
                    panic!(
                        "Dereference on non-pointer type :{:?}",
                        self.src.get_typeinfo(instructions)
                    );
                }
            }
            UnaryOperator::AddressOf => {
                if self.src.is_return_reference(instructions) == false {
                    panic!("AddressOf on non-reference");
                }
            }
            UnaryOperator::Increment => {
                if src_type.is_const() {
                    panic!("Increment on const variable");
                }
                match src_type {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64 => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::AddAssign(AddAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt8(1)),
                            }));
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        } else {
                            panic!("Increment on non-reference");
                        }
                    }
                    TypeInfo::Pointer(t) => {
                        let move_size = t.number_of_primitives();
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::AddAssign(AddAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                            }));
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        } else {
                            panic!("Increment on non-reference");
                        }
                    }
                    _ => panic!(
                        "Unary Increment not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
            UnaryOperator::Decrement => {
                if src_type.is_const() {
                    panic!("Decrement on const variable");
                }
                match src_type {
                    TypeInfo::Int8
                    | TypeInfo::UInt8
                    | TypeInfo::Int16
                    | TypeInfo::UInt16
                    | TypeInfo::Int32
                    | TypeInfo::UInt32
                    | TypeInfo::Int64
                    | TypeInfo::UInt64 => {
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::SubAssign(SubAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt8(1)),
                            }));
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        } else {
                            panic!("Decrement on non-reference");
                        }
                    }
                    TypeInfo::Pointer(t) => {
                        let move_size = t.number_of_primitives();
                        if self.src.is_return_reference(instructions) {
                            instructions.push(Instruction::SubAssign(SubAssign {
                                lhs: Operand::Derefed(0, 0),
                                rhs: Operand::Value(VariableData::UInt64(move_size as u64)),
                            }));
                            instructions.push(Instruction::MoveRegister(MoveRegister {
                                operand_from: Operand::Derefed(0, 0),
                                operand_to: Operand::Register(0),
                            }));
                        } else {
                            panic!("Decrement on non-reference");
                        }
                    }
                    _ => panic!(
                        "Unary Decrement not implemented for {:?}",
                        self.src.get_typeinfo(instructions)
                    ),
                };
            }
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let srctype = self.src.get_typeinfo(instructions);
        match self.op {
            UnaryOperator::Plus => srctype.remove_const(),
            UnaryOperator::Minus => match srctype.remove_const() {
                TypeInfo::Int8 => TypeInfo::Int8,
                TypeInfo::UInt8 => TypeInfo::Int8,
                TypeInfo::Int16 => TypeInfo::Int16,
                TypeInfo::UInt16 => TypeInfo::Int16,
                TypeInfo::Int32 => TypeInfo::Int32,
                TypeInfo::UInt32 => TypeInfo::Int32,
                TypeInfo::Int64 => TypeInfo::Int64,
                TypeInfo::UInt64 => TypeInfo::Int64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                TypeInfo::Pointer(t) => TypeInfo::Pointer(t),
                TypeInfo::Array(t, _) => TypeInfo::Pointer(t),
                _ => panic!("Unary Minus not implemented for {:?}", srctype),
            },
            UnaryOperator::LogicalNot => TypeInfo::UInt8,
            UnaryOperator::BitwiseNot => match srctype.remove_const() {
                TypeInfo::Int8 => TypeInfo::Int8,
                TypeInfo::UInt8 => TypeInfo::Int8,
                TypeInfo::Int16 => TypeInfo::Int16,
                TypeInfo::UInt16 => TypeInfo::Int16,
                TypeInfo::Int32 => TypeInfo::Int32,
                TypeInfo::UInt32 => TypeInfo::Int32,
                TypeInfo::Int64 => TypeInfo::Int64,
                TypeInfo::UInt64 => TypeInfo::Int64,
                _ => panic!("BitwiseNot not implemented for {:?}", srctype),
            },
            UnaryOperator::Dereference => {
                if let TypeInfo::Pointer(t) = srctype.remove_const() {
                    instructions.get_true_typeinfo(t.as_ref()).clone()
                } else {
                    panic!("Dereference on non-pointer type");
                }
            }
            UnaryOperator::AddressOf => {
                if self.src.is_return_reference(instructions) == false {
                    panic!("AddressOf on non-reference");
                }
                TypeInfo::Pointer(Box::new(srctype))
            }
            UnaryOperator::Increment | UnaryOperator::Decrement => srctype.remove_const(),
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        match self.op {
            UnaryOperator::Dereference => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    ShiftLeft,
    ShiftRight,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
}

/// for logical and, or
#[derive(Debug, Clone)]
pub struct ExprLogicalBinary {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprLogicalBinary {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let lhs_type = self.lhs.get_typeinfo(instructions);
        let rhs_type = self.rhs.get_typeinfo(instructions);
        match lhs_type.remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match rhs_type.remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        }
        match self.op {
            BinaryOperator::LogicalAnd => {
                let zero_label = instructions.get_unique_label();
                let end_label = instructions.get_unique_label();
                instructions.push(Instruction::JumpZero(JumpZero {
                    operand_cond: Operand::Register(0),
                    label: zero_label.clone(),
                }));

                // lhs is true here
                // eval rhs
                self.rhs.emit(instructions);
                if self.rhs.is_return_reference(instructions) {
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Derefed(0, 0),
                        operand_to: Operand::Register(0),
                    }));
                }
                instructions.push(Instruction::JumpZero(JumpZero {
                    label: zero_label.clone(),
                    operand_cond: Operand::Register(0),
                }));
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(1)),
                    operand_to: Operand::Register(0),
                }));
                instructions.push(Instruction::Jump(Jump {
                    label: end_label.clone(),
                }));

                instructions.set_label(&zero_label);
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(0)),
                    operand_to: Operand::Register(0),
                }));
                instructions.set_label(&end_label);
            }
            BinaryOperator::LogicalOr => {
                let one_label = instructions.get_unique_label();
                let end_label = instructions.get_unique_label();
                instructions.push(Instruction::JumpNonZero(JumpNonZero {
                    operand_cond: Operand::Register(0),
                    label: one_label.clone(),
                }));

                // lhs is false here
                // eval rhs
                self.rhs.emit(instructions);
                if self.rhs.is_return_reference(instructions) {
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Derefed(0, 0),
                        operand_to: Operand::Register(0),
                    }));
                }
                instructions.push(Instruction::JumpNonZero(JumpNonZero {
                    label: one_label.clone(),
                    operand_cond: Operand::Register(0),
                }));
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(0)),
                    operand_to: Operand::Register(0),
                }));
                instructions.push(Instruction::Jump(Jump {
                    label: end_label.clone(),
                }));

                instructions.set_label(&one_label);
                instructions.push(Instruction::MoveRegister(MoveRegister {
                    operand_from: Operand::Value(VariableData::UInt8(1)),
                    operand_to: Operand::Register(0),
                }));
                instructions.set_label(&end_label);
            }
            _ => {
                panic!(
                    "Invalid operator for LogicalBinaryExpression: {:?}",
                    self.op
                );
            }
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.lhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-integral type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        TypeInfo::UInt8
    }
}

#[derive(Debug, Clone)]
pub struct ExprComparison {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprComparison {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        match self.lhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-numeric type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-numeric type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        // eval lhs and push to stack
        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }

        // eval rhs
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        }

        // pop lhs from stack
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        let lhs_register = Operand::Register(1);
        let rhs_register = Operand::Register(0);

        match self.op {
            BinaryOperator::LessThan => {
                // lhs < rhs
                instructions.push(Instruction::LessThan(LessThan {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                }));
            }
            BinaryOperator::GreaterThan => {
                // lhs > rhs
                instructions.push(Instruction::LessThan(LessThan {
                    lhs: rhs_register,
                    rhs: lhs_register,
                    to: Operand::Register(0),
                }));
            }
            BinaryOperator::LessThanOrEqual => {
                // lhs <= rhs
                // !( lhs > rhs )
                instructions.push(Instruction::LessThan(LessThan {
                    lhs: rhs_register,
                    rhs: lhs_register,
                    to: Operand::Register(0),
                }));
                instructions.push(Instruction::LogicalNot(LogicalNot {
                    operand: Operand::Register(0),
                }));
            }
            BinaryOperator::GreaterThanOrEqual => {
                // lhs >= rhs
                // !( lhs < rhs )
                instructions.push(Instruction::LessThan(LessThan {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                }));
                instructions.push(Instruction::LogicalNot(LogicalNot {
                    operand: Operand::Register(0),
                }));
            }
            BinaryOperator::Equal => {
                // lhs == rhs
                instructions.push(Instruction::Equal(Equal {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                }));
            }
            BinaryOperator::NotEqual => {
                // !(lhs == rhs )
                instructions.push(Instruction::Equal(Equal {
                    lhs: lhs_register,
                    rhs: rhs_register,
                    to: Operand::Register(0),
                }));
                instructions.push(Instruction::LogicalNot(LogicalNot {
                    operand: Operand::Register(0),
                }));
            }
            _ => panic!("Invalid operator for ComparisonExpression: {:?}", self.op),
        }
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.lhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-int type (LHS:{:?})",
                self.op,
                self.lhs.get_typeinfo(instructions)
            ),
        }
        match self.rhs.get_typeinfo(instructions).remove_const() {
            TypeInfo::UInt8
            | TypeInfo::Int8
            | TypeInfo::UInt16
            | TypeInfo::Int16
            | TypeInfo::UInt32
            | TypeInfo::Int32
            | TypeInfo::UInt64
            | TypeInfo::Int64
            | TypeInfo::Float32
            | TypeInfo::Float64
            | TypeInfo::Pointer(_)
            | TypeInfo::Array(_, _) => {}
            _ => panic!(
                "{:?} on non-int type (RHS:{:?})",
                self.op,
                self.rhs.get_typeinfo(instructions)
            ),
        }

        TypeInfo::UInt8
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}
// lhs = rhs
#[derive(Debug, Clone)]
pub struct ExprAssign {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprAssign {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let lhs_type = self.lhs.get_typeinfo(instructions);
        // check lhs is not const
        if lhs_type.is_const() {
            panic!("Assign to const variable");
        }
        // check lhs is reference
        if self.lhs.is_return_reference(instructions) == false {
            panic!("Assign on non-reference");
        }

        let rhs_type = self.rhs.get_typeinfo(instructions);

        // check if it is non-numeric <-> non-numeric assignment
        if let TypeInfo::Struct(lhs_type) = lhs_type {
            if let TypeInfo::Struct(rhs_type) = rhs_type.remove_const() {
                if &lhs_type != &rhs_type {
                    panic!(
                        "Struct assignment with different types: {:?} = {:?}",
                        lhs_type, rhs_type
                    );
                }
                // struct = struct assignment
                self.rhs.emit(instructions);
                instructions.push(Instruction::PushStack(PushStack {
                    operand: Operand::Register(0),
                }));
                self.lhs.emit(instructions);
                instructions.push(Instruction::PopStack(PopStack {
                    operand: Operand::Register(1),
                }));

                let count = lhs_type.number_of_primitives();
                instructions.push(Instruction::AssignStruct(AssignStruct {
                    count,
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            } else {
                // struct = other assignment; panic
                panic!(
                    "Struct assignment with non-struct type: {:?} = {:?}",
                    lhs_type,
                    self.rhs.get_typeinfo(instructions)
                );
            }
        } else {
            // normal assignment

            if self.lhs.is_return_reference(instructions) == false {
                panic!("Assign on non-reference");
            }
            self.rhs.emit(instructions);
            if self.rhs.is_return_reference(instructions) {
                instructions.push(Instruction::PushStack(PushStack {
                    operand: Operand::Derefed(0, 0),
                }));
            } else {
                instructions.push(Instruction::PushStack(PushStack {
                    operand: Operand::Register(0),
                }));
            }
            self.lhs.emit(instructions);
            instructions.push(Instruction::PopStack(PopStack {
                operand: Operand::Register(1),
            }));
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.lhs.get_typeinfo(instructions),
                lhs: Operand::Derefed(0, 0),
                rhs: Operand::Register(1),
            }));
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.lhs.is_return_reference(instructions)
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}

// += -= *= /= %=
// <<= >>= &= |= ^=
#[derive(Debug, Clone)]
pub struct ExprAssignOp {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprAssignOp {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let lhs_type = self.lhs.get_typeinfo(instructions);
        if self.lhs.is_return_reference(instructions) == false {
            panic!("{:?} on non-reference", self.op);
        }
        if lhs_type.is_const() {
            panic!("{:?} on const variable", self.op);
        }

        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }
        self.lhs.emit(instructions);
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));
        match self.op {
            BinaryOperator::AddAssign => {
                if let TypeInfo::Pointer(t) = lhs_type {
                    let move_size = t.number_of_primitives();
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                        operand_to: Operand::Register(2),
                    }));
                    instructions.push(Instruction::MulAssign(MulAssign {
                        lhs: Operand::Register(2),
                        rhs: Operand::Register(1),
                    }));
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(2),
                    }));
                } else {
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(1),
                    }));
                }
            }
            BinaryOperator::SubAssign => {
                if let TypeInfo::Pointer(t) = lhs_type {
                    let move_size = t.number_of_primitives();
                    instructions.push(Instruction::MoveRegister(MoveRegister {
                        operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                        operand_to: Operand::Register(2),
                    }));
                    instructions.push(Instruction::MulAssign(MulAssign {
                        lhs: Operand::Register(2),
                        rhs: Operand::Register(1),
                    }));
                    instructions.push(Instruction::SubAssign(SubAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(2),
                    }));
                } else {
                    instructions.push(Instruction::SubAssign(SubAssign {
                        lhs: Operand::Derefed(0, 0),
                        rhs: Operand::Register(1),
                    }));
                }
            }
            BinaryOperator::MulAssign => {
                instructions.push(Instruction::MulAssign(MulAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::DivAssign => {
                instructions.push(Instruction::DivAssign(DivAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::ModAssign => {
                instructions.push(Instruction::ModAssign(ModAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::ShiftLeftAssign => {
                instructions.push(Instruction::ShiftLeftAssign(ShiftLeftAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::ShiftRightAssign => {
                instructions.push(Instruction::ShiftRightAssign(ShiftRightAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::BitwiseAndAssign => {
                instructions.push(Instruction::BitwiseAndAssign(BitwiseAndAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::BitwiseOrAssign => {
                instructions.push(Instruction::BitwiseOrAssign(BitwiseOrAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::BitwiseXorAssign => {
                instructions.push(Instruction::BitwiseXorAssign(BitwiseXorAssign {
                    lhs: Operand::Derefed(0, 0),
                    rhs: Operand::Register(1),
                }));
            }
            _ => panic!("Invalid operator for AssignExpression: {:?}", self.op),
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.lhs.is_return_reference(instructions)
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}

#[derive(Debug, Clone)]
pub struct ExprAdditive {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprAdditive {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        // for type-check panic
        self.get_typeinfo(instructions);

        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        // pointer arithmetic
        if let TypeInfo::Pointer(t) = self.lhs.get_typeinfo(instructions).remove_const() {
            let move_size = t.number_of_primitives();
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                operand_to: Operand::Register(2),
            }));
            instructions.push(Instruction::MulAssign(MulAssign {
                lhs: Operand::Register(2),
                rhs: Operand::Register(1),
            }));

            match self.op {
                BinaryOperator::Add => {
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    }));
                }
                BinaryOperator::Sub => {
                    instructions.push(Instruction::SubAssign(SubAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    }));
                }
                _ => panic!("Invalid operator for AdditiveExpression: {:?}", self.op),
            }
        } else if let TypeInfo::Array(t, _) = self.lhs.get_typeinfo(instructions).remove_const() {
            let move_size = t.number_of_primitives();
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Value(VariableData::Int64(move_size as i64)),
                operand_to: Operand::Register(2),
            }));
            instructions.push(Instruction::MulAssign(MulAssign {
                lhs: Operand::Register(2),
                rhs: Operand::Register(1),
            }));

            match self.op {
                BinaryOperator::Add => {
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    }));
                }
                BinaryOperator::Sub => {
                    instructions.push(Instruction::SubAssign(SubAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(2),
                    }));
                }
                _ => panic!("Invalid operator for AdditiveExpression: {:?}", self.op),
            }
        } else {
            match self.op {
                BinaryOperator::Add => {
                    instructions.push(Instruction::AddAssign(AddAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(1),
                    }));
                }
                BinaryOperator::Sub => {
                    instructions.push(Instruction::SubAssign(SubAssign {
                        lhs: Operand::Register(0),
                        rhs: Operand::Register(1),
                    }));
                }
                _ => panic!("Invalid operator for AdditiveExpression: {:?}", self.op),
            }
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let lhs_type = self.lhs.get_typeinfo(instructions).remove_const();
        let rhs_type = self.rhs.get_typeinfo(instructions).remove_const();
        // choose bigger-precision type
        match lhs_type {
            TypeInfo::UInt8 | TypeInfo::Int8 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float32 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float32,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float64 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float64,
                TypeInfo::Float32 => TypeInfo::Float64,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Pointer(t) => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Pointer(t),
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Pointer(t),
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Pointer(t),
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Pointer(t),
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Array(t, _) => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Pointer(t),
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Pointer(t),
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Pointer(t),
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Pointer(t),
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprMultiplicative {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprMultiplicative {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        match self.op {
            BinaryOperator::Mul => {
                instructions.push(Instruction::MulAssign(MulAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::Div => {
                instructions.push(Instruction::DivAssign(DivAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::Mod => {
                instructions.push(Instruction::ModAssign(ModAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            _ => panic!(
                "Invalid operator for MultiplicativeExpression: {:?}",
                self.op
            ),
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let lhs_type = self.lhs.get_typeinfo(instructions).remove_const();
        let rhs_type = self.rhs.get_typeinfo(instructions).remove_const();
        // choose bigger-precision type
        match lhs_type {
            TypeInfo::UInt8 | TypeInfo::Int8 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float32 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float32,
                TypeInfo::Float32 => TypeInfo::Float32,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::Float64 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::Float64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::Float64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::Float64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::Float64,
                TypeInfo::Float32 => TypeInfo::Float64,
                TypeInfo::Float64 => TypeInfo::Float64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}
#[derive(Debug, Clone)]
pub struct ExprShift {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprShift {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        }

        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        match self.op {
            BinaryOperator::ShiftLeft => {
                instructions.push(Instruction::ShiftLeftAssign(ShiftLeftAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::ShiftRight => {
                instructions.push(Instruction::ShiftRightAssign(ShiftRightAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            _ => panic!("Invalid operator for ShiftExpression: {:?}", self.op),
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.lhs.get_typeinfo(instructions)
    }
}
#[derive(Debug, Clone)]
pub struct ExprBitwise {
    pub op: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprBitwise {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.rhs.emit(instructions);
        if self.rhs.is_return_reference(instructions) {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::PushStack(PushStack {
                operand: Operand::Register(0),
            }));
        }

        self.lhs.emit(instructions);
        if self.lhs.is_return_reference(instructions) {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::Assign(Assign {
                lhs_type: self.get_typeinfo(instructions),
                lhs: Operand::Register(0),
                rhs: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::PopStack(PopStack {
            operand: Operand::Register(1),
        }));

        match self.op {
            BinaryOperator::BitwiseAnd => {
                instructions.push(Instruction::BitwiseAndAssign(BitwiseAndAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::BitwiseOr => {
                instructions.push(Instruction::BitwiseOrAssign(BitwiseOrAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            BinaryOperator::BitwiseXor => {
                instructions.push(Instruction::BitwiseXorAssign(BitwiseXorAssign {
                    lhs: Operand::Register(0),
                    rhs: Operand::Register(1),
                }));
            }
            _ => panic!("Invalid operator for BitwiseExpression: {:?}", self.op),
        }
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        let lhs_type = self.lhs.get_typeinfo(instructions).remove_const();
        let rhs_type = self.rhs.get_typeinfo(instructions).remove_const();
        // choose bigger-precision type
        match lhs_type {
            TypeInfo::UInt8 | TypeInfo::Int8 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt8,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt16 | TypeInfo::Int16 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt16,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt16,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt32 | TypeInfo::Int32 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt32,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt32,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt32,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            TypeInfo::UInt64 | TypeInfo::Int64 => match rhs_type {
                TypeInfo::UInt8 | TypeInfo::Int8 => TypeInfo::UInt64,
                TypeInfo::UInt16 | TypeInfo::Int16 => TypeInfo::UInt64,
                TypeInfo::UInt32 | TypeInfo::Int32 => TypeInfo::UInt64,
                TypeInfo::UInt64 | TypeInfo::Int64 => TypeInfo::UInt64,
                _ => panic!(
                    "{:?} not implemented between {:?} and {:?}",
                    self.op,
                    self.lhs.get_typeinfo(instructions),
                    self.rhs.get_typeinfo(instructions)
                ),
            },
            _ => panic!(
                "{:?} not implemented between {:?} and {:?}",
                self.op,
                self.lhs.get_typeinfo(instructions),
                self.rhs.get_typeinfo(instructions)
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprPostArrow {
    pub src: Box<Expression>,
    pub member: String,
}
impl ExprPostArrow {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let member_offset = match self.src.get_typeinfo(instructions).remove_const() {
            TypeInfo::Pointer(t) => match instructions.get_true_typeinfo(t.as_ref()).remove_const()
            {
                TypeInfo::Struct(sinfo) => {
                    let mut member_offset: Option<usize> = None;
                    for (_, name, offset) in sinfo.fields.as_ref().unwrap() {
                        if name == &self.member {
                            member_offset = Some(*offset);
                            break;
                        }
                    }
                    member_offset.expect(format!("member not found: {:?}", self.member).as_str())
                }
                _ => panic!("-> operator on non-struct type: {:?}", t),
            },
            _ => panic!(
                "-> operator on non-pointer type: {:?}",
                self.src.get_typeinfo(instructions)
            ),
        };

        self.src.emit(instructions);
        if self.src.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::AddAssign(AddAssign {
            lhs: Operand::Register(0),
            rhs: Operand::Value(VariableData::UInt64(member_offset as u64)),
        }));
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        match self.src.get_typeinfo(instructions).remove_const() {
            TypeInfo::Pointer(t) => {
                let t_type = instructions.get_true_typeinfo(t.as_ref());
                match t_type.remove_const() {
                    TypeInfo::Struct(sinfo) => {
                        let mut member_type: Option<TypeInfo> = None;
                        for (type_, name, _) in sinfo.fields.as_ref().unwrap() {
                            if name == &self.member {
                                member_type = Some(type_.clone());
                                break;
                            }
                        }
                        let member_type = member_type
                            .expect(format!("member not found: {:?}", self.member).as_str());
                        if t_type.is_const() {
                            member_type.add_const()
                        } else {
                            member_type
                        }
                    }
                    _ => panic!("-> operator on non-struct type: {:?}", t),
                }
            }
            _ => panic!(
                "-> operator on non-pointer type: {:?}",
                self.src.get_typeinfo(instructions)
            ),
        }
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        match self.get_typeinfo(instructions).remove_const() {
            TypeInfo::Array(_, _) => false,
            _ => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExprConditional {
    pub cond: Box<Expression>,
    pub then_expr: Box<Expression>,
    pub else_expr: Box<Expression>,
}
impl ExprConditional {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        let else_label = instructions.get_unique_label();
        let end_label = instructions.get_unique_label();

        self.cond.emit(instructions);
        if self.cond.is_return_reference(instructions) {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Derefed(0, 0),
            }));
        } else {
            instructions.push(Instruction::JumpZero(JumpZero {
                label: else_label.clone(),
                operand_cond: Operand::Register(0),
            }));
        }

        self.then_expr.emit(instructions);
        if self.then_expr.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            }));
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            }));
        }
        instructions.push(Instruction::Jump(Jump {
            label: end_label.clone(),
        }));
        instructions.set_label(&else_label);
        self.else_expr.emit(instructions);
        if self.else_expr.is_return_reference(instructions) {
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Derefed(0, 0),
                operand_to: Operand::Register(1),
            }));
            instructions.push(Instruction::MoveRegister(MoveRegister {
                operand_from: Operand::Register(1),
                operand_to: Operand::Register(0),
            }));
        }
        instructions.set_label(&end_label);
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.then_expr.get_typeinfo(instructions)
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub struct ExprComma {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}
impl ExprComma {
    pub fn emit(&self, instructions: &mut InstructionGenerator) {
        self.lhs.emit(instructions);
        self.rhs.emit(instructions);
    }
    pub fn is_return_reference(&self, instructions: &InstructionGenerator) -> bool {
        self.rhs.is_return_reference(instructions)
    }
    pub fn get_typeinfo(&self, instructions: &InstructionGenerator) -> TypeInfo {
        self.rhs.get_typeinfo(instructions)
    }
}

#[derive(Debug, Clone)]
pub struct ExprInitializerList {
    pub initializers: Vec<Expression>,
}
impl ExprInitializerList {
    pub fn emit(&self, _instructions: &mut InstructionGenerator) {
        panic!("InitializerListExpression.eval not implemented");
    }
    pub fn get_typeinfo(&self, _instructions: &InstructionGenerator) -> TypeInfo {
        panic!("InitializerListExpression.get_typeinfo not implemented");
    }
    pub fn is_return_reference(&self, _instructions: &InstructionGenerator) -> bool {
        panic!("InitializerListExpression.is_return_reference not implemented");
    }
}
