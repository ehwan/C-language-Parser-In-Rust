#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SizeType {
    Byte,
    Word,
    DWord,
    QWord,
}
impl SizeType {
    pub fn from_size(size: usize) -> SizeType {
        match size {
            1 => SizeType::Byte,
            2 => SizeType::Word,
            4 => SizeType::DWord,
            8 => SizeType::QWord,
            _ => panic!("Invalid size: {}", size),
        }
    }
    pub fn to_size(&self) -> usize {
        match self {
            SizeType::Byte => 1,
            SizeType::Word => 2,
            SizeType::DWord => 4,
            SizeType::QWord => 8,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    /// value, size
    Constant(u64),
    /// register number
    Register(usize),
    /// [ register number ]
    Deref(usize),
}
impl Operand {}
