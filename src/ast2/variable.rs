use super::CVType;

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub address: Address,
    pub cv_type: CVType,
}

#[derive(Debug, Clone)]
pub struct VariablePool {
    pub max_size: usize,
    pub size: usize,
    /// save `size` before `push()` to restore upon `pop()`
    pub old_sizes: Vec<usize>,
}

impl VariablePool {
    pub fn new() -> Self {
        VariablePool {
            max_size: 0,
            size: 0,
            old_sizes: Vec::new(),
        }
    }

    pub fn push(&mut self, size: usize, align: usize) -> usize {
        self.old_sizes.push(self.size);
        let offset = (self.size + align - 1) / align * align;
        self.size = offset + size;
        self.max_size = self.max_size.max(self.size);
        offset
    }
    pub fn pop(&mut self) {
        let size = self.old_sizes.pop().unwrap();
        self.size = size;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Address {
    /// absolute index
    Global(usize),
    /// relative index from base pointer
    Local(usize),
    /// index on function array
    Function(usize),
    /// constant Text section
    Text(usize),
}

impl Address {
    pub fn is_global(&self) -> bool {
        matches!(self, Address::Global(_))
    }
    pub fn is_local(&self) -> bool {
        matches!(self, Address::Local(_))
    }
    pub fn is_function(&self) -> bool {
        matches!(self, Address::Function(_))
    }
    pub fn is_text(&self) -> bool {
        matches!(self, Address::Text(_))
    }

    /*
    xxx  61 bits
    ^^^   ^^^^^  actual address
    |||
    ^^^ address type specifier

    000 : Global
    001 : Local
    010 : Function
    011 : Text
    */
    pub fn into_u64(self) -> u64 {
        match self {
            Address::Global(x) => (x as u64) | (0b000u64 << 61),
            Address::Local(x) => (x as u64) | (0b001u64 << 61),
            Address::Function(x) => (x as u64) | (0b010u64 << 61),
            Address::Text(x) => (x as u64) | (0b011u64 << 61),
        }
    }
    pub fn from_u64(x: u64) -> Self {
        let specifier = x >> 61;
        let address = (x & ((1u64 << 61) - 1)) as usize;
        match specifier {
            0 => Address::Global(address),
            1 => Address::Local(address),
            2 => Address::Function(address),
            3 => Address::Text(address),
            _ => unreachable!("invalid address"),
        }
    }
}
impl Into<u64> for Address {
    fn into(self) -> u64 {
        self.into_u64()
    }
}
impl From<u64> for Address {
    fn from(x: u64) -> Self {
        Address::from_u64(x)
    }
}
