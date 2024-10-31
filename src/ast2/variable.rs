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

    pub fn into_u64(self) -> u64 {
        match self {
            Address::Global(x) => (x as u64) | (1u64 << 63),
            Address::Local(x) => (x as u64) | (1u64 << 62),
            Address::Function(x) => (x as u64) | (3u64 << 62),
        }
    }
    pub fn from_u64(x: u64) -> Self {
        match x >> 62 {
            1 => Address::Global((x & !(1u64 << 63)) as usize),
            2 => Address::Local((x & !(1u64 << 62)) as usize),
            3 => Address::Function((x & !(3u64 << 62)) as usize),
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
