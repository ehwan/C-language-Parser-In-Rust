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
}

impl VariablePool {
    pub fn new() -> Self {
        VariablePool {
            max_size: 0,
            size: 0,
        }
    }

    pub fn add(&mut self, size: usize, align: usize) -> usize {
        let offset = (self.size + align - 1) / align * align;
        self.size = offset + size;
        self.max_size = self.max_size.max(self.size);
        offset
    }
    #[allow(unused)]
    pub fn remove(&mut self, size: usize, align: usize) {
        self.size -= size;
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
