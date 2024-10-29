#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Void,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,

    Struct,
    Union,
    Enum,

    Pointer(Box<CVType>),
    Array(ArrayType),
    Function(FunctionType),
}
impl PrimitiveType {
    pub fn is_integer(&self) -> bool {
        match self {
            PrimitiveType::UInt8
            | PrimitiveType::UInt16
            | PrimitiveType::UInt32
            | PrimitiveType::UInt64
            | PrimitiveType::Int8
            | PrimitiveType::Int16
            | PrimitiveType::Int32
            | PrimitiveType::Int64 => true,
            _ => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            PrimitiveType::UInt8
            | PrimitiveType::UInt16
            | PrimitiveType::UInt32
            | PrimitiveType::UInt64
            | PrimitiveType::Int8
            | PrimitiveType::Int16
            | PrimitiveType::Int32
            | PrimitiveType::Int64
            | PrimitiveType::Float32
            | PrimitiveType::Float64 => true,
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            PrimitiveType::Float32 | PrimitiveType::Float64 => true,
            _ => false,
        }
    }
}

impl PartialEq for PrimitiveType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PrimitiveType::Void, PrimitiveType::Void) => true,
            (PrimitiveType::UInt8, PrimitiveType::UInt8) => true,
            (PrimitiveType::UInt16, PrimitiveType::UInt16) => true,
            (PrimitiveType::UInt32, PrimitiveType::UInt32) => true,
            (PrimitiveType::UInt64, PrimitiveType::UInt64) => true,
            (PrimitiveType::Int8, PrimitiveType::Int8) => true,
            (PrimitiveType::Int16, PrimitiveType::Int16) => true,
            (PrimitiveType::Int32, PrimitiveType::Int32) => true,
            (PrimitiveType::Int64, PrimitiveType::Int64) => true,
            (PrimitiveType::Float32, PrimitiveType::Float32) => true,
            (PrimitiveType::Float64, PrimitiveType::Float64) => true,
            (PrimitiveType::Struct, PrimitiveType::Struct) => true,
            (PrimitiveType::Union, PrimitiveType::Union) => true,
            (PrimitiveType::Enum, PrimitiveType::Enum) => true,
            (PrimitiveType::Pointer(a), PrimitiveType::Pointer(b)) => a == b,
            (PrimitiveType::Array(a), PrimitiveType::Array(b)) => a == b,
            (PrimitiveType::Function(a), PrimitiveType::Function(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub type_: Box<CVType>,
    pub size: usize,
}
impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_ && self.size == other.size
    }
}
#[derive(Debug, Clone)]
pub struct FunctionType {
    // maybe no need CV qualifier for return type?
    pub return_type: Box<CVType>,
    pub args: Vec<CVType>,
    pub variadic: bool,
}
impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        self.return_type == other.return_type
            && self.args == other.args
            && self.variadic == other.variadic
    }
}

#[derive(Debug, Clone)]
pub struct CVType {
    pub type_: PrimitiveType,
    pub const_: bool,
    pub volatile: bool,
}
impl CVType {
    pub fn from_primitive(type_: PrimitiveType) -> Self {
        Self {
            type_,
            const_: false,
            volatile: false,
        }
    }

    pub fn into_pointer(self) -> Self {
        Self {
            type_: PrimitiveType::Pointer(Box::new(self)),
            const_: false,
            volatile: false,
        }
    }
    pub fn into_array(self, size: usize) -> Self {
        Self {
            type_: PrimitiveType::Array(ArrayType {
                type_: Box::new(self),
                size,
            }),
            const_: false,
            volatile: false,
        }
    }
}
impl PartialEq for CVType {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_ && self.const_ == other.const_ && self.volatile == other.volatile
    }
}

#[derive(Debug, Clone)]
pub struct StorageQualifier {
    pub static_: bool,
    pub register: bool,
    pub extern_: bool,
    pub typedef: bool,
    pub auto: bool,
}
impl StorageQualifier {
    pub fn new() -> Self {
        Self {
            static_: false,
            register: false,
            extern_: false,
            typedef: false,
            auto: false,
        }
    }
}
