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

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub type_: Box<CVType>,
    pub size: usize,
}
#[derive(Debug, Clone)]
pub struct FunctionType {
    // maybe no need CV qualifier for return type?
    pub return_type: Box<CVType>,
    pub args: Vec<CVType>,
    pub variadic: bool,
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
