#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    Struct(StructType),
    Union(StructType),
    Enum(EnumType),

    Pointer(Box<CVType>),
    Array(ArrayType),
    Function(FunctionType),
}
impl PrimitiveType {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            PrimitiveType::UInt8
                | PrimitiveType::UInt16
                | PrimitiveType::UInt32
                | PrimitiveType::UInt64
                | PrimitiveType::Int8
                | PrimitiveType::Int16
                | PrimitiveType::Int32
                | PrimitiveType::Int64
        )
    }
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            PrimitiveType::UInt8
                | PrimitiveType::UInt16
                | PrimitiveType::UInt32
                | PrimitiveType::UInt64
                | PrimitiveType::Int8
                | PrimitiveType::Int16
                | PrimitiveType::Int32
                | PrimitiveType::Int64
                | PrimitiveType::Float32
                | PrimitiveType::Float64
        )
    }
    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveType::Float32 | PrimitiveType::Float64)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayType {
    pub cv_type: Box<CVType>,
    pub size: usize,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    // maybe no need CV qualifier for return type?
    pub return_type: Box<CVType>,
    pub args: Vec<CVType>,
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructType {
    pub name: Option<String>,
    pub body: Option<StructBody>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructBody {
    pub members: Vec<StructMember>,
    pub size: usize,
    pub align: usize,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructMember {
    pub name: String,
    pub cv_type: CVType,
    pub offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumType {
    pub name: Option<String>,
    pub body: Option<EnumBody>,
    /// integer representation of enum type
    pub type_: Box<PrimitiveType>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumBody {
    pub members: Vec<EnumMember>,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumMember {
    pub name: String,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
                cv_type: Box::new(self),
                size,
            }),
            const_: false,
            volatile: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
