use super::CompileError;

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
    pub fn is_struct(&self) -> bool {
        matches!(self, PrimitiveType::Struct(_))
    }
    pub fn is_union(&self) -> bool {
        matches!(self, PrimitiveType::Union(_))
    }
    pub fn is_enum(&self) -> bool {
        matches!(self, PrimitiveType::Enum(_))
    }

    pub fn is_bool_castable(&self) -> bool {
        match self {
            PrimitiveType::UInt8
            | PrimitiveType::UInt16
            | PrimitiveType::UInt32
            | PrimitiveType::UInt64
            | PrimitiveType::Int8
            | PrimitiveType::Int16
            | PrimitiveType::Int32
            | PrimitiveType::Int64
            | PrimitiveType::Pointer(_)
            | PrimitiveType::Array(_) => true,
            PrimitiveType::Enum(enum_type) => enum_type.type_.is_bool_castable(),
            _ => false,
        }
    }
    pub fn bit_common_type(&self, other: &Self) -> Option<Self> {
        let lhs = match self {
            PrimitiveType::UInt8 => 0,
            PrimitiveType::UInt16 => 1,
            PrimitiveType::UInt32 => 2,
            PrimitiveType::UInt64 => 3,
            PrimitiveType::Int8 => 0,
            PrimitiveType::Int16 => 1,
            PrimitiveType::Int32 => 2,
            PrimitiveType::Int64 => 3,
            _ => return None,
        };
        let rhs = match other {
            PrimitiveType::UInt8 => 0,
            PrimitiveType::UInt16 => 1,
            PrimitiveType::UInt32 => 2,
            PrimitiveType::UInt64 => 3,
            PrimitiveType::Int8 => 0,
            PrimitiveType::Int16 => 1,
            PrimitiveType::Int32 => 2,
            PrimitiveType::Int64 => 3,
            _ => return None,
        };
        let common = lhs.max(rhs);
        match common {
            0 => Some(PrimitiveType::UInt8),
            1 => Some(PrimitiveType::UInt16),
            2 => Some(PrimitiveType::UInt32),
            3 => Some(PrimitiveType::UInt64),
            _ => unreachable!(),
        }
    }
    pub fn common_type(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (PrimitiveType::UInt8, PrimitiveType::UInt8) => Some(PrimitiveType::UInt8),
            (PrimitiveType::UInt8, PrimitiveType::UInt16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt8, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt8, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt8, PrimitiveType::Int8) => Some(PrimitiveType::UInt8),
            (PrimitiveType::UInt8, PrimitiveType::Int16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt8, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt8, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt8, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::UInt8, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::UInt8, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::UInt8, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::UInt8, _) => None,

            (PrimitiveType::UInt16, PrimitiveType::UInt8) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt16, PrimitiveType::UInt16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt16, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt16, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt16, PrimitiveType::Int8) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt16, PrimitiveType::Int16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::UInt16, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt16, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt16, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::UInt16, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::UInt16, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::UInt16, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::UInt16, _) => None,

            (PrimitiveType::UInt32, PrimitiveType::UInt8) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::UInt16) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt32, PrimitiveType::Int8) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::Int16) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::UInt32, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt32, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::UInt32, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::UInt32, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::UInt32, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::UInt32, _) => None,

            (PrimitiveType::UInt64, PrimitiveType::UInt8) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::UInt16) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::UInt32) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::Int8) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::Int16) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::Int32) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::UInt64, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::UInt64, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::UInt64, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::UInt64, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::UInt64, _) => None,

            (PrimitiveType::Int8, PrimitiveType::UInt8) => Some(PrimitiveType::UInt8),
            (PrimitiveType::Int8, PrimitiveType::UInt16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int8, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int8, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int8, PrimitiveType::Int8) => Some(PrimitiveType::UInt8),
            (PrimitiveType::Int8, PrimitiveType::Int16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int8, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int8, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int8, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Int8, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Int8, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::Int8, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::Int8, _) => None,

            (PrimitiveType::Int16, PrimitiveType::UInt8) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int16, PrimitiveType::UInt16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int16, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int16, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int16, PrimitiveType::Int8) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int16, PrimitiveType::Int16) => Some(PrimitiveType::UInt16),
            (PrimitiveType::Int16, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int16, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int16, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Int16, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Int16, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::Int16, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::Int16, _) => None,

            (PrimitiveType::Int32, PrimitiveType::UInt8) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::UInt16) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::UInt32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int32, PrimitiveType::Int8) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::Int16) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::Int32) => Some(PrimitiveType::UInt32),
            (PrimitiveType::Int32, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int32, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Int32, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Int32, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::Int32, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::Int32, _) => None,

            (PrimitiveType::Int64, PrimitiveType::UInt8) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::UInt16) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::UInt32) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::UInt64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::Int8) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::Int16) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::Int32) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::Int64) => Some(PrimitiveType::UInt64),
            (PrimitiveType::Int64, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Int64, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Int64, PrimitiveType::Array(t)) => Some(t.cv_type.type_.clone()),
            (PrimitiveType::Int64, PrimitiveType::Pointer(t)) => Some(t.as_ref().type_.clone()),
            (PrimitiveType::Int64, _) => None,

            (PrimitiveType::Float32, PrimitiveType::UInt8) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::UInt16) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::UInt32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::UInt64) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Int8) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Int16) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Int32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Int64) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Float32) => Some(PrimitiveType::Float32),
            (PrimitiveType::Float32, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float32, _) => None,

            (PrimitiveType::Float64, PrimitiveType::UInt8) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::UInt16) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::UInt32) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::UInt64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Int8) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Int16) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Int32) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Int64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Float32) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, PrimitiveType::Float64) => Some(PrimitiveType::Float64),
            (PrimitiveType::Float64, _) => None,

            (PrimitiveType::Pointer(t), PrimitiveType::UInt8) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::UInt16) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::UInt32) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::UInt64) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::Int8) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::Int16) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::Int32) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), PrimitiveType::Int64) => {
                Some(PrimitiveType::Pointer(t.clone()))
            }
            (PrimitiveType::Pointer(t), _) => None,

            (PrimitiveType::Array(t), PrimitiveType::UInt8) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::UInt16) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::UInt32) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::UInt64) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::Int8) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::Int16) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::Int32) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), PrimitiveType::Int64) => {
                Some(PrimitiveType::Pointer(t.cv_type.clone()))
            }
            (PrimitiveType::Array(t), _) => None,

            _ => None,
        }
    }

    pub fn sizeof(&self) -> Result<usize, CompileError> {
        Ok(match self {
            PrimitiveType::Void => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::UInt8 | PrimitiveType::Int8 => 1,
            PrimitiveType::UInt16 | PrimitiveType::Int16 => 2,
            PrimitiveType::UInt32 | PrimitiveType::Int32 | PrimitiveType::Float32 => 4,
            PrimitiveType::UInt64 | PrimitiveType::Int64 | PrimitiveType::Float64 => 8,
            PrimitiveType::Pointer(_) => 8,
            PrimitiveType::Array(ArrayType { cv_type, size }) => cv_type.type_.sizeof()? * (*size),
            PrimitiveType::Function(_) => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::Struct(s) | PrimitiveType::Union(s) => match &s.body {
                Some(s) => s.size,
                None => return Err(CompileError::SizeofIncompleteType),
            },
            PrimitiveType::Enum(e) => e.type_.sizeof()?,
        })
    }
    pub fn alignof(&self) -> Result<usize, CompileError> {
        Ok(match self {
            PrimitiveType::Void => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::UInt8 | PrimitiveType::Int8 => 1,
            PrimitiveType::UInt16 | PrimitiveType::Int16 => 2,
            PrimitiveType::UInt32 | PrimitiveType::Int32 | PrimitiveType::Float32 => 4,
            PrimitiveType::UInt64 | PrimitiveType::Int64 | PrimitiveType::Float64 => 8,
            PrimitiveType::Pointer(_) => 8,
            PrimitiveType::Array(ArrayType { cv_type, size: _ }) => cv_type.type_.alignof()?,
            PrimitiveType::Function(_) => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::Struct(s) | PrimitiveType::Union(s) => match &s.body {
                Some(s) => s.align,
                None => return Err(CompileError::AlignofIncompleteType),
            },
            PrimitiveType::Enum(e) => e.type_.alignof()?,
        })
    }
    pub fn to_unsigned(&self) -> Option<Self> {
        match self {
            PrimitiveType::Int8 => Some(PrimitiveType::UInt8),
            PrimitiveType::Int16 => Some(PrimitiveType::UInt16),
            PrimitiveType::Int32 => Some(PrimitiveType::UInt32),
            PrimitiveType::Int64 => Some(PrimitiveType::UInt64),
            PrimitiveType::UInt8 => Some(PrimitiveType::UInt8),
            PrimitiveType::UInt16 => Some(PrimitiveType::UInt16),
            PrimitiveType::UInt32 => Some(PrimitiveType::UInt32),
            PrimitiveType::UInt64 => Some(PrimitiveType::UInt64),
            _ => None,
        }
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

impl StructType {
    pub fn struct_from_decls(name: Option<String>, decls: Vec<(String, CVType)>) -> Self {
        let mut size = 0;
        let mut align = 1;
        let mut members = Vec::new();
        for (name, member) in decls.into_iter() {
            let member_size = member.type_.sizeof().unwrap();
            let member_align = member.type_.alignof().unwrap();
            let offset = ((size + member_align - 1) / member_align) * member_align;
            align = std::cmp::max(align, member_align);
            size = offset + member_size;

            members.push(StructMember {
                name,
                cv_type: member,
                offset,
            });
        }
        let body = StructBody {
            size,
            align,
            members,
        };

        StructType {
            name,
            body: Some(body),
        }
    }
    pub fn union_from_decls(name: Option<String>, decls: Vec<(String, CVType)>) -> Self {
        let mut size = 0;
        let mut align = 1;
        let mut members = Vec::new();
        for (name, member) in decls.into_iter() {
            let member_size = member.type_.sizeof().unwrap();
            let member_align = member.type_.alignof().unwrap();
            let offset = 0;
            align = std::cmp::max(align, member_align);
            size = std::cmp::max(size, member_size);

            members.push(StructMember {
                name,
                cv_type: member,
                offset,
            });
        }
        let body = StructBody {
            size,
            align,
            members,
        };

        StructType {
            name,
            body: Some(body),
        }
    }
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
impl EnumType {
    pub fn enum_from_decls(name: Option<String>, decls: Vec<(String, Option<i64>)>) -> Self {
        let mut last_value = 0;
        let mut members = Vec::new();
        for (name, value) in decls.into_iter() {
            let value = match value {
                Some(value) => value,
                None => last_value + 1,
            };
            last_value = value;
            members.push(EnumMember { name, value });
        }
        EnumType {
            name,
            body: Some(EnumBody { members }),
            type_: Box::new(PrimitiveType::Int64),
        }
    }
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
