use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Struct(StructInfo),
    Union(UnionInfo),
    Enum(EnumInfo),
    Pointer(Box<TypeInfo>),
    Array(Box<TypeInfo>, Option<usize>),
    Function(Box<TypeInfo>, Vec<TypeInfo>),

    // for typedef
    // temporary store the name of the type; will be replaced by the actual type in emitting
    Identifier(String),
}
impl Default for TypeInfo {
    fn default() -> Self {
        TypeInfo::Int32
    }
}
impl TypeInfo {
    pub fn sizeof(&self) -> usize {
        match self {
            TypeInfo::Void => panic!("sizeof(void) is invalid"),
            TypeInfo::Int8 => 1,
            TypeInfo::Int16 => 2,
            TypeInfo::Int32 => 4,
            TypeInfo::Int64 => 8,
            TypeInfo::UInt8 => 1,
            TypeInfo::UInt16 => 2,
            TypeInfo::UInt32 => 4,
            TypeInfo::UInt64 => 8,
            TypeInfo::Float32 => 4,
            TypeInfo::Float64 => 8,
            TypeInfo::Struct(info) => info.sizeof(),
            TypeInfo::Union(info) => info.sizeof(),
            TypeInfo::Enum(_) => 8,
            TypeInfo::Pointer(_) => 8,
            TypeInfo::Array(info, size) => info.sizeof() * size.unwrap_or(1),
            TypeInfo::Function(_, _) => panic!("sizeof(function) is invalid"),
            TypeInfo::Identifier(_) => panic!("sizeof(identifier) is invalid"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, TypeInfo>>,
}
impl StructInfo {
    pub fn sizeof(&self) -> usize {
        let mut size: usize = 0;
        for (_, field) in self.fields.as_ref().unwrap() {
            size += field.sizeof();
        }
        size
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct UnionInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, TypeInfo>>,
}
impl UnionInfo {
    pub fn sizeof(&self) -> usize {
        let mut size: usize = 0;
        for (_, field) in self.fields.as_ref().unwrap() {
            size = size.max(field.sizeof());
        }
        size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, i64>>,
}
