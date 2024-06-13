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
            TypeInfo::Array(info, size) => {
                info.sizeof() * size.expect("sizeof: Array size is not defined")
            }
            TypeInfo::Function(_, _) => panic!("sizeof(function) is invalid"),
            TypeInfo::Identifier(_) => panic!("sizeof(identifier) is invalid"),
        }
    }
    pub fn number_of_primitives(&self) -> usize {
        match self {
            TypeInfo::Void => 0,
            TypeInfo::Int8 | TypeInfo::Int16 | TypeInfo::Int32 | TypeInfo::Int64 => 1,
            TypeInfo::UInt8 | TypeInfo::UInt16 | TypeInfo::UInt32 | TypeInfo::UInt64 => 1,
            TypeInfo::Float32 | TypeInfo::Float64 => 1,
            TypeInfo::Struct(structinfo) => structinfo.number_of_primitives(),
            TypeInfo::Pointer(_) => 1,
            TypeInfo::Array(info, Some(size)) => info.number_of_primitives() * size,
            _ => panic!("number_of_primitives: unsupported type: {:?}", self),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInfo {
    pub name: Option<String>,
    pub fields: Option<HashMap<String, (TypeInfo, usize)>>,
}
impl StructInfo {
    pub fn sizeof(&self) -> usize {
        let mut size: usize = 0;
        for (_, (fieldtype, _)) in self.fields.as_ref().unwrap() {
            size += fieldtype.sizeof();
        }
        size
    }
    pub fn number_of_primitives(&self) -> usize {
        let mut count: usize = 0;
        for (_, (fieldtype, _)) in self.fields.as_ref().unwrap() {
            match fieldtype {
                TypeInfo::UInt8 | TypeInfo::UInt16 | TypeInfo::UInt32 | TypeInfo::UInt64 => {
                    count += 1
                }
                TypeInfo::Int8 | TypeInfo::Int16 | TypeInfo::Int32 | TypeInfo::Int64 => count += 1,
                TypeInfo::Float32 | TypeInfo::Float64 => count += 1,
                TypeInfo::Struct(structinfo) => count += structinfo.number_of_primitives(),
                _ => panic!("number_of_primitives: unsupported type: {:?}", fieldtype),
            }
        }
        count
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
