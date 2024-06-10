use crate::ast::typename::TypeInfo;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum VariableData {
    Void,
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    Struct(StructData),
    Union(UnionData),
    Enum(EnumData),
    Pointer(Rc<RefCell<(TypeInfo, VariableData)>>),
    Array(Vec<Rc<RefCell<(TypeInfo, VariableData)>>>),
}

impl VariableData {
    // pub fn assign(&mut self, rhs: VariableData) {}
    pub fn init_default(typeinfo: &TypeInfo) -> VariableData {
        match typeinfo {
            TypeInfo::Void => VariableData::Void,
            TypeInfo::Int8 => VariableData::Int8(0),
            TypeInfo::Int16 => VariableData::Int16(0),
            TypeInfo::Int32 => VariableData::Int32(0),
            TypeInfo::Int64 => VariableData::Int64(0),
            TypeInfo::UInt8 => VariableData::UInt8(0),
            TypeInfo::UInt16 => VariableData::UInt16(0),
            TypeInfo::UInt32 => VariableData::UInt32(0),
            TypeInfo::UInt64 => VariableData::UInt64(0),
            TypeInfo::Float32 => VariableData::Float32(0.0),
            TypeInfo::Float64 => VariableData::Float64(0.0),
            TypeInfo::Struct(info) => {
                let mut fields = HashMap::new();
                for (name, typeinfo) in info.fields.clone().unwrap() {
                    fields.insert(
                        name,
                        Rc::new(RefCell::new((
                            typeinfo.clone(),
                            VariableData::init_default(&typeinfo),
                        ))),
                    );
                }
                VariableData::Struct(StructData { fields })
            }
            TypeInfo::Union(info) => {
                let mut fields = HashMap::new();
                for (name, typeinfo) in info.fields.clone().unwrap() {
                    fields.insert(
                        name,
                        Rc::new(RefCell::new((
                            typeinfo.clone(),
                            VariableData::init_default(&typeinfo),
                        ))),
                    );
                }
                VariableData::Struct(StructData { fields })
            }
            TypeInfo::Enum(_) => VariableData::Enum(EnumData { value: 0 }),
            TypeInfo::Array(typeinfo, Some(size)) => {
                let mut array = Vec::new();
                for _ in 0..*size {
                    array.push(Rc::new(RefCell::new((
                        typeinfo.as_ref().clone(),
                        VariableData::init_default(typeinfo),
                    ))));
                }
                VariableData::Array(array)
            }
            TypeInfo::Pointer(_) => {
                todo!("VariableData::init_default: {:?}", typeinfo)
            }
            _ => panic!("VariableData::init_default: {:?}", typeinfo),
        }
    }
    pub fn deep_clone(&self) -> VariableData {
        match self {
            VariableData::Void => VariableData::Void,
            VariableData::Int8(i) => VariableData::Int8(*i),
            VariableData::Int16(i) => VariableData::Int16(*i),
            VariableData::Int32(i) => VariableData::Int32(*i),
            VariableData::Int64(i) => VariableData::Int64(*i),
            VariableData::UInt8(i) => VariableData::UInt8(*i),
            VariableData::UInt16(i) => VariableData::UInt16(*i),
            VariableData::UInt32(i) => VariableData::UInt32(*i),
            VariableData::UInt64(i) => VariableData::UInt64(*i),
            VariableData::Float32(f) => VariableData::Float32(*f),
            VariableData::Float64(f) => VariableData::Float64(*f),
            VariableData::Struct(data) => {
                let mut fields = HashMap::new();
                for (name, field) in &data.fields {
                    let typeinfo = field.borrow().0.clone();
                    let deep_cloned = field.borrow().1.deep_clone();
                    fields.insert(name.clone(), Rc::new(RefCell::new((typeinfo, deep_cloned))));
                }
                VariableData::Struct(StructData { fields })
            }
            VariableData::Union(data) => {
                let mut fields = HashMap::new();
                for (name, field) in &data.fields {
                    let typeinfo = field.borrow().0.clone();
                    let deep_cloned = field.borrow().1.deep_clone();
                    fields.insert(name.clone(), Rc::new(RefCell::new((typeinfo, deep_cloned))));
                }
                VariableData::Struct(StructData { fields })
            }
            VariableData::Enum(data) => VariableData::Enum(EnumData { value: data.value }),
            VariableData::Pointer(ptr) => VariableData::Pointer(ptr.clone()),
            VariableData::Array(array) => {
                let mut new_array = Vec::new();
                for elem in array {
                    let typeinfo = elem.borrow().0.clone();
                    let deep_cloned = elem.borrow().1.deep_clone();
                    new_array.push(Rc::new(RefCell::new((typeinfo, deep_cloned))));
                }
                VariableData::Array(new_array)
            }
        }
    }
    pub fn cast_to(&self, typeinfo: &TypeInfo) -> Option<VariableData> {
        match self {
            VariableData::UInt8(lu8) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lu8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lu8 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lu8 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lu8 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lu8 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lu8 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lu8 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lu8 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lu8 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lu8 as f64)),
                _ => None,
            },
            VariableData::UInt16(lu16) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lu16 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lu16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lu16 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lu16 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lu16 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lu16 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lu16 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lu16 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lu16 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lu16 as f64)),
                _ => None,
            },
            VariableData::UInt32(lu32) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lu32 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lu32 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lu32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lu32 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lu32 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lu32 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lu32 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lu32 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lu32 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lu32 as f64)),
                _ => None,
            },
            VariableData::UInt64(lu64) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lu64 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lu64 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lu64 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lu64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lu64 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lu64 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lu64 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lu64 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lu64 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lu64 as f64)),
                _ => None,
            },
            VariableData::Int8(li8) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*li8 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*li8 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*li8 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*li8 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*li8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*li8 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*li8 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*li8 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*li8 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*li8 as f64)),
                _ => None,
            },
            VariableData::Int16(li16) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*li16 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*li16 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*li16 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*li16 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*li16 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*li16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*li16 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*li16 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*li16 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*li16 as f64)),
                _ => None,
            },
            VariableData::Int32(li32) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*li32 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*li32 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*li32 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*li32 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*li32 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*li32 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*li32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*li32 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*li32 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*li32 as f64)),
                _ => None,
            },
            VariableData::Int64(li64) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*li64 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*li64 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*li64 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*li64 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*li64 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*li64 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*li64 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*li64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*li64 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*li64 as f64)),
                _ => None,
            },
            VariableData::Float32(lf32) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lf32 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lf32 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lf32 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lf32 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lf32 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lf32 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lf32 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lf32 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lf32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lf32 as f64)),
                _ => None,
            },
            VariableData::Float64(lf64) => match typeinfo {
                TypeInfo::UInt8 => Some(VariableData::UInt8(*lf64 as u8)),
                TypeInfo::UInt16 => Some(VariableData::UInt16(*lf64 as u16)),
                TypeInfo::UInt32 => Some(VariableData::UInt32(*lf64 as u32)),
                TypeInfo::UInt64 => Some(VariableData::UInt64(*lf64 as u64)),
                TypeInfo::Int8 => Some(VariableData::Int8(*lf64 as i8)),
                TypeInfo::Int16 => Some(VariableData::Int16(*lf64 as i16)),
                TypeInfo::Int32 => Some(VariableData::Int32(*lf64 as i32)),
                TypeInfo::Int64 => Some(VariableData::Int64(*lf64 as i64)),
                TypeInfo::Float32 => Some(VariableData::Float32(*lf64 as f32)),
                TypeInfo::Float64 => Some(VariableData::Float64(*lf64)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn from_i64(i: i64, typeinfo: &TypeInfo) -> VariableData {
        match typeinfo {
            TypeInfo::Int8 => VariableData::Int8(i as i8),
            TypeInfo::Int16 => VariableData::Int16(i as i16),
            TypeInfo::Int32 => VariableData::Int32(i as i32),
            TypeInfo::Int64 => VariableData::Int64(i),
            TypeInfo::UInt8 => VariableData::UInt8(i as u8),
            TypeInfo::UInt16 => VariableData::UInt16(i as u16),
            TypeInfo::UInt32 => VariableData::UInt32(i as u32),
            TypeInfo::UInt64 => VariableData::UInt64(i as u64),
            _ => panic!("VariableData::from_i64: {:?}", typeinfo),
        }
    }
    pub fn from_u64(i: u64, typeinfo: &TypeInfo) -> VariableData {
        match typeinfo {
            TypeInfo::Int8 => VariableData::Int8(i as i8),
            TypeInfo::Int16 => VariableData::Int16(i as i16),
            TypeInfo::Int32 => VariableData::Int32(i as i32),
            TypeInfo::Int64 => VariableData::Int64(i as i64),
            TypeInfo::UInt8 => VariableData::UInt8(i as u8),
            TypeInfo::UInt16 => VariableData::UInt16(i as u16),
            TypeInfo::UInt32 => VariableData::UInt32(i as u32),
            TypeInfo::UInt64 => VariableData::UInt64(i),
            _ => panic!("VariableData::from_u64: {:?}", typeinfo),
        }
    }
    pub fn from_f64(f: f64, typeinfo: &TypeInfo) -> VariableData {
        match typeinfo {
            TypeInfo::Float32 => VariableData::Float32(f as f32),
            TypeInfo::Float64 => VariableData::Float64(f),
            _ => panic!("VariableData::from_f64: {:?}", typeinfo),
        }
    }
    pub fn to_i64(&self) -> i64 {
        match self {
            VariableData::Int8(i) => *i as i64,
            VariableData::Int16(i) => *i as i64,
            VariableData::Int32(i) => *i as i64,
            VariableData::Int64(i) => *i,
            VariableData::UInt8(i) => *i as i64,
            VariableData::UInt16(i) => *i as i64,
            VariableData::UInt32(i) => *i as i64,
            VariableData::UInt64(i) => *i as i64,
            _ => panic!("VariableData::to_i64: {:?}", self),
        }
    }
    pub fn to_u64(&self) -> u64 {
        match self {
            VariableData::Int8(i) => *i as u64,
            VariableData::Int16(i) => *i as u64,
            VariableData::Int32(i) => *i as u64,
            VariableData::Int64(i) => *i as u64,
            VariableData::UInt8(i) => *i as u64,
            VariableData::UInt16(i) => *i as u64,
            VariableData::UInt32(i) => *i as u64,
            VariableData::UInt64(i) => *i,
            _ => panic!("VariableData::to_u64: {:?}", self),
        }
    }
    pub fn to_f64(&self) -> f64 {
        match self {
            VariableData::Int8(i) => *i as f64,
            VariableData::Int16(i) => *i as f64,
            VariableData::Int32(i) => *i as f64,
            VariableData::Int64(i) => *i as f64,
            VariableData::UInt8(i) => *i as f64,
            VariableData::UInt16(i) => *i as f64,
            VariableData::UInt32(i) => *i as f64,
            VariableData::UInt64(i) => *i as f64,
            VariableData::Float32(f) => *f as f64,
            VariableData::Float64(f) => *f,
            _ => panic!("VariableData::to_f64: {:?}", self),
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        match self {
            VariableData::Int8(_) => true,
            VariableData::Int16(_) => true,
            VariableData::Int32(_) => true,
            VariableData::Int64(_) => true,
            _ => false,
        }
    }
    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            VariableData::UInt8(_) => true,
            VariableData::UInt16(_) => true,
            VariableData::UInt32(_) => true,
            VariableData::UInt64(_) => true,
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            VariableData::Float32(_) => true,
            VariableData::Float64(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructData {
    pub fields: HashMap<String, Rc<RefCell<(TypeInfo, VariableData)>>>,
}
#[derive(Debug, Clone)]
pub struct UnionData {
    pub fields: HashMap<String, Rc<RefCell<(TypeInfo, VariableData)>>>,
}

#[derive(Debug, Clone)]
pub struct EnumData {
    pub value: i64,
}
