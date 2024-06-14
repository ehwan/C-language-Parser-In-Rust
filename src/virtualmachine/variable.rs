use crate::ast::typename::TypeInfo;

#[derive(Debug, Clone)]
pub enum VariableData {
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
}

impl Default for VariableData {
    fn default() -> Self {
        VariableData::Int32(0)
    }
}

impl VariableData {
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*lu8 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*lu16 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*lu32 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*lu64 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*li8 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*li16 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*li32 as u64)),
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
                TypeInfo::Pointer(_) => Some(VariableData::UInt64(*li64 as u64)),
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
            TypeInfo::Pointer(_) => VariableData::UInt64(i as u64),
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
            TypeInfo::Pointer(_) => VariableData::UInt64(i as u64),
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
