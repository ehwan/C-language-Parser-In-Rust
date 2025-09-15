use super::CompileError;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Integer {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
}
impl Integer {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Integer::Int8 | Integer::Int16 | Integer::Int32 | Integer::Int64
        )
    }
    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Integer::UInt8 | Integer::UInt16 | Integer::UInt32 | Integer::UInt64
        )
    }
    pub fn to_unsigned(&self) -> Self {
        match self {
            Integer::Int8 => Integer::UInt8,
            Integer::Int16 => Integer::UInt16,
            Integer::Int32 => Integer::UInt32,
            Integer::Int64 => Integer::UInt64,
            _ => *self,
        }
    }
    pub fn sizeof(&self) -> usize {
        match self {
            Integer::UInt8 | Integer::Int8 => 1,
            Integer::UInt16 | Integer::Int16 => 2,
            Integer::UInt32 | Integer::Int32 => 4,
            Integer::UInt64 | Integer::Int64 => 8,
        }
    }
    pub fn alignof(&self) -> usize {
        match self {
            Integer::UInt8 | Integer::Int8 => 1,
            Integer::UInt16 | Integer::Int16 => 2,
            Integer::UInt32 | Integer::Int32 => 4,
            Integer::UInt64 | Integer::Int64 => 8,
        }
    }
    pub fn from_size_signed(size: usize, unsigned: bool) -> Self {
        match (size, unsigned) {
            (1, true) => Integer::UInt8,
            (2, true) => Integer::UInt16,
            (4, true) => Integer::UInt32,
            (8, true) => Integer::UInt64,
            (1, false) => Integer::Int8,
            (2, false) => Integer::Int16,
            (4, false) => Integer::Int32,
            (8, false) => Integer::Int64,
            _ => panic!("Invalid size"),
        }
    }
    pub fn common_type(&self, other: &Integer) -> Self {
        let size = self.sizeof().max(other.sizeof());
        let unsigned = self.is_unsigned() || other.is_unsigned();
        Integer::from_size_signed(size, unsigned)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Float {
    Float32,
    Float64,
}
impl Float {
    pub fn sizeof(&self) -> usize {
        match self {
            Float::Float32 => 4,
            Float::Float64 => 8,
        }
    }
    pub fn alignof(&self) -> usize {
        match self {
            Float::Float32 => 4,
            Float::Float64 => 8,
        }
    }
    pub fn common_type(&self, other: &Float) -> Self {
        match (self, other) {
            (Float::Float64, _) => Float::Float64,
            (_, Float::Float64) => Float::Float64,
            _ => Float::Float32,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    Void,
    Integer(Integer),
    Float(Float),

    Pointer(Box<CVType>),
    Array(ArrayType),

    Struct(StructType),
    Union(StructType),
    Enum(EnumType),
    Function(FunctionType),
}
impl PrimitiveType {
    // @TODO
    pub fn is_castable(&self, to: &PrimitiveType) -> bool {
        match (self, to) {
            (PrimitiveType::Integer(_), PrimitiveType::Integer(_)) => true,
            (PrimitiveType::Integer(_), PrimitiveType::Float(_)) => true,
            (PrimitiveType::Integer(_), PrimitiveType::Pointer(_)) => true,

            (PrimitiveType::Float(_), PrimitiveType::Integer(_)) => true,
            (PrimitiveType::Float(_), PrimitiveType::Float(_)) => true,

            (PrimitiveType::Pointer(_), PrimitiveType::Pointer(_)) => true,
            (PrimitiveType::Pointer(_), PrimitiveType::Integer(_)) => true,

            (PrimitiveType::Array(_), PrimitiveType::Integer(_)) => true,
            (PrimitiveType::Array(_), PrimitiveType::Pointer(_)) => true,

            _ => false,
        }
    }
    pub fn is_implicitly_castable(&self, to: &PrimitiveType) -> bool {
        match (self, to) {
            (PrimitiveType::Integer(_), PrimitiveType::Integer(_)) => true,
            (PrimitiveType::Integer(_), PrimitiveType::Float(_)) => true,

            (PrimitiveType::Float(_), PrimitiveType::Float(_)) => true,

            (PrimitiveType::Pointer(from), PrimitiveType::Pointer(to)) => {
                from.type_.is_implicitly_castable(&to.type_)
            }

            (PrimitiveType::Array(from), PrimitiveType::Pointer(to)) => {
                from.cv_type.type_.is_implicitly_castable(&to.type_)
            }

            _ => false,
        }
    }
    pub fn is_bool_castable(&self) -> bool {
        matches!(
            self,
            PrimitiveType::Integer(_) | PrimitiveType::Pointer(_) | PrimitiveType::Array(_)
        )
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
    pub fn sizeof(&self) -> Result<usize, CompileError> {
        Ok(match self {
            PrimitiveType::Void => return Err(CompileError::SizeofIncompleteType),
            PrimitiveType::Integer(i) => i.sizeof(),
            PrimitiveType::Float(f) => f.sizeof(),
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
            PrimitiveType::Integer(i) => i.alignof(),
            PrimitiveType::Float(f) => f.alignof(),
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

    // @TODO
    pub fn common_type(&self, other: &PrimitiveType) -> Option<PrimitiveType> {
        match (self, other) {
            (PrimitiveType::Void, _) => None,
            (_, PrimitiveType::Void) => None,

            (PrimitiveType::Integer(i1), PrimitiveType::Integer(i2)) => {
                Some(PrimitiveType::Integer(i1.common_type(i2)))
            }
            (PrimitiveType::Integer(_), PrimitiveType::Float(f)) => Some(PrimitiveType::Float(*f)),
            (PrimitiveType::Integer(_), _) => None,

            (PrimitiveType::Float(f), PrimitiveType::Integer(_)) => Some(PrimitiveType::Float(*f)),
            (PrimitiveType::Float(f1), PrimitiveType::Float(f2)) => {
                Some(PrimitiveType::Float(f1.common_type(f2)))
            }
            (PrimitiveType::Float(_), _) => None,

            (PrimitiveType::Struct(s1), PrimitiveType::Struct(s2)) => {
                if s1 == s2 {
                    Some(PrimitiveType::Struct(s1.clone()))
                } else {
                    None
                }
            }
            (PrimitiveType::Struct(_), _) => None,

            (PrimitiveType::Union(s1), PrimitiveType::Union(s2)) => {
                if s1 == s2 {
                    Some(PrimitiveType::Union(s1.clone()))
                } else {
                    None
                }
            }
            (PrimitiveType::Union(_), _) => None,

            (PrimitiveType::Enum(e1), PrimitiveType::Enum(e2)) => {
                if e1 == e2 {
                    Some(PrimitiveType::Enum(e1.clone()))
                } else {
                    None
                }
            }
            (PrimitiveType::Enum(_), _) => None,

            (PrimitiveType::Pointer(p1), PrimitiveType::Pointer(p2)) => {
                if p1 == p2 {
                    Some(PrimitiveType::Pointer(p1.clone()))
                } else {
                    None
                }
            }
            (PrimitiveType::Pointer(_), _) => None,

            (PrimitiveType::Array(a1), PrimitiveType::Array(a2)) => a1
                .clone()
                .to_pointer()
                .common_type(&a2.clone().to_pointer()),
            (PrimitiveType::Array(_), _) => None,

            (PrimitiveType::Function(f1), PrimitiveType::Function(f2)) => {
                if f1 == f2 {
                    Some(PrimitiveType::Function(f1.clone()))
                } else {
                    None
                }
            }
            (PrimitiveType::Function(_), _) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayType {
    pub cv_type: Box<CVType>,
    pub size: usize,
}
impl ArrayType {
    pub fn to_pointer(self) -> PrimitiveType {
        PrimitiveType::Pointer(self.cv_type)
    }
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
            type_: Box::new(PrimitiveType::Integer(Integer::Int64)),
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
    pub fn sizeof(&self) -> Result<usize, CompileError> {
        self.type_.sizeof()
    }
    pub fn alignof(&self) -> Result<usize, CompileError> {
        self.type_.alignof()
    }
}

impl std::ops::Deref for CVType {
    type Target = PrimitiveType;
    fn deref(&self) -> &Self::Target {
        &self.type_
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
