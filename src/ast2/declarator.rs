use super::CVType;
use super::CompileError;
use super::PrimitiveType;

#[derive(Debug, Clone)]
pub struct CombinedDeclarator {
    /// variable name, for direct declarator
    pub name: Option<String>,
    pub cv_type: CVType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SpecifierQualifierCollector {
    pub const_: bool,
    pub volatile: bool,

    pub void: bool,

    pub unsigned: bool,
    pub signed: bool,

    pub int: bool,
    pub char: bool,
    pub short: bool,
    pub long: bool,

    pub float: bool,
    pub double: bool,

    pub type_: Option<CVType>,
}

impl SpecifierQualifierCollector {
    pub fn new() -> Self {
        Self {
            const_: false,
            volatile: false,

            void: false,

            unsigned: false,
            signed: false,

            int: false,
            char: false,
            short: false,
            long: false,

            float: false,
            double: false,

            type_: None,
        }
    }
    pub fn set_const(&mut self) -> Result<(), CompileError> {
        if self.void {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.const_ = true;
        Ok(())
    }
    pub fn set_volatile(&mut self) -> Result<(), CompileError> {
        if self.void {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.volatile = true;
        Ok(())
    }
    pub fn set_int(&mut self) -> Result<(), CompileError> {
        if self.void || self.char || self.float || self.double || self.type_.is_some() {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.int = true;
        Ok(())
    }
    pub fn set_char(&mut self) -> Result<(), CompileError> {
        if self.void
            || self.int
            || self.short
            || self.long
            || self.float
            || self.double
            || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.char = true;
        Ok(())
    }
    pub fn set_short(&mut self) -> Result<(), CompileError> {
        if self.void || self.char || self.long || self.float || self.double || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.short = true;
        Ok(())
    }
    pub fn set_long(&mut self) -> Result<(), CompileError> {
        if self.void || self.char || self.short || self.float || self.double || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.long = true;
        Ok(())
    }
    pub fn set_signed(&mut self) -> Result<(), CompileError> {
        if self.void || self.signed || self.float || self.double || self.type_.is_some() {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.signed = true;
        Ok(())
    }
    pub fn set_unsigned(&mut self) -> Result<(), CompileError> {
        if self.void || self.signed || self.float || self.double || self.type_.is_some() {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.unsigned = true;
        Ok(())
    }
    pub fn set_float(&mut self) -> Result<(), CompileError> {
        if self.void
            || self.unsigned
            || self.signed
            || self.int
            || self.char
            || self.short
            || self.long
            || self.double
            || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.float = true;
        Ok(())
    }
    pub fn set_double(&mut self) -> Result<(), CompileError> {
        if self.void
            || self.unsigned
            || self.signed
            || self.int
            || self.char
            || self.short
            || self.long
            || self.float
            || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.double = true;
        Ok(())
    }
    pub fn set_void(&mut self) -> Result<(), CompileError> {
        if self.const_
            || self.volatile
            || self.unsigned
            || self.signed
            || self.int
            || self.char
            || self.short
            || self.long
            || self.float
            || self.double
            || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.void = true;
        Ok(())
    }
    pub fn set_typename(&mut self, cv_type: CVType) -> Result<(), CompileError> {
        if self.void
            || self.unsigned
            || self.signed
            || self.int
            || self.char
            || self.short
            || self.long
            || self.float
            || self.double
            || self.type_.is_some()
        {
            return Err(CompileError::InvalidTypeSpecifier);
        }
        self.type_ = Some(cv_type);
        Ok(())
    }

    pub fn into_type(self) -> Result<CVType, CompileError> {
        let mut base_type = {
            if self.void {
                CVType::from_primitive(PrimitiveType::Void)
            } else if self.float {
                CVType::from_primitive(PrimitiveType::Float32)
            } else if self.double {
                CVType::from_primitive(PrimitiveType::Float64)
            } else if self.char {
                if self.unsigned {
                    CVType::from_primitive(PrimitiveType::UInt8)
                } else {
                    CVType::from_primitive(PrimitiveType::Int8)
                }
            } else if self.short {
                if self.unsigned {
                    CVType::from_primitive(PrimitiveType::UInt16)
                } else {
                    CVType::from_primitive(PrimitiveType::Int16)
                }
            } else if self.long {
                if self.unsigned {
                    CVType::from_primitive(PrimitiveType::UInt64)
                } else {
                    CVType::from_primitive(PrimitiveType::Int64)
                }
            } else if self.int {
                if self.unsigned {
                    CVType::from_primitive(PrimitiveType::UInt32)
                } else {
                    CVType::from_primitive(PrimitiveType::Int32)
                }
            } else {
                match self.type_ {
                    Some(t) => t,
                    None => return Err(CompileError::InvalidTypeSpecifier),
                }
            }
        };
        base_type.const_ = self.const_;
        base_type.volatile = self.volatile;
        Ok(base_type)
    }
}
