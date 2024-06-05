use std::any::Any;
use std::boxed::Box;

pub enum VariableType {
    Int,
    Float,
    Bool,
}
pub struct Variable {
    pub data: Box<dyn Any>,
}
