use super::CVType;

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    /// Unique identifier for the variable
    pub uid: usize,
    pub cv_type: CVType,
}
