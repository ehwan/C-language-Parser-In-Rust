use super::CVType;

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub cv_type: CVType,
}
