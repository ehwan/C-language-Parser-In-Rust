use super::CVType;

#[derive(Debug, Clone)]
pub struct CombinedDeclarator {
    /// variable name, for direct declarator
    pub name: Option<String>,
    pub type_: CVType,
}
