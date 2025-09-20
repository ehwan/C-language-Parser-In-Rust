use crate::semantic::CompileError;

#[derive(Debug, Clone)]
pub struct LabelInfo {
    pub name: String,
    scope_path: Option<Vec<usize>>,
    from_path: Vec<Vec<usize>>,
    pub uid: usize,
}

impl LabelInfo {
    pub fn new(name: String, uid: usize) -> Self {
        Self {
            name,
            scope_path: None,
            from_path: vec![],
            uid,
        }
    }
    fn is_invalid_goto(label_path: &Vec<usize>, from_path: &Vec<usize>) -> bool {
        !from_path.starts_with(label_path)
    }
    pub fn get_label_scope(&self) -> Option<&Vec<usize>> {
        self.scope_path.as_ref()
    }
    pub fn set_label_scope(&mut self, scope_path: Vec<usize>) -> Result<(), CompileError> {
        if self.scope_path.is_some() {
            return Err(CompileError::MultipleLabelDefinition(self.name.clone()));
        }
        for from_path in &self.from_path {
            if Self::is_invalid_goto(&scope_path, from_path) {
                return Err(CompileError::GotoInvalidLabel(self.name.clone()));
            }
        }
        self.scope_path = Some(scope_path);
        Ok(())
    }
    pub fn add_from_scope(&mut self, from_path: Vec<usize>) -> Result<(), CompileError> {
        if let Some(label_path) = &self.scope_path {
            if Self::is_invalid_goto(label_path, &from_path) {
                return Err(CompileError::GotoInvalidLabel(self.name.clone()));
            }
        }
        self.from_path.push(from_path);
        Ok(())
    }
}
