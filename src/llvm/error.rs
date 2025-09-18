#[derive(Debug)]
pub enum CompileError {
    BuilderError(inkwell::builder::BuilderError),
}
