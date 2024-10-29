#[derive(Debug, Clone)]
pub enum CompileError {
    InvalidCase,
    InvalidContinue,
    InvalidBreak,
    /// switch body must be a compound statement '{' ... '}'.
    /// switch body must start with `case` or `default`.
    InvalidSwitchBody,
    InvalidReturn,
    InvalidDefault,
    MultipleDefault,

    NoFunctionName,
    InvalidFunctionDefinition,

    LabelDefinitionOutsideFunction,
    MultipleLabelDefinition(String),

    GotoOutsideFunction,
    GotoInvalidLabel(String),

    DeclarationWithoutName,

    BracketOnNonArrayOrPointer,
    BracketIndexNotInteger,

    ArrowOnNonPointer,

    SizeofIncompleteType,

    NegativeArraySize,
    ArraySizeNotInteger,

    MemberAccessOnNonStructOrUnion,

    MultipleVariableDefinition(String),
    VariableNotFound(String),

    NestedFunctionDefinition,

    FunctionDifferentSignature(String),
    MultipleFunctionDefinition(String),
}
