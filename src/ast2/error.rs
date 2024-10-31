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

    /// same member name in struct or union or enum
    MemberRedefined(String),

    BracketOnNonArrayOrPointer,
    BracketIndexNotInteger,

    /// variable declaration incomplete type
    IncompleteType,

    /// use 'struct A' but A is not struct
    StructMismatch(String),
    /// use 'union A' but A is not union
    UnionMismatch(String),
    /// use 'enum A' but A is not enum
    EnumMismatch(String),

    SizeofIncompleteType,
    AlignofIncompleteType,

    NegativeArraySize,
    ArraySizeNotInteger,

    MemberOnNonStructOrUnion,
    MemberOnIncompleteType,
    MemberNotFound(String),

    ArrowOnNonPointer,
    ArrowOnNonStructOrUnion,
    ArrowOnIncompleteType,
    ArrowNotFound(String),

    MultipleVariableDefinition(String),
    VariableNotFound(String),

    NestedFunctionDefinition,

    FunctionDifferentSignature(String),
    MultipleFunctionDefinition(String),

    TypeRedifinition(String),
    TypeNotFound(String),

    /// ex) `int short;`, `unsigned long signed;` invalid combination
    InvalidTypeSpecifier,

    CallNonFunction,

    ConditionalTypeMismatch,
    ConditionalNotBool,

    BitwiseOpOnNonInteger,
    LogicalOpOnNonBool,
    ArithmeticOpOnNonNumeric,
    DereferenceOnNonPointer,

    NotAssignable,
    AssignToArray,
    AssignToConst,
}
