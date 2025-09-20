use super::{CVType, PrimitiveType};

#[derive(Debug, Clone)]
pub enum CompileError {
    InvalidCase,
    InvalidContinue,
    InvalidBreak,
    /// switch body must be a compound statement '{' ... '}'.
    /// switch body must start with `case` or `default`.
    InvalidSwitchBody,
    SwitchConditionNotInt(PrimitiveType),
    InvalidReturn,
    InvalidDefault,
    MultipleDefault,

    NoFunctionName,
    InvalidFunctionDefinition,

    LabelDefinitionOutsideFunction(String),
    MultipleLabelDefinition(String),

    GotoOutsideFunction(String),
    GotoInvalidLabel(String), // goto a label that variable scope is not matching
    LabelNotDefined(String),  // goto undefined label

    DeclarationWithoutName,

    /// same member name in struct or union or enum
    MemberRedefined(String),

    BracketOnNonArrayOrPointer,
    BracketIndexNotInteger(PrimitiveType),

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

    NestedFunctionDefinition(String),

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

    /// not possible type cast from PrimitiveType to PrimitiveType
    InvalidCast(PrimitiveType, PrimitiveType),

    /// there is no common type between PrimitiveType and PrimitiveType for operation
    NoCommonType(PrimitiveType, PrimitiveType),

    CallWithWrongNumberOfArguments,
    CallWithWrongArgumentType,

    InvalidOperandType(PrimitiveType, PrimitiveType),

    EnumValueNotInteger,

    /// Must be convertible to bool (integer type)
    InvalidIfCondition(PrimitiveType),
    InitializeTypeMismatch(PrimitiveType, PrimitiveType),

    DistinctPointer(CVType, CVType),

    PointerSubDifferentType,

    InvalidStorageClassSpecifier,

    ReturnTypeMismatch(String),

    ArrayInitializeWithNonInitializerList(PrimitiveType),
    ArrayInitializerTooManyElements(String, usize, usize),
}
