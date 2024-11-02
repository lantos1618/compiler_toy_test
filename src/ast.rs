// ast.rs

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
    pub globals: Vec<VarDecl>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: AstType,
    pub body: Option<AstBlock>, // None for external functions
    pub external_lib: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub typ: AstType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub typ: AstType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub typ: AstType,
    pub initializer: Option<Expr>,
    pub is_mutable: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum AstType {
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Boolean,
    Char,
    Void,
    String,
    Struct(String),             // Named struct type
    Array(Box<AstType>, usize),    // Array type with element type and size
    Pointer(Box<AstType>),         // Pointer to type
    Function(Box<FunctionType>), // Function type
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<AstType>,
    pub return_type: Box<AstType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct AstBlock {
    pub statements: Vec<Stmt>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    Assignment {
        target: Expr,  // Can be variable or field access
        value: Expr,
    },
    Block(AstBlock),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Box<Stmt>,
    },
    Return(Option<Expr>),
    Break,
    Continue,
    Expression(Expr),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    CharLiteral(char),
    StringLiteral(String),
    Variable(String),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        function: Box<Expr>, // Supports calling lambdas
        arguments: Vec<Expr>,
    },
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    FieldAccess {
        expr: Box<Expr>,
        field: String,
    },
    ArrayInit {
        elements: Vec<Expr>,
    },
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        typ: AstType,
    },
    Lambda {
        params: Vec<Param>,
        return_type: Option<AstType>,
        body: AstBlock,
        captures: Vec<String>,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,       // Logical AND
    Or,        // Logical OR
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,    // Arithmetic negation
    Not,       // Logical NOT
    BitNot,    // Bitwise NOT
}
