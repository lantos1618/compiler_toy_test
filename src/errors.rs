// src/errors.rs

use crate::ast::*;

use thiserror::Error;
use cranelift_module::ModuleError;
use cranelift_codegen::CodegenError;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Undefined variable '{0}'")]
    UndefinedVariable(String),

    #[error("Undefined function '{0}'")]
    UndefinedFunction(String),

    #[error("Unsupported type: {0}")]
    UnsupportedType(String),

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String },

    #[error("Function '{0}' has no body")]
    MissingFunctionBody(String),

    #[error("Invalid assignment target")]
    InvalidAssignmentTarget,

    #[error("Statement not implemented: {0:?}")]
    UnimplementedStatement(Stmt),

    #[error("Expression not implemented: {0:?}")]
    UnimplementedExpression(Expr),

    #[error("Binary operation not implemented: {0:?}")]
    UnimplementedBinaryOp(BinaryOp),

    #[error("Cannot create default value for type {0:?}")]
    DefaultValueError(cranelift::prelude::types::Type),

    #[error("Cranelift error: {0}")]
    CraneliftError(String),

    #[error("Module error: {0}")]
    ModuleError(#[from] ModuleError),
}
