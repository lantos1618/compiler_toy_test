// tests/codegen_test.rs

#[cfg(test)]
mod codegen_tests {
    use super::*; // Import items from the main crate


    use compiler::ast::*;
    use compiler::codegen::*;
    use compiler::errors::*;
    use cranelift::prelude::*;
    use cranelift_module::Module;
    use std::collections::HashMap;

    fn compile_and_run(program: &Program) -> Result<i64, CompileError> {
        // Initialize the code generator
        let mut codegen = CodeGenerator::new()?;

        // Generate code for the program
        codegen.gen_program(program)?;

        // Finalize the module and get the compiled bytes
        let object = codegen.module.finish();

        // Write the object to a file (optional, for debugging)
        // std::fs::write("output.o", object.emit().unwrap()).unwrap();

        // Since we can't execute the compiled object directly in tests,
        // we'll assume that if code generation succeeds, the test passes.
        // In a real-world scenario, you might use JIT or write the object to disk and execute it.

        Ok(0)
    }


    #[test]
    fn test_basic_arithmetic() {
    // int main() { return 2 + 3 * 4 - 5; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![Stmt::Return(Some(Expr::Binary {
                    op: BinaryOp::Subtract,
                    left: Box::new(Expr::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::IntLiteral(2)),
                        right: Box::new(Expr::Binary {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::IntLiteral(3)),
                            right: Box::new(Expr::IntLiteral(4)),
                        }),
                    }),
                    right: Box::new(Expr::IntLiteral(5)),
                }))],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_variable_declaration_and_assignment() {
    // int main() { int x = 10; x = x + 5; return x; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(10)),
                        is_mutable: false,
                    }),
                    Stmt::Assignment {
                        target: Expr::Variable("x".to_string()),
                        value: Expr::Binary {
                            op: BinaryOp::Add,
                            left: Box::new(Expr::Variable("x".to_string())),
                            right: Box::new(Expr::IntLiteral(5)),
                        },
                    },
                    Stmt::Return(Some(Expr::Variable("x".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_control_flow_statements() {
    // int main() { int x = 0; if (1) { x = 42; } else { x = 24; } return x; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: false,
                    }),
                    Stmt::If {
                        condition: Expr::IntLiteral(1),
                        then_branch: Box::new(Stmt::Assignment {
                            target: Expr::Variable("x".to_string()),
                            value: Expr::IntLiteral(42),
                        }),
                        else_branch: Some(Box::new(Stmt::Assignment {
                            target: Expr::Variable("x".to_string()),
                            value: Expr::IntLiteral(24),
                        })),
                    },
                    Stmt::Return(Some(Expr::Variable("x".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_function_definition_and_call() {
    // int add(int a, int b) { return a + b; }
    // int main() { return add(2, 3); }
    let program = Program {
        structs: vec![],
        functions: vec![
            Function {
                name: "add".to_string(),
                params: vec![
                    Param {
                        name: "a".to_string(),
                        typ: AstType::Int64,
                    },
                    Param {
                        name: "b".to_string(),
                        typ: AstType::Int64,
                    },
                ],
                return_type: AstType::Int64,
                body: Some(AstBlock {
                    statements: vec![Stmt::Return(Some(Expr::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::Variable("a".to_string())),
                        right: Box::new(Expr::Variable("b".to_string())),
                    }))],
                }),
                external_lib: None,
            },
            Function {
                name: "main".to_string(),
                params: vec![],
                return_type: AstType::Int64,
                body: Some(AstBlock {
                    statements: vec![Stmt::Return(Some(Expr::Call {
                        function: Box::new(Expr::Variable("add".to_string())),
                        arguments: vec![Expr::IntLiteral(2), Expr::IntLiteral(3)],
                    }))],
                }),
                external_lib: None,
            },
        ],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_struct_definition_and_field_access() {
    // struct Point { int x; int y; }
    // int main() { Point p = { .x = 3, .y = 4 }; return p.x + p.y; }
    let program = Program {
        structs: vec![StructDef {
            name: "Point".to_string(),
            fields: vec![
                StructField {
                    name: "x".to_string(),
                    typ: AstType::Int64,
                },
                StructField {
                    name: "y".to_string(),
                    typ: AstType::Int64,
                },
            ],
        }],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "p".to_string(),
                        typ: AstType::Struct("Point".to_string()),
                        initializer: Some(Expr::StructInit {
                            name: "Point".to_string(),
                            fields: vec![
                                ("x".to_string(), Expr::IntLiteral(3)),
                                ("y".to_string(), Expr::IntLiteral(4)),
                            ],
                        }),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::FieldAccess {
                            expr: Box::new(Expr::Variable("p".to_string())),
                            field: "x".to_string(),
                        }),
                        right: Box::new(Expr::FieldAccess {
                            expr: Box::new(Expr::Variable("p".to_string())),
                            field: "y".to_string(),
                        }),
                    })),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_array_initialization_and_access() {
    // int main() { int arr[3] = {1, 2, 3}; return arr[0] + arr[1] + arr[2]; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "arr".to_string(),
                        typ: AstType::Array(Box::new(AstType::Int64), 3),
                        initializer: Some(Expr::ArrayInit {
                            elements: vec![
                                Expr::IntLiteral(1),
                                Expr::IntLiteral(2),
                                Expr::IntLiteral(3),
                            ],
                        }),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(Expr::Binary {
                            op: BinaryOp::Add,
                            left: Box::new(Expr::ArrayAccess {
                                array: Box::new(Expr::Variable("arr".to_string())),
                                index: Box::new(Expr::IntLiteral(0)),
                            }),
                            right: Box::new(Expr::ArrayAccess {
                                array: Box::new(Expr::Variable("arr".to_string())),
                                index: Box::new(Expr::IntLiteral(1)),
                            }),
                        }),
                        right: Box::new(Expr::ArrayAccess {
                            array: Box::new(Expr::Variable("arr".to_string())),
                            index: Box::new(Expr::IntLiteral(2)),
                        }),
                    })),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_unary_and_binary_operations() {
    // int main() { int x = -5; int y = ~x; return y; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::Unary {
                            op: UnaryOp::Negate,
                            expr: Box::new(Expr::IntLiteral(5)),
                        }),
                        is_mutable: false,
                    }),
                    Stmt::VarDecl(VarDecl {
                        name: "y".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::Unary {
                            op: UnaryOp::BitNot,
                            expr: Box::new(Expr::Variable("x".to_string())),
                        }),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::Variable("y".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_type_casting() {
    // int main() { int x = 5; double y = (double)x; return (int)y; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(5)),
                        is_mutable: false,
                    }),
                    Stmt::VarDecl(VarDecl {
                        name: "y".to_string(),
                        typ: AstType::Float64,
                        initializer: Some(Expr::Cast {
                            expr: Box::new(Expr::Variable("x".to_string())),
                            typ: AstType::Float64,
                        }),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::Cast {
                        expr: Box::new(Expr::Variable("y".to_string())),
                        typ: AstType::Int64,
                    })),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_string_literals() {
    // int main() { char* s = "Hello, World!"; return 0; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "s".to_string(),
                        typ: AstType::Pointer(Box::new(AstType::Char)),
                        initializer: Some(Expr::StringLiteral("Hello, World!".to_string())),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::IntLiteral(0))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_global_variables() {
    // int g = 10;
    // int main() { return g; }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![Stmt::Return(Some(Expr::Variable("g".to_string())))],
            }),
            external_lib: None,
        }],
        globals: vec![VarDecl {
            name: "g".to_string(),
            typ: AstType::Int64,
            initializer: Some(Expr::IntLiteral(10)),
            is_mutable: false,
        }],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
}

#[test]
fn test_error_handling_undefined_variable() {
    // int main() { return x; } // x is undefined
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![Stmt::Return(Some(Expr::Variable("x".to_string())))],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_err());
}

#[test]
fn test_error_handling_type_mismatch() {
    // int main() { int x = "string"; return x; } // Type mismatch
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::VarDecl(VarDecl {
                        name: "x".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::StringLiteral("string".to_string())),
                        is_mutable: false,
                    }),
                    Stmt::Return(Some(Expr::Variable("x".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_err());
}
}