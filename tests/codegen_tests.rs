// tests/codegen_test.rs

#[cfg(test)]
mod codegen_tests {
    use compiler::ast::*;
    use compiler::codegen::*;
    use compiler::errors::*;
    use cranelift::prelude::*;
    use cranelift_jit::{JITBuilder, JITModule};

    fn compile_and_run(program: &Program) -> Result<i64, CompileError> {
        // Set up the target ISA
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder()
            .map_err(|e| CompileError::CraneliftError(e.to_string()))?;
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| CompileError::CraneliftError(e.to_string()))?;

        // Create JIT instance
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);

        let mut codegen = create_jit_code_generator()?;

        // Generate code for the program
        codegen.gen_program(program)?;

        // Look up the main function
        let main_fn = codegen.get_function("main")?;
        
        // Get the function pointer and cast it to the right type
        let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(main_fn) };
        
        // Call the function
        Ok(main())
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

#[test]
fn test_while_loop() {
    // Test a simple while loop that counts to 5
    // int main() { 
    //     int i = 0;
    //     int sum = 0;
    //     while (i < 5) {
    //         sum = sum + i;
    //         i = i + 1;
    //     }
    //     return sum;
    // }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    // Initialize i = 0
                    Stmt::VarDecl(VarDecl {
                        name: "i".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: true,
                    }),
                    // Initialize sum = 0
                    Stmt::VarDecl(VarDecl {
                        name: "sum".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: true,
                    }),
                    // while (i < 5)
                    Stmt::While {
                        condition: Expr::Binary {
                            op: BinaryOp::LessThan,
                            left: Box::new(Expr::Variable("i".to_string())),
                            right: Box::new(Expr::IntLiteral(5)),
                        },
                        body: Box::new(Stmt::Block(AstBlock {
                            statements: vec![
                                // sum = sum + i
                                Stmt::Assignment {
                                    target: Expr::Variable("sum".to_string()),
                                    value: Expr::Binary {
                                        op: BinaryOp::Add,
                                        left: Box::new(Expr::Variable("sum".to_string())),
                                        right: Box::new(Expr::Variable("i".to_string())),
                                    },
                                },
                                // i = i + 1
                                Stmt::Assignment {
                                    target: Expr::Variable("i".to_string()),
                                    value: Expr::Binary {
                                        op: BinaryOp::Add,
                                        left: Box::new(Expr::Variable("i".to_string())),
                                        right: Box::new(Expr::IntLiteral(1)),
                                    },
                                },
                            ],
                        })),
                    },
                    // return sum
                    Stmt::Return(Some(Expr::Variable("sum".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 10); // 0 + 1 + 2 + 3 + 4 = 10
}
#[test]
fn test_nested_while_loops() {
    // Tests nested while loops
    // int main() {
    //     int i = 0, j = 0, sum = 0;
    //     while (i < 3) {
    //         j = 0;
    //         while (j < 2) {
    //             sum = sum + (i * j);
    //             j = j + 1;
    //         }
    //         i = i + 1;
    //     }
    //     return sum;
    // }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    // Initialize variables
                    Stmt::VarDecl(VarDecl {
                        name: "i".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: true,
                    }),
                    Stmt::VarDecl(VarDecl {
                        name: "j".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: true,
                    }),
                    Stmt::VarDecl(VarDecl {
                        name: "sum".to_string(),
                        typ: AstType::Int64,
                        initializer: Some(Expr::IntLiteral(0)),
                        is_mutable: true,
                    }),
                    // Outer while loop
                    Stmt::While {
                        condition: Expr::Binary {
                            op: BinaryOp::LessThan,
                            left: Box::new(Expr::Variable("i".to_string())),
                            right: Box::new(Expr::IntLiteral(3)),
                        },
                        body: Box::new(Stmt::Block(AstBlock {
                            statements: vec![
                                // Reset j
                                Stmt::Assignment {
                                    target: Expr::Variable("j".to_string()),
                                    value: Expr::IntLiteral(0),
                                },
                                // Inner while loop
                                Stmt::While {
                                    condition: Expr::Binary {
                                        op: BinaryOp::LessThan,
                                        left: Box::new(Expr::Variable("j".to_string())),
                                        right: Box::new(Expr::IntLiteral(2)),
                                    },
                                    body: Box::new(Stmt::Block(AstBlock {
                                        statements: vec![
                                            // sum += i * j
                                            Stmt::Assignment {
                                                target: Expr::Variable("sum".to_string()),
                                                value: Expr::Binary {
                                                    op: BinaryOp::Add,
                                                    left: Box::new(Expr::Variable("sum".to_string())),
                                                    right: Box::new(Expr::Binary {
                                                        op: BinaryOp::Multiply,
                                                        left: Box::new(Expr::Variable("i".to_string())),
                                                        right: Box::new(Expr::Variable("j".to_string())),
                                                    }),
                                                },
                                            },
                                            // j++
                                            Stmt::Assignment {
                                                target: Expr::Variable("j".to_string()),
                                                value: Expr::Binary {
                                                    op: BinaryOp::Add,
                                                    left: Box::new(Expr::Variable("j".to_string())),
                                                    right: Box::new(Expr::IntLiteral(1)),
                                                },
                                            },
                                        ],
                                    })),
                                },
                                // i++
                                Stmt::Assignment {
                                    target: Expr::Variable("i".to_string()),
                                    value: Expr::Binary {
                                        op: BinaryOp::Add,
                                        left: Box::new(Expr::Variable("i".to_string())),
                                        right: Box::new(Expr::IntLiteral(1)),
                                    },
                                },
                            ],
                        })),
                    },
                    // Return sum
                    Stmt::Return(Some(Expr::Variable("sum".to_string()))),
                ],
            }),
            external_lib: None,
        }],
        globals: vec![],
    };

    let result = compile_and_run(&program);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 3); // Corrected expected result
}

#[test]
fn test_external_function_call() {
    // Tests calling an external function (like puts)
    let program = Program {
        structs: vec![],
        functions: vec![
            // Declare external puts function
            Function {
                name: "puts".to_string(),
                params: vec![Param {
                    name: "s".to_string(),
                    typ: AstType::Pointer(Box::new(AstType::Int8)),
                }],
                return_type: AstType::Int32,
                body: None,
                external_lib: Some("c".to_string()),
            },
            // Main function that calls puts
            Function {
                name: "main".to_string(),
                params: vec![],
                return_type: AstType::Int64,
                body: Some(AstBlock {
                    statements: vec![
                        Stmt::Expression(Expr::Call {
                            function: Box::new(Expr::Variable("puts".to_string())),
                            arguments: vec![Expr::StringLiteral("Hello, World!".to_string())],
                        }),
                        Stmt::Return(Some(Expr::IntLiteral(0))),
                    ],
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
fn test_complex_arithmetic() {
    // Tests more complex arithmetic expressions with operator precedence
    // int main() {
    //     return (2 + 3 * 4) / 2 - (5 % 3) * 2;
    // }
    let program = Program {
        structs: vec![],
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: AstType::Int64,
            body: Some(AstBlock {
                statements: vec![
                    Stmt::Return(Some(Expr::Binary {
                        op: BinaryOp::Subtract,
                        left: Box::new(Expr::Binary {
                            op: BinaryOp::Divide,
                            left: Box::new(Expr::Binary {
                                op: BinaryOp::Add,
                                left: Box::new(Expr::IntLiteral(2)),
                                right: Box::new(Expr::Binary {
                                    op: BinaryOp::Multiply,
                                    left: Box::new(Expr::IntLiteral(3)),
                                    right: Box::new(Expr::IntLiteral(4)),
                                }),
                            }),
                            right: Box::new(Expr::IntLiteral(2)),
                        }),
                        right: Box::new(Expr::Binary {
                            op: BinaryOp::Multiply,
                            left: Box::new(Expr::Binary {
                                op: BinaryOp::Modulo,
                                left: Box::new(Expr::IntLiteral(5)),
                                right: Box::new(Expr::IntLiteral(3)),
                            }),
                            right: Box::new(Expr::IntLiteral(2)),
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
    assert_eq!(result.unwrap(), 3); // (2 + 12) / 2 - (2 * 2) = 7 - 4 = 3
}
}