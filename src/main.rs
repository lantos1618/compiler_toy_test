use compiler::{ast::*, codegen::create_jit_code_generator};
use cranelift_codegen::gimli::Expression;



fn main() {

    let print_func = Function {
        name: "puts".to_string(),
        params: vec![
            Param {
                name: "str".to_string(),
                typ: AstType::Pointer(Box::new(AstType::Int8)),
            }
        ],
        return_type: AstType::Int32,
        body: None,
        external_lib: Some("c".to_string()), // Links to libc
    };

    let main_func = Function {
        name: "main".to_string(),
        params: vec![],
        return_type: AstType::Int64,
        body: Some(AstBlock {
            statements: vec![
                // Initialize counter
                Stmt::VarDecl(VarDecl {
                    name: "i".to_string(),
                    typ: AstType::Int64,
                    initializer: Some(Expr::IntLiteral(0)),
                    is_mutable: true,  // Important: make it mutable!
                }),
                
                Stmt::While { 
                    condition: Expr::Binary {
                        op: BinaryOp::LessThan,
                        left: Box::new(Expr::Variable("i".to_string())),
                        right: Box::new(Expr::IntLiteral(10)),
                    },
                    body: Box::new(Stmt::Block(AstBlock {
                        statements: vec![
                            // Print message
                            Stmt::Expression(Expr::Call {
                                function: Box::new(Expr::Variable("puts".to_string())),
                                arguments: vec![Expr::StringLiteral("Hello, World!\n".to_string())],
                            }),
                            // Increment i
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
                Stmt::Return(Some(Expr::IntLiteral(0))),
            ],
        }),
        external_lib: None,
    };

    let program = Program {
        functions: vec![print_func, main_func],
        structs: vec![],
        globals: vec![],
    };

    let mut codegen = match create_jit_code_generator() {
        Ok(codegen) => codegen,
        Err(e) => {
            eprintln!("Error creating code generator: {}", e);
            return;
        }
    };
    codegen.gen_program(&program).unwrap();

    let main_fn = codegen.get_function("main").unwrap();
    let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(main_fn) };
    main();

}
