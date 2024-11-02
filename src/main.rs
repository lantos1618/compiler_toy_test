// src/main.rs

mod ast;
mod codegen;
mod errors;

fn main() {
    // Create a sample program manually
    let program = ast::Program {
        functions: vec![
            // External function declaration (e.g., 'printf' from C standard library)
            // int printf(const char *format, ...)
            ast::Function {
                name: "printf".to_string(),
                params: vec![
                    ast::Parameter {
                        name: "format".to_string(),
                        typ: ast::AstType::Pointer(Box::new(ast::AstType::Int8)),
                    },
                ],
                return_type: ast::AstType::Int32,
                body: None,
                external_lib: Some("libc".to_string()),
            },
            // Main function
            ast::Function {
                name: "main".to_string(),
                params: vec![],
                return_type: ast::AstType::Int32,
                body: Some(ast::AstBlock {
                    statements: vec![
                        // x = 42
                        ast::Stmt::VarDecl(ast::VarDecl {
                            name: "x".to_string(),
                            typ: ast::AstType::Int64,
                            initializer: Some(ast::Expr::IntLiteral(42)),
                            is_mutable: true,
                        }),
                        // Call to puts("Hello, World!")
                        ast::Stmt::Expression(ast::Expr::Call {
                            function: Box::new(ast::Expr::Variable("puts".to_string())),
                            arguments: vec![ast::Expr::StringLiteral("Hello, World!\0".to_string())],
                        }),
                        // Return x
                        ast::Stmt::Return(Some(ast::Expr::Variable("x".to_string()))),
                    ],
                }),
                external_lib: None,
            },
        ],
        structs: vec![],
        globals: vec![],
    };

    let mut codegen = codegen::CodeGenerator::new();

    match codegen.compile_program(&program) {
        Ok(_) => {
            println!("Program compiled successfully");
            // Execute the main function
            let main_func = codegen
                .module
                .get_function_by_name("main")
                .expect("Function 'main' not found");

            // Finalize the module and get the function pointer
            let code = codegen.module.get_finalized_function(main_func);

            // Cast the function pointer to a callable type
            let main_fn = unsafe { std::mem::transmute::<_, fn() -> i32>(code) };

            // Call the main function
            let result = main_fn();

            println!("Program exited with code {}", result);
        }
        Err(e) => {
            eprintln!("Error during code generation: {}", e);
        }
    }
}
