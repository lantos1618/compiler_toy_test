use compiler::{ast::*, codegen::create_jit_code_generator};
use cranelift_codegen::gimli::Expression;



fn main() {

    let printf_func = Function {
        name: "printf".to_string(),
        params: vec![
            Param {
                name: "format".to_string(),
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
        return_type: AstType::Int32,
        body: Some(AstBlock {
            statements: vec![
                Stmt::Expression(*Box::new(
                    Expr::Call {
                        function: Box::new(Expr::Variable("printf".to_string())),
                        arguments: vec![Expr::StringLiteral("Hello, World!\n".to_string())],
                    }
                )),
                Stmt::Return(Some(Expr::IntLiteral(0))),
            ],
        }),
        external_lib: None,
    };

    let program = Program {
        functions: vec![printf_func, main_func],
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

}
