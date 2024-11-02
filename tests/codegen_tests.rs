extern crate compiler;
use compiler::ast::*;
use compiler::codegen::CodeGenerator;

#[test]
fn test_simple_function() {
    let program = Program {
        functions: vec![Function {
            name: "main".to_string(),
            params: vec![],
            return_type: Type::Int64,
            body: Some(Block {
                statements: vec![Stmt::Return(Some(Expr::IntLiteral(42)))],
            }),
            is_external: false,
        }],
        structs: vec![],
        globals: vec![],
    };

    let mut codegen = CodeGenerator::new();

    match codegen.compile_program(&program) {
        Ok(_) => {
            let module = codegen.get_module();
            let main_func_id = codegen.functions.get("main").unwrap();
            let main_func = module.get_finalized_function(*main_func_id);

            let main_fn = unsafe { std::mem::transmute::<_, fn() -> i64>(main_func) };
            let result = main_fn();
            assert_eq!(result, 42);
        }
        Err(e) => {
            panic!("Compilation error: {}", e);
        }
    }
} 