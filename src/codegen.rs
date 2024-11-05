// src/codegen.rs

use cranelift::prelude::*;
use cranelift_codegen::{self, settings, Context};
use cranelift_module::{
    DataDescription, DataId, FuncId, Linkage, Module, ModuleResult,
};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_frontend::Variable;
use cranelift_codegen::ir::{
    StackSlotData, StackSlotKind, GlobalValue, FuncRef,
    Function as IrFunction,  // Note: renamed to avoid confusion with ast::Function
};
use std::collections::{HashMap, HashSet, BTreeMap};
use std::fmt::Write;

use crate::ast::*;
use crate::errors::CompileError;

// First, let's define our module enum
pub enum CompilerModule {
    JIT(JITModule),
    Object(ObjectModule),
}

pub struct CodeGenerator {
    pub module: CompilerModule,
    functions: HashMap<String, FuncId>,
    string_data: HashMap<String, DataId>,
    structs: HashMap<String, StructInfo>,
    globals: HashMap<String, GlobalVarInfo>,
    pub last_ir: Option<String>,
}

struct StructInfo {
    field_offsets: HashMap<String, i32>,
    total_size: i32,
    field_types: HashMap<String, AstType>,
}

struct CodeGenContext {
    terminated_blocks: HashSet<Block>,
    current_function: Option<String>,
    next_var_index: usize,
    scopes: Vec<ScopeInfo>,
    loop_stack: Vec<LoopContext>,
    current_block: Option<Block>,
}

struct VarInfo {
    var: Variable,
    typ: AstType,
}

struct GlobalVarInfo {
    data_id: DataId,
    typ: AstType,
}

struct LoopContext {
    header_block: Block,
    exit_block: Block,
}

struct BlockInfo {
    block: Block,
    sealed: bool,
    has_terminator: bool,
    phi_nodes: BTreeMap<usize, Value>,
    instruction_count: usize,
}

struct ScopeInfo {
    variables: HashMap<String, VarInfo>,
    block_info: HashMap<Block, BlockInfo>,
}

impl CodeGenerator {
    /// Creates a new instance of the code generator with a provided module.
    pub fn new(module: CompilerModule) -> Self {
        Self {
            module,
            functions: HashMap::new(),
            string_data: HashMap::new(),
            structs: HashMap::new(),
            globals: HashMap::new(),
            last_ir: None,
        }
    }

    /// Generates code for the entire program.
    pub fn gen_program(&mut self, program: &Program) -> Result<(), CompileError> {
        // Handle global variables first
        for global in &program.globals {
            self.declare_global(global)?;
        }

        // Then handle structs to get their sizes and field offsets
        for struct_def in &program.structs {
            self.register_struct(struct_def)?;
        }

        // Declare all functions to handle forward references
        for function in &program.functions {
            self.declare_function(function)?;
        }

        // Define each function
        for function in &program.functions {
            if function.body.is_some() {
                self.define_function(function)?;
            } else if function.external_lib.is_some() {
                // External functions are already declared
                continue;
            } else {
                return Err(CompileError::MissingFunctionBody(function.name.clone()));
            }
        }

        Ok(())
    }

    /// Registers struct definitions to calculate field offsets and sizes.
    fn register_struct(&mut self, struct_def: &StructDef) -> Result<(), CompileError> {
        let mut field_offsets = HashMap::new();
        let mut field_types = HashMap::new();
        let mut current_offset = 0;

        for field in &struct_def.fields {
            let field_size = self.get_type_size(&field.typ)?;
            // Add alignment padding if needed
            let alignment = field_size.min(8); // Maximum 8-byte alignment
            let padding = (alignment - (current_offset % alignment)) % alignment;
            current_offset += padding;

            field_offsets.insert(field.name.clone(), current_offset);
            field_types.insert(field.name.clone(), field.typ.clone());
            current_offset += field_size;
        }

        // Add final padding to align the total size
        let total_alignment = 8; // Use 8-byte alignment for the whole struct
        let final_padding = (total_alignment - (current_offset % total_alignment)) % total_alignment;
        let total_size = current_offset + final_padding;

        self.structs.insert(
            struct_def.name.clone(),
            StructInfo {
                field_offsets,
                field_types,
                total_size,
            },
        );

        Ok(())
    }

    /// Declares a function signature without defining it.
    fn declare_function(&mut self, function: &Function) -> Result<(), CompileError> {
        let mut signature = self.module.make_signature();

        // Add parameters and return type to signature
        for param in &function.params {
            let ty = self.get_cranelift_type(&param.typ)?;
            signature.params.push(AbiParam::new(ty));
        }

        if function.return_type != AstType::Void {
            let ret_ty = self.get_cranelift_type(&function.return_type)?;
            signature.returns.push(AbiParam::new(ret_ty));
        }

        let _linkage = if let Some(lib_name) = &function.external_lib {
            // For external functions, use the library name as part of the symbol name
            let symbol_name = if lib_name.is_empty() {
                function.name.clone()
            } else {
                function.name.clone()
                // format!("{}_{}", lib_name, function.name)
            };
            
            // Declare the function with the proper symbol name based on module type
            match &mut self.module {
                CompilerModule::JIT(m) => {
                    self.functions.insert(
                        function.name.clone(),
                        m.declare_function(&symbol_name, Linkage::Import, &signature)?
                    );
                }
                CompilerModule::Object(m) => {
                    self.functions.insert(
                        function.name.clone(),
                        m.declare_function(&symbol_name, Linkage::Import, &signature)?
                    );
                }
            }
            Linkage::Import
        } else {
            match &mut self.module {
                CompilerModule::JIT(m) => {
                    self.functions.insert(
                        function.name.clone(),
                        m.declare_function(&function.name, Linkage::Export, &signature)?
                    );
                }
                CompilerModule::Object(m) => {
                    self.functions.insert(
                        function.name.clone(),
                        m.declare_function(&function.name, Linkage::Export, &signature)?
                    );
                }
            }
            Linkage::Export
        };

        Ok(())
    }

    /// Declares a global variable.
    fn declare_global(&mut self, global: &VarDecl) -> Result<(), CompileError> {
        let data_id = self.module.declare_data(
            &global.name,
            Linkage::Export,
            true,  // writable
            false, // tls
        )?;

        let mut desc = DataDescription::new();
        let _ty = self.get_cranelift_type(&global.typ)?;
        
        if let Some(init) = &global.initializer {
            match init {
                Expr::StringLiteral(s) => {
                    desc.define(
                        s.as_bytes()
                            .iter()
                            .cloned()
                            .chain(std::iter::once(0)) // Null-terminate
                            .collect::<Vec<u8>>()
                            .into_boxed_slice(),
                    );
                }
                _ => {
                    let value = self.evaluate_constant_expr(init)?;
                    let bytes = match _ty {
                        types::I8 => vec![value as u8],
                        types::I16 => (value as i16).to_le_bytes().to_vec(),
                        types::I32 => (value as i32).to_le_bytes().to_vec(),
                        types::I64 => value.to_le_bytes().to_vec(),
                        _ => return Err(CompileError::UnsupportedType(format!("{:?}", _ty))),
                    };
                    desc.define(bytes.into_boxed_slice());
                }
            }
        } else {
            desc.define_zeroinit(_ty.bytes() as usize);
        }

        self.module.define_data(data_id, &desc)?;
        self.globals.insert(global.name.clone(), GlobalVarInfo {
            data_id,
            typ: global.typ.clone(),
        });
        Ok(())
    }

    /// Defines the body of a function.
    fn define_function(&mut self, function: &Function) -> Result<(), CompileError> {
        let func_id = *self.functions.get(&function.name)
            .ok_or_else(|| CompileError::UndefinedFunction(function.name.clone()))?;

        let func_decl = match &self.module {
            CompilerModule::JIT(m) => m.declarations().get_function_decl(func_id),
            CompilerModule::Object(m) => m.declarations().get_function_decl(func_id),
        };

        // Create code generation context
        let mut codegen_ctx = CodeGenContext::new();
        codegen_ctx.current_function = Some(function.name.clone());

        // Create context and builder
        let mut context = self.module.make_context();
        context.func.signature = func_decl.signature.clone();

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

        // Create and set up the entry block
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        self.ensure_block_has_instruction(&codegen_ctx, &mut builder);

        // Ensure entry block has an instruction before declaring variables
        builder.ins().iconst(types::I64, 0);

        // Declare function parameters as variables
        for (i, param) in function.params.iter().enumerate() {
            let var = codegen_ctx.create_variable(param.name.clone(), param.typ.clone());
            let _ty = self.get_cranelift_type(&param.typ)?;
            builder.declare_var(var, _ty);

            let param_value = builder.block_params(entry_block)[i];
            builder.def_var(var, param_value);
        }

        // Generate the function body
        if let Some(body) = &function.body {
            self.gen_block(&mut codegen_ctx, &mut builder, body)?;
        }

        // Ensure function ends with a return
        let current_block = builder.current_block().unwrap();
        if !codegen_ctx.is_block_terminated(current_block) {
            if builder.func.signature.returns.is_empty() {
                self.ensure_block_has_instruction(&codegen_ctx, &mut builder);
                builder.ins().return_(&[]);
                codegen_ctx.mark_block_terminated(current_block);
            } else {
                return Err(CompileError::MissingFunctionBody(function.name.clone()));
            }
        }

        // Seal entry block and finalize
        builder.seal_block(entry_block);
        builder.seal_all_blocks();
        builder.finalize();

        // Before finalizing, capture the IR
        self.capture_ir(&context);

        // Define the function in the module
        match &mut self.module {
            CompilerModule::JIT(m) => {
                if let Err(e) = m.define_function(func_id, &mut context) {
                    // Keep the IR for error reporting
                    return Err(CompileError::ModuleError(e.into()));
                }
                m.clear_context(&mut context);
            }
            CompilerModule::Object(m) => {
                if let Err(e) = m.define_function(func_id, &mut context) {
                    // Keep the IR for error reporting
                    return Err(CompileError::ModuleError(e.into()));
                }
                m.clear_context(&mut context);
            }
        }

        Ok(())
    }

    /// Generates code for a block of statements.
    fn gen_block(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        block: &AstBlock,
    ) -> Result<(), CompileError> {
        for stmt in &block.statements {
            self.gen_statement(ctx, builder, stmt)?;
        }
        Ok(())
    }

    /// Generates code for a single statement.
    fn gen_statement(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        stmt: &Stmt,
    ) -> Result<(), CompileError> {
        match stmt {
            Stmt::VarDecl(var_decl) => {
                let var = ctx.create_variable(var_decl.name.clone(), var_decl.typ.clone());
                let _ty = self.get_cranelift_type(&var_decl.typ)?;
                builder.declare_var(var, _ty);

                if let Some(initializer) = &var_decl.initializer {
                    let init_type = self.get_expr_type(ctx, initializer)?;
                    if init_type != var_decl.typ {
                        return Err(CompileError::TypeMismatch {
                            expected: format!("{:?}", var_decl.typ),
                            found: format!("{:?}", init_type),
                        });
                    }
                    let value = self.gen_expression(ctx, builder, initializer)?;
                    builder.def_var(var, value);
                } else {
                    let zero = self.default_value(builder, _ty)?;
                    builder.def_var(var, zero);
                }
            }
            Stmt::Assignment { target, value } => {
                let value = self.gen_expression(ctx, builder, value)?;
                self.gen_assignment(ctx, builder, target, value)?;
            }
            Stmt::Expression(expr) => {
                self.gen_expression(ctx, builder, expr)?;
            }
            Stmt::Return(expr_opt) => {
                let return_values = if let Some(expr) = expr_opt {
                    let ret_value = self.gen_expression(ctx, builder, expr)?;
                    vec![ret_value]
                } else {
                    vec![]
                };
                builder.ins().return_(&return_values);
                ctx.mark_block_terminated(builder.current_block().unwrap());
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.gen_if_statement(ctx, builder, condition, then_branch, else_branch)?;
            }
            Stmt::While { condition, body } => {
                self.gen_while_statement(ctx, builder, condition, body)?;
            }
            Stmt::Block(block) => {
                self.gen_block(ctx, builder, block)?;
            }
            Stmt::Break => self.handle_break(ctx, builder),
            Stmt::Continue => self.handle_continue(ctx, builder),
            // Implement other statements like For
            _ => return Err(CompileError::UnimplementedStatement(stmt.clone())),
        }
        Ok(())
    }

    /// Generates code for an assignment.
    fn gen_assignment(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        target: &Expr,
        value: Value,
    ) -> Result<(), CompileError> {
        match target {
            Expr::Variable(name) => {
                let value_type = self.get_expr_type(ctx, &Expr::Variable(name.clone()))?;
                let target_type = builder.func.dfg.value_type(value);
                let expected_type = self.get_cranelift_type(&value_type)?;
                
                if expected_type != target_type {
                    return Err(CompileError::TypeMismatch {
                        expected: format!("{:?}", expected_type),
                        found: format!("{:?}", target_type),
                    });
                }

                if let Some(var_info) = ctx.get_variable(name) {
                    builder.def_var(var_info.var, value);
                } else if let Some(global_info) = self.globals.get(name) {
                    let data_id = global_info.data_id;
                    let gv = self.module.declare_data_in_func(data_id, &mut builder.func);
                    let ptr = builder.ins().symbol_value(self.module.isa().pointer_type(), gv);
                    let mem_flags = MemFlags::new();
                    builder.ins().store(mem_flags, value, ptr, 0);
                } else {
                    return Err(CompileError::UndefinedVariable(name.clone()));
                }
            }
            Expr::FieldAccess { expr, field } => {
                let ptr = self.gen_expression(ctx, builder, expr)?;
                let field_offset = self.get_field_offset(ctx, expr, field)?;
                let addr = builder.ins().iadd_imm(ptr, field_offset as i64);
                let mem_flags = MemFlags::new();
                builder.ins().store(mem_flags, value, addr, 0);
            }
            Expr::ArrayAccess { array, index } => {
                let base = self.gen_expression(ctx, builder, array)?;
                let idx = self.gen_expression(ctx, builder, index)?;
                let element_size = self.get_element_size(ctx, array)?;
                let offset = builder.ins().imul_imm(idx, element_size as i64);
                let addr = builder.ins().iadd(base, offset);
                let mem_flags = MemFlags::new();
                builder.ins().store(mem_flags, value, addr, 0);
            }
            _ => return Err(CompileError::InvalidAssignmentTarget),
        }
        Ok(())
    }

    /// Generates code for an if statement.
    fn gen_if_statement(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        condition: &Expr,
        then_branch: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), CompileError> {
        self.ensure_block_has_instruction(ctx, builder);
        let cond_value = self.gen_expression(ctx, builder, condition)?;
        
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let merge_block = builder.create_block();

        builder.ins().brif(cond_value, then_block, &[], else_block, &[]);
        ctx.mark_block_terminated(builder.current_block().unwrap());

        // Then block
        self.switch_to_block(ctx, builder, then_block);
        ctx.push_scope();
        self.gen_statement(ctx, builder, then_branch)?;
        ctx.pop_scope();
        self.terminate_block(ctx, builder, then_block, merge_block);
        builder.seal_block(then_block);

        // Else block
        self.switch_to_block(ctx, builder, else_block);
        if let Some(else_stmt) = else_branch {
            ctx.push_scope();
            self.gen_statement(ctx, builder, else_stmt)?;
            ctx.pop_scope();
        }
        self.terminate_block(ctx, builder, else_block, merge_block);
        builder.seal_block(else_block);

        // Merge block
        self.switch_to_block(ctx, builder, merge_block);
        builder.seal_block(merge_block);

        Ok(())
    }

    /// Generates code for a while loop.
    fn gen_while_statement(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        condition: &Expr,
        body: &Box<Stmt>,
    ) -> Result<(), CompileError> {
        // Create blocks
        let header_block = builder.create_block();
        let body_block = builder.create_block();
        let follow_block = builder.create_block();

        // Register all blocks
        ctx.register_block(header_block);
        ctx.register_block(body_block);
        ctx.register_block(follow_block);

        // Push loop context for break/continue
        ctx.push_loop(header_block, follow_block);

        // Jump from current block to header block
        self.terminate_block(ctx, builder, ctx.current_block.unwrap(), header_block);

        // Generate header block (condition evaluation)
        self.switch_to_block(ctx, builder, header_block);
        let cond_val = self.gen_expression(ctx, builder, condition)?;
        builder.ins().brif(cond_val, body_block, &[], follow_block, &[]);
        ctx.mark_block_terminated(header_block);

        // Generate body block
        self.switch_to_block(ctx, builder, body_block);
        ctx.push_scope();
        self.gen_statement(ctx, builder, body)?;
        ctx.pop_scope();
        
        // If body block isn't already terminated, jump back to header
        if !ctx.is_block_terminated(body_block) {
            self.terminate_block(ctx, builder, body_block, header_block);
        }

        // Seal blocks in correct order
        self.seal_block(ctx, builder, body_block);
        self.seal_block(ctx, builder, header_block);

        // Pop loop context
        ctx.pop_loop();

        // Switch to follow block
        self.switch_to_block(ctx, builder, follow_block);

        Ok(())
    }

    /// Generates code for an expression and returns the resulting value.
    fn gen_expression(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        expr: &Expr,
    ) -> Result<Value, CompileError> {
        match expr {
            Expr::IntLiteral(val) => {
                let ty = types::I64;
                Ok(builder.ins().iconst(ty, *val))
            }
            Expr::FloatLiteral(val) => {
                let ty = types::F64;
                Ok(builder.ins().f64const(*val))
            }
            Expr::BoolLiteral(val) => {
                let ty = types::I8;
                Ok(builder.ins().iconst(ty, if *val { 1 } else { 0 }))
            }
            Expr::CharLiteral(ch) => {
                let ty = types::I8;
                Ok(builder.ins().iconst(ty, *ch as i64))
            }
            Expr::StringLiteral(s) => {
                let gv = self.get_or_create_string(s)?;
                let gv = self.module.declare_data_in_func(gv, &mut builder.func);
                Ok(builder.ins().symbol_value(self.module.isa().pointer_type(), gv))
            }
            Expr::Variable(name) => {
                if let Some(var_info) = ctx.get_variable(name) {
                    Ok(builder.use_var(var_info.var))
                } else if let Some(global_info) = self.globals.get(name) {
                    let data_id = global_info.data_id;
                    let gv = self.module.declare_data_in_func(data_id, &mut builder.func);
                    let ptr = builder.ins().symbol_value(self.module.isa().pointer_type(), gv);
                    let ty = self.get_cranelift_type(&global_info.typ)?;
                    let mem_flags = MemFlags::new();
                    Ok(builder.ins().load(ty, mem_flags, ptr, 0))
                } else {
                    Err(CompileError::UndefinedVariable(name.clone()))
                }
            }
            Expr::Binary { op, left, right } => {
                let left_val = self.gen_expression(ctx, builder, left)?;
                let right_val = self.gen_expression(ctx, builder, right)?;
                self.gen_binary_op(builder, op.clone(), left_val, right_val)
            }
            Expr::Unary { op, expr } => {
                let val = self.gen_expression(ctx, builder, expr)?;
                self.gen_unary_op(builder, op.clone(), val)
            }
            Expr::Call { function, arguments } => {
                self.gen_function_call(ctx, builder, function, arguments)
            }
            Expr::FieldAccess { expr, field } => {
                let ptr = self.gen_expression(ctx, builder, expr)?;
                let field_offset = self.get_field_offset(ctx, expr, field)?;
                let addr = builder.ins().iadd_imm(ptr, field_offset as i64);
                let ty = self.get_expr_type(ctx, expr)?.clone();
                let field_type = self.get_field_type(&ty, field)?;
                let cl_type = self.get_cranelift_type(&field_type)?;
                let mem_flags = MemFlags::new();
                Ok(builder.ins().load(cl_type, mem_flags, addr, 0))
            }
            Expr::ArrayAccess { array, index } => {
                let base = self.gen_expression(ctx, builder, array)?;
                let idx = self.gen_expression(ctx, builder, index)?;
                let element_size = self.get_element_size(ctx, array)?;
                let offset = builder.ins().imul_imm(idx, element_size as i64);
                let addr = builder.ins().iadd(base, offset);
                let element_type = self.get_element_type(ctx, array)?;
                let cl_type = self.get_cranelift_type(&element_type)?;
                let mem_flags = MemFlags::new();
                Ok(builder.ins().load(cl_type, mem_flags, addr, 0))
            }
            Expr::StructInit { name, fields } => {
                self.gen_struct_init(ctx, builder, name, fields)
            }
            Expr::ArrayInit { elements } => {
                self.gen_array_init(ctx, builder, elements)
            }
            Expr::Cast { expr, typ } => {
                let val = self.gen_expression(ctx, builder, expr)?;
                self.gen_cast(builder, val, typ)
            }
            Expr::Lambda { .. } => Err(CompileError::UnimplementedExpression(expr.clone())),
            // Implement other expressions as needed
            _ => Err(CompileError::UnimplementedExpression(expr.clone())),
        }
    }

    /// Generates code for a binary operation.
    fn gen_binary_op(
        &mut self,
        builder: &mut FunctionBuilder,
        op: BinaryOp,
        left: Value,
        right: Value,
    ) -> Result<Value, CompileError> {
        let left_ty = builder.func.dfg.value_type(left);
        let right_ty = builder.func.dfg.value_type(right);

        // Ensure both operands have the same type
        if left_ty != right_ty {
            return Err(CompileError::TypeMismatch {
                expected: format!("{:?}", left_ty),
                found: format!("{:?}", right_ty),
            });
        }

        match op {
            BinaryOp::Add => Ok(builder.ins().iadd(left, right)),
            BinaryOp::Subtract => Ok(builder.ins().isub(left, right)),
            BinaryOp::Multiply => Ok(builder.ins().imul(left, right)),
            BinaryOp::Divide => Ok(builder.ins().sdiv(left, right)),
            BinaryOp::Modulo => Ok(builder.ins().srem(left, right)),
            BinaryOp::Equal => {
                let cmp = builder.ins().icmp(IntCC::Equal, left, right);
                Ok(cmp)
            }
            BinaryOp::NotEqual => {
                let cmp = builder.ins().icmp(IntCC::NotEqual, left, right);
                Ok(cmp)
            }
            BinaryOp::LessThan => {
                let cmp = builder.ins().icmp(IntCC::SignedLessThan, left, right);
                Ok(cmp)
            }
            BinaryOp::LessThanOrEqual => {
                let cmp = builder.ins().icmp(IntCC::SignedLessThanOrEqual, left, right);
                Ok(cmp)
            }
            BinaryOp::GreaterThan => {
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
                Ok(cmp)
            }
            BinaryOp::GreaterThanOrEqual => {
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left, right);
                Ok(cmp)
            }
            BinaryOp::And => Ok(builder.ins().band(left, right)),
            BinaryOp::Or => Ok(builder.ins().bor(left, right)),
            BinaryOp::BitAnd => Ok(builder.ins().band(left, right)),
            BinaryOp::BitOr => Ok(builder.ins().bor(left, right)),
            BinaryOp::BitXor => Ok(builder.ins().bxor(left, right)),
            BinaryOp::ShiftLeft => Ok(builder.ins().ishl(left, right)),
            BinaryOp::ShiftRight => Ok(builder.ins().sshr(left, right)),
            _ => Err(CompileError::UnimplementedBinaryOp(op)),
        }
    }

    /// Generates code for a unary operation.
    fn gen_unary_op(
        &mut self,
        builder: &mut FunctionBuilder,
        op: UnaryOp,
        val: Value,
    ) -> Result<Value, CompileError> {
        match op {
            UnaryOp::Negate => Ok(builder.ins().ineg(val)),
            UnaryOp::Not => {
                let zero = builder.ins().iconst(types::I8, 0);
                let cmp = builder.ins().icmp(IntCC::Equal, val, zero);
                Ok(builder.ins().bmask(types::I8, cmp))
            }
            UnaryOp::BitNot => Ok(builder.ins().bnot(val)),
            _ => Err(CompileError::UnimplementedExpression(Expr::Unary {
                op,
                expr: Box::new(Expr::IntLiteral(0)), // Placeholder
            })),
        }
    }

    /// Generates code for a function call.
    fn gen_function_call(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        function_expr: &Box<Expr>,
        arguments: &[Expr],
    ) -> Result<Value, CompileError> {
        if let Expr::Variable(func_name) = function_expr.as_ref() {
            let func_id = *self
                .functions
                .get(func_name)
                .ok_or_else(|| CompileError::UndefinedFunction(func_name.clone()))?;

            // Get the function declaration to check if it's external
            let func_decl = self.module.declarations().get_function_decl(func_id);
            let func_ref = self.module.declare_func_in_func(func_id, &mut builder.func);

            // Generate argument values
            let mut arg_values = Vec::new();
            for arg in arguments {
                let arg_val = self.gen_expression(ctx, builder, arg)?;
                arg_values.push(arg_val);
            }

            // Call the function
            let call = builder.ins().call(func_ref, &arg_values);

            // Handle the return value
            let results = builder.inst_results(call);
            if results.is_empty() {
                // Void function; return a dummy value
                Ok(builder.ins().iconst(types::I8, 0))
            } else {
                Ok(results[0])
            }
        } else {
            Err(CompileError::UnimplementedExpression(
                function_expr.as_ref().clone(),
            ))
        }
    }

    /// Generates code for struct initialization.
    fn gen_struct_init(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        name: &str,
        fields: &[(String, Expr)],
    ) -> Result<Value, CompileError> {
        // Get struct info and total size first
        let total_size = {
            let struct_info = self
                .structs
                .get(name)
                .ok_or_else(|| CompileError::UnsupportedType(name.to_string()))?;
            struct_info.total_size as i64
        };

        // Allocate stack slot with alignment
        let struct_slot = builder.create_sized_stack_slot(
            StackSlotData::new(StackSlotKind::ExplicitSlot, total_size as u32, 8),
        );
        let struct_ptr = builder.ins().stack_addr(
            self.module.isa().pointer_type(),
            struct_slot,
            0,
        );

        // Initialize fields
        for (field_name, expr) in fields {
            let field_offset = {
                let struct_info = self.structs.get(name).unwrap();
                *struct_info
                    .field_offsets
                    .get(field_name)
                    .ok_or_else(|| CompileError::UndefinedVariable(field_name.clone()))?
            };
            
            let value = self.gen_expression(ctx, builder, expr)?;
            let addr = builder.ins().iadd_imm(struct_ptr, field_offset as i64);
            let mem_flags = MemFlags::trusted();
            builder.ins().store(mem_flags, value, addr, 0);
        }

        Ok(struct_ptr)
    }

    /// Generates code for array initialization.
    fn gen_array_init(
        &mut self,
        ctx: &mut CodeGenContext,
        builder: &mut FunctionBuilder,
        elements: &[Expr],
    ) -> Result<Value, CompileError> {
        let element_count = elements.len() as i64;
        let element_type = self.get_expr_type(ctx, &elements[0])?;
        let element_size = self.get_type_size(&element_type)?;

        // Allocate stack slot with alignment
        let total_size = element_count * element_size as i64;
        let array_slot = builder.create_sized_stack_slot(
            StackSlotData::new(StackSlotKind::ExplicitSlot, total_size as u32, 8),
        );
        let array_ptr = builder.ins().stack_addr(
            self.module.isa().pointer_type(),
            array_slot,
            0,
        );

        // Initialize elements
        for (i, expr) in elements.iter().enumerate() {
            let value = self.gen_expression(ctx, builder, expr)?;
            let offset = builder.ins().iconst(
                self.module.isa().pointer_type(),
                i as i64 * element_size as i64,
            );
            let addr = builder.ins().iadd(array_ptr, offset);
            let mem_flags = MemFlags::trusted();
            builder.ins().store(mem_flags, value, addr, 0);
        }

        Ok(array_ptr)
    }

    /// Generates code for a cast operation.
    fn gen_cast(
        &mut self,
        builder: &mut FunctionBuilder,
        val: Value,
        typ: &AstType,
    ) -> Result<Value, CompileError> {
        let target_ty = self.get_cranelift_type(typ)?;
        let val_ty = builder.func.dfg.value_type(val);

        if val_ty == target_ty {
            Ok(val)
        } else if val_ty.is_int() && target_ty.is_int() {
            if val_ty.bits() < target_ty.bits() {
                Ok(builder.ins().uextend(target_ty, val))
            } else {
                Ok(builder.ins().ireduce(target_ty, val))
            }
        } else if val_ty.is_int() && target_ty.is_float() {
            Ok(builder.ins().fcvt_from_uint(target_ty, val))
        } else if val_ty.is_float() && target_ty.is_int() {
            Ok(builder.ins().fcvt_to_uint_sat(target_ty, val))
        } else if val_ty.is_float() && target_ty.is_float() {
            if val_ty.bits() < target_ty.bits() {
                Ok(builder.ins().fpromote(target_ty, val))
            } else {
                Ok(builder.ins().fdemote(target_ty, val))
            }
        } else {
            Err(CompileError::TypeMismatch {
                expected: format!("{:?}", target_ty),
                found: format!("{:?}", val_ty),
            })
        }
    }

    /// Gets or creates a global data ID for a string literal.
    fn get_or_create_string(&mut self, s: &str) -> Result<DataId, CompileError> {
        if let Some(&data_id) = self.string_data.get(s) {
            Ok(data_id)
        } else {
            let name = format!("str_{}", self.string_data.len());
            let data_id = match &mut self.module {
                CompilerModule::JIT(m) => {
                    m.declare_data(&name, Linkage::Local, false, false)?
                }
                CompilerModule::Object(m) => {
                    m.declare_data(&name, Linkage::Local, false, false)?
                }
            };

            let mut desc = DataDescription::new();
            desc.define(
                s.as_bytes()
                    .iter()
                    .cloned()
                    .chain(std::iter::once(0))
                    .collect::<Vec<u8>>()
                    .into_boxed_slice(),
            );

            match &mut self.module {
                CompilerModule::JIT(m) => m.define_data(data_id, &desc)?,
                CompilerModule::Object(m) => m.define_data(data_id, &desc)?,
            }

            self.string_data.insert(s.to_string(), data_id);
            Ok(data_id)
        }
    }

    /// Gets the Cranelift type for an AST type.
    fn get_cranelift_type(&self, ast_type: &AstType) -> Result<Type, CompileError> {
        match ast_type {
            AstType::Int8 | AstType::UInt8 | AstType::Char => Ok(types::I8),
            AstType::Int16 | AstType::UInt16 => Ok(types::I16),
            AstType::Int32 | AstType::UInt32 => Ok(types::I32),
            AstType::Int64 | AstType::UInt64 => Ok(types::I64),
            AstType::Float32 => Ok(types::F32),
            AstType::Float64 => Ok(types::F64),
            AstType::Boolean => Ok(types::I8),
            AstType::String => Ok(self.module.pointer_type()),
            AstType::Pointer(_) => Ok(self.module.pointer_type()),
            AstType::Array(_, _) => Ok(self.module.pointer_type()),
            AstType::Struct(_) => Ok(self.module.pointer_type()),
            AstType::Void => Err(CompileError::UnsupportedType(
                "Void type is not a value type".to_string(),
            )),
            _ => Err(CompileError::UnsupportedType(format!("{:?}", ast_type))),
        }
    }

    /// Gets the size of an AST type in bytes.
    fn get_type_size(&self, ast_type: &AstType) -> Result<i32, CompileError> {
        match ast_type {
            AstType::Struct(name) => {
                let struct_info = self.structs.get(name).ok_or_else(|| {
                    CompileError::UnsupportedType(format!("Unknown struct '{}'", name))
                })?;
                Ok(struct_info.total_size)
            }
            _ => {
                let ty = self.get_cranelift_type(ast_type)?;
                Ok(ty.bytes() as i32)
            }
        }
    }

    /// Generates a default value for a given type.
    fn default_value(
        &self,
        builder: &mut FunctionBuilder,
        ty: Type,
    ) -> Result<Value, CompileError> {
        if ty.is_int() {
            Ok(builder.ins().iconst(ty, 0))
        } else if ty.is_float() {
            if ty == types::F32 {
                Ok(builder.ins().f32const(0.0))
            } else if ty == types::F64 {
                Ok(builder.ins().f64const(0.0))
            } else {
                Err(CompileError::UnsupportedType(format!("{:?}", ty)))
            }
        } else {
            Err(CompileError::DefaultValueError(ty))
        }
    }

    /// Evaluates a constant expression at compile time.
    fn evaluate_constant_expr(&self, expr: &Expr) -> Result<i64, CompileError> {
        match expr {
            Expr::IntLiteral(val) => Ok(*val),
            // Implement other constant expressions as needed
            _ => Err(CompileError::UnimplementedExpression(expr.clone())),
        }
    }

    /// Gets the field offset within a struct.
    fn get_field_offset(
        &self,
        ctx: &CodeGenContext,
        expr: &Expr,
        field: &str,
    ) -> Result<i32, CompileError> {
        if let AstType::Struct(name) = self.get_expr_type(ctx, expr)? {
            let struct_info = self
                .structs
                .get(&name)
                .ok_or_else(|| CompileError::UnsupportedType(name.clone()))?;
            struct_info
                .field_offsets
                .get(field)
                .cloned()
                .ok_or_else(|| CompileError::UndefinedVariable(field.to_string()))
        } else {
            Err(CompileError::InvalidAssignmentTarget)
        }
    }

    /// Gets the type of an expression.
    fn get_expr_type(
        &self,
        ctx: &CodeGenContext,
        expr: &Expr,
    ) -> Result<AstType, CompileError> {
        match expr {
            Expr::IntLiteral(_) => Ok(AstType::Int64),
            Expr::FloatLiteral(_) => Ok(AstType::Float64),
            Expr::BoolLiteral(_) => Ok(AstType::Boolean),
            Expr::CharLiteral(_) => Ok(AstType::Char),
            Expr::StringLiteral(_) => Ok(AstType::Pointer(Box::new(AstType::Char))),
            Expr::Variable(name) => {
                if let Some(var_info) = ctx.get_variable(name) {
                    Ok(var_info.typ.clone())
                } else if let Some(global_info) = self.globals.get(name) {
                    Ok(global_info.typ.clone())
                } else {
                    Err(CompileError::UndefinedVariable(name.clone()))
                }
            }
            Expr::ArrayAccess { array, .. } => {
                if let AstType::Array(element_type, _) = self.get_expr_type(ctx, array)? {
                    Ok(*element_type)
                } else {
                    Err(CompileError::UnsupportedType("Not an array type".to_string()))
                }
            }
            Expr::FieldAccess { expr, field } => {
                if let AstType::Struct(struct_name) = self.get_expr_type(ctx, expr)? {
                    if let Some(struct_info) = self.structs.get(&struct_name) {
                        if let Some(field_type) = struct_info.field_types.get(field) {
                            Ok(field_type.clone())
                        } else {
                            Err(CompileError::UndefinedVariable(field.clone()))
                        }
                    } else {
                        Err(CompileError::UnsupportedType(format!("Unknown struct '{}'", struct_name)))
                    }
                } else {
                    Err(CompileError::UnsupportedType("Not a struct type".to_string()))
                }
            }
            Expr::Binary { left, .. } => {
                // For now, assume binary operations return the same type as their left operand
                self.get_expr_type(ctx, left)
            }
            Expr::Unary { expr, .. } => {
                // Unary operations generally preserve their operand type
                self.get_expr_type(ctx, expr)
            }
            Expr::Cast { typ, .. } => {
                // Cast expressions have an explicit type
                Ok(typ.clone())
            }
            Expr::Call { function, .. } => {
                if let Expr::Variable(func_name) = function.as_ref() {
                    if let Some(func_id) = self.functions.get(func_name) {
                        let _func_decl = self.module.declarations().get_function_decl(*func_id);
                        if !_func_decl.signature.returns.is_empty() {
                            // Convert ABI type back to AST type
                            // This is a simplification - you might need more sophisticated mapping
                            Ok(AstType::Int64) // Default to Int64 for now
                        } else {
                            Ok(AstType::Void)
                        }
                    } else {
                        Err(CompileError::UndefinedFunction(func_name.clone()))
                    }
                } else {
                    Err(CompileError::UnimplementedExpression((**function).clone()))
                }
            }
            Expr::StructInit { name, .. } => Ok(AstType::Struct(name.clone())),
            Expr::ArrayInit { elements } => {
                if elements.is_empty() {
                    Err(CompileError::UnsupportedType("Empty array".to_string()))
                } else {
                    let element_type = self.get_expr_type(ctx, &elements[0])?;
                    Ok(AstType::Array(Box::new(element_type), elements.len()))
                }
            }
            Expr::Lambda { .. } => Err(CompileError::UnimplementedExpression(expr.clone())),
            _ => Err(CompileError::UnimplementedExpression(expr.clone())),
        }
    }

    /// Gets the element size of an array.
    fn get_element_size(
        &self,
        ctx: &CodeGenContext,
        array_expr: &Expr,
    ) -> Result<i32, CompileError> {
        let array_type = self.get_expr_type(ctx, array_expr)?;
        if let AstType::Array(element_type, _) = array_type {
            self.get_type_size(&element_type)?
        } else {
            Err(CompileError::UnsupportedType(format!(
                "Expected array type, found {:?}",
                array_type
            )))
        }
    }

    /// Gets the element type of an array.
    fn get_element_type(
        &self,
        ctx: &CodeGenContext,
        array_expr: &Expr,
    ) -> Result<AstType, CompileError> {
        let array_type = self.get_expr_type(ctx, array_expr)?;
        if let AstType::Array(element_type, _) = array_type {
            Ok(*element_type)
        } else {
            Err(CompileError::UnsupportedType(format!(
                "Expected array type, found {:?}",
                array_type
            )))
        }
    }

    /// Gets the field type of a struct field.
    fn get_field_type(
        &self,
        struct_type: &AstType,
        field: &str,
    ) -> Result<AstType, CompileError> {
        if let AstType::Struct(name) = struct_type {
            let struct_info = self.structs.get(name).ok_or_else(|| {
                CompileError::UnsupportedType(format!("Unknown struct '{}'", name))
            })?;
            struct_info
                .field_types
                .get(field)
                .cloned()
                .ok_or_else(|| CompileError::UndefinedVariable(field.to_string()))
        } else {
            Err(CompileError::UnsupportedType(format!("{:?}", struct_type)))
        }
    }

    /// Gets a function pointer by name.
    pub fn get_function(&mut self, name: &str) -> Result<*const u8, CompileError> {
        let id = self.functions.get(name)
            .ok_or_else(|| CompileError::UndefinedVariable(name.to_string()))?;

        match &mut self.module {
            CompilerModule::JIT(jit_module) => {
                jit_module.finalize_definitions()
                    .map_err(|e| CompileError::ModuleError(e.into()))?;
                Ok(jit_module.get_finalized_function(*id))
            }
            CompilerModule::Object(_) => {
                Err(CompileError::ModuleError(
                    cranelift_module::ModuleError::Backend(
                        anyhow::Error::msg("Cannot get function pointer from object module")
                    )
                ))
            }
        }
    }

    // Helper methods for block handling
    fn ensure_block_has_instruction(&self, ctx: &CodeGenContext, builder: &mut FunctionBuilder) {
        if let Some(block) = ctx.current_block {
            if !ctx.is_block_terminated(block) && ctx.is_block_empty(block) {
                builder.ins().nop();
            }
        }
    }

    fn switch_to_block(&mut self, ctx: &mut CodeGenContext, builder: &mut FunctionBuilder, block: Block) {
        // Ensure current block is properly terminated if it exists
        if let Some(current_block) = ctx.current_block {
            if !ctx.is_block_terminated(current_block) {
                self.ensure_block_has_instruction(ctx, builder);
            }
        }
        
        // Register the new block if it's not already registered
        if ctx.get_block_info(block).is_none() {
            ctx.register_block(block);
        }
        
        // Switch to new block
        builder.switch_to_block(block);
        ctx.set_current_block(block);
        
        // Only add nop if the block is completely empty
        if ctx.is_block_empty(block) {
            builder.ins().nop();
        }
    }

    fn terminate_block(&mut self, ctx: &mut CodeGenContext, builder: &mut FunctionBuilder, 
                      from_block: Block, to_block: Block) {
        if !ctx.is_block_terminated(from_block) {
            self.ensure_block_has_instruction(ctx, builder);
            builder.ins().jump(to_block, &[]);
            ctx.mark_block_terminated(from_block);
        }
    }

    fn seal_block(&mut self, ctx: &mut CodeGenContext, builder: &mut FunctionBuilder, block: Block) {
        if !ctx.is_block_sealed(block) {
            if let Some(info) = ctx.get_block_info(block) {
                for (&var_idx, &value) in info.phi_nodes.iter() {
                    let var = Variable::new(var_idx);
                    builder.def_var(var, value);
                }
            }
            
            builder.seal_block(block);
            ctx.mark_block_sealed(block);
        }
    }

    // Add this helper method to capture IR
    fn capture_ir(&mut self, context: &Context) {
        let mut ir_string = String::new();
        writeln!(ir_string, "Function IR:").unwrap();
        writeln!(ir_string, "{}", context.func.display()).unwrap();
        self.last_ir = Some(ir_string);
    }

    fn handle_break(&mut self, ctx: &mut CodeGenContext, builder: &mut FunctionBuilder) -> Result<(), CompileError> {
        if let Some(loop_ctx) = ctx.loop_stack.last().cloned() {
            self.ensure_block_has_instruction(ctx, builder);
            builder.ins().jump(loop_ctx.exit_block, &[]);
            ctx.mark_block_terminated(builder.current_block().unwrap());
            Ok(())
        } else {
            Err(CompileError::InvalidBreak)
        }
    }

    fn handle_continue(&mut self, ctx: &mut CodeGenContext, builder: &mut FunctionBuilder) -> Result<(), CompileError> {
        if let Some(loop_ctx) = ctx.loop_stack.last().cloned() {
            self.ensure_block_has_instruction(ctx, builder);
            builder.ins().jump(loop_ctx.header_block, &[]);
            ctx.mark_block_terminated(builder.current_block().unwrap());
            Ok(())
        } else {
            Err(CompileError::InvalidContinue)
        }
    }
}

#[derive(Clone)]
struct VariableInfo {
    typ: AstType,
    var: Variable,
}

pub fn create_jit_code_generator() -> Result<CodeGenerator, CompileError> {
    
    let isa = cranelift_native::builder()
    .unwrap()
    .finish(
        settings::Flags::new(settings::builder())
    ).unwrap();
    
    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    
    let module = JITModule::new(builder);

    Ok(CodeGenerator {
        module: CompilerModule::JIT(module),
        functions: HashMap::new(),
        string_data: HashMap::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        last_ir: None,
    })
}

pub fn create_object_code_generator() -> Result<CodeGenerator, CompileError> {
    let isa_builder = cranelift_native::builder()
        .map_err(|e| CompileError::CraneliftError(e.to_string()))?;
    let flag_builder = settings::builder();
    let flags = settings::Flags::new(flag_builder);
    let isa = isa_builder
        .finish(flags)
        .map_err(|e| CompileError::CraneliftError(e.to_string()))?;

    let builder = ObjectBuilder::new(
        isa,
        "my_module",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| CompileError::ModuleError(e.into()))?;

    let module = ObjectModule::new(builder);

    Ok(CodeGenerator {
        module: CompilerModule::Object(module),
        functions: HashMap::new(),
        string_data: HashMap::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        last_ir: None,
    })
}

impl CodeGenContext {
    fn new() -> Self {
        Self {
            terminated_blocks: HashSet::new(),
            current_function: None,
            next_var_index: 0,
            scopes: vec![ScopeInfo {
                variables: HashMap::new(),
                block_info: HashMap::new(),
            }],
            loop_stack: Vec::new(),
            current_block: None,
        }
    }

    fn create_variable(&mut self, name: String, typ: AstType) -> Variable {
        let var = Variable::new(self.next_var_index);
        self.next_var_index += 1;
        let var_info = VarInfo { var, typ };
        
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.variables.insert(name, var_info);
        }
        var
    }

    fn get_variable(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_info) = scope.variables.get(name) {
                return Some(var_info);
            }
        }
        None
    }

    fn mark_block_terminated(&mut self, block: Block) {
        if let Some(info) = self.get_block_info_mut(block) {
            info.has_terminator = true;
        }
        self.terminated_blocks.insert(block);
    }

    fn is_block_terminated(&self, block: Block) -> bool {
        self.get_block_info(block).map_or(false, |info| info.has_terminator)
    }

    fn push_scope(&mut self) {
        self.scopes.push(ScopeInfo {
            variables: HashMap::new(),
            block_info: HashMap::new(),
        });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_loop(&mut self, header: Block, exit: Block) {
        self.loop_stack.push(LoopContext {
            header_block: header,
            exit_block: exit,
        });
    }

    fn pop_loop(&mut self) -> Option<LoopContext> {
        self.loop_stack.pop()
    }

    fn current_loop(&self) -> Option<&LoopContext> {
        self.loop_stack.last()
    }

    fn register_block(&mut self, block: Block) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.block_info.insert(block, BlockInfo {
                block,
                sealed: false,
                has_terminator: false,
                phi_nodes: HashMap::new(),
                instruction_count: 0,
            });
        }
    }

    fn set_current_block(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    fn get_block_info(&self, block: Block) -> Option<&BlockInfo> {
        self.scopes.last()?.block_info.get(&block)
    }

    fn get_block_info_mut(&mut self, block: Block) -> Option<&mut BlockInfo> {
        self.scopes.last_mut()?.block_info.get_mut(&block)
    }

    fn mark_block_sealed(&mut self, block: Block) {
        if let Some(info) = self.get_block_info_mut(block) {
            info.sealed = true;
        }
    }

    fn is_block_sealed(&self, block: Block) -> bool {
        self.get_block_info(block).map_or(false, |info| info.sealed)
    }

    fn add_phi_node(&mut self, block: Block, var: Variable, value: Value) {
        if let Some(info) = self.get_block_info_mut(block) {
            info.phi_nodes.insert(var.index(), value);
        }
    }

    fn increment_block_instructions(&mut self, block: Block) {
        if let Some(info) = self.get_block_info_mut(block) {
            info.instruction_count += 1;
        }
    }

    fn is_block_empty(&self, block: Block) -> bool {
        self.get_block_info(block)
            .map_or(true, |info| info.instruction_count == 0)
    }
}

impl CompilerModule {
    fn isa(&self) -> &dyn cranelift_codegen::isa::TargetIsa {
        match self {
            CompilerModule::JIT(m) => m.isa(),
            CompilerModule::Object(m) => m.isa(),
        }
    }

    fn declarations(&self) -> &cranelift_module::ModuleDeclarations {
        match self {
            CompilerModule::JIT(m) => m.declarations(),
            CompilerModule::Object(m) => m.declarations(),
        }
    }

    fn declare_data(
        &mut self,
        name: &str,
        linkage: Linkage,
        writable: bool,
        tls: bool,
    ) -> ModuleResult<DataId> {
        match self {
            CompilerModule::JIT(m) => m.declare_data(name, linkage, writable, tls),
            CompilerModule::Object(m) => m.declare_data(name, linkage, writable, tls),
        }
    }

    fn define_data(
        &mut self,
        data_id: DataId,
        data: &DataDescription,
    ) -> ModuleResult<()> {
        match self {
            CompilerModule::JIT(m) => m.define_data(data_id, data),
            CompilerModule::Object(m) => m.define_data(data_id, data),
        }
    }

    fn declare_data_in_func(&self, data: DataId, func: &mut IrFunction) -> GlobalValue {
        match self {
            CompilerModule::JIT(m) => m.declare_data_in_func(data, func),
            CompilerModule::Object(m) => m.declare_data_in_func(data, func),
        }
    }

    fn declare_func_in_func(
        &mut self,
        func_id: FuncId,
        func: &mut IrFunction,
    ) -> FuncRef {
        match self {
            CompilerModule::JIT(m) => m.declare_func_in_func(func_id, func),
            CompilerModule::Object(m) => m.declare_func_in_func(func_id, func),
        }
    }

    fn make_context(&self) -> Context {
        match self {
            CompilerModule::JIT(m) => m.make_context(),
            CompilerModule::Object(m) => m.make_context(),
        }
    }

    fn clear_context(&self, ctx: &mut Context) {
        match self {
            CompilerModule::JIT(m) => m.clear_context(ctx),
            CompilerModule::Object(m) => m.clear_context(ctx),
        }
    }

    fn make_signature(&self) -> Signature {
        match self {
            CompilerModule::JIT(m) => m.make_signature(),
            CompilerModule::Object(m) => m.make_signature(),
        }
    }

    fn pointer_type(&self) -> Type {
        self.isa().pointer_type()
    }
}


