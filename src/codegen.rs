// src/codegen.rs

use cranelift::prelude::*;
use cranelift_codegen::{self, ir::FuncRef, settings};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::errors::CompileError;

pub struct CodeGenerator {
    pub module: ObjectModule,
    functions: HashMap<String, FuncId>,
    variables: HashMap<String, Variable>,
    string_data: HashMap<String, DataId>,
    structs: HashMap<String, StructInfo>,
}

struct StructInfo {
    field_offsets: HashMap<String, i32>,
    total_size: i32,
    field_types: HashMap<String, AstType>,
}

struct CodeGenContext {
    variables: HashMap<String, Variable>,
    terminated_blocks: HashSet<Block>,
    current_function: Option<String>,
}

impl CodeGenContext {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            terminated_blocks: HashSet::new(),
            current_function: None,
        }
    }

    fn mark_block_terminated(&mut self, block: Block) {
        self.terminated_blocks.insert(block);
    }

    fn is_block_terminated(&self, block: Block) -> bool {
        self.terminated_blocks.contains(&block)
    }
}

impl CodeGenerator {
    /// Creates a new instance of the code generator.
    pub fn new() -> Result<Self, CompileError> {
        // Set up the target ISA
        let isa_builder = cranelift_native::builder()
            .map_err(|e| CompileError::CraneliftError(e.to_string()))?;
        let flag_builder = settings::builder();
        // You can set Cranelift flags here if needed
        let flags = settings::Flags::new(flag_builder);
        let isa = isa_builder
            .finish(flags)
            .map_err(|e| CompileError::CraneliftError(e.to_string()))?;

        // Create the object module
        let builder = ObjectBuilder::new(
            isa,
            "my_module",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| CompileError::ModuleError(e.into()))?;

        let module = ObjectModule::new(builder);

        Ok(Self {
            module,
            functions: HashMap::new(),
            variables: HashMap::new(),
            string_data: HashMap::new(),
            structs: HashMap::new(),
        })
    }

    /// Generates code for the entire program.
    pub fn gen_program(&mut self, program: &Program) -> Result<(), CompileError> {
        // First, handle structs to get their sizes and field offsets
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

        // Handle global variables
        for global in &program.globals {
            self.declare_global(global)?;
        }

        Ok(())
    }

    /// Registers struct definitions to calculate field offsets and sizes.
    fn register_struct(&mut self, struct_def: &StructDef) -> Result<(), CompileError> {
        let mut field_offsets = HashMap::new();
        let mut field_types = HashMap::new();
        let mut offset = 0;
        for field in &struct_def.fields {
            field_offsets.insert(field.name.clone(), offset);
            field_types.insert(field.name.clone(), field.typ.clone());
            let ty_size = self.get_type_size(&field.typ)?;
            offset += ty_size;
        }
        self.structs.insert(
            struct_def.name.clone(),
            StructInfo {
                field_offsets,
                total_size: offset,
                field_types,
            },
        );
        Ok(())
    }

    /// Declares a function signature without defining it.
    fn declare_function(&mut self, function: &Function) -> Result<(), CompileError> {
        let mut signature = self.module.make_signature();

        for param in &function.params {
            let ty = self.get_cranelift_type(&param.typ)?;
            signature.params.push(AbiParam::new(ty));
        }

        if function.return_type != AstType::Void {
            let ret_ty = self.get_cranelift_type(&function.return_type)?;
            signature.returns.push(AbiParam::new(ret_ty));
        }

        let linkage = if function.external_lib.is_some() {
            Linkage::Import
        } else {
            Linkage::Export
        };

        let func_id = self
            .module
            .declare_function(&function.name, linkage, &signature)?;

        self.functions.insert(function.name.clone(), func_id);

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

        let mut data_desc = DataDescription::new();
        let ty = self.get_cranelift_type(&global.typ)?;
        let size = ty.bytes() as usize;

        if let Some(initializer) = &global.initializer {
            // Initialize with given value
            let value = self.evaluate_constant_expr(initializer)?;
            let bytes = match ty {
                types::I8 => (value as i8).to_le_bytes().to_vec(),
                types::I16 => (value as i16).to_le_bytes().to_vec(),
                types::I32 => (value as i32).to_le_bytes().to_vec(),
                types::I64 => value.to_le_bytes().to_vec(),
                _ => return Err(CompileError::UnsupportedType(format!("{:?}", ty))),
            };
            data_desc.define(bytes.into_boxed_slice());
        } else {
            // Zero-initialized data
            data_desc.define_zeroinit(size);
        }

        self.module
            .define_data(data_id, &data_desc)
            .map_err(|e| CompileError::ModuleError(e.into()))?;

        Ok(())
    }

    /// Defines the body of a function.
    fn define_function(&mut self, function: &Function) -> Result<(), CompileError> {
        let func_id = *self
            .functions
            .get(&function.name)
            .ok_or_else(|| CompileError::UndefinedFunction(function.name.clone()))?;

        let func_decl = self.module.declarations().get_function_decl(func_id);

        // Create code generation context
        let mut codegen_ctx = CodeGenContext::new();
        codegen_ctx.current_function = Some(function.name.clone());

        // Move context and builder_context into local variables
        let mut context = self.module.make_context();
        context.func.signature = func_decl.signature.clone();

        {
            let mut builder_context = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Declare function parameters as variables
            for (i, param) in function.params.iter().enumerate() {
                let var = Variable::new(i);
                codegen_ctx.variables.insert(param.name.clone(), var);
                let ty = self.get_cranelift_type(&param.typ)?;
                builder.declare_var(var, ty);

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
                    builder.ins().return_(&[]);
                    codegen_ctx.mark_block_terminated(current_block);
                } else {
                    return Err(CompileError::MissingFunctionBody(function.name.clone()));
                }
            }

            builder.seal_all_blocks();
            builder.finalize();
        }

        // Define the function in the module
        self.module
            .define_function(func_id, &mut context)
            .map_err(|e| CompileError::ModuleError(e.into()))?;
        self.module.clear_context(&mut context);

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
                let var_index = ctx.variables.len();
                let var = Variable::new(var_index);
                ctx.variables.insert(var_decl.name.clone(), var);
                let ty = self.get_cranelift_type(&var_decl.typ)?;
                builder.declare_var(var, ty);

                if let Some(initializer) = &var_decl.initializer {
                    let value = self.gen_expression(ctx, builder, initializer)?;
                    builder.def_var(var, value);
                } else {
                    let zero = self.default_value(builder, ty)?;
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
            Stmt::Break => {
                // Implement break statement
                return Err(CompileError::UnimplementedStatement(stmt.clone()));
            }
            Stmt::Continue => {
                // Implement continue statement
                return Err(CompileError::UnimplementedStatement(stmt.clone()));
            }
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
                if let Some(var) = ctx.variables.get(name) {
                    builder.def_var(*var, value);
                } else {
                    return Err(CompileError::UndefinedVariable(name.clone()));
                }
            }
            Expr::FieldAccess { expr, field } => {
                let ptr = self.gen_expression(ctx, builder, expr)?;
                let field_offset = self.get_field_offset(expr, field)?;
                let addr = builder.ins().iadd_imm(ptr, field_offset as i64);
                let mem_flags = MemFlags::new();
                builder.ins().store(mem_flags, value, addr, 0);
            }
            Expr::ArrayAccess { array, index } => {
                let base = self.gen_expression(ctx, builder, array)?;
                let idx = self.gen_expression(ctx, builder, index)?;
                let element_size = self.get_element_size(array)?;
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
        let cond_value = self.gen_expression(ctx, builder, condition)?;
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let merge_block = builder.create_block();

        builder.ins().brif(cond_value, then_block, &[], else_block, &[]);
        let current_block = builder.current_block().unwrap();
        ctx.mark_block_terminated(current_block);

        // Then block
        builder.switch_to_block(then_block);
        self.gen_statement(ctx, builder, then_branch)?;
        if !ctx.is_block_terminated(then_block) {
            builder.ins().jump(merge_block, &[]);
            ctx.mark_block_terminated(then_block);
        }
        builder.seal_block(then_block);

        // Else block
        builder.switch_to_block(else_block);
        if let Some(else_stmt) = else_branch {
            self.gen_statement(ctx, builder, else_stmt)?;
        }
        if !ctx.is_block_terminated(else_block) {
            builder.ins().jump(merge_block, &[]);
            ctx.mark_block_terminated(else_block);
        }
        builder.seal_block(else_block);

        // Merge block
        builder.switch_to_block(merge_block);
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
        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let exit_block = builder.create_block();

        builder.ins().jump(loop_header, &[]);
        let current_block = builder.current_block().unwrap();
        ctx.mark_block_terminated(current_block);

        // Loop header
        builder.switch_to_block(loop_header);
        let cond_value = self.gen_expression(ctx, builder, condition)?;
        builder.ins().brif(cond_value, loop_body, &[], exit_block, &[]);
        ctx.mark_block_terminated(loop_header);
        builder.seal_block(loop_header);

        // Loop body
        builder.switch_to_block(loop_body);
        self.gen_statement(ctx, builder, body)?;
        if !ctx.is_block_terminated(loop_body) {
            builder.ins().jump(loop_header, &[]);
            ctx.mark_block_terminated(loop_body);
        }
        builder.seal_block(loop_body);

        // Exit block
        builder.switch_to_block(exit_block);
        builder.seal_block(exit_block);

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
                if let Some(var) = ctx.variables.get(name) {
                    Ok(builder.use_var(*var))
                } else {
                    return Err(CompileError::UndefinedVariable(name.clone()));
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
                let field_offset = self.get_field_offset(expr, field)?;
                let addr = builder.ins().iadd_imm(ptr, field_offset as i64);
                let ty = self.get_expr_type(expr)?.clone();
                let field_type = self.get_field_type(&ty, field)?;
                let cl_type = self.get_cranelift_type(&field_type)?;
                let mem_flags = MemFlags::new();
                Ok(builder.ins().load(cl_type, mem_flags, addr, 0))
            }
            Expr::ArrayAccess { array, index } => {
                let base = self.gen_expression(ctx, builder, array)?;
                let idx = self.gen_expression(ctx, builder, index)?;
                let element_size = self.get_element_size(array)?;
                let offset = builder.ins().imul_imm(idx, element_size as i64);
                let addr = builder.ins().iadd(base, offset);
                let element_type = self.get_element_type(array)?;
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
                Ok(builder.ins().bmask(left_ty, cmp))
            }
            BinaryOp::NotEqual => {
                let cmp = builder.ins().icmp(IntCC::NotEqual, left, right);
                Ok(builder.ins().bmask(left_ty, cmp))
            }
            BinaryOp::LessThan => {
                let cmp = builder.ins().icmp(IntCC::SignedLessThan, left, right);
                Ok(builder.ins().bmask(left_ty, cmp))
            }
            BinaryOp::LessThanOrEqual => {
                let cmp = builder.ins().icmp(IntCC::SignedLessThanOrEqual, left, right);
                Ok(builder.ins().bmask(left_ty, cmp))
            }
            BinaryOp::GreaterThan => {
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
                Ok(builder.ins().bmask(left_ty, cmp))
            }
            BinaryOp::GreaterThanOrEqual => {
                let cmp = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left, right);
                Ok(builder.ins().bmask(left_ty, cmp))
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
            let func_ref = self.module.declare_func_in_func(func_id, &mut builder.func);

            let mut arg_values = Vec::new();
            for arg in arguments {
                let arg_val = self.gen_expression(ctx, builder, arg)?;
                arg_values.push(arg_val);
            }

            let call = builder.ins().call(func_ref, &arg_values);

            let results = builder.inst_results(call);
            if results.is_empty() {
                // Void function; return a dummy value (e.g., zero)
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

        // Allocate memory
        let size = builder.ins().iconst(types::I64, total_size);
        let malloc = self.get_malloc(builder)?;
        let call = builder.ins().call(malloc, &[size]);
        let struct_ptr = builder.inst_results(call)[0];

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
            let mem_flags = MemFlags::new();
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
        let element_type = self.get_expr_type(&elements[0])?;
        let element_size = self.get_type_size(&element_type)?;

        // Allocate space for the array
        let total_size = element_count * element_size as i64;
        let size = builder.ins().iconst(types::I64, total_size);
        let malloc = self.get_malloc(builder)?;
        let call = builder.ins().call(malloc, &[size]);
        let results = builder.inst_results(call);
        let array_ptr = results[0];

        // Initialize elements
        for (i, expr) in elements.iter().enumerate() {
            let value = self.gen_expression(ctx, builder, expr)?;
            let offset = builder.ins().iconst(types::I64, i as i64 * element_size as i64);
            let addr = builder.ins().iadd(array_ptr, offset);
            let mem_flags = MemFlags::new();
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
            let data_id = self.module.declare_data(
                &name,
                Linkage::Local,
                false, // writable
                false, // tls
            )?;

            let mut data_desc = DataDescription::new();
            data_desc.define(
                s.as_bytes()
                    .iter()
                    .cloned()
                    .chain(std::iter::once(0)) // Null-terminate
                    .collect::<Vec<u8>>()
                    .into_boxed_slice(),
            );

            self.module
                .define_data(data_id, &data_desc)
                .map_err(|e| CompileError::ModuleError(e.into()))?;
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
            AstType::String => Ok(self.module.isa().pointer_type()),
            AstType::Pointer(_) => Ok(self.module.isa().pointer_type()),
            AstType::Array(_, _) => Ok(self.module.isa().pointer_type()),
            AstType::Struct(_) => Ok(self.module.isa().pointer_type()),
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
    fn get_field_offset(&self, expr: &Expr, field: &str) -> Result<i32, CompileError> {
        if let AstType::Struct(name) = self.get_expr_type(expr)? {
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
    fn get_expr_type(&self, expr: &Expr) -> Result<AstType, CompileError> {
        // Implement type inference or tracking as needed
        Err(CompileError::UnimplementedExpression(expr.clone()))
    }

    /// Gets the malloc function reference.
    fn get_malloc(
        &mut self,
        builder: &mut FunctionBuilder,
    ) -> Result<FuncRef, CompileError> {
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(types::I64));
        signature
            .returns
            .push(AbiParam::new(self.module.isa().pointer_type()));

        let malloc_id = self
            .module
            .declare_function("malloc", Linkage::Import, &signature)?;
        Ok(self.module.declare_func_in_func(malloc_id, &mut builder.func))
    }

    /// Gets the element size of an array.
    fn get_element_size(&self, array_expr: &Expr) -> Result<i32, CompileError> {
        let array_type = self.get_expr_type(array_expr)?;
        if let AstType::Array(element_type, _) = array_type {
            self.get_type_size(&element_type)
        } else {
            Err(CompileError::UnsupportedType(format!(
                "Expected array type, found {:?}",
                array_type
            )))
        }
    }

    /// Gets the element type of an array.
    fn get_element_type(&self, array_expr: &Expr) -> Result<AstType, CompileError> {
        let array_type = self.get_expr_type(array_expr)?;
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
}
