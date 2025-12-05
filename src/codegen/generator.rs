use anyhow::{Context as AnyhowContext, Result as AnyhowResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use std::collections::HashMap;

use crate::codegen::{is_void_type, lang_type_to_llvm, CodegenError};
use crate::lexer::TypeBase;
use crate::parser::{
    BinaryOp, ComparisonOp, ExprKind, Expression, Function, GlobalVar, LiteralValue, Program,
    Statement, StatementKind,
};
use crate::parser::LangType;

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Track function declarations
    functions: HashMap<String, FunctionValue<'ctx>>,

    // Track local variables (stack allocations)
    // We use a Vec of HashMaps to handle nested scopes
    variables: Vec<HashMap<String, PointerValue<'ctx>>>,

    // Track variable types for implicit casting
    variable_types: Vec<HashMap<String, BasicTypeEnum<'ctx>>>,

    // Track global variables
    global_variables: HashMap<String, PointerValue<'ctx>>,

    // Current function being generated
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    #[must_use]
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
            variables: vec![HashMap::new()], // Start with global scope
            variable_types: vec![HashMap::new()], // Start with global scope
            global_variables: HashMap::new(),
            current_function: None,
        }
    }

    /// Generate LLVM IR from a program
    /// # Errors
    /// Returns `CodegenError` if any of the nested functions fail
    pub fn generate(&mut self, program: &Program) -> AnyhowResult<()> {
        // Generate global string literals first (they might be referenced by globals)
        for (i, s) in program.string_literals.iter().enumerate() {
            self.generate_string_literal(i, s);
        }

        // First pass: Declare all functions (for forward references)
        for func in &program.functions {
            self.declare_function(func)
                .with_context(|| format!("failed to declare function '{}'", func.proto.name))?;
        }

        // Generate global variables
        for global in &program.global_vars {
            self.generate_global_variable(global)
                .with_context(|| format!("failed to generate global variable '{}'", global.name))?;
        }

        // Second pass: Generate function bodies
        for func in &program.functions {
            if !func.proto.is_extern {
                self.generate_function(func)
                    .with_context(|| format!("failed to generate function '{}'", func.proto.name))?;
            }
        }

        Ok(())
    }

    /// Declare a function (without body)
    fn declare_function(&mut self, func: &Function) -> Result<FunctionValue<'ctx>, CodegenError> {
        // Convert parameter types
        let param_types: Result<Vec<_>, _> = func
            .proto
            .params
            .iter()
            .map(|(ty, _)| lang_type_to_llvm(self.context, ty))
            .collect();
        let param_types = param_types?;

        // Convert return type
        let return_type = if is_void_type(&func.proto.return_type) {
            None
        } else {
            Some(lang_type_to_llvm(self.context, &func.proto.return_type)?)
        };

        // Create function type
        let fn_type = if let Some(ret_ty) = return_type {
            let param_types: Vec<_> = param_types.iter().map(|ty| (*ty).into()).collect();
            ret_ty.fn_type(&param_types, false)
        } else {
            let param_types: Vec<_> = param_types.iter().map(|ty| (*ty).into()).collect();
            self.context.void_type().fn_type(&param_types, false)
        };

        // Add function to module
        let function = self.module.add_function(&func.proto.name, fn_type, None);

        // Set parameter names
        for (i, (_, param_name)) in func.proto.params.iter().enumerate() {
            function
                .get_nth_param(u32::try_from(i).expect("Parameter index out of bounds"))
                .unwrap()
                .set_name(param_name);
        }

        self.functions.insert(func.proto.name.clone(), function);
        Ok(function)
    }

    /// Generate a function with its body
    fn generate_function(&mut self, func: &Function) -> Result<(), CodegenError> {
        let function = *self.functions.get(&func.proto.name).ok_or_else(|| {
            CodegenError::UndefinedFunction(func.proto.name.clone(), func.proto.pos)
        })?;

        self.current_function = Some(function);

        // Create entry block
        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        // Enter function scope
        self.enter_scope();

        // Allocate space for parameters and store them
        for (i, (param_type, param_name)) in func.proto.params.iter().enumerate() {
            let param_value = function.get_nth_param(u32::try_from(i).expect("Parameter index out of bounds")).unwrap();
            let param_llvm_type = lang_type_to_llvm(self.context, param_type)?;

            // Allocate stack space for parameter
            let alloca = self
                .builder
                .build_alloca(param_llvm_type, param_name)
                ?;

            // Store parameter value
            self.builder
                .build_store(alloca, param_value)
                ?;

            // Add to variables
            self.add_variable(param_name.clone(), alloca, param_llvm_type);
        }

        // Generate function body
        for stmt in &func.body {
            self.generate_statement(stmt)?;
        }

        // If function doesn't have an explicit return, add one
        if !self.block_has_terminator() {
            if is_void_type(&func.proto.return_type) {
                self.builder
                    .build_return(None)
                    ?;
            } else {
                // Return a zero value for non-void functions without explicit return
                let zero = self.get_zero_value(&func.proto.return_type)?;
                self.builder
                    .build_return(Some(&zero))
                    ?;
            }
        }

        // Exit function scope
        self.exit_scope();
        self.current_function = None;

        Ok(())
    }

    /// Generate a global variable
    fn generate_global_variable(&mut self, global: &GlobalVar) -> Result<(), CodegenError> {
        let global_type = lang_type_to_llvm(self.context, &global.var_type)?;

        let global_var =
            self.module
                .add_global(global_type, Some(AddressSpace::default()), &global.name);

        // Set initializer
        if let Some(init_expr) = &global.initializer {
            // For now, global initializers must be constants
            // This is a simplification - LLVM requires constant expressions for globals
            let init_value = self.generate_constant_expression(init_expr)?;
            global_var.set_initializer(&init_value);
        } else {
            // Initialize to zero
            global_var.set_initializer(&global_type.const_zero());
        }

        self.global_variables
            .insert(global.name.clone(), global_var.as_pointer_value());
        Ok(())
    }

    /// Generate a string literal
    fn generate_string_literal(&mut self, index: usize, value: &str) {
        let string_name = format!(".str.{index}");
        let string_value = self.context.const_string(value.as_bytes(), true);
        let global_string = self.module.add_global(
            string_value.get_type(),
            Some(AddressSpace::default()),
            &string_name,
        );
        global_string.set_initializer(&string_value);
        global_string.set_constant(true);

        // Store in global variables with special naming
        self.global_variables
            .insert(string_name, global_string.as_pointer_value());
    }

    /// Generate code for a statement
    fn generate_statement(&mut self, stmt: &Statement) -> Result<(), CodegenError> {
        match &stmt.kind {
            StatementKind::Expression(expr) => {
                // For expression statements, we might have void function calls
                // Handle them specially
                if let ExprKind::FunctionCall { name, args } = &expr.kind {
                    self.generate_function_call_statement(name, args, expr.pos)?;
                    Ok(())
                } else {
                    self.generate_expression(expr)?;
                    Ok(())
                }
            }

            StatementKind::VarDecl {
                var_type,
                name,
                initializer,
            } => {
                let llvm_type = lang_type_to_llvm(self.context, var_type)?;

                // Allocate stack space with proper alignment
                let alloca = self
                    .builder
                    .build_alloca(llvm_type, name)
                    ?;

                // Initialize if provided
                if let Some(init_expr) = initializer {
                    let mut init_value = self.generate_expression(init_expr)?;

                    // If types don't match, insert implicit cast
                    if init_value.get_type() != llvm_type {
                        init_value = self.cast_value(init_value, llvm_type, &init_expr.expr_type)?;
                    }

                    self.builder
                        .build_store(alloca, init_value)
                        ?;
                } else {
                    // Initialize to zero
                    let zero = llvm_type.const_zero();
                    self.builder
                        .build_store(alloca, zero)
                        ?;
                }

                self.add_variable(name.clone(), alloca, llvm_type);
                Ok(())
            }

            StatementKind::VarAssign { name, value } => {
                let var_ptr = self
                    .lookup_variable(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone(), stmt.pos))?;

                let var_type = self
                    .lookup_variable_type(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone(), stmt.pos))?;

                let mut value_llvm = self.generate_expression(value)?;

                // If types don't match, insert implicit cast
                if value_llvm.get_type() != var_type {
                    value_llvm = self.cast_value(value_llvm, var_type, &value.expr_type)?;
                }

                self.builder
                    .build_store(var_ptr, value_llvm)
                    ?;

                Ok(())
            }

            StatementKind::DerefAssign { target, value } => {
                // Extract the pointer from the dereference expression
                match &target.kind {
                    ExprKind::Dereference(ptr_expr) => {
                        // Generate the pointer expression (not the dereference itself)
                        let ptr = self.generate_expression(ptr_expr)?;

                        // Generate the value to store
                        let value_llvm = self.generate_expression(value)?;

                        // Store the value at the dereferenced location
                        self.builder
                            .build_store(ptr.into_pointer_value(), value_llvm)?;

                        Ok(())
                    }
                    _ => Err(CodegenError::InvalidOperation(
                        "DerefAssign target must be a dereference expression".to_string(),
                        target.pos,
                    )),
                }
            }

            StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    let value = self.generate_expression(expr)?;
                    self.builder
                        .build_return(Some(&value))?;
                } else {
                    self.builder
                        .build_return(None)?;
                }
                Ok(())
            }

            StatementKind::If {
                condition,
                then_block,
                else_block,
            } => {
                self.generate_if_statement(condition, then_block, else_block.as_ref().map(Vec::as_slice))?;
                Ok(())
            }

            StatementKind::While { condition, body } => {
                self.generate_while_loop(condition, body)?;
                Ok(())
            }

            StatementKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.generate_for_loop(init.clone(), condition.as_ref(), increment.clone(), body)?;
                Ok(())
            }

            StatementKind::Block(statements) => {
                self.enter_scope();
                for stmt in statements {
                    self.generate_statement(stmt)?;
                }
                self.exit_scope();
                Ok(())
            }
        }
    }

    /// Generate an if statement
    fn generate_if_statement(
        &mut self,
        condition: &Expression,
        then_block: &[Statement],
        else_block: Option<&[Statement]>,
    ) -> Result<(), CodegenError> {
        let function = self
            .current_function
            .ok_or(CodegenError::UnexpectedStatement(condition.pos))?;

        // Generate condition
        let cond_value = self.generate_expression(condition)?;
        let cond_int = self.value_to_bool(cond_value)?;

        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "ifcont");

        // Branch on condition
        self.builder
            .build_conditional_branch(cond_int, then_bb, else_bb)
            ?;

        // Generate then block
        self.builder.position_at_end(then_bb);
        for stmt in then_block {
            self.generate_statement(stmt)?;
        }
        if !self.block_has_terminator() {
            self.builder
                .build_unconditional_branch(merge_bb)
                ?;
        }

        // Generate else block
        self.builder.position_at_end(else_bb);
        if let Some(else_stmts) = else_block {
            for stmt in else_stmts {
                self.generate_statement(stmt)?;
            }
        }
        if !self.block_has_terminator() {
            self.builder
                .build_unconditional_branch(merge_bb)
                ?;
        }

        // Continue at merge block
        self.builder.position_at_end(merge_bb);

        Ok(())
    }

    /// Generate a while loop
    fn generate_while_loop(
        &mut self,
        condition: &Expression,
        body: &[Statement],
    ) -> Result<(), CodegenError> {
        let function = self
            .current_function
            .ok_or(CodegenError::UnexpectedStatement(condition.pos))?;

        let cond_bb = self.context.append_basic_block(function, "while.cond");
        let body_bb = self.context.append_basic_block(function, "while.body");
        let end_bb = self.context.append_basic_block(function, "while.end");

        // Jump to condition
        self.builder
            .build_unconditional_branch(cond_bb)
            ?;

        // Generate condition
        self.builder.position_at_end(cond_bb);
        let cond_value = self.generate_expression(condition)?;
        let cond_int = self.value_to_bool(cond_value)?;
        self.builder
            .build_conditional_branch(cond_int, body_bb, end_bb)
            ?;

        // Generate body
        self.builder.position_at_end(body_bb);
        for stmt in body {
            self.generate_statement(stmt)?;
        }
        if !self.block_has_terminator() {
            self.builder
                .build_unconditional_branch(cond_bb)
                ?;
        }

        // Continue after loop
        self.builder.position_at_end(end_bb);

        Ok(())
    }

    /// Generate a for loop
    fn generate_for_loop(
        &mut self,
        init: Option<Box<Statement>>,
        condition: Option<&Expression>,
        increment: Option<Box<Statement>>,
        body: &[Statement],
    ) -> Result<(), CodegenError> {
        let function = self
            .current_function
            .ok_or_else(|| CodegenError::UnexpectedStatement(body[0].pos))?;

        // Enter scope for loop variable
        self.enter_scope();

        // Generate init
        if let Some(init_stmt) = init {
            self.generate_statement(&init_stmt)?;
        }

        let cond_bb = self.context.append_basic_block(function, "for.cond");
        let body_bb = self.context.append_basic_block(function, "for.body");
        let inc_bb = self.context.append_basic_block(function, "for.inc");
        let end_bb = self.context.append_basic_block(function, "for.end");

        // Jump to condition
        self.builder
            .build_unconditional_branch(cond_bb)
            ?;

        // Generate condition
        self.builder.position_at_end(cond_bb);
        let cond_value = if let Some(cond_expr) = condition {
            let cond_val = self.generate_expression(cond_expr)?;
            self.value_to_bool(cond_val)?
        } else {
            // No condition means infinite loop (true)
            self.context.bool_type().const_all_ones()
        };
        self.builder
            .build_conditional_branch(cond_value, body_bb, end_bb)
            ?;

        // Generate body
        self.builder.position_at_end(body_bb);
        for stmt in body {
            self.generate_statement(stmt)?;
        }
        if !self.block_has_terminator() {
            self.builder
                .build_unconditional_branch(inc_bb)
                ?;
        }

        // Generate increment
        self.builder.position_at_end(inc_bb);
        if let Some(inc_stmt) = increment {
            self.generate_statement(&inc_stmt)?;
        }
        self.builder
            .build_unconditional_branch(cond_bb)
            ?;

        // Continue after loop
        self.builder.position_at_end(end_bb);

        // Exit loop scope
        self.exit_scope();

        Ok(())
    }

    /// Generate code for an expression
    fn generate_expression(
        &mut self,
        expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match &expr.kind {
            ExprKind::Literal(lit) => self.generate_literal(lit, &expr.expr_type),

            ExprKind::Variable(name) => {
                let var_ptr = self
                    .lookup_variable(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone(), expr.pos))?;

                let var_type = lang_type_to_llvm(self.context, &expr.expr_type)?;

                Ok(self.builder
                    .build_load(var_type, var_ptr, name)?)
            }

            ExprKind::Binary { left, op, right } => self.generate_binary_op(left, op, right),

            ExprKind::Comparison { left, op, right } => self.generate_comparison(left, op, right),

            ExprKind::Reference(expr) => {
                // Get the address of the expression
                match &expr.kind {
                    ExprKind::Variable(name) => {
                        let var_ptr = self.lookup_variable(name)
                        .ok_or_else(|| {
                            CodegenError::UndefinedVariable(name.clone(), expr.pos)
                        })?;
                        Ok(var_ptr.into())
                    }
                    ExprKind::Dereference(inner) => {
                        // &*ptr = ptr
                        self.generate_expression(inner)
                    }
                    _ => Err(CodegenError::InvalidOperation(
                        "Cannot take address of non-lvalue".to_string(),
                        expr.pos,
                    )),
                }
            }

            ExprKind::Dereference(inner_expr) => {
                let ptr = self.generate_expression(inner_expr)?;
                // The type to load is the type of the dereference expression itself (the pointee type),
                // not the type of the pointer expression
                let derefed_type = if inner_expr.expr_type.pointer_depth == 0 {
                    return Err(CodegenError::TypeError(
                        "Cannot dereference a non-pointer type".to_string(),
                        expr.pos,
                    ));
                } else {
                    LangType {
                        base: inner_expr.expr_type.base.clone(),
                        size_bits: inner_expr.expr_type.size_bits,
                        pointer_depth: inner_expr.expr_type.pointer_depth - 1,
                        is_const: inner_expr.expr_type.is_const,
                    }
                };
                let pointee_type = lang_type_to_llvm(self.context, &derefed_type)?;
                Ok(self.builder
                    .build_load(pointee_type, ptr.into_pointer_value(), "deref")?)
            }

            ExprKind::FunctionCall { name, args } => {
                self.generate_function_call(name, args, expr.pos)
            }

            ExprKind::Cast { expr, target_type } => self.generate_cast(expr, target_type),
        }
    }

    /// Generate a literal value
    #[allow(clippy::cast_sign_loss)]
    fn generate_literal(
        &self,
        lit: &LiteralValue,
        ty: &crate::lexer::LangType,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match lit {
            LiteralValue::Integer(val) => {
                let llvm_type = lang_type_to_llvm(self.context, ty)?;
                match llvm_type {
                    BasicTypeEnum::IntType(int_ty) => {
                        Ok(int_ty.const_int(*val as u64, true).into())
                    }
                    _ => Err(CodegenError::TypeError(
                        "Integer literal must have integer type".to_string(),
                        crate::lexer::Position::new(0, 0),
                    )),
                }
            }

            LiteralValue::Float(val) => {
                let llvm_type = lang_type_to_llvm(self.context, ty)?;
                match llvm_type {
                    BasicTypeEnum::FloatType(float_ty) => Ok(float_ty.const_float(*val).into()),
                    _ => Err(CodegenError::TypeError(
                        "Float literal must have float type".to_string(),
                        crate::lexer::Position::new(0, 0),
                    )),
                }
            }

            LiteralValue::String(index) => {
                // Look up the string global
                let string_name = format!(".str.{index}");
                let global_ptr = self.global_variables.get(&string_name)
                    .expect("Internal error: String literal global not found");

                // Cast to i8*
                let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
                let casted = self
                    .builder
                    .build_pointer_cast(*global_ptr, i8_ptr_type, "str")
                    ?;

                Ok(casted.into())
            }
        }
    }

    fn generate_int_binary_op(
        &mut self,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let is_signed = matches!(left.expr_type.base, TypeBase::SInt);
        let left_int = self.generate_expression(left)?.into_int_value();
        let right_int = self.generate_expression(right)?.into_int_value();
        let res = match op {
            BinaryOp::Add => self
                .builder
                .build_int_add(left_int, right_int, "add")
                .map(Into::into)?,
            BinaryOp::Sub => self
                .builder
                .build_int_sub(left_int, right_int, "sub")
                .map(Into::into)?,
            BinaryOp::Mul => self
                .builder
                .build_int_mul(left_int, right_int, "mul")
                .map(Into::into)?,
            BinaryOp::Div => {
                if is_signed {
                    self.builder
                        .build_int_signed_div(left_int, right_int, "sdiv")
                        .map(Into::into)?
                } else {
                    self.builder
                        .build_int_unsigned_div(left_int, right_int, "udiv")
                        .map(Into::into)?
                }
            }
            BinaryOp::Mod => {
                if is_signed {
                    self.builder
                        .build_int_signed_rem(left_int, right_int, "srem")
                        .map(Into::into)?
                } else {
                    self.builder
                        .build_int_unsigned_rem(left_int, right_int, "urem")
                        .map(Into::into)?
                }
            }
            BinaryOp::And => self
                .builder
                .build_and(left_int, right_int, "and")
                .map(Into::into)?,
            BinaryOp::Or => self
                .builder
                .build_or(left_int, right_int, "or")
                .map(Into::into)?,
            BinaryOp::Xor => self
                .builder
                .build_xor(left_int, right_int, "xor")
                .map(Into::into)?,
            BinaryOp::LeftShift => self
                .builder
                .build_left_shift(left_int, right_int, "shl")
                .map(Into::into)?,
            BinaryOp::RightShift => {
                if is_signed {
                    self.builder
                        .build_right_shift(left_int, right_int, true, "ashr")
                        .map(Into::into)?
                } else {
                    self.builder
                        .build_right_shift(left_int, right_int, false, "lshr")
                        .map(Into::into)?
                }
            }
        };
        Ok(res)
    }

    fn generate_float_binary_op(
        &mut self,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let left_float = self.generate_expression(left)?.into_float_value();
        let right_float =self.generate_expression(right)?.into_float_value();
        match op {
            BinaryOp::Add => Ok(self
                .builder
                .build_float_add(left_float, right_float, "fadd")
                .map(Into::into)?),
            BinaryOp::Sub => Ok(self
                .builder
                .build_float_sub(left_float, right_float, "fsub")
                .map(Into::into)?),
            BinaryOp::Mul => Ok(self
                .builder
                .build_float_mul(left_float, right_float, "fmul")
                .map(Into::into)?),
            BinaryOp::Div => Ok(self
                .builder
                .build_float_div(left_float, right_float, "fdiv")
                .map(Into::into)?),
            _ => Err(CodegenError::InvalidOperation(
                format!("Operator {op:?} not supported for floats"),
                left.pos,
            ))
        }
    }

    fn generate_pointer_binary_op(&mut self, left: &Expression, op: &BinaryOp, right: &Expression) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if right.expr_type.pointer_depth > 0 {
            return Err(CodegenError::InvalidOperation(
                "Pointer arithmetic only allowed with integers".to_string(),
                left.pos,
            ));
        }
        let left_ptr = self.generate_expression(left)?.into_pointer_value();
        let right_int = self.generate_expression(right)?.into_int_value();
        let pointee_type = lang_type_to_llvm(self.context, &LangType {
            base: left.expr_type.base.clone(),
            size_bits: left.expr_type.size_bits,
            pointer_depth: left.expr_type.pointer_depth - 1,
            is_const: left.expr_type.is_const,
        })?;
        
        match op {
            BinaryOp::Add => unsafe {
                Ok(self.builder.build_gep(pointee_type, left_ptr, &[right_int], "ptr_add")
                    .map(Into::into)?)
            },
            BinaryOp::Sub => {
                let neg_right = self.builder
                    .build_int_neg(right_int, "neg")
                    ?;
                unsafe {
                    Ok(self.builder.build_gep(pointee_type, left_ptr, &[neg_right], "ptr_sub")
                        .map(Into::into)?)
                }
            },
            _ => Err(CodegenError::InvalidOperation(
                format!("Operator {op:?} not supported for pointers"),
                left.pos,
            )),
        }
    }

    /// Generate a binary operation
    fn generate_binary_op(
        &mut self,
        left: &Expression,
        op: &BinaryOp,
        right: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Determine if we're working with floats or ints
        let is_float = matches!(left.expr_type.base, TypeBase::SFloat);

        // Pointers are special. They should only be allowed to be manipulated by integers, and you can only do addition and subtraction.
        let is_pointer = left.expr_type.pointer_depth > 0;

        if is_float {
            self.generate_float_binary_op(left, op, right)
        } else if is_pointer {
            self.generate_pointer_binary_op(left, op, right)
        } else {
            self.generate_int_binary_op(left, op, right)
        }
    }

    /// Generate a comparison
    fn generate_comparison(
        &mut self,
        left: &Expression,
        op: &ComparisonOp,
        right: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let left_val = self.generate_expression(left)?;
        let right_val = self.generate_expression(right)?;

        let is_float = matches!(left.expr_type.base, TypeBase::SFloat);

        if is_float {
            let left_float = left_val.into_float_value();
            let right_float = right_val.into_float_value();

            let predicate = match op {
                ComparisonOp::Equal => inkwell::FloatPredicate::OEQ,
                ComparisonOp::NotEqual => inkwell::FloatPredicate::ONE,
                ComparisonOp::Less => inkwell::FloatPredicate::OLT,
                ComparisonOp::Greater => inkwell::FloatPredicate::OGT,
                ComparisonOp::LessEqual => inkwell::FloatPredicate::OLE,
                ComparisonOp::GreaterEqual => inkwell::FloatPredicate::OGE,
            };

            let cmp = self
                .builder
                .build_float_compare(predicate, left_float, right_float, "fcmp")
                ?;

            // Extend to i32
            Ok(self.builder
                .build_int_z_extend(cmp, self.context.i32_type(), "cmp_ext")
                .map(Into::into)?)
        } else {
            let left_int = left_val.into_int_value();
            let right_int = right_val.into_int_value();
            let is_signed = matches!(left.expr_type.base, TypeBase::SInt);

            let predicate = match op {
                ComparisonOp::Equal => IntPredicate::EQ,
                ComparisonOp::NotEqual => IntPredicate::NE,
                ComparisonOp::Less => {
                    if is_signed {
                        IntPredicate::SLT
                    } else {
                        IntPredicate::ULT
                    }
                }
                ComparisonOp::Greater => {
                    if is_signed {
                        IntPredicate::SGT
                    } else {
                        IntPredicate::UGT
                    }
                }
                ComparisonOp::LessEqual => {
                    if is_signed {
                        IntPredicate::SLE
                    } else {
                        IntPredicate::ULE
                    }
                }
                ComparisonOp::GreaterEqual => {
                    if is_signed {
                        IntPredicate::SGE
                    } else {
                        IntPredicate::UGE
                    }
                }
            };

            let cmp = self
                .builder
                .build_int_compare(predicate, left_int, right_int, "icmp")
                ?;

            // Extend to i32
            Ok(self.builder
                .build_int_z_extend(cmp, self.context.i32_type(), "cmp_ext")
                .map(Into::into)?)
        }
    }

    /// Generate a function call (expression context - must return a value)
    fn generate_function_call(
        &mut self,
        name: &str,
        args: &[Expression],
        pos: crate::lexer::Position,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let function = *self
            .functions
            .get(name)
            .ok_or_else(|| CodegenError::UndefinedFunction(name.to_string(), pos))?;

        let mut arg_values = Vec::new();
        for arg in args {
            let val = self.generate_expression(arg)?;
            arg_values.push(val.into());
        }

        let call_result = self
            .builder
            .build_call(function, &arg_values, "call")
            ?;

        // Extract BasicValueEnum from the call result
        call_result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::MissingReturn(name.to_string(), pos))
    }

    /// Generate a function call as a statement (void return is OK)
    fn generate_function_call_statement(
        &mut self,
        name: &str,
        args: &[Expression],
        pos: crate::lexer::Position,
    ) -> Result<(), CodegenError> {
        let function = *self
            .functions
            .get(name)
            .ok_or_else(|| CodegenError::UndefinedFunction(name.to_string(), pos))?;

        let mut arg_values = Vec::new();
        for arg in args {
            let val = self.generate_expression(arg)?;
            arg_values.push(val.into());
        }

        self.builder
            .build_call(function, &arg_values, "call")
            ?;

        Ok(())
    }

    /// Generate a type cast
    fn generate_cast(
        &mut self,
        expr: &Expression,
        target_type: &crate::lexer::LangType,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let value = self.generate_expression(expr)?;
        let target_llvm_type = lang_type_to_llvm(self.context, target_type)?;
        self.cast_value(value, target_llvm_type, &expr.expr_type)
    }

    /// Cast a value to a target LLVM type
    fn cast_value(
        &self,
        value: BasicValueEnum<'ctx>,
        target_llvm_type: BasicTypeEnum<'ctx>,
        source_lang_type: &crate::lexer::LangType,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // If types already match, no cast needed
        if value.get_type() == target_llvm_type {
            return Ok(value);
        }

        // Determine target lang type properties from LLVM type
        let target_is_pointer = matches!(target_llvm_type, BasicTypeEnum::PointerType(_));
        let target_is_float = matches!(target_llvm_type, BasicTypeEnum::FloatType(_));
        let target_is_int = matches!(target_llvm_type, BasicTypeEnum::IntType(_));

        // Handle pointer casts
        if target_is_pointer {
            return if source_lang_type.pointer_depth == 0 {
                Ok(self
                    .builder
                    .build_int_to_ptr(
                        value.into_int_value(),
                        target_llvm_type.into_pointer_type(),
                        "inttoptr",
                    )?.into())
            } else {
                Ok(self
                    .builder
                    .build_pointer_cast(
                        value.into_pointer_value(),
                        target_llvm_type.into_pointer_type(),
                        "ptrcast",
                    )?.into())
            }
        }

        // Handle int to float
        if target_is_float && value.is_int_value() {
            let int_val = value.into_int_value();
            let is_signed = matches!(source_lang_type.base, TypeBase::SInt);

            return Ok(if is_signed {
                self.builder
                    .build_signed_int_to_float(
                        int_val,
                        target_llvm_type.into_float_type(),
                        "sitofp",
                    )
                    .map(Into::into)?
            } else {
                self.builder
                    .build_unsigned_int_to_float(
                        int_val,
                        target_llvm_type.into_float_type(),
                        "uitofp",
                    )
                    .map(Into::into)?
            });
        }

        // Handle float to int
        if target_is_int && value.is_float_value() {
            let float_val = value.into_float_value();
            let target_int_type = target_llvm_type.into_int_type();
            // Assume unsigned for now if we don't have type info
            // TODO: Pass target lang type to know if signed/unsigned
            return Ok(self.builder
                .build_float_to_signed_int(
                    float_val,
                    target_int_type,
                    "fptosi",
                )
                .map(Into::into)?);
        }

        // Handle pointer to int
        if target_is_int && value.is_pointer_value() {
            let ptr_val = value.into_pointer_value();
            let target_int_type = target_llvm_type.into_int_type();
            return Ok(self.builder
                .build_ptr_to_int(ptr_val, target_int_type, "ptrtoint")
                .map(Into::into)?);
        }

        // Handle int to int (resize)
        if target_is_int && value.is_int_value() {
            let int_val = value.into_int_value();
            let target_int_type = target_llvm_type.into_int_type();
            let source_bits = int_val.get_type().get_bit_width();
            let target_bits = target_int_type.get_bit_width();
            let is_signed = matches!(source_lang_type.base, TypeBase::SInt);

            return if target_bits > source_bits {
                // Extend
                Ok(if is_signed {
                    self.builder
                        .build_int_s_extend(int_val, target_int_type, "sext")
                        .map(Into::into)?
                } else {
                    self.builder
                        .build_int_z_extend(int_val, target_int_type, "zext")
                        .map(Into::into)?
                })
            } else if target_bits < source_bits {
                // Truncate
                Ok(self.builder
                    .build_int_truncate(int_val, target_int_type, "trunc")
                    .map(Into::into)?)
            } else {
                // Same size, no cast needed
                Ok(value)
            };
        }

        // If we can't handle the cast, return the value as-is
        Ok(value)
    }

    /// Generate a constant expression (for global initializers)
    fn generate_constant_expression(
        &self,
        expr: &Expression,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match &expr.kind {
            ExprKind::Literal(lit) => self.generate_constant_literal(lit, &expr.expr_type),
            _ => Err(CodegenError::InvalidOperation(
                "Non-constant expression in global initializer".to_string(),
                expr.pos,
            )),
        }
    }

    /// Generate a constant literal (without using the builder)
    #[allow(clippy::cast_sign_loss)]
    fn generate_constant_literal(
        &self,
        lit: &LiteralValue,
        ty: &crate::lexer::LangType,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match lit {
            LiteralValue::Integer(val) => {
                let llvm_type = lang_type_to_llvm(self.context, ty)?;
                match llvm_type {
                    BasicTypeEnum::IntType(int_ty) => {
                        Ok(int_ty.const_int(*val as u64, true).into())
                    }
                    _ => Err(CodegenError::TypeError(
                        "Integer literal must have integer type".to_string(),
                        crate::lexer::Position::new(0, 0),
                    )),
                }
            }

            LiteralValue::Float(val) => {
                let llvm_type = lang_type_to_llvm(self.context, ty)?;
                match llvm_type {
                    BasicTypeEnum::FloatType(float_ty) => Ok(float_ty.const_float(*val).into()),
                    _ => Err(CodegenError::TypeError(
                        "Float literal must have float type".to_string(),
                        crate::lexer::Position::new(0, 0),
                    )),
                }
            }

            LiteralValue::String(index) => {
                // Look up the string global
                let string_name = format!(".str.{index}");
                let global_ptr = self.global_variables.get(&string_name).expect(
                    "Internal error: String literal global not found",
                );

                // For constants, we can just use the global pointer directly
                // Cast to i8* using const_cast
                let i8_ptr_type = self.context.ptr_type(AddressSpace::default());
                Ok(global_ptr.const_cast(i8_ptr_type).into())
            }
        }
    }

    /// Convert a value to a boolean (i1) for conditionals
    fn value_to_bool(&self, value: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, CodegenError> {
        if value.is_int_value() {
            let int_val = value.into_int_value();
            // Compare with zero
            let zero = int_val.get_type().const_zero();
            Ok(self.builder
                .build_int_compare(IntPredicate::NE, int_val, zero, "tobool")?)
        } else if value.is_float_value() {
            let float_val = value.into_float_value();
            let zero = float_val.get_type().const_zero();
            Ok(self.builder
                .build_float_compare(inkwell::FloatPredicate::ONE, float_val, zero, "tobool")?)
        } else {
            Err(CodegenError::TypeError(
                "Cannot convert value to boolean".to_string(),
                crate::lexer::Position::new(0, 0),
            ))
        }
    }

    /// Check if the current block has a terminator
    fn block_has_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .and_then(inkwell::basic_block::BasicBlock::get_terminator)
            .is_some()
    }

    /// Get a zero value for a type
    fn get_zero_value(
        &self,
        ty: &crate::lexer::LangType,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let llvm_type = lang_type_to_llvm(self.context, ty)?;
        Ok(llvm_type.const_zero())
    }

    // Scope management
    fn enter_scope(&mut self) {
        self.variables.push(HashMap::new());
        self.variable_types.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.variables.pop();
        self.variable_types.pop();
    }

    fn add_variable(&mut self, name: String, ptr: PointerValue<'ctx>, ty: BasicTypeEnum<'ctx>) {
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(name.clone(), ptr);
        }
        if let Some(type_scope) = self.variable_types.last_mut() {
            type_scope.insert(name, ty);
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<PointerValue<'ctx>> {
        // Search from innermost to outermost scope
        for scope in self.variables.iter().rev() {
            if let Some(ptr) = scope.get(name) {
                return Some(*ptr);
            }
        }
        // Check globals
        self.global_variables.get(name).copied()
    }

    fn lookup_variable_type(&self, name: &str) -> Option<BasicTypeEnum<'ctx>> {
        // Search from innermost to outermost scope
        for scope in self.variable_types.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(*ty);
            }
        }
        None
    }

    /// Get the LLVM module
    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    /// Print the LLVM IR to a string
    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Write LLVM IR to a file
    /// # Panics
    /// When writing to the file fails
    /// # Errors
    /// Never
    pub fn write_to_file(&self, path: &std::path::Path) -> Result<(), CodegenError> {
        self.module
            .print_to_file(path).expect("Failed to write LLVM IR to file");
        Ok(())
    }
}
