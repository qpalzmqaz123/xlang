mod traits;
mod var_table;

use std::{any::Any, mem::ManuallyDrop, ops::Deref};

use inkwell::{
    context::Context as InkwellContext,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module as InkwellModule,
    passes::PassManager,
};

use crate::{
    error::{self, Result},
    lexer::Lexer,
    parser::Parser,
    LLVMValue, StmtListNode,
};

pub use var_table::VarTable;

use self::traits::{AddLLVMGlobal, LLVMParamMulti, LLVMRetSingle};

#[macro_export]
macro_rules! add_global_function {
    ($ctx:expr, $name:expr, |$($arg_name:ident: $arg_ty:ty),*|:$ret_ty:ty $body:block) => {{
        // Generate boxed closure, because rust dynamic trait use double pointer, which size is 16, so we use double layer box
        let closure: Box<Box<dyn Fn($($arg_ty),*) -> $ret_ty>> = Box::new(Box::new(move |$($arg_name: $arg_ty),*| -> $ret_ty {
            $body
        }));

        // Static wrapper function, first argument is boxed dyn Fn pointer
        extern "C" fn wrapper(boxed_closure_addr: usize, $($arg_name: $arg_ty),*) -> $ret_ty {
            let closure = boxed_closure_addr as *mut Box<dyn Fn($($arg_ty),*) -> $ret_ty>;
            unsafe {
                (*closure)($($arg_name),*)
            }
        }

        // Get boxed closure address
        let addr = closure.as_ref() as *const _ as usize;

        $ctx.add_global_rust_closure::<($($arg_ty),*), $ret_ty>($name, addr, Box::new(closure), wrapper as usize)
    }};
}

pub struct Context {
    ll_ctx: InkwellContext,
    ll_mod: ManuallyDrop<InkwellModule<'static>>,
    ll_ee: ManuallyDrop<ExecutionEngine<'static>>,
    init_vartable: VarTable<'static>,
    closure_list: Vec<Box<dyn Any>>,
}

unsafe impl Send for Context {}

impl Context {
    pub fn new() -> Result<Self> {
        // Only run on 64-bit systems
        assert_eq!(8, std::mem::size_of::<usize>());

        // Create llvm context
        let ll_ctx = InkwellContext::create();

        // Create llvm module
        let ll_mod = ll_ctx.create_module("main");

        // Create llvm engine
        let ll_ee = ll_mod
            .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
            .map_err(|e| error::internal!("LLVM create execution engine error: {}", e))?;

        let ll_mod = ManuallyDrop::new(unsafe { std::mem::transmute(ll_mod) });
        let ll_ee = ManuallyDrop::new(unsafe { std::mem::transmute(ll_ee) });

        Ok(Self {
            ll_ctx,
            ll_mod,
            ll_ee,
            init_vartable: VarTable::new(),
            closure_list: vec![],
        })
    }

    pub fn add_global<T: AddLLVMGlobal<'static>>(&mut self, name: &str, addr: usize) -> Result<()> {
        // Convert context lifetime to static
        let ctx: &'static InkwellContext = unsafe { std::mem::transmute(&self.ll_ctx) };

        // Get module reference
        let module = self.ll_mod.deref();

        // Get execution engine reference
        let engine = self.ll_ee.deref();

        // Add global value to module
        let value = T::add_module_global(name, ctx, &module);

        // Set global variable value
        self.init_vartable.insert(name, value.clone());

        // Set variable address
        match value {
            LLVMValue::Pointer(p) => engine.add_global_mapping(&p.get_pointer_value(), addr),
            _ => return Err(error::internal!("Unsupported global value: {:?}", value)),
        }

        Ok(())
    }

    pub fn add_global_c_function<A, R>(&mut self, name: &str, addr: usize) -> Result<()>
    where
        A: LLVMParamMulti<'static>,
        R: LLVMRetSingle<'static>,
    {
        // Convert context lifetime to static
        let ctx: &'static InkwellContext = unsafe { std::mem::transmute(&self.ll_ctx) };

        // Get module reference
        let module = self.ll_mod.deref();

        // Get execution engine reference
        let engine = self.ll_ee.deref();

        // Create function type
        let fn_ty = R::fn_type(ctx, &A::param_types(ctx));

        // Create function
        let fn_val = module.add_function(name, fn_ty, None);

        // Add function to execution engine
        engine.add_global_mapping(&fn_val, addr);

        Ok(())
    }

    pub fn add_global_rust_closure<A, R>(
        &mut self,
        name: &str,
        boxed_closure_addr: usize,
        boxed_closure_any: Box<dyn Any>,
        wrapper_c_fn_addr: usize,
    ) -> Result<()>
    where
        A: LLVMParamMulti<'static>,
        R: LLVMRetSingle<'static>,
    {
        // Convert context lifetime to static
        let ctx: &'static InkwellContext = unsafe { std::mem::transmute(&self.ll_ctx) };

        // Create builder
        let builder = ctx.create_builder();

        // Get module reference
        let module = self.ll_mod.deref();

        // Create super function type and value, this function is called directly by xlang code
        let super_fn_ty = R::fn_type(ctx, &A::param_types(ctx));
        let super_fn_val = module.add_function(name, super_fn_ty, None);

        // Create basic block
        builder.position_at_end(ctx.append_basic_block(super_fn_val, ""));

        // Generate wrapper function type
        let super_param_types = A::param_types(ctx);
        let wrapper_param_types = [vec![ctx.i64_type().into()], super_param_types].concat();
        let wrapper_fn_ty =
            R::fn_type(ctx, &wrapper_param_types).ptr_type(inkwell::AddressSpace::Generic);

        // Convert wrapper_c_fn_addr to wrapper function pointer
        let wrapper_fn_ptr = builder.build_int_to_ptr(
            ctx.i64_type().const_int(wrapper_c_fn_addr as _, false),
            wrapper_fn_ty,
            "",
        );

        // Genrate wrapper function params
        let super_params = super_fn_val
            .get_param_iter()
            .map(|v| v.into())
            .collect::<Vec<_>>();
        let params = [
            vec![ctx
                .i64_type()
                .const_int(boxed_closure_addr as _, false)
                .into()],
            super_params,
        ]
        .concat();

        // Convert pointer to callable value
        let rust_fn_val = inkwell::values::CallableValue::try_from(wrapper_fn_ptr)
            .map_err(|_| error::internal!("Failed to convert pointer to callable value"))?;

        // Call wrapper function
        let fn_ret = builder
            .build_call(rust_fn_val, &params, "")
            .try_as_basic_value()
            .left();

        // Return result
        builder.build_return(fn_ret.as_ref().map(|v| v as _));

        // Save closure
        self.closure_list.push(boxed_closure_any);

        Ok(())
    }

    pub fn load(&self, source: &str) -> Result<()> {
        log::trace!("Load source: '{}'", source);

        let lexer = Lexer::new("main", source)?;
        let node = Parser::new(lexer).parse()?;

        self.load_ast(node)?;

        Ok(())
    }

    pub fn load_ast(&self, node: StmtListNode) -> Result<()> {
        log::trace!("Load aste: {:?}", node);

        let module = self.ll_mod.deref();
        let ctx: &'static InkwellContext = unsafe { std::mem::transmute(&self.ll_ctx) };

        // Create global variable table
        let mut global_vtb = self.init_vartable.clone();

        // Compile
        node.compile(ctx, module, &mut global_vtb)?;
        log::trace!("Raw llvm ir: \n{}", module.print_to_string().to_string());

        // Verify llvm ir
        #[cfg(debug_assertions)]
        module
            .verify()
            .map_err(|e| error::internal!("LLVM verifty error: {}", e))?;

        // Create llvm pass manager
        let fpm = PassManager::create(module);
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        // fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.finalize();

        // Optimize llvm ir
        for func in module.get_functions() {
            fpm.run_on(&func);
        }
        log::trace!(
            "Optimized llvm ir: \n{}",
            module.print_to_string().to_string()
        );

        Ok(())
    }

    pub unsafe fn get_function<'ctx, F>(&'ctx self, fn_name: &str) -> Result<JitFunction<'ctx, F>>
    where
        F: UnsafeFunctionPointer,
    {
        let engine = self.ll_ee.deref();
        let f = engine
            .get_function::<F>(fn_name)
            .map_err(|e| error::internal!("Get function error: {}", e))?;

        Ok(f)
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.ll_ee);
            ManuallyDrop::drop(&mut self.ll_mod);
        }
    }
}
