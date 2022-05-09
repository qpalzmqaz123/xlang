mod traits;
mod var_table;

use std::{mem::ManuallyDrop, ops::Deref};

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

use self::traits::{AddLLVMGlobal, LLVMMulti, LLVMSingle};

pub struct Context {
    ll_ctx: InkwellContext,
    ll_mod: ManuallyDrop<InkwellModule<'static>>,
    ll_ee: ManuallyDrop<ExecutionEngine<'static>>,
    init_vartable: VarTable<'static>,
}

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
        A: LLVMMulti<'static>,
        R: LLVMSingle<'static>,
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
