use inkwell::{context::Context, module::Module};

use crate::{
    context::VarTable,
    error::{self, Result},
};

use super::{BlockNode, FnArgsNode, IdentNode, LLVMPointerValue, LLVMType, LLVMValue, TypeNode};

#[derive(Debug, Clone)]
pub struct FnStmtNode {
    pub name: IdentNode,
    pub args: FnArgsNode,
    pub ret_ty: TypeNode,
    pub body: BlockNode,
}

impl<'ctx, 'md> FnStmtNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<()> {
        // Prepare args
        let args = self.args.compile(ctx)?;
        let (arg_names, arg_types) =
            args.into_iter()
                .fold((vec![], vec![]), |(mut names, mut types), (name, ty)| {
                    names.push(name);
                    types.push(ty);
                    (names, types)
                });

        // Verify arg types
        for ty in &arg_types {
            if let LLVMType::Void(_) = ty {
                return Err(error::semanteme!(
                    self.name.position.module,
                    self.name.position.line,
                    self.name.position.col,
                    "Function arguments cannot be void"
                ));
            }
        }

        // Prepare return type
        let ret_ty = self.ret_ty.compile(ctx)?;

        // Generate function type
        let fn_ty = ret_ty.fn_type(&arg_types, false);

        // Prepare function name
        let fn_name = self.name.as_ref();

        // Add function
        let fn_val = module.add_function(fn_name, fn_ty, None);

        // Add bb1
        let bb1 = ctx.append_basic_block(fn_val, "bb1");

        // Create function body builder
        let builder = ctx.create_builder();
        builder.position_at_end(bb1);

        // Set arg names & create arg map
        let mut vtb = VarTable::with_super(vtb);
        for (arg_index, arg_ty) in arg_types.iter().enumerate() {
            let arg_name = arg_names.get(arg_index).ok_or(error::internal!(
                "Cannot get function param name of index {}",
                arg_index
            ))?;
            let arg_val = fn_val
                .get_nth_param(arg_index as _)
                .ok_or(error::internal!(
                    "Cannot get function param of index {}",
                    arg_index
                ))?;

            // Alloc arg ptr
            let ptr = match &arg_ty {
                LLVMType::Void(_) => {
                    return Err(error::semanteme!(
                        self.name.position.module,
                        self.name.position.line,
                        self.name.position.col,
                        "Cannot use void type as function param"
                    ))
                }
                LLVMType::Bool(ty) | LLVMType::I64(ty) => {
                    builder.build_alloca(ty.clone(), arg_name)
                }
                LLVMType::F64(ty) => builder.build_alloca(ty.clone(), arg_name),
                LLVMType::Pointer(ty) => builder.build_alloca(ty.get_pointer_type(), arg_name),
            };

            // Store arg value to ptr
            builder.build_store(ptr, arg_val);

            // Insert value to var_map
            let ptr_ty = arg_ty.as_ref_type(ptr.get_type());
            let ptr_val = LLVMPointerValue::from_pointer_value(ptr, &ptr_ty);
            vtb.insert(arg_name.to_string(), LLVMValue::Pointer(ptr_val));
        }

        // Compile body
        let has_ret = self
            .body
            .compile(ctx, module, fn_val, &builder, &mut vtb, &ret_ty)?;
        if !has_ret {
            return Err(error::semanteme!(
                self.name.position.module,
                self.name.position.line,
                self.name.position.col,
                "Function must return a value"
            ));
        }

        Ok(())
    }
}
