use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{ast::LLVMType, context::VarTable, error::Result};

use super::{AssignNode, LLVMPointerValue, LLVMValue, TypeNode};

#[derive(Debug, Clone)]
pub struct LetAssignNode {
    pub ty: TypeNode,
    pub assign: AssignNode,
}

impl<'ctx, 'md, 'bd> LetAssignNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<()> {
        let name = self.assign.ident.as_ref();
        let ty = self.ty.compile(ctx)?;

        // Alloc ptr
        let ptr = match &ty {
            LLVMType::Bool(ty) | LLVMType::I64(ty) => builder.build_alloca(ty.clone(), name),
            LLVMType::F64(ty) => builder.build_alloca(ty.clone(), name),
            LLVMType::Pointer(ty) => builder.build_alloca(ty.get_pointer_type(), name),
        };

        // Save ptr to var_map
        let ptr_ty = ty.as_ref_type(ptr.get_type());
        let ptr_val = LLVMPointerValue::from_pointer_value(ptr, &ptr_ty);
        vtb.insert(name.to_string(), LLVMValue::Pointer(ptr_val));

        // Compile assign
        self.assign.compile(ctx, module, builder, vtb)?;

        Ok(())
    }
}
