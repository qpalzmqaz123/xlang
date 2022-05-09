use inkwell::context::Context;

use crate::error::Result;

use super::{IdentNode, LLVMType, TypeNode};

#[derive(Debug, Clone)]
pub struct IdTypeNode {
    pub ident: IdentNode,
    pub ty: TypeNode,
}

impl<'ctx> IdTypeNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<(&str, LLVMType<'ctx>)> {
        let ident = self.ident.as_ref();
        let llvm_ty = self.ty.compile(ctx)?;

        Ok((ident, llvm_ty))
    }
}
