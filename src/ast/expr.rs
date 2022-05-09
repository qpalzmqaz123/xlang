use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{context::VarTable, error::Result};

use super::{BinaryNode, FnCallNode, IdentNode, LLVMType, LLVMValue, LitNode, UnaryNode};

#[derive(Debug, Clone)]
pub enum ExprNode {
    Lit(LitNode),
    Ident(IdentNode),
    Binary(BinaryNode),
    FnCall(FnCallNode),
    Unary(UnaryNode),
}

impl<'ctx, 'md, 'bd> ExprNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ty: &LLVMType<'ctx>,
    ) -> Result<LLVMValue<'ctx>> {
        let val = match self {
            Self::Lit(node) => node.compile(ctx, builder)?,
            Self::Ident(node) => node.compile(ctx, builder, vtb, ty)?,
            Self::Binary(node) => node.compile(ctx, module, builder, vtb, ty)?,
            Self::FnCall(node) => node.compile(ctx, module, builder, vtb, ty)?,
            Self::Unary(node) => node.compile(ctx, module, builder, vtb, ty)?,
        };

        Ok(val)
    }
}
