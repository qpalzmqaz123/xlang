use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{context::VarTable, error::Result};

use super::{BinaryNode, FnCallNode, IdentNode, LLVMValue, LitNode, UnaryNode};

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
    ) -> Result<Option<LLVMValue<'ctx>>> {
        let val = match self {
            Self::Lit(node) => Some(node.compile(ctx, builder)?),
            Self::Ident(node) => Some(node.compile(ctx, builder, vtb)?),
            Self::Binary(node) => Some(node.compile(ctx, module, builder, vtb)?),
            Self::FnCall(node) => node.compile(ctx, module, builder, vtb)?,
            Self::Unary(node) => Some(node.compile(ctx, module, builder, vtb)?),
        };

        Ok(val)
    }
}
