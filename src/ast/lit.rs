use inkwell::{builder::Builder, context::Context};

use crate::error::Result;

use super::{BooleanNode, FloatNode, IntegerNode, LLVMValue};

#[derive(Debug, Clone)]
pub enum LitNode {
    Integer(IntegerNode),
    Float(FloatNode),
    Bool(BooleanNode),
}

impl<'ctx, 'bd> LitNode {
    pub fn compile(&self, ctx: &'ctx Context, _: &'bd Builder<'ctx>) -> Result<LLVMValue<'ctx>> {
        let val = match self {
            Self::Integer(node) => node.compile(ctx)?,
            Self::Float(node) => node.compile(ctx)?,
            Self::Bool(node) => node.compile(ctx)?,
        };

        Ok(val)
    }
}
