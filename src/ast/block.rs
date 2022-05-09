use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::{context::VarTable, error::Result};

use super::{BlockStmtNode, LLVMType};

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub list: Vec<BlockStmtNode>,
}

impl<'ctx, 'md, 'bd> BlockNode {
    /// Return has-return value
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        fn_val: FunctionValue<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ret_ty: &LLVMType<'ctx>,
    ) -> Result<bool> {
        for node in &self.list {
            node.compile(ctx, module, fn_val, builder, vtb, ret_ty)?;

            // Skip the rest of the statements if we're returning
            if let BlockStmtNode::Return(_) = node {
                return Ok(true);
            }
        }

        Ok(false)
    }
}
