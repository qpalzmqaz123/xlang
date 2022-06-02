use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::{context::VarTable, error::Result, FnCallNode};

use super::{AssignNode, BranchNode, LLVMType, LetAssignNode, ReturnNode};

#[derive(Debug, Clone)]
pub enum BlockStmtNode {
    LetAssign(LetAssignNode),
    Return(ReturnNode),
    Assign(AssignNode),
    Branch(BranchNode),
    FnCall(FnCallNode),
}

impl<'ctx, 'md, 'bd> BlockStmtNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        fn_val: FunctionValue<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ret_ty: &LLVMType<'ctx>,
    ) -> Result<()> {
        match self {
            Self::LetAssign(node) => node.compile(ctx, module, builder, vtb)?,
            Self::Return(node) => node.compile(ctx, module, builder, vtb, ret_ty)?,
            Self::Assign(node) => node.compile(ctx, module, builder, vtb)?,
            Self::Branch(node) => node.compile(ctx, module, fn_val, builder, vtb, ret_ty)?,
            Self::FnCall(node) => node
                .compile(ctx, module, builder, vtb, ret_ty)
                .map(|_| ())?,
        }

        Ok(())
    }
}
