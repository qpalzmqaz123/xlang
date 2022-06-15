use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::{
    context::VarTable,
    error::{self, Result},
    lexer::Position,
};

use super::{BlockNode, ExprNode, LLVMType, LLVMValue};

#[derive(Debug, Clone)]
pub struct BranchNode {
    pub cond: ExprNode,
    pub then: BlockNode,
    pub otherwise: Option<BlockNode>,
    pub position: Position,
}

impl<'ctx, 'md, 'bd> BranchNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        fn_val: FunctionValue<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ret_ty: &LLVMType<'ctx>,
    ) -> Result<()> {
        let cond_res = self
            .cond
            .compile(ctx, module, builder, vtb)?
            .ok_or(error::semanteme!(
                self.position.module,
                self.position.line,
                self.position.col,
                "Condition of branch cannot be void",
            ))?;

        let then_bb = ctx.append_basic_block(fn_val, "then");
        let else_bb = ctx.append_basic_block(fn_val, "else");
        let after_bb = ctx.append_basic_block(fn_val, "after");

        match cond_res {
            LLVMValue::Bool(cond) => {
                builder.build_conditional_branch(cond, then_bb, else_bb);
            }
            _ => {
                return Err(error::semanteme!(
                    self.position.module,
                    self.position.line,
                    self.position.col,
                    "Expected a bool expression"
                ))
            }
        }

        // Process the then block
        builder.position_at_end(then_bb);
        let has_return = self
            .then
            .compile(ctx, module, fn_val, builder, vtb, ret_ty)?;
        if !has_return {
            builder.build_unconditional_branch(after_bb);
        }

        // Process the else block
        builder.position_at_end(else_bb);
        let mut has_return = false;
        if let Some(otherwise) = &self.otherwise {
            has_return = otherwise.compile(ctx, module, fn_val, builder, vtb, ret_ty)?;
        }
        if !has_return {
            builder.build_unconditional_branch(after_bb);
        }

        // Process the after block
        builder.position_at_end(after_bb);

        Ok(())
    }
}
