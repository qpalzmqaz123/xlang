use inkwell::{context::Context, module::Module};

use crate::{context::VarTable, error::Result};

use super::FnStmtNode;

#[derive(Debug, Clone)]
pub enum StmtNode {
    Fn(FnStmtNode),
}

impl<'ctx, 'md> StmtNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<()> {
        match self {
            Self::Fn(node) => node.compile(ctx, module, vtb)?,
        }

        Ok(())
    }
}
