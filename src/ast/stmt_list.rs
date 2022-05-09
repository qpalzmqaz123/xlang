use inkwell::{context::Context, module::Module};

use crate::{context::VarTable, error::Result};

use super::StmtNode;

#[derive(Debug, Clone)]
pub struct StmtListNode {
    pub list: Vec<StmtNode>,
}

impl<'ctx, 'md> StmtListNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<()> {
        for node in &self.list {
            node.compile(ctx, module, vtb)?;
        }

        Ok(())
    }
}
