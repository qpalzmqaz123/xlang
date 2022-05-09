use inkwell::context::Context;

use crate::error::Result;

use super::{IdTypeNode, LLVMType};

#[derive(Debug, Clone)]
pub struct FnArgsNode {
    pub args: Vec<IdTypeNode>,
}

impl<'ctx> FnArgsNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<Vec<(&str, LLVMType<'ctx>)>> {
        let mut list = vec![];
        for arg in &self.args {
            list.push(arg.compile(ctx)?);
        }

        Ok(list)
    }
}
