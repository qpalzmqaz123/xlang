use inkwell::context::Context;

use crate::{error::Result, lexer::Position};

use super::LLVMValue;

#[derive(Debug, Clone)]
pub struct BooleanNode {
    pub value: bool,
    pub position: Position,
}

impl<'ctx, 'bd> BooleanNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<LLVMValue<'ctx>> {
        Ok(LLVMValue::Bool(
            ctx.bool_type().const_int(self.value as _, false),
        ))
    }
}
