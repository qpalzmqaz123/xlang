use inkwell::context::Context;

use crate::{error::Result, lexer::Position};

use super::LLVMValue;

#[derive(Debug, Clone)]
pub struct IntegerNode {
    pub value: i64,
    pub position: Position,
}

impl<'ctx, 'bd> IntegerNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<LLVMValue<'ctx>> {
        Ok(LLVMValue::I64(
            ctx.i64_type().const_int(self.value as _, false),
        ))
    }
}
