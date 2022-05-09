use inkwell::context::Context;

use crate::{error::Result, lexer::Position};

use super::LLVMValue;

#[derive(Debug, Clone)]
pub struct FloatNode {
    pub value: f64,
    pub position: Position,
}

impl<'ctx, 'bd> FloatNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<LLVMValue<'ctx>> {
        Ok(LLVMValue::F64(ctx.f64_type().const_float(self.value)))
    }
}
