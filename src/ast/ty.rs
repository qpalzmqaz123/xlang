use inkwell::context::Context;

use crate::error::{self, Result};

use super::{IdentNode, LLVMType};

#[derive(Debug, Clone)]
pub enum TypeNode {
    Ident(IdentNode),
}

impl<'ctx> TypeNode {
    pub fn compile(&self, ctx: &'ctx Context) -> Result<LLVMType<'ctx>> {
        let ty = match self {
            TypeNode::Ident(ident) => {
                let ty = ident.as_ref();
                match ty {
                    "bool" => LLVMType::Bool(ctx.bool_type()),
                    "i64" => LLVMType::I64(ctx.i64_type()),
                    "f64" => LLVMType::F64(ctx.f64_type()),
                    _ => {
                        return Err(error::semanteme!(
                            ident.position.module,
                            ident.position.line,
                            ident.position.col,
                            "Unknown type: `{}`",
                            ty,
                        ))
                    }
                }
            }
        };

        Ok(ty)
    }
}
