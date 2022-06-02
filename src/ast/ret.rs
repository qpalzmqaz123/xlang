use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    context::VarTable,
    error::{self, Result},
    lexer::Position,
};

use super::{ExprNode, LLVMType, LLVMValue};

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub ret: Option<ExprNode>,
    pub position: Position,
}

impl<'ctx, 'md, 'bd> ReturnNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ret_ty: &LLVMType<'ctx>,
    ) -> Result<()> {
        use LLVMValue::*;

        if let Some(ret) = &self.ret {
            let ret_val = ret.compile(ctx, module, builder, vtb, ret_ty)?;
            if ret_ty != &ret_val.get_type() {
                return Err(error::semanteme!(
                    self.position.module,
                    self.position.line,
                    self.position.col,
                    "Type mismatch with function return type"
                ));
            }

            let _ = match ret_val {
                Bool(v) | I64(v) => {
                    builder.build_return(Some(&v));
                }
                F64(v) => {
                    builder.build_return(Some(&v));
                }
                Pointer(v) => {
                    builder.build_return(Some(&v.get_pointer_value()));
                }
            };
        } else {
            builder.build_return(None);
        };

        Ok(())
    }
}
