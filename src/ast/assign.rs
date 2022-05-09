use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    context::VarTable,
    error::{self, Result},
};

use super::{ExprNode, IdentNode, LLVMValue};

#[derive(Debug, Clone)]
pub struct AssignNode {
    pub ident: IdentNode,
    pub expr: ExprNode,
}

impl<'ctx, 'md, 'bd> AssignNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<()> {
        let left = vtb
            .get(&self.ident.ident)
            .ok_or(error::semanteme!(
                self.ident.position.module,
                self.ident.position.line,
                self.ident.position.col,
                "Variable not found",
            ))?
            .clone();
        let (left_ptr, left_ty) = match left {
            LLVMValue::Pointer(p) => (p.get_pointer_value(), p.get_type().get_element_type()),
            _ => {
                return Err(error::semanteme!(
                    self.ident.position.module,
                    self.ident.position.line,
                    self.ident.position.col,
                    "Left side of assignment must be a pointer",
                ))
            }
        };

        let right = self.expr.compile(ctx, module, builder, vtb, &left_ty)?;

        if left_ty != right.get_type() {
            return Err(error::semanteme!(
                self.ident.position.module,
                self.ident.position.line,
                self.ident.position.col,
                "Type mismatch in assignment",
            ));
        }

        match right {
            LLVMValue::Bool(v) | LLVMValue::I64(v) => {
                builder.build_store(left_ptr, v);
            }
            LLVMValue::F64(v) => {
                builder.build_store(left_ptr, v);
            }
            LLVMValue::Pointer(v) => {
                builder.build_store(left_ptr, v.get_pointer_value());
            }
        }

        Ok(())
    }
}
