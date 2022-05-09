use inkwell::{builder::Builder, context::Context};

use crate::{
    context::VarTable,
    error::{self, Result},
    lexer::Position,
};

use super::{LLVMType, LLVMValue};

#[derive(Debug, Clone)]
pub struct IdentNode {
    pub ident: String,
    pub position: Position,
}

impl ToString for IdentNode {
    fn to_string(&self) -> String {
        self.ident.clone()
    }
}

impl AsRef<str> for IdentNode {
    fn as_ref(&self) -> &str {
        &self.ident
    }
}

impl<'ctx, 'bd> IdentNode {
    pub fn compile(
        &self,
        _: &'ctx Context,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        _: &LLVMType<'ctx>,
    ) -> Result<LLVMValue<'ctx>> {
        use LLVMValue::*;

        let name = self.as_ref();
        let val = vtb.get(name).ok_or(error::semanteme!(
            self.position.module,
            self.position.line,
            self.position.col,
            "Undefined variable",
        ))?;

        let ptr_val = match val {
            Pointer(v) => v,
            _ => {
                return Err(error::semanteme!(
                    self.position.module,
                    self.position.line,
                    self.position.col,
                    "Variable is not a pointer",
                ))
            }
        };

        let elem_ty = ptr_val.get_type().get_element_type();
        let val = builder.build_load(ptr_val.get_pointer_value(), "");

        Ok(LLVMValue::from_basic_value_enum(val, &elem_ty))
    }
}
