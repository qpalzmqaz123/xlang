use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    context::VarTable,
    error::{self, Result},
};

use super::{ExprNode, IdentNode, LLVMValue};

#[derive(Debug, Clone)]
pub struct FnCallNode {
    pub fn_name: IdentNode,
    pub args: Vec<ExprNode>,
}

impl<'ctx, 'md, 'bd> FnCallNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<Option<LLVMValue<'ctx>>> {
        let fn_name = self.fn_name.as_ref();
        let fn_val = module.get_function(fn_name).ok_or(error::semanteme!(
            self.fn_name.position.module,
            self.fn_name.position.line,
            self.fn_name.position.col,
            "Undefined function",
        ))?;

        // Verify argument count
        if fn_val.count_params() != self.args.len() as u32 {
            return Err(error::semanteme!(
                self.fn_name.position.module,
                self.fn_name.position.line,
                self.fn_name.position.col,
                "Function call argument count mismatch",
            ));
        }

        let mut args = vec![];
        for (index, arg) in self.args.iter().enumerate() {
            // Compile argument
            let val = arg
                .compile(ctx, module, builder, vtb)?
                .ok_or(error::semanteme!(
                    self.fn_name.position.module,
                    self.fn_name.position.line,
                    self.fn_name.position.col,
                    "Argument of function call cannot be void",
                ))?;

            // Verify argument type
            let arg_val = fn_val.get_nth_param(index as u32).ok_or(error::semanteme!(
                self.fn_name.position.module,
                self.fn_name.position.line,
                self.fn_name.position.col,
                "Function call argument count mismatch, unreachable",
            ))?;

            if LLVMValue::from_basic_value_enum(arg_val).get_type() != val.get_type() {
                return Err(error::semanteme!(
                    self.fn_name.position.module,
                    self.fn_name.position.line,
                    self.fn_name.position.col,
                    "Function call argument type mismatch",
                ));
            }

            args.push(val.into());
        }

        let call_res = builder
            .build_call(fn_val, &args, "")
            .try_as_basic_value()
            .left()
            .map(|v| LLVMValue::from_basic_value_enum(v));

        Ok(call_res)
    }
}
