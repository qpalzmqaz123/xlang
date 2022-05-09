use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    context::VarTable,
    error::{self, Result},
    lexer::{Operator, Position},
};

use super::{ExprNode, LLVMType, LLVMValue};

#[derive(Debug, Clone)]
pub struct UnaryNode {
    pub expr: Box<ExprNode>,
    pub op: Operator,
    pub op_position: Position,
}

impl<'ctx, 'md, 'bd> UnaryNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
        ty: &LLVMType<'ctx>,
    ) -> Result<LLVMValue<'ctx>> {
        use LLVMValue::*;
        use Operator::*;

        let val = self.expr.compile(ctx, module, builder, vtb, ty)?;

        let val = match self.op {
            Minus => match val {
                I64(v) => I64(builder.build_int_sub(ctx.i64_type().const_int(0, false), v, "")),
                F64(v) => F64(builder.build_float_sub(ctx.f64_type().const_float(0.0), v, "")),
                _ => {
                    return Err(error::semanteme!(
                        self.op_position.module,
                        self.op_position.line,
                        self.op_position.col,
                        "Only number types can be used in minus"
                    ))
                }
            },
            Not => match val {
                Bool(v) => Bool(builder.build_not(v, "")),
                _ => {
                    return Err(error::semanteme!(
                        self.op_position.module,
                        self.op_position.line,
                        self.op_position.col,
                        "Only bool type can be used in not"
                    ))
                }
            },
            Add | Sub | Mul | Div | Rem | Eq | Ne | Gt | Ge | Lt | Le | Shl | Shr | And | Or
            | BitAnd | BitOr => {
                return Err(error::semanteme!(
                    self.op_position.module,
                    self.op_position.line,
                    self.op_position.col,
                    "Binary operator is not supported for unary expression",
                ))
            }
        };

        Ok(val)
    }
}
