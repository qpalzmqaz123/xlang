use inkwell::{builder::Builder, context::Context, module::Module, FloatPredicate, IntPredicate};

use crate::{
    context::VarTable,
    error::{self, Result},
    lexer::{Operator, Position},
};

use super::{ExprNode, LLVMValue};

#[derive(Debug, Clone)]
pub struct BinaryNode {
    pub left: Box<ExprNode>,
    pub right: Box<ExprNode>,
    pub op: Operator,
    pub op_position: Position,
}

impl<'ctx, 'md, 'bd> BinaryNode {
    pub fn compile(
        &self,
        ctx: &'ctx Context,
        module: &'md Module<'ctx>,
        builder: &'bd Builder<'ctx>,
        vtb: &mut VarTable<'ctx>,
    ) -> Result<LLVMValue<'ctx>> {
        use Operator::*;

        let left = self
            .left
            .compile(ctx, module, builder, vtb)?
            .ok_or(error::semanteme!(
                self.op_position.module,
                self.op_position.line,
                self.op_position.col,
                "Left side of binary operator cannot be void",
            ))?;
        let right = self
            .right
            .compile(ctx, module, builder, vtb)?
            .ok_or(error::semanteme!(
                self.op_position.module,
                self.op_position.line,
                self.op_position.col,
                "Right side of binary operator cannot be void",
            ))?;
        let res = match left {
            LLVMValue::Bool(left) => match right {
                LLVMValue::Bool(right) => {
                    #[rustfmt::skip]
                    let val = match self.op {
                        Eq => LLVMValue::Bool(builder.build_int_compare(IntPredicate::EQ, left, right, "")),
                        Ne => LLVMValue::Bool(builder.build_int_compare(IntPredicate::NE, left, right, "")),
                        Gt => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SGT, left, right, "")),
                        Ge => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SGE, left, right, "")),
                        Lt => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SLT, left, right, "")),
                        Le => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SLE, left, right, "")),
                        And | BitAnd => LLVMValue::Bool(builder.build_and(left, right, "")),
                        Or | BitOr => LLVMValue::Bool(builder.build_or(left, right, "")),
                        Add | Sub | Mul | Div | Rem | Shl | Shr | Minus | Not => {
                            return Err(error::semanteme!(
                                self.op_position.module,
                                self.op_position.line,
                                self.op_position.col,
                                "Invalid operator for bool binop"
                            ))
                        }
                    };

                    val
                }
                _ => {
                    return Err(error::semanteme!(
                        self.op_position.module,
                        self.op_position.line,
                        self.op_position.col,
                        "Binop operands must be of the same type"
                    ))
                }
            },
            LLVMValue::I64(left) => match right {
                LLVMValue::I64(right) => {
                    #[rustfmt::skip]
                    let val = match self.op {
                        Add => LLVMValue::I64(builder.build_int_add(left, right, "")),
                        Sub => LLVMValue::I64(builder.build_int_sub(left, right, "")),
                        Mul => LLVMValue::I64(builder.build_int_mul(left, right, "")),
                        Div => LLVMValue::I64(builder.build_int_signed_div(left, right, "")),
                        Rem => LLVMValue::I64(builder.build_int_signed_rem(left, right, "")),
                        Shl => LLVMValue::I64(builder.build_left_shift(left, right, "")),
                        Shr => LLVMValue::I64(builder.build_right_shift(left, right, true, "")),
                        BitAnd => LLVMValue::I64(builder.build_and(left, right, "")),
                        BitOr => LLVMValue::I64(builder.build_or(left, right, "")),
                        Eq => LLVMValue::Bool(builder.build_int_compare(IntPredicate::EQ, left, right, "")),
                        Ne => LLVMValue::Bool(builder.build_int_compare(IntPredicate::NE, left, right, "")),
                        Gt => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SGT, left, right, "")),
                        Ge => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SGE, left, right, "")),
                        Lt => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SLT, left, right, "")),
                        Le => LLVMValue::Bool(builder.build_int_compare(IntPredicate::SLE, left, right, "")),
                        Not | Minus | And | Or => {
                            return Err(error::semanteme!(
                                self.op_position.module,
                                self.op_position.line,
                                self.op_position.col,
                                "Invalid operator for i64 binop"
                            ))
                        }
                    };

                    val
                }
                _ => {
                    return Err(error::semanteme!(
                        self.op_position.module,
                        self.op_position.line,
                        self.op_position.col,
                        "Binop operands must be of the same type"
                    ))
                }
            },
            LLVMValue::F64(left) => match right {
                LLVMValue::F64(right) => {
                    #[rustfmt::skip]
                    let val = match self.op {
                        Add => LLVMValue::F64(builder.build_float_add(left, right, "")),
                        Sub => LLVMValue::F64(builder.build_float_sub(left, right, "")),
                        Mul => LLVMValue::F64(builder.build_float_mul(left, right, "")),
                        Div => LLVMValue::F64(builder.build_float_div(left, right, "")),
                        Rem => LLVMValue::F64(builder.build_float_rem(left, right, "")),
                        Eq => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::OEQ, left, right, "")),
                        Ne => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::ONE, left, right, "")),
                        Gt => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::OGT, left, right, "")),
                        Ge => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::OGE, left, right, "")),
                        Lt => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::OLT, left, right, "")),
                        Le => LLVMValue::Bool(builder.build_float_compare(FloatPredicate::OLE, left, right, "")),
                        Not | Minus | Shl | Shr | And | Or | BitAnd | BitOr => {
                            return Err(error::semanteme!(
                                self.op_position.module,
                                self.op_position.line,
                                self.op_position.col,
                                "Invalid operator for f64 binop"
                            ))
                        }
                    };

                    val
                }
                _ => {
                    return Err(error::semanteme!(
                        self.op_position.module,
                        self.op_position.line,
                        self.op_position.col,
                        "Binop operands must be of the same type"
                    ))
                }
            },
            _ => {
                return Err(error::semanteme!(
                    self.op_position.module,
                    self.op_position.line,
                    self.op_position.col,
                    "Unsupport binop value type"
                ))
            }
        };

        Ok(res)
    }
}
