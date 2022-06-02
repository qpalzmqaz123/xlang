mod assign;
mod binary;
mod block;
mod block_stmt;
mod boolean;
mod branch;
mod expr;
mod float;
mod fn_args;
mod fn_call;
mod fn_stmt;
mod id_type;
mod ident;
mod integer;
mod let_assign;
mod lit;
mod ret;
mod stmt;
mod stmt_list;
mod ty;
mod unary;

use inkwell::{
    types::{
        AnyTypeEnum, BasicMetadataTypeEnum, BasicType, FloatType, FunctionType, IntType,
        PointerType, VoidType,
    },
    values::{BasicMetadataValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue},
};

pub use self::{
    assign::AssignNode, binary::BinaryNode, block::BlockNode, block_stmt::BlockStmtNode,
    boolean::BooleanNode, branch::BranchNode, expr::ExprNode, float::FloatNode,
    fn_args::FnArgsNode, fn_call::FnCallNode, fn_stmt::FnStmtNode, id_type::IdTypeNode,
    ident::IdentNode, integer::IntegerNode, let_assign::LetAssignNode, lit::LitNode,
    ret::ReturnNode, stmt::StmtNode, stmt_list::StmtListNode, ty::TypeNode, unary::UnaryNode,
};

#[derive(Debug, Clone, PartialEq)]
pub enum LLVMValue<'ctx> {
    Bool(IntValue<'ctx>),
    I64(IntValue<'ctx>),
    F64(FloatValue<'ctx>),
    Pointer(LLVMPointerValue<'ctx>),
}

impl<'ctx> LLVMValue<'ctx> {
    pub fn from_basic_value_enum(v: BasicValueEnum<'ctx>, ty: &LLVMType) -> Self {
        match v {
            BasicValueEnum::IntValue(v) => match ty {
                LLVMType::I64(_) => LLVMValue::I64(v),
                _ => unreachable!(),
            },
            BasicValueEnum::FloatValue(v) => match ty {
                LLVMType::F64(_) => LLVMValue::F64(v),
                _ => unreachable!(),
            },
            BasicValueEnum::PointerValue(v) => match ty {
                LLVMType::Pointer(pty) => {
                    LLVMValue::Pointer(LLVMPointerValue::from_pointer_value(v, pty))
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn get_type(&self) -> LLVMType<'ctx> {
        match self {
            LLVMValue::Bool(v) => LLVMType::Bool(v.get_type()),
            LLVMValue::I64(v) => LLVMType::I64(v.get_type()),
            LLVMValue::F64(v) => LLVMType::F64(v.get_type()),
            LLVMValue::Pointer(v) => LLVMType::Pointer(v.get_type()),
        }
    }
}

impl<'ctx> Into<BasicMetadataValueEnum<'ctx>> for LLVMValue<'ctx> {
    fn into(self) -> BasicMetadataValueEnum<'ctx> {
        use LLVMValue::*;

        match self {
            Bool(v) => BasicMetadataValueEnum::IntValue(v),
            I64(v) => BasicMetadataValueEnum::IntValue(v),
            F64(v) => BasicMetadataValueEnum::FloatValue(v),
            Pointer(v) => v.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LLVMPointerValue<'ctx> {
    Void(PointerValue<'ctx>),
    Bool(PointerValue<'ctx>),
    I64(PointerValue<'ctx>),
    F64(PointerValue<'ctx>),
    Pointer(Box<LLVMPointerValue<'ctx>>),
}

impl<'ctx> LLVMPointerValue<'ctx> {
    pub fn from_pointer_value(v: PointerValue<'ctx>, ty: &LLVMPointerType) -> Self {
        match ty {
            LLVMPointerType::Void(_) => LLVMPointerValue::Void(v),
            LLVMPointerType::Bool(_) => LLVMPointerValue::Bool(v),
            LLVMPointerType::I64(_) => LLVMPointerValue::I64(v),
            LLVMPointerType::F64(_) => LLVMPointerValue::F64(v),
            LLVMPointerType::Pointer(pty) => {
                LLVMPointerValue::Pointer(Box::new(Self::from_pointer_value(v, pty)))
            }
        }
    }

    pub fn get_type(&self) -> LLVMPointerType<'ctx> {
        match self {
            LLVMPointerValue::Void(v) => LLVMPointerType::Bool(v.get_type()),
            LLVMPointerValue::Bool(v) => LLVMPointerType::Bool(v.get_type()),
            LLVMPointerValue::I64(v) => LLVMPointerType::I64(v.get_type()),
            LLVMPointerValue::F64(v) => LLVMPointerType::F64(v.get_type()),
            LLVMPointerValue::Pointer(v) => LLVMPointerType::Pointer(Box::new(v.get_type())),
        }
    }

    pub fn get_pointer_value(&self) -> PointerValue<'ctx> {
        use LLVMPointerValue::*;

        match self {
            Void(v) | Bool(v) | I64(v) | F64(v) => v.clone(),
            Pointer(v) => v.get_pointer_value(),
        }
    }
}

impl<'ctx> Into<BasicMetadataValueEnum<'ctx>> for LLVMPointerValue<'ctx> {
    fn into(self) -> BasicMetadataValueEnum<'ctx> {
        use LLVMPointerValue::*;

        match self {
            Void(v) | Bool(v) | I64(v) | F64(v) => BasicMetadataValueEnum::PointerValue(v),
            Pointer(v) => (*v).into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LLVMType<'ctx> {
    Void(VoidType<'ctx>),
    Bool(IntType<'ctx>),
    I64(IntType<'ctx>),
    F64(FloatType<'ctx>),
    Pointer(LLVMPointerType<'ctx>),
}

impl<'ctx> LLVMType<'ctx> {
    pub fn fn_type(&self, param_types: &[LLVMType<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        use LLVMType::*;

        let llvm_param_types = param_types
            .iter()
            .map(|v| v.clone().into())
            .collect::<Vec<_>>();

        match self {
            Void(v) => v.fn_type(&llvm_param_types, is_var_args),
            Bool(v) | I64(v) => v.fn_type(&llvm_param_types, is_var_args),
            F64(v) => v.fn_type(&llvm_param_types, is_var_args),
            Pointer(v) => v.fn_type(&llvm_param_types, is_var_args),
        }
    }

    pub fn as_ref_type(&self, ptr: PointerType<'ctx>) -> LLVMPointerType {
        use LLVMType::*;

        match self {
            Void(_) => LLVMPointerType::Void(ptr),
            Bool(_) => LLVMPointerType::Bool(ptr),
            I64(_) => LLVMPointerType::I64(ptr),
            F64(_) => LLVMPointerType::F64(ptr),
            Pointer(ty) => LLVMPointerType::Pointer(Box::new(ty.clone())),
        }
    }
}

impl<'ctx> Into<BasicMetadataTypeEnum<'ctx>> for LLVMType<'ctx> {
    fn into(self) -> BasicMetadataTypeEnum<'ctx> {
        use LLVMType::*;

        match self {
            Void(_) => unreachable!(),
            Bool(v) | I64(v) => v.into(),
            F64(v) => v.into(),
            Pointer(p) => p.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LLVMPointerType<'ctx> {
    Void(PointerType<'ctx>),
    Bool(PointerType<'ctx>),
    I64(PointerType<'ctx>),
    F64(PointerType<'ctx>),
    Pointer(Box<LLVMPointerType<'ctx>>),
}

impl<'ctx> LLVMPointerType<'ctx> {
    pub fn fn_type(
        &self,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        use LLVMPointerType::*;

        match self {
            Void(v) | Bool(v) | I64(v) | F64(v) => v.fn_type(param_types, is_var_args),
            Pointer(v) => v.fn_type(param_types, is_var_args),
        }
    }

    pub fn get_pointer_type(&self) -> PointerType<'ctx> {
        use LLVMPointerType::*;

        match self {
            Void(v) | Bool(v) | I64(v) | F64(v) => v.clone(),
            Pointer(v) => v.get_pointer_type(),
        }
    }

    pub fn get_element_type(&self) -> LLVMType<'ctx> {
        use LLVMPointerType::*;

        let any_ty = self.get_pointer_type().get_element_type();
        match self {
            Void(_) => match any_ty {
                AnyTypeEnum::VoidType(v) => LLVMType::Void(v),
                _ => unreachable!(),
            },
            Bool(_) => match any_ty {
                AnyTypeEnum::IntType(v) => LLVMType::Bool(v),
                _ => unreachable!(),
            },
            I64(_) => match any_ty {
                AnyTypeEnum::IntType(v) => LLVMType::I64(v),
                _ => unreachable!(),
            },
            F64(_) => match any_ty {
                AnyTypeEnum::FloatType(v) => LLVMType::F64(v),
                _ => unreachable!(),
            },
            Pointer(v) => LLVMType::Pointer(v.get_pointer_element_type()),
        }
    }

    pub fn get_pointer_element_type(&self) -> LLVMPointerType<'ctx> {
        use LLVMPointerType::*;

        let any_ty = self.get_pointer_type().get_element_type();
        match self {
            Void(_) => match any_ty {
                AnyTypeEnum::PointerType(v) => LLVMPointerType::Void(v),
                _ => unreachable!(),
            },
            Bool(_) => match any_ty {
                AnyTypeEnum::PointerType(v) => LLVMPointerType::Bool(v),
                _ => unreachable!(),
            },
            I64(_) => match any_ty {
                AnyTypeEnum::PointerType(v) => LLVMPointerType::I64(v),
                _ => unreachable!(),
            },
            F64(_) => match any_ty {
                AnyTypeEnum::PointerType(v) => LLVMPointerType::F64(v),
                _ => unreachable!(),
            },
            Pointer(ty) => LLVMPointerType::Pointer(Box::new(ty.get_pointer_element_type())),
        }
    }
}

impl<'ctx> Into<BasicMetadataTypeEnum<'ctx>> for LLVMPointerType<'ctx> {
    fn into(self) -> BasicMetadataTypeEnum<'ctx> {
        use LLVMPointerType::*;

        match self {
            Void(v) | Bool(v) | I64(v) | F64(v) => v.into(),
            Pointer(v) => (*v).into(),
        }
    }
}
