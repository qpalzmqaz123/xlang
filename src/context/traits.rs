use inkwell::{
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, FunctionType},
};

use crate::{LLVMPointerValue, LLVMValue};

macro_rules! impl_llvm_ret_single {
    ($ty:ty, $func:ident) => {
        impl<'ctx> LLVMRetSingle<'ctx> for $ty {
            fn fn_type(
                ctx: &'ctx Context,
                param_types: &[BasicMetadataTypeEnum<'ctx>],
            ) -> FunctionType<'ctx> {
                ctx.$func().fn_type(param_types, false)
            }
        }
    };
}

macro_rules! impl_llvm_param_single {
    ($ty:ty, $func:ident) => {
        impl<'ctx> LLVMParamSingle<'ctx> for $ty {
            fn to_basic_metadata_type_enum(ctx: &'ctx Context) -> BasicMetadataTypeEnum<'ctx> {
                ctx.$func().into()
            }
        }
    };
}

macro_rules! impl_llvm_param_multi_tuple {
    () => {
        impl<'ctx> LLVMParamMulti<'ctx> for () {
            fn param_types(_: &'ctx Context) -> Vec<BasicMetadataTypeEnum<'ctx>> {
                vec![]
            }
        }
    };
    ($ty:ident) => {
        impl<'ctx, $ty: LLVMParamSingle<'ctx>> LLVMParamMulti<'ctx> for $ty {
            fn param_types(ctx: &'ctx Context) -> Vec<BasicMetadataTypeEnum<'ctx>> {
                vec![
                    $ty::to_basic_metadata_type_enum(ctx)
                ]
            }
        }
    };
    ($($ty:ident),*) => {
        impl<'ctx, $($ty: LLVMParamSingle<'ctx>),*> LLVMParamMulti<'ctx> for ($($ty),*) {
            fn param_types(ctx: &'ctx Context) -> Vec<BasicMetadataTypeEnum<'ctx>> {
                vec![
                    $($ty::to_basic_metadata_type_enum(ctx)),*
                ]
            }
        }
    };
}

pub trait LLVMRetSingle<'ctx> {
    fn fn_type(
        ctx: &'ctx Context,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
    ) -> FunctionType<'ctx>;
}

impl<'ctx> LLVMRetSingle<'ctx> for () {
    fn fn_type(
        ctx: &'ctx Context,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        ctx.void_type().fn_type(param_types, false)
    }
}

impl_llvm_ret_single!(bool, bool_type);
impl_llvm_ret_single!(i64, i64_type);
impl_llvm_ret_single!(f64, f64_type);

pub trait LLVMParamSingle<'ctx>: LLVMRetSingle<'ctx> {
    fn to_basic_metadata_type_enum(ctx: &'ctx Context) -> BasicMetadataTypeEnum<'ctx>;
}

impl_llvm_param_single!(bool, bool_type);
impl_llvm_param_single!(i64, i64_type);
impl_llvm_param_single!(f64, f64_type);

pub trait LLVMParamMulti<'ctx> {
    fn param_types(ctx: &'ctx Context) -> Vec<BasicMetadataTypeEnum<'ctx>>;
}

impl_llvm_param_multi_tuple!();
impl_llvm_param_multi_tuple!(A);
impl_llvm_param_multi_tuple!(A, B);
impl_llvm_param_multi_tuple!(A, B, C);
impl_llvm_param_multi_tuple!(A, B, C, D);
impl_llvm_param_multi_tuple!(A, B, C, D, E);
impl_llvm_param_multi_tuple!(A, B, C, D, E, F);
impl_llvm_param_multi_tuple!(A, B, C, D, E, F, G);
impl_llvm_param_multi_tuple!(A, B, C, D, E, F, G, H);
impl_llvm_param_multi_tuple!(A, B, C, D, E, F, G, H, I);
impl_llvm_param_multi_tuple!(A, B, C, D, E, F, G, H, I, J);

pub trait AddLLVMGlobal<'ctx> {
    fn add_module_global(name: &str, ctx: &'ctx Context, module: &Module<'ctx>) -> LLVMValue<'ctx>;
}

impl<'ctx> AddLLVMGlobal<'ctx> for i64 {
    fn add_module_global(name: &str, ctx: &'ctx Context, module: &Module<'ctx>) -> LLVMValue<'ctx> {
        let ptr = module
            .add_global(ctx.i64_type(), None, name)
            .as_pointer_value();
        LLVMValue::Pointer(LLVMPointerValue::I64(ptr))
    }
}
