use crate::{
    ast::{IntTy, UintTy},
    codegen::function::compile_function,
    ir::{self, ItemFn, ItemKind, Package, SymbolId, Variable},
};
use std::collections::HashMap;
use tja::{FunctionIdx, GlobalIdx};

pub struct ModuleCtx<'a, 'ir> {
    pub ctx: &'a crate::Context<'ir>,
    pub tja_ctx: tja::hir::Context,
    pub module_idx: tja::hir::ModuleIdx,
    pub globals: HashMap<SymbolId, GlobalIdx>,
    pub functions: HashMap<SymbolId, FunctionIdx>,
    pub adt: HashMap<ir::ty::AdtIdx, tja::ty::TyIdx>,
}

impl<'a, 'ir> ModuleCtx<'a, 'ir> {
    fn new(ctx: &'a crate::Context<'ir>) -> Self {
        let mut tja_ctx = tja::hir::Context::new();
        let module_idx = tja_ctx.create_module("main".into());

        Self {
            ctx,
            tja_ctx,
            module_idx,
            globals: HashMap::new(),
            functions: HashMap::new(),
            adt: HashMap::new(),
        }
    }

    pub fn lower_ty(&mut self, ty: &'ir ir::Ty<'ir>) -> tja::ty::TyIdx {
        match ty {
            ir::Ty::Void => self.tja_ctx.ty_storage.void_ty,
            ir::Ty::Bool => self.tja_ctx.ty_storage.i8_ty,
            ir::Ty::Int(ty) => match ty {
                IntTy::I8 => self.tja_ctx.ty_storage.i8_ty,
                IntTy::I16 => self.tja_ctx.ty_storage.i16_ty,
                IntTy::I32 => self.tja_ctx.ty_storage.i32_ty,
                IntTy::I64 => self.tja_ctx.ty_storage.i64_ty,
                IntTy::Isize => self.tja_ctx.ty_storage.i64_ty,
            },
            ir::Ty::UInt(ty) => match ty {
                UintTy::U8 => self.tja_ctx.ty_storage.i8_ty,
                UintTy::U16 => self.tja_ctx.ty_storage.i16_ty,
                UintTy::U32 => self.tja_ctx.ty_storage.i32_ty,
                UintTy::U64 => self.tja_ctx.ty_storage.i64_ty,
                UintTy::Usize => self.tja_ctx.ty_storage.i64_ty,
            },
            ir::Ty::Ptr(_) => self.tja_ctx.ty_storage.ptr_ty,
            ir::Ty::Fn(_, _) => self.tja_ctx.ty_storage.ptr_ty,
            ir::Ty::Adt(idx) => match self.adt.get(&idx) {
                Some(ty) => *ty,
                None => {
                    let adt = self.ctx.get_adt(*idx);

                    match adt.kind {
                        ir::ty::AdtKind::Struct => {
                            let variant = &adt.variants[0];
                            let fields = variant
                                .fields
                                .iter()
                                .map(|def| self.lower_ty(def.ty))
                                .collect();
                            let ty_idx =
                                self.tja_ctx.ty_storage.add_ty(tja::ty::Ty::Struct(fields));

                            self.adt.insert(*idx, ty_idx);

                            ty_idx
                        }
                        _ => unimplemented!(),
                    }
                }
            },
            ir::Ty::Infer(_) => unreachable!(),
            _ => todo!(),
        }
    }

    pub fn get_module(&mut self) -> tja::hir::Wrapper<'_, &mut tja::hir::Module> {
        self.tja_ctx.get_module(self.module_idx)
    }
}

pub fn compile<'ir>(ctx: &crate::Context<'ir>, package: &Package<'ir>) {
    let mut mod_ctx = ModuleCtx::new(ctx);

    for item in &package.items {
        match &item.kind {
            ItemKind::Fn(ItemFn {
                name,
                params,
                ret_ty,
                block: _,
            }) => {
                let params = params.iter().map(|(_, ty)| mod_ctx.lower_ty(ty)).collect();
                let ret_ty = mod_ctx.lower_ty(ret_ty);
                let idx = mod_ctx
                    .get_module()
                    .create_fn(name.to_string(), params, ret_ty);

                mod_ctx.functions.insert(item.id.into(), idx);
            }
            ItemKind::Global(Variable { name, ty: _, value }) => {
                assert!(
                    value.is_none(),
                    "codegen can't handle globals with value yet :c"
                );

                let ty = package.symbols[&item.id.into()].ty();
                let ty = mod_ctx.lower_ty(ty);
                let idx = mod_ctx
                    .get_module()
                    .create_global(name.to_string(), ty, None);

                mod_ctx.globals.insert(item.id.into(), idx);
            }
        }
    }

    for item in &package.items {
        match &item.kind {
            ItemKind::Fn(func) => {
                compile_function(&mut mod_ctx, item.id.into(), func, package);
            }
            ItemKind::Global(_) => (),
        }
    }

    let mut ctx = mod_ctx.tja_ctx;
    let target = tja::targets::amd64::Target::new();
    let mut hir_pass_manager = tja::hir::pass::ModulePassManager::new();
    let mut mir_pass_manager = tja::mir::pass::ModulePassManager::new();
    let mut pass_ctx = tja::pass::Context::new(&ctx.ty_storage, &target);

    target.add_hir_passes(&mut hir_pass_manager);
    target.add_mir_passes(&mut mir_pass_manager);

    for module in &mut ctx.modules {
        hir_pass_manager.run(module, &mut pass_ctx);

        let mut module = pass_ctx.mir_module.take().unwrap();

        mir_pass_manager.run(&mut module, &mut pass_ctx);
        tja::targets::amd64::emit_binary::emit_binary(
            &mut module,
            &target,
            &ctx.ty_storage,
            tja::targets::amd64::emit_binary::Options {
                assembly_only: true,
                object_only: false,
                shared: false,
            },
        )
        .expect("failed to compile")
    }
}
