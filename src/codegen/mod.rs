mod function;

use crate::{
    Context,
    ast::{IntTy, UintTy},
    ir,
};
use function::compile_function;
use std::collections::HashMap;
use tja::{hir, targets::amd64::emit_binary};

struct Codegen<'a, 'ir> {
    ctx: &'a Context<'ir>,
    tja_ctx: hir::Context,
    module: &'a ir::Module<'ir>,
    module_idx: hir::ModuleIdx,
    functions: HashMap<ir::FunctionIdx, hir::FunctionIdx>,
    adt: HashMap<ir::AdtIdx, hir::ty::TyIdx>,
}

impl<'a, 'ir> Codegen<'a, 'ir> {
    fn new(ctx: &'a Context<'ir>, module: &'a ir::Module<'ir>) -> Self {
        let mut tja_ctx = hir::Context::new();
        let module_idx = tja_ctx.create_module("main".into());

        Self {
            ctx,
            tja_ctx,
            module,
            module_idx,
            functions: HashMap::new(),
            adt: HashMap::new(),
        }
    }

    fn lower_ty(&mut self, ty: &'ir ir::Ty<'ir>) -> hir::ty::TyIdx {
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
                        ir::AdtKind::Struct => {
                            let variant = &adt.variants[0];
                            let fields = variant
                                .fields
                                .iter()
                                .map(|def| self.lower_ty(def.ty))
                                .collect();
                            let ty_idx =
                                self.tja_ctx.ty_storage.add_ty(hir::ty::Ty::Struct(fields));

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

    fn get_module(&mut self) -> hir::Wrapper<&mut hir::Module> {
        self.tja_ctx.get_module(self.module_idx)
    }
}

pub fn compile<'ir>(ctx: &Context<'ir>, module: &ir::Module<'ir>) {
    let mut codegen = Codegen::new(ctx, module);

    for global in &module.globals {
        let ty = codegen.lower_ty(global.ty);

        codegen.get_module().globals.push(hir::Global {
            name: global.name.clone(),
            ty,
            value: None,
        });
    }

    for idx in 0..module.functions.len() {
        compile_function(&mut codegen, idx);
    }

    let target = tja::targets::amd64::Target::new();
    let mut hir_pass_manager = tja::hir::pass::ModulePassManager::new();
    let mut mir_pass_manager = tja::mir::pass::ModulePassManager::new();
    let mut ctx = tja::pass::Context::new(&codegen.tja_ctx.ty_storage, &target);

    target.add_hir_passes(&mut hir_pass_manager);
    target.add_mir_passes(&mut mir_pass_manager);

    for module in &mut codegen.tja_ctx.modules {
        hir_pass_manager.run(module, &mut ctx);

        let mut module = ctx.mir_module.take().unwrap();

        mir_pass_manager.run(&mut module, &mut ctx);
        tja::targets::amd64::emit_binary::emit_binary(
            &mut module,
            &target,
            emit_binary::Options {
                assembly_only: true,
                object_only: false,
                shared: false,
            },
        )
        .expect("failed to compile")
    }
}
