mod scopes;

use crate::Context;
use crate::ast::{self, IntTy, Item, ItemKind, UintTy};
use crate::ir::{
    self, BasicBlockIdx, FunctionIdx, Module,
    ty::{AdtKind, FieldDef, VariantDef},
};
use scopes::ScopeTable;
use std::collections::HashMap;

impl From<scopes::VariableKind> for ir::Storage {
    fn from(value: scopes::VariableKind) -> Self {
        match value {
            scopes::VariableKind::Global(idx) => Self::Global(idx),
            scopes::VariableKind::Local(idx) => Self::Local(idx),
        }
    }
}

#[derive(Debug)]
pub struct Lowering<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    scopes: ScopeTable<'ir>,
    module: Module<'ir>,
    fn_idx: FunctionIdx,
    block_idx: BasicBlockIdx,
    globals: HashMap<ir::GlobalIdx, &'ir ir::Ty<'ir>>,
}

impl<'a, 'ir> Lowering<'a, 'ir> {
    pub fn new(ctx: &'a mut Context<'ir>) -> Self {
        Self {
            ctx,
            scopes: ScopeTable::new(),
            module: Module::new(),
            fn_idx: 0,
            block_idx: 0,
            globals: HashMap::new(),
        }
    }

    pub fn lower(mut self, ast: Vec<Item>) -> Module<'ir> {
        self.prefill_scopes_table(&ast);

        for item in ast {
            self.lower_item(item);
        }

        let globals = std::mem::take(&mut self.globals);
        let mut globals = globals.into_iter().collect::<Vec<_>>();
        globals.sort_by_key(|(idx, _)| *idx);

        for (expected, real) in (0..globals.len()).zip(globals.iter().map(|(idx, _)| idx)) {
            assert_eq!(expected, *real, "global indices are not consecutive");
        }

        self.module.globals = globals.into_iter().map(|(_, ty)| ty).collect();
        self.module
    }

    fn prefill_scopes_table(&mut self, ast: &[Item]) {
        let mut precalc_global_idx: ir::GlobalIdx = 0;

        for item in ast {
            match &item.kind {
                ItemKind::Global(variable) => {
                    let ty = self.lower_ty(variable.ty.clone());

                    self.scopes
                        .insert_global(variable.name.clone(), ty, precalc_global_idx);
                    precalc_global_idx += 1;
                }
                ItemKind::Fn {
                    ret_ty,
                    name,
                    params,
                    ..
                } => {
                    let params = params
                        .iter()
                        .map(|(_, ty)| self.lower_ty(ty.clone()))
                        .collect();
                    let ret_ty = self.lower_ty(ret_ty.clone());

                    self.scopes.insert_fn(name.clone(), params, ret_ty);
                }
                ItemKind::Struct { name, fields } => {
                    let variant = VariantDef {
                        name: name.clone(),
                        fields: fields
                            .iter()
                            .map(|(name, ty)| FieldDef {
                                name: name.clone(),
                                ty: self.lower_ty(ty.clone()),
                            })
                            .collect(),
                    };
                    let adt_idx = self.ctx.mk_adt(name.clone(), AdtKind::Struct);
                    let adt = self.ctx.get_adt_mut(adt_idx);

                    adt.variants.push(variant);
                    self.scopes
                        .insert_ty(name.clone(), self.ctx.allocator.alloc(ir::Ty::Adt(adt_idx)))
                }
            }
        }
    }

    fn lower_item(&mut self, item: Item) {
        match item.kind {
            ItemKind::Global(variable) => {
                let variable = self.scopes.get_variable(&variable.name).unwrap();
                let idx = if let scopes::VariableKind::Global(idx) = variable.kind {
                    idx
                } else {
                    unreachable!();
                };

                self.globals.insert(idx, variable.ty);
            }
            ItemKind::Fn {
                ret_ty,
                name,
                params,
                block,
            } => {
                self.scopes.enter_new(item.id);

                let arg_count = params.len();
                let locals: Vec<_> = params
                    .iter()
                    .map(|(_, ty)| self.lower_ty(ty.clone()))
                    .collect();

                for (i, (ty, (name, _))) in locals.iter().zip(params.iter()).enumerate() {
                    self.scopes.insert_local(name.clone(), *ty, i);
                }

                let ret_ty = self.lower_ty(ret_ty);
                let idx = self.module.functions.len();

                self.module.functions.push(ir::Function {
                    name,
                    basic_blocks: Vec::new(),
                    locals,
                    arg_count,
                    ret_ty,
                });
                self.fn_idx = idx;
                self.block_idx = self.get_fn().create_block();

                if let Some(block) = block {
                    self.lower_block(block);
                }

                self.scopes.leave();
            }
            ItemKind::Struct { .. } => (),
        }
    }

    fn lower_block(&mut self, block: ast::Block) {
        for stmt in block.stmts {
            self.lower_stmt(stmt);
        }
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) {
        match stmt.kind {
            ast::StmtKind::Local(variable) => {
                let ty = self.lower_ty(variable.ty);
                let idx = self.get_fn().create_local(ty);

                self.scopes.insert_local(variable.name, ty, idx);

                if let Some(expr) = variable.value {
                    let rvalue = self.lower_expr(expr);
                    self.get_basic_block()
                        .statements
                        .push(ir::Statement::Assign(
                            ir::Place {
                                storage: ir::Storage::Local(idx),
                                projection: Vec::new(),
                            },
                            rvalue,
                        ));
                }
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> ir::Rvalue {
        match expr.kind {
            ast::ExprKind::Ident(ident) => {
                let variable = self
                    .scopes
                    .get_variable(&ident)
                    .expect(&format!("ident `{ident}` not found"));

                ir::Rvalue::Use(ir::Operand::Place(ir::Place {
                    storage: variable.kind.clone().into(),
                    projection: Vec::new(),
                }))
            }
            _ => unreachable!(),
        }
    }

    fn lower_ty(&mut self, ty: ast::Ty) -> &'ir ir::Ty<'ir> {
        match ty {
            ast::Ty::Null => self.ctx.types.null,
            ast::Ty::Void => self.ctx.types.void,
            ast::Ty::Bool => self.ctx.types.bool,
            ast::Ty::Int(ty) => match ty {
                ast::IntTy::I8 => self.ctx.types.i8,
                ast::IntTy::I16 => self.ctx.types.i16,
                ast::IntTy::I32 => self.ctx.types.i32,
                ast::IntTy::I64 => self.ctx.types.i64,
                ast::IntTy::Isize => self.ctx.types.isize,
            },
            ast::Ty::UInt(ty) => match ty {
                ast::UintTy::U8 => self.ctx.types.u8,
                ast::UintTy::U16 => self.ctx.types.u16,
                ast::UintTy::U32 => self.ctx.types.u32,
                ast::UintTy::U64 => self.ctx.types.u64,
                ast::UintTy::Usize => self.ctx.types.usize,
            },
            ast::Ty::Ptr(ty) => self
                .ctx
                .allocator
                .alloc(ir::Ty::Ptr(self.lower_ty(*ty.clone()))),
            ast::Ty::Array { ty, len } => self.ctx.allocator.alloc(ir::Ty::Array(ir::TyArray {
                len,
                ty: self.lower_ty(*ty.clone()),
            })),
            ast::Ty::Fn(params, ret_ty) => {
                let mut alloced_params = Vec::new();

                for ty in params {
                    alloced_params.push(self.lower_ty(ty.clone()));
                }

                let params = &*self.ctx.allocator.alloc_slice_copy(&alloced_params);

                self.ctx
                    .allocator
                    .alloc(ir::Ty::Fn(params, self.lower_ty(*ret_ty.clone())))
            }
            ast::Ty::Ident(ident) => self.scopes.get_variable(&ident).unwrap().ty,
            ast::Ty::Infer => {
                todo!();
                //self.ctx
                //    .allocator
                //    .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var()));
            }
        }
    }

    fn get_fn(&mut self) -> &mut ir::Function<'ir> {
        &mut self.module.functions[self.fn_idx]
    }

    fn get_basic_block(&mut self) -> &mut ir::BasicBlock {
        self.module.functions[self.fn_idx].get_block_mut(self.block_idx)
    }
}
