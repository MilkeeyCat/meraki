mod scopes;
mod ty_collector;

use crate::Context;
use crate::ast::node_id::NodeId;
use crate::ast::{self, Item, ItemKind};
use crate::ir::{
    self, BasicBlockIdx, FunctionIdx, Module,
    ty::{AdtKind, FieldDef, VariantDef},
};
use scopes::ScopeTable;
use std::collections::{HashMap, HashSet};
use ty_collector::collect_nodes_types;

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
    functions: HashMap<NodeId, ir::FunctionIdx>,
    types: HashMap<NodeId, ir::AdtIdx>,
    referenced_types: HashMap<ir::AdtIdx, HashSet<ir::AdtIdx>>,
    nodes_types: HashMap<NodeId, &'ir ir::Ty<'ir>>,
}

impl<'a, 'ir> Lowering<'a, 'ir> {
    pub fn new(ctx: &'a mut Context<'ir>) -> Self {
        Self {
            ctx,
            scopes: ScopeTable::new(),
            module: Module::new(),
            fn_idx: 0,
            block_idx: 0,
            functions: HashMap::new(),
            types: HashMap::new(),
            referenced_types: HashMap::new(),
            nodes_types: HashMap::new(),
        }
    }

    pub fn lower(mut self, ast: Vec<Item>) -> Module<'ir> {
        self.prefill_scopes_table(&ast);

        for item in ast {
            self.lower_item(item);
        }

        self.module
    }

    fn populate_types(&mut self, ast: &[Item]) {
        let pred = |item: &&Item| matches!(item.kind, ItemKind::Struct { .. });

        for item in ast.iter().filter(pred) {
            match &item.kind {
                ItemKind::Struct { name, .. } => {
                    let adt_idx = self.ctx.mk_adt(name.clone(), AdtKind::Struct);

                    self.scopes
                        .insert_ty(name.clone(), self.ctx.allocator.alloc(ir::Ty::Adt(adt_idx)));
                    self.types.insert(item.id, adt_idx);
                }
                ItemKind::Global(_) | ItemKind::Fn { .. } => unreachable!(),
            }
        }

        for item in ast.iter().filter(pred) {
            match &item.kind {
                ItemKind::Struct { name, fields } => {
                    let variant = VariantDef {
                        name: name.clone(),
                        fields: fields
                            .into_iter()
                            .map(|(name, ty)| {
                                let ty = self.lower_ty(ty.clone());

                                if let ir::Ty::Adt(idx) = ty {
                                    assert!(
                                        self.referenced_types
                                            .get(idx)
                                            .map(|set| set.contains(&self.types[&item.id]))
                                            .is_none(),
                                        "Recursive types, field `{name}`"
                                    );

                                    self.referenced_types
                                        .entry(self.types[&item.id])
                                        .or_default()
                                        .insert(*idx);
                                }

                                FieldDef {
                                    name: name.clone(),
                                    ty,
                                }
                            })
                            .collect(),
                    };
                    let adt_idx = self.types[&item.id];
                    let adt = self.ctx.get_adt_mut(adt_idx);

                    adt.variants.push(variant);
                }
                ItemKind::Global(_) | ItemKind::Fn { .. } => unreachable!(),
            }
        }
    }

    fn prefill_scopes_table(&mut self, ast: &[Item]) {
        self.populate_types(ast);
        let mut precalc_global_idx: ir::GlobalIdx = 0;

        for item in ast.iter() {
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
                    let params: Vec<_> = params
                        .iter()
                        .map(|(_, ty)| self.lower_ty(ty.clone()))
                        .collect();
                    let ret_ty = self.lower_ty(ret_ty.clone());
                    let idx = self.module.create_fn(name.clone(), &params, ret_ty);

                    self.scopes.insert_fn(name.clone(), params, ret_ty, idx);
                    self.functions.insert(item.id, idx);
                }
                ItemKind::Struct { .. } => (),
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

                self.module.add_global_with_idx(idx, variable.ty);
                //TODO: lower global's value
            }
            ItemKind::Fn {
                mut block, params, ..
            } => {
                self.fn_idx = self.functions[&item.id];
                self.block_idx = self.get_fn().create_block();
                self.scopes.enter_new(item.id);

                for (idx, (name, _)) in params.into_iter().enumerate() {
                    let ty = self.get_fn().locals[idx];

                    self.scopes.insert_local(name, ty, idx);
                }

                if let Some(block) = &mut block {
                    self.nodes_types = collect_nodes_types(self, block, item.id);
                }

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
                let ty = self.nodes_types[&stmt.id];
                let idx = self.get_fn().create_local(ty);
                *self.scopes.get_variable_mut(&variable.name).unwrap() = scopes::Variable {
                    kind: scopes::VariableKind::Local(idx),
                    ty,
                };

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
                ty: self.lower_ty(*ty),
            })),
            ast::Ty::Fn(params, ret_ty) => {
                let params: Vec<_> = params.into_iter().map(|ty| self.lower_ty(ty)).collect();
                let params = self.ctx.allocator.alloc_slice_copy(&params);

                self.ctx
                    .allocator
                    .alloc(ir::Ty::Fn(params, self.lower_ty(*ret_ty.clone())))
            }
            ast::Ty::Ident(ident) => self
                .scopes
                .get_ty(&ident)
                .expect(&format!("type `{ident}` not found")),
            ast::Ty::Infer => unreachable!(),
        }
    }

    fn get_fn(&mut self) -> &mut ir::Function<'ir> {
        &mut self.module.functions[self.fn_idx]
    }

    fn get_basic_block(&mut self) -> &mut ir::BasicBlock {
        self.module.functions[self.fn_idx].get_block_mut(self.block_idx)
    }
}
