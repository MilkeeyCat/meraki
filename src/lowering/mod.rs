mod symbol_table;

use crate::{
    Context, ast,
    ir::{
        self, Id, Package, Symbol,
        ty::{AdtKind, FieldDef, VariantDef},
    },
};
use symbol_table::SymbolTable;

struct Lowering<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    ir: Package<'ir>,
    symbol_table: SymbolTable<'ir>,
    id: usize,
}

impl<'a, 'ir> Lowering<'a, 'ir> {
    fn new(ctx: &'a mut Context<'ir>) -> Self {
        Self {
            ctx,
            ir: Package::new(),
            symbol_table: SymbolTable::new(),
            id: 0,
        }
    }

    fn next_id(&mut self) -> Id {
        let id = Id(self.id);

        self.id += 1;

        id
    }

    fn collect_symbols(&mut self, items: &[ast::Item]) {
        for item in items {
            match item {
                ast::Item::Global(variable) => {
                    let id = self.next_id();

                    assert!(
                        self.symbol_table.insert_symbol(variable.name.clone(), id),
                        "Symbol {} is defined multiple times",
                        &variable.name
                    );
                }
                ast::Item::Fn { name, .. } => {
                    let id = self.next_id();

                    assert!(
                        self.symbol_table.insert_symbol(name.clone(), id),
                        "Symbol {name} is defined multiple times"
                    );
                }
                ast::Item::Struct { name, .. } => {
                    let adt_idx = self.ctx.mk_adt(name.clone(), AdtKind::Struct);

                    self.symbol_table
                        .insert_ty(name.clone(), self.ctx.allocator.alloc(ir::Ty::Adt(adt_idx)));
                }
            }
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> ir::Expr<'ir> {
        match &expr.kind {
            ast::ExprKind::Binary { op, left, right } => ir::Expr {
                id: self.next_id(),
                kind: Box::new(ir::ExprKind::Binary(
                    op.clone(),
                    self.lower_expr(left),
                    self.lower_expr(right),
                )),
            },
            ast::ExprKind::Lit(lit) => {
                let lit = match lit {
                    ast::ExprLit::Int(value) => ir::ExprLit::Int(*value),
                    ast::ExprLit::UInt(value) => ir::ExprLit::UInt(*value),
                    ast::ExprLit::Bool(value) => ir::ExprLit::Bool(*value),
                    ast::ExprLit::String(value) => ir::ExprLit::String(value.to_string()),
                    ast::ExprLit::Null => ir::ExprLit::Null,
                };

                ir::Expr {
                    id: self.next_id(),
                    kind: Box::new(ir::ExprKind::Lit(lit)),
                }
            }
            ast::ExprKind::Ident(ident) => ir::Expr {
                id: self.next_id(),
                kind: Box::new(ir::ExprKind::Ident(
                    self.symbol_table.find_symbol(ident).unwrap().into(),
                )),
            },
            ast::ExprKind::Struct { name, fields } => ir::Expr {
                id: self.next_id(),
                kind: Box::new(ir::ExprKind::Struct(
                    self.lower_ty(&ast::Ty::Ident(name.to_string())),
                    fields
                        .iter()
                        .map(|(name, expr)| (name.to_string(), self.lower_expr(expr)))
                        .collect(),
                )),
            },
            ast::ExprKind::Field { expr, field } => ir::Expr {
                id: self.next_id(),
                kind: Box::new(ir::ExprKind::Field(
                    self.lower_expr(expr),
                    field.to_string(),
                )),
            },
            ast::ExprKind::FunctionCall { expr, arguments } => ir::Expr {
                id: self.next_id(),
                kind: Box::new(ir::ExprKind::Call(
                    self.lower_expr(expr),
                    arguments.iter().map(|expr| self.lower_expr(expr)).collect(),
                )),
            },
            _ => unimplemented!(),
        }
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> ir::Stmt<'ir> {
        match stmt {
            ast::Stmt::Local(ast::Variable { ty, name, value }) => {
                let id = self.next_id();
                let ty = self.lower_ty(ty);

                self.ir.add_symbol(id.into(), Symbol::Variable(ty));
                self.symbol_table.insert_symbol(name.to_string(), id);

                ir::Stmt {
                    id,
                    kind: ir::StmtKind::Local(ir::Variable {
                        name: name.to_string(),
                        ty,
                        value: value.as_ref().map(|value| self.lower_expr(value)),
                    }),
                }
            }
            ast::Stmt::Expr(expr) => ir::Stmt {
                id: self.next_id(),
                kind: ir::StmtKind::Expr(self.lower_expr(expr)),
            },
            ast::Stmt::Return(expr) => ir::Stmt {
                id: self.next_id(),
                kind: ir::StmtKind::Return(expr.as_ref().map(|expr| self.lower_expr(expr))),
            },
            _ => unimplemented!(),
        }
    }

    fn lower_item(&mut self, item: &ast::Item) -> Option<ir::Item<'ir>> {
        match item {
            ast::Item::Global(ast::Variable { ty, name, value }) => {
                let id = self.symbol_table.find_symbol(name).unwrap();
                let ty = self.lower_ty(ty);

                self.ir.add_symbol(id.into(), Symbol::Variable(ty));
                Some(ir::Item {
                    id,
                    kind: ir::ItemKind::Global(ir::Variable {
                        name: name.to_string(),
                        ty,
                        value: value.as_ref().map(|value| self.lower_expr(value)),
                    }),
                })
            }
            ast::Item::Fn {
                ret_ty,
                name,
                params,
                block,
            } => {
                self.symbol_table.enter();

                let id = self.symbol_table.find_symbol(name).unwrap();
                let params: Vec<_> = params
                    .iter()
                    .map(|(name, ty)| {
                        let id = self.next_id();
                        let ty = self.lower_ty(ty);

                        assert!(self.symbol_table.insert_symbol(name.to_string(), id));
                        self.ir.add_symbol(id.into(), Symbol::Variable(ty));

                        (id, ty)
                    })
                    .collect();
                let ret_ty = self.lower_ty(ret_ty);

                let block = block.as_ref().map(|block| self.lower_block(block));

                self.symbol_table.leave();
                self.ir.add_symbol(
                    id.into(),
                    Symbol::Fn {
                        ty: self.ctx.allocator.alloc(ir::Ty::Fn(
                            self.ctx
                                .allocator
                                .alloc_slice_fill_iter(params.iter().map(|(_, ty)| *ty)),
                            ret_ty,
                        )),
                        params: params.iter().map(|(id, _)| (*id).into()).collect(),
                    },
                );

                Some(ir::Item {
                    id,
                    kind: ir::ItemKind::Fn(ir::ItemFn {
                        name: name.to_string(),
                        params,
                        ret_ty,
                        block,
                    }),
                })
            }
            ast::Item::Struct { name, fields } => {
                //FIXME: add recursive type check https://github.com/MilkeeyCat/meraki/blob/d4a073c81b531bfe50ef3901eeabecea3ef1cf35/src/lowering/mod.rs#L88-L100
                let variant = VariantDef {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, ty)| {
                            let ty = self.lower_ty(ty);

                            FieldDef {
                                name: name.clone(),
                                ty,
                            }
                        })
                        .collect(),
                };
                let adt_idx = self.symbol_table.find_ty(name).unwrap().adt_idx();
                let adt = self.ctx.get_adt_mut(adt_idx);

                adt.variants.push(variant);

                None
            }
        }
    }

    fn lower_block(&mut self, block: &ast::Block) -> ir::Block<'ir> {
        ir::Block(
            block
                .stmts
                .iter()
                .map(|stmt| self.lower_stmt(stmt))
                .collect(),
        )
    }

    fn lower_ty(&mut self, ty: &ast::Ty) -> &'ir ir::Ty<'ir> {
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
            ast::Ty::Ptr(ty) => self.ctx.allocator.alloc(ir::Ty::Ptr(self.lower_ty(ty))),
            ast::Ty::Array { ty, len } => self.ctx.allocator.alloc(ir::Ty::Array(ir::TyArray {
                len: *len,
                ty: self.lower_ty(ty),
            })),
            ast::Ty::Fn(params, ret_ty) => {
                let params: Vec<_> = params.into_iter().map(|ty| self.lower_ty(ty)).collect();
                let params = self.ctx.allocator.alloc_slice_copy(&params);

                self.ctx
                    .allocator
                    .alloc(ir::Ty::Fn(params, self.lower_ty(ret_ty)))
            }
            ast::Ty::Ident(ident) => self
                .symbol_table
                .find_ty(&ident)
                .expect(&format!("Type `{ident}` not found")),
            ast::Ty::Infer => &ir::Ty::Infer(None),
        }
    }
}

pub fn lower<'ir>(ctx: &mut Context<'ir>, items: Vec<ast::Item>) -> Package<'ir> {
    let mut lowering = Lowering::new(ctx);

    lowering.symbol_table.enter();
    lowering.collect_symbols(&items);

    for item in items {
        if let Some(item) = lowering.lower_item(&item) {
            lowering.ir.items.push(item);
        }
    }

    lowering.ir
}
