mod scopes;

use crate::{
    ir::{self, Id, OrderedMap, Stmt},
    parser::{self, BinOp, Item, UnOp, Variable},
    ty_problem, Context,
};
use scopes::Scopes;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Lowering<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    types: HashMap<parser::Ty, &'ir ir::Ty<'ir>>,
    scopes: Scopes<'ir>,
    nodes: Vec<ir::Node<'ir>>,
    globals: Vec<ir::Global<'ir>>,
    nodes_map: HashMap<Id, ir::Node<'ir>>,
    id: Id,
    ret_ty: Option<&'ir ir::Ty<'ir>>,
}

impl<'a, 'ir> Lowering<'a, 'ir> {
    pub fn new(ctx: &'a mut Context<'ir>) -> Self {
        Self {
            ctx,
            types: HashMap::new(),
            scopes: Scopes::new(),
            nodes: Vec::new(),
            globals: Vec::new(),
            nodes_map: HashMap::new(),
            id: Id::default(),
            ret_ty: None,
        }
    }

    pub fn lower(mut self, ast: Vec<Item>) {
        self.scopes.enter();

        ast.into_iter().for_each(|item| {
            self.lower_item(item);
        });

        let globals = self.ctx.allocator.alloc_slice_copy(&self.globals);
        self.ctx.ir.set_globals(globals);
    }

    pub fn lower_item(&mut self, item: Item) -> Option<ir::Item<'ir>> {
        match item {
            Item::Struct(item) => {
                let mut fields = Vec::new();
                let ty = self.ctx.allocator.alloc(ir::Ty::Struct(self.id));

                self.types.insert(parser::Ty::Ident(item.name), ty);

                for (field, ty) in item.fields {
                    fields.push((&*self.ctx.allocator.alloc_str(&field), self.lower_ty(ty)));
                }

                let fields = self.ctx.allocator.alloc_slice_copy(&fields);

                self.globals.push(ir::Global(
                    self.ctx
                        .allocator
                        .alloc([ir::Node::Item(ir::Item::Struct(fields))]),
                ));
                self.id.global_id += 1;

                None
            }
            Item::Fn(item) => {
                self.id.node_id = 1;
                self.scopes.enter();

                let ret_ty = self.lower_ty(item.ret_ty);
                self.ret_ty = Some(ret_ty);

                let stmts = if let Some(block) = item.block {
                    block
                        .0
                        .into_iter()
                        .map(|stmt| self.lower_stmt(stmt))
                        .collect()
                } else {
                    Vec::new()
                };

                self.ret_ty = None;
                self.scopes.leave();

                let params: Vec<&'ir ir::Ty<'ir>> = item
                    .params
                    .into_iter()
                    .map(|(_, ty)| self.lower_ty(ty))
                    .collect();
                let signature = ir::Signature {
                    params: self.ctx.allocator.alloc_slice_copy(&params),
                    ret_ty,
                };

                self.nodes.insert(
                    0,
                    ir::Node::Item(ir::Item::Fn(self.ctx.allocator.alloc(ir::ItemFn {
                        id: Id {
                            global_id: self.id.global_id,
                            node_id: 0,
                        },
                        name: self.ctx.allocator.alloc_str(&item.name),
                        signature,
                        block: ir::Block(self.ctx.allocator.alloc_slice_copy(&stmts)),
                    }))),
                );

                self.globals
                    .push(ir::Global(self.ctx.allocator.alloc_slice_copy(&self.nodes)));
                self.nodes.clear();
                self.id.global_id += 1;
                self.id.node_id = 0;

                None
            }
            Item::Global(var) => {
                let name = var.name.clone();
                let ir_var = self.lower_var_decl(var);
                let node = ir::Node::Item(ir::Item::Global(ir_var));

                self.scopes.insert_symbol(name, self.id);
                self.nodes_map.insert(self.id, node);

                self.globals
                    .push(ir::Global(self.ctx.allocator.alloc_slice_copy(&[node])));
                self.id.global_id += 1;

                Some(ir::Item::Global(ir_var))
            }
        }
    }

    fn lower_var_decl(&mut self, variable: Variable) -> &'ir ir::Variable<'ir> {
        let ty = self.lower_ty(variable.ty);

        let initializer = if let Some(expr) = variable.value {
            let expr = self.lower_expr(expr);
            let let_ty_var_id = self.tys_ty_var_id(ty);
            let expr_ty_var_id = self.tys_ty_var_id(expr.ty);

            self.ctx.ty_problem.eq(let_ty_var_id, expr_ty_var_id);

            Some(expr)
        } else {
            None
        };

        let ir_variable = self.ctx.allocator.alloc(ir::Variable {
            id: self.id,
            name: self.ctx.allocator.alloc_str(&variable.name),
            ty,
            initializer,
        });

        ir_variable
    }

    fn lower_stmt(&mut self, stmt: parser::Stmt) -> ir::Stmt<'ir> {
        match stmt {
            parser::Stmt::Local(var) => {
                let name = var.name.clone();
                let ir_var = self.lower_var_decl(var);
                let node = ir::Node::Stmt(ir::Stmt::Local(ir_var));

                self.scopes.insert_symbol(name, self.id);
                self.nodes_map.insert(self.id, node);

                self.nodes.push(node);
                self.id.node_id += 1;

                ir::Stmt::Local(ir_var)
            }
            parser::Stmt::Item(item) => ir::Stmt::Item(self.lower_item(item).unwrap()),
            parser::Stmt::Expr(expr) => ir::Stmt::Expr(self.lower_expr(expr)),
            parser::Stmt::Return(stmt) => {
                let expr = stmt.expr.map(|expr| {
                    let expr = self.lower_expr(expr);
                    let expr_ty_var_id = self.tys_ty_var_id(expr.ty);
                    let ret_ty_var = self.tys_ty_var_id(self.ret_ty.unwrap());

                    self.ctx.ty_problem.eq(expr_ty_var_id, ret_ty_var);

                    expr
                });

                ir::Stmt::Return(expr)
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: parser::Expr) -> ir::Expr<'ir> {
        match expr {
            parser::Expr::Binary(parser::ExprBinary {
                op,
                ref left,
                ref right,
            }) => {
                // TODO: remove clones
                let lhs = self.lower_expr(*left.clone());
                let rhs = self.lower_expr(*right.clone());

                let lhs_ty_var_id = self.tys_ty_var_id(lhs.ty);
                let rhs_ty_var_id = self.tys_ty_var_id(rhs.ty);

                let ty = match op {
                    BinOp::Add => {
                        let ty = self.expr_ty(&expr);
                        let expr = self.tys_ty_var_id(ty);

                        self.ctx
                            .ty_problem
                            .bin_add(expr, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    BinOp::Sub => {
                        let ty = self.expr_ty(&expr);
                        let expr = self.tys_ty_var_id(ty);

                        self.ctx
                            .ty_problem
                            .bin_sub(expr, lhs_ty_var_id, rhs_ty_var_id);

                        ty
                    }
                    _ => {
                        self.ctx.ty_problem.eq(lhs_ty_var_id, rhs_ty_var_id);

                        lhs.ty
                    }
                };

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Binary(
                        op,
                        self.ctx.allocator.alloc(lhs),
                        self.ctx.allocator.alloc(rhs),
                    ),
                }
            }
            parser::Expr::Ident(parser::ExprIdent(ref ident)) => {
                let id = self.scopes.get_symbol(ident).unwrap();
                let ty = self.expr_ty(&expr);

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Ident(id),
                }
            }
            parser::Expr::Lit(ref lit) => {
                let ty = self.expr_ty(&expr);
                let kind = match lit {
                    parser::ExprLit::Int(lit) => ir::ExprKind::Lit(ir::ExprLit::Int(*lit)),
                    parser::ExprLit::UInt(lit) => ir::ExprKind::Lit(ir::ExprLit::UInt(*lit)),
                    parser::ExprLit::Bool(lit) => ir::ExprKind::Lit(ir::ExprLit::Bool(*lit)),
                    parser::ExprLit::String(lit) => {
                        ir::ExprKind::Lit(ir::ExprLit::String(self.ctx.allocator.alloc_str(&lit)))
                    }
                    parser::ExprLit::Null => ir::ExprKind::Lit(ir::ExprLit::Null),
                };

                ir::Expr { ty, kind }
            }
            parser::Expr::Unary(parser::ExprUnary { op, expr }) => {
                let ir_expr = self.lower_expr(*expr);
                let ty = match op {
                    UnOp::Address => self.ctx.allocator.alloc(ir::Ty::Ptr(ir_expr.ty)),
                    UnOp::Deref => {
                        let deref = &*self
                            .ctx
                            .allocator
                            .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var()));
                        let reference = self
                            .ctx
                            .ty_problem
                            .new_typed_ty_var(self.ctx.allocator.alloc(ir::Ty::Ptr(deref)));
                        let expr_ty_var_id = self.tys_ty_var_id(ir_expr.ty);

                        self.ctx.ty_problem.eq(expr_ty_var_id, reference);

                        deref
                    }
                    _ => ir_expr.ty,
                };

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Unary(op, self.ctx.allocator.alloc(ir_expr)),
                }
            }
            parser::Expr::Struct(parser::ExprStruct { name, fields }) => {
                let ty = self.lower_ty(parser::Ty::Ident(name));
                let ir::Ty::Struct(id) = ty else {
                    unreachable!();
                };
                let fields = &*self.ctx.allocator.alloc_slice_copy(
                    fields
                        .into_iter()
                        .map(|(field, expr)| {
                            let expr = self.lower_expr(expr);

                            match self.globals[id.global_id].0[id.node_id] {
                                ir::Node::Item(item) => match item {
                                    ir::Item::Struct(fields) => {
                                        match OrderedMap::get(&fields, &field.as_str()) {
                                            Some(ty) => {
                                                let expr_ty_var_id = self.tys_ty_var_id(expr.ty);
                                                let field_ty_var_id = self.tys_ty_var_id(*ty);

                                                self.ctx
                                                    .ty_problem
                                                    .eq(expr_ty_var_id, field_ty_var_id);
                                            }
                                            None => unreachable!(),
                                        }
                                    }
                                    _ => unreachable!(),
                                },
                                _ => unreachable!(),
                            };

                            (&*self.ctx.allocator.alloc_str(&field), expr)
                        })
                        .collect::<Vec<_>>()
                        .as_slice(),
                );

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Struct(fields),
                }
            }
            parser::Expr::Field(parser::ExprField { expr, field }) => {
                let expr = self.lower_expr(*expr);
                let ty = self.lower_ty(parser::Ty::Infer);
                let field = self.ctx.allocator.alloc_str(field.as_str());
                let expr_ty_var = self.tys_ty_var_id(expr.ty);
                let field_ty_var = self.tys_ty_var_id(ty);

                self.ctx.ty_problem.field(expr_ty_var, field_ty_var, field);

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Field(self.ctx.allocator.alloc(expr), field),
                }
            }
            parser::Expr::Cast(parser::ExprCast { expr, ty }) => {
                let expr = self.lower_expr(*expr);
                let ty = self.lower_ty(ty);

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Cast(self.ctx.allocator.alloc(expr), ty),
                }
            }
            _ => todo!(),
        }
    }

    fn lower_ty(&mut self, ty: parser::Ty) -> &'ir ir::Ty<'ir> {
        match self.types.get(&ty) {
            Some(ty) => *ty,
            None => {
                let ir_ty = match &ty {
                    parser::Ty::Null => self.ctx.allocator.alloc(ir::Ty::Null),
                    parser::Ty::Void => self.ctx.allocator.alloc(ir::Ty::Void),
                    parser::Ty::Bool => self.ctx.allocator.alloc(ir::Ty::Bool),
                    parser::Ty::Int(ty) => match ty {
                        parser::IntTy::I8 => self.ctx.allocator.alloc(ir::Ty::Int(ir::IntTy::I8)),
                        parser::IntTy::I16 => self.ctx.allocator.alloc(ir::Ty::Int(ir::IntTy::I16)),
                        parser::IntTy::I32 => self.ctx.allocator.alloc(ir::Ty::Int(ir::IntTy::I32)),
                        parser::IntTy::I64 => self.ctx.allocator.alloc(ir::Ty::Int(ir::IntTy::I64)),
                        parser::IntTy::Isize => {
                            self.ctx.allocator.alloc(ir::Ty::Int(ir::IntTy::Isize))
                        }
                    },
                    parser::Ty::UInt(ty) => match ty {
                        parser::UintTy::U8 => {
                            self.ctx.allocator.alloc(ir::Ty::UInt(ir::UintTy::U8))
                        }
                        parser::UintTy::U16 => {
                            self.ctx.allocator.alloc(ir::Ty::UInt(ir::UintTy::U16))
                        }
                        parser::UintTy::U32 => {
                            self.ctx.allocator.alloc(ir::Ty::UInt(ir::UintTy::U32))
                        }
                        parser::UintTy::U64 => {
                            self.ctx.allocator.alloc(ir::Ty::UInt(ir::UintTy::U64))
                        }
                        parser::UintTy::Usize => {
                            self.ctx.allocator.alloc(ir::Ty::UInt(ir::UintTy::Usize))
                        }
                    },
                    parser::Ty::Ptr(ref ty) => self
                        .ctx
                        .allocator
                        .alloc(ir::Ty::Ptr(self.lower_ty(*ty.clone()))),
                    parser::Ty::Array(parser::TyArray { ref ty, len }) => {
                        self.ctx.allocator.alloc(ir::Ty::Array(ir::TyArray {
                            len: *len,
                            ty: self.lower_ty(*ty.clone()),
                        }))
                    }
                    parser::Ty::Fn(ref params, ref ret_ty) => {
                        let mut alloced_params = Vec::new();

                        for ty in params {
                            alloced_params.push(self.lower_ty(ty.clone()));
                        }

                        let params = &*self.ctx.allocator.alloc_slice_copy(&alloced_params);

                        self.ctx
                            .allocator
                            .alloc(ir::Ty::Fn(params, self.lower_ty(*ret_ty.clone())))
                    }
                    parser::Ty::Ident(ident) => {
                        return self.scopes.get_type(ident).unwrap();
                    }
                    parser::Ty::Infer => self
                        .ctx
                        .allocator
                        .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
                };

                if !matches!(ty, parser::Ty::Infer) {
                    self.types.insert(ty, ir_ty);
                }

                ir_ty
            }
        }
    }

    fn expr_ty(&mut self, expr: &parser::Expr) -> &'ir ir::Ty<'ir> {
        match expr {
            parser::Expr::Binary(_) => self
                .ctx
                .allocator
                .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
            parser::Expr::Lit(lit) => match lit {
                parser::ExprLit::Bool(_) => &ir::Ty::Bool,
                parser::ExprLit::String(_) => &ir::Ty::Ptr(&ir::Ty::UInt(ir::UintTy::U8)),
                _ => self
                    .ctx
                    .allocator
                    .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
            },
            parser::Expr::Ident(parser::ExprIdent(ident)) => {
                let id = self.scopes.get_symbol(ident).unwrap();

                match self.nodes_map.get(&id).unwrap() {
                    ir::Node::Stmt(stmt) => match stmt {
                        Stmt::Local(stmt) => stmt.ty,
                        _ => unreachable!(),
                    },
                    _ => panic!("nono"),
                }
            }
            _ => todo!(),
        }
    }

    fn tys_ty_var_id(&mut self, ty: &'ir ir::Ty<'ir>) -> ty_problem::Id {
        match ty {
            ir::Ty::Infer(id) => *id,
            ty => self.ctx.ty_problem.new_typed_ty_var(ty),
        }
    }
}
