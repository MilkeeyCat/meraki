mod scopes;

use crate::{
    ast::{self, BinOp, IntTy, Item, UintTy, UnOp, Variable},
    ir::{self, Id, OrderedMap, Stmt},
    ty_problem, Context,
};
use scopes::Scopes;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Lowering<'a, 'ir> {
    ctx: &'a mut Context<'ir>,
    types: HashMap<ast::Ty, &'ir ir::Ty<'ir>>,
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
            Item::Struct { name, fields } => {
                let ty = self.ctx.allocator.alloc(ir::Ty::Struct(self.id));
                self.types.insert(ast::Ty::Ident(name), ty);

                let fields = fields
                    .into_iter()
                    .map(|(field, ty)| (&*self.ctx.allocator.alloc_str(&field), self.lower_ty(ty)))
                    .collect::<Vec<_>>();
                let fields = self.ctx.allocator.alloc_slice_copy(&fields);

                self.globals.push(ir::Global(
                    self.ctx
                        .allocator
                        .alloc([ir::Node::Item(ir::Item::Struct(fields))]),
                ));
                self.id.global_id += 1;

                None
            }
            Item::Fn {
                ret_ty,
                name,
                params,
                block,
            } => {
                self.id.node_id = 1;
                self.scopes.enter();

                let ret_ty = self.lower_ty(ret_ty);
                self.ret_ty = Some(ret_ty);

                let stmts = if let Some(block) = block {
                    block
                        .stmts
                        .into_iter()
                        .map(|stmt| self.lower_stmt(stmt))
                        .collect()
                } else {
                    Vec::new()
                };

                self.ret_ty = None;
                self.scopes.leave();

                let params: Vec<&'ir ir::Ty<'ir>> = params
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
                        name: self.ctx.allocator.alloc_str(&name),
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

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> ir::Stmt<'ir> {
        match stmt {
            ast::Stmt::Local(var) => {
                let name = var.name.clone();
                let ir_var = self.lower_var_decl(var);
                let node = ir::Node::Stmt(ir::Stmt::Local(ir_var));

                self.scopes.insert_symbol(name, self.id);
                self.nodes_map.insert(self.id, node);

                self.nodes.push(node);
                self.id.node_id += 1;

                ir::Stmt::Local(ir_var)
            }
            ast::Stmt::Item(item) => ir::Stmt::Item(self.lower_item(item).unwrap()),
            ast::Stmt::Expr(expr) => ir::Stmt::Expr(self.lower_expr(expr)),
            ast::Stmt::Return(expr) => {
                let expr = expr.map(|expr| {
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

    fn lower_expr(&mut self, expr: ast::Expr) -> ir::Expr<'ir> {
        match expr {
            ast::Expr::Binary {
                op,
                ref left,
                ref right,
            } => {
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
            ast::Expr::Ident(ref ident) => {
                let id = self.scopes.get_symbol(ident).unwrap();
                let ty = self.expr_ty(&expr);

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Ident(id),
                }
            }
            ast::Expr::Lit(ref lit) => {
                let ty = self.expr_ty(&expr);
                let kind = match lit {
                    ast::ExprLit::Int(lit) => ir::ExprKind::Lit(ir::ExprLit::Int(*lit)),
                    ast::ExprLit::UInt(lit) => ir::ExprKind::Lit(ir::ExprLit::UInt(*lit)),
                    ast::ExprLit::Bool(lit) => ir::ExprKind::Lit(ir::ExprLit::Bool(*lit)),
                    ast::ExprLit::String(lit) => {
                        ir::ExprKind::Lit(ir::ExprLit::String(self.ctx.allocator.alloc_str(&lit)))
                    }
                    ast::ExprLit::Null => ir::ExprKind::Lit(ir::ExprLit::Null),
                };

                ir::Expr { ty, kind }
            }
            ast::Expr::Unary { op, expr } => {
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
            ast::Expr::Struct { name, fields } => {
                let ty = self.lower_ty(ast::Ty::Ident(name));
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
            ast::Expr::Field { expr, field } => {
                let expr = self.lower_expr(*expr);
                let ty = self.lower_ty(ast::Ty::Infer);
                let field = self.ctx.allocator.alloc_str(field.as_str());
                let expr_ty_var = self.tys_ty_var_id(expr.ty);
                let field_ty_var = self.tys_ty_var_id(ty);

                self.ctx.ty_problem.field(expr_ty_var, field_ty_var, field);

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Field(self.ctx.allocator.alloc(expr), field),
                }
            }
            ast::Expr::Cast { expr, ty } => {
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

    fn lower_ty(&mut self, ty: ast::Ty) -> &'ir ir::Ty<'ir> {
        match self.types.get(&ty) {
            Some(ty) => *ty,
            None => {
                let ir_ty = match &ty {
                    ast::Ty::Null => self.ctx.allocator.alloc(ir::Ty::Null),
                    ast::Ty::Void => self.ctx.allocator.alloc(ir::Ty::Void),
                    ast::Ty::Bool => self.ctx.allocator.alloc(ir::Ty::Bool),
                    ast::Ty::Int(ty) => match ty {
                        ast::IntTy::I8 => self.ctx.allocator.alloc(ir::Ty::Int(IntTy::I8)),
                        ast::IntTy::I16 => self.ctx.allocator.alloc(ir::Ty::Int(IntTy::I16)),
                        ast::IntTy::I32 => self.ctx.allocator.alloc(ir::Ty::Int(IntTy::I32)),
                        ast::IntTy::I64 => self.ctx.allocator.alloc(ir::Ty::Int(IntTy::I64)),
                        ast::IntTy::Isize => self.ctx.allocator.alloc(ir::Ty::Int(IntTy::Isize)),
                    },
                    ast::Ty::UInt(ty) => match ty {
                        ast::UintTy::U8 => self.ctx.allocator.alloc(ir::Ty::UInt(UintTy::U8)),
                        ast::UintTy::U16 => self.ctx.allocator.alloc(ir::Ty::UInt(UintTy::U16)),
                        ast::UintTy::U32 => self.ctx.allocator.alloc(ir::Ty::UInt(UintTy::U32)),
                        ast::UintTy::U64 => self.ctx.allocator.alloc(ir::Ty::UInt(UintTy::U64)),
                        ast::UintTy::Usize => self.ctx.allocator.alloc(ir::Ty::UInt(UintTy::Usize)),
                    },
                    ast::Ty::Ptr(ref ty) => self
                        .ctx
                        .allocator
                        .alloc(ir::Ty::Ptr(self.lower_ty(*ty.clone()))),
                    ast::Ty::Array { ty, len } => {
                        self.ctx.allocator.alloc(ir::Ty::Array(ir::TyArray {
                            len: *len,
                            ty: self.lower_ty(*ty.clone()),
                        }))
                    }
                    ast::Ty::Fn(ref params, ref ret_ty) => {
                        let mut alloced_params = Vec::new();

                        for ty in params {
                            alloced_params.push(self.lower_ty(ty.clone()));
                        }

                        let params = &*self.ctx.allocator.alloc_slice_copy(&alloced_params);

                        self.ctx
                            .allocator
                            .alloc(ir::Ty::Fn(params, self.lower_ty(*ret_ty.clone())))
                    }
                    ast::Ty::Ident(ident) => {
                        return self.scopes.get_type(ident).unwrap();
                    }
                    ast::Ty::Infer => self
                        .ctx
                        .allocator
                        .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
                };

                if ty != ast::Ty::Infer {
                    self.types.insert(ty, ir_ty);
                }

                ir_ty
            }
        }
    }

    fn expr_ty(&mut self, expr: &ast::Expr) -> &'ir ir::Ty<'ir> {
        match expr {
            ast::Expr::Binary { .. } => self
                .ctx
                .allocator
                .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
            ast::Expr::Lit(lit) => match lit {
                ast::ExprLit::Bool(_) => &ir::Ty::Bool,
                ast::ExprLit::String(_) => &ir::Ty::Ptr(&ir::Ty::UInt(UintTy::U8)),
                _ => self
                    .ctx
                    .allocator
                    .alloc(ir::Ty::Infer(self.ctx.ty_problem.new_infer_ty_var())),
            },
            ast::Expr::Ident(ident) => {
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
