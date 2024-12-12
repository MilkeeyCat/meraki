mod scopes;

use crate::{
    ir::{self, Id, Ir, Stmt},
    parser::{self, Item, Variable},
};
use bumpalo::Bump;
use scopes::Scopes;
use std::{cell::UnsafeCell, collections::HashMap};

#[derive(Debug)]
pub struct Lowering<'a, 'ir> {
    allocator: &'ir Bump,
    ir: &'a mut Ir<'ir>,
    types: HashMap<parser::Ty, &'ir ir::Ty<'ir>>,
    scopes: Scopes<'ir>,
    nodes: Vec<ir::Node<'ir>>,
    globals: Vec<ir::Global<'ir>>,
    nodes_map: HashMap<Id, ir::Node<'ir>>,
    id: Id,
}

impl<'a, 'ir> Lowering<'a, 'ir> {
    pub fn new(allocator: &'ir Bump, ir: &'a mut Ir<'ir>) -> Self {
        Self {
            allocator,
            ir,
            types: HashMap::new(),
            scopes: Scopes::new(),
            nodes: Vec::new(),
            globals: Vec::new(),
            nodes_map: HashMap::new(),
            id: Id::default(),
        }
    }

    pub fn lower(mut self, ast: Vec<Item>) {
        self.scopes.enter();

        ast.into_iter().for_each(|item| {
            self.lower_item(item);
        });

        let globals = self.allocator.alloc_slice_copy(&self.globals);
        self.ir.set_globals(globals);
    }

    pub fn lower_item(&mut self, item: Item) -> Option<ir::Item<'ir>> {
        match item {
            Item::Struct(item) => {
                let mut fields = Vec::new();

                for (field, ty) in item.fields {
                    fields.push((&*self.allocator.alloc_str(&field), self.lower_ty(ty)));
                }

                let fields = self.allocator.alloc_slice_copy(&fields);
                let ty = self.allocator.alloc(ir::Ty::Struct(fields));

                self.types.insert(parser::Ty::Ident(item.name), ty);

                None
            }
            Item::Fn(item) => {
                self.id.node_id = 1;
                self.scopes.enter();

                let stmts = if let Some(block) = item.block {
                    block
                        .0
                        .into_iter()
                        .map(|stmt| self.lower_stmt(stmt))
                        .collect()
                } else {
                    Vec::new()
                };

                self.scopes.leave();

                let params: Vec<&'ir ir::Ty<'ir>> = item
                    .params
                    .into_iter()
                    .map(|(_, ty)| self.lower_ty(ty))
                    .collect();
                let signature = ir::Signature {
                    params: self.allocator.alloc_slice_copy(&params),
                    ret_ty: self.lower_ty(item.ret_ty),
                };

                self.nodes.insert(
                    0,
                    ir::Node::Item(ir::Item::Fn(self.allocator.alloc(ir::ItemFn {
                        id: Id {
                            global_id: self.id.global_id,
                            node_id: 0,
                        },
                        name: self.allocator.alloc_str(&item.name),
                        signature,
                        block: ir::Block(self.allocator.alloc_slice_copy(&stmts)),
                    }))),
                );

                self.globals
                    .push(ir::Global(self.allocator.alloc_slice_copy(&self.nodes)));
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
                    .push(ir::Global(self.allocator.alloc_slice_copy(&[node])));
                self.id.global_id += 1;

                Some(ir::Item::Global(ir_var))
            }
        }
    }

    fn lower_var_decl(&mut self, variable: Variable) -> &'ir ir::Variable<'ir> {
        let initializer = if let Some(expr) = variable.value {
            Some(self.lower_expr(expr))
        } else {
            None
        };

        let ir_variable = self.allocator.alloc(ir::Variable {
            id: self.id,
            name: self.allocator.alloc_str(&variable.name),
            ty: self
                .allocator
                .alloc(UnsafeCell::new(self.lower_ty(variable.ty))),
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
                let expr = stmt.expr.map(|expr| self.lower_expr(expr));

                ir::Stmt::Return(expr)
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: parser::Expr) -> ir::Expr<'ir> {
        let ty = self.allocator.alloc(UnsafeCell::new(self.expr_ty(&expr)));

        match expr {
            parser::Expr::Binary(parser::ExprBinary { op, left, right }) => {
                let lhs = self.lower_expr(*left);
                let rhs = self.lower_expr(*right);

                ir::Expr {
                    ty: self
                        .allocator
                        .alloc(UnsafeCell::new(self.lower_ty(parser::Ty::Infer))),
                    kind: ir::ExprKind::Binary(
                        op,
                        self.allocator.alloc(lhs),
                        self.allocator.alloc(rhs),
                    ),
                }
            }
            parser::Expr::Ident(parser::ExprIdent(ref ident)) => {
                let id = self.scopes.get_symbol(ident).unwrap();

                ir::Expr {
                    ty,
                    kind: ir::ExprKind::Ident(id),
                }
            }
            parser::Expr::Lit(ref lit) => {
                let kind = match lit {
                    parser::ExprLit::Int(lit) => ir::ExprKind::Lit(ir::ExprLit::Int(*lit)),
                    parser::ExprLit::UInt(lit) => ir::ExprKind::Lit(ir::ExprLit::UInt(*lit)),
                    parser::ExprLit::Bool(lit) => ir::ExprKind::Lit(ir::ExprLit::Bool(*lit)),
                    parser::ExprLit::String(lit) => {
                        ir::ExprKind::Lit(ir::ExprLit::String(self.allocator.alloc_str(&lit)))
                    }
                    parser::ExprLit::Null => ir::ExprKind::Lit(ir::ExprLit::Null),
                };

                ir::Expr { ty, kind }
            }
            _ => todo!(),
        }
    }

    fn lower_ty(&mut self, ty: parser::Ty) -> &'ir ir::Ty<'ir> {
        match self.types.get(&ty) {
            Some(ty) => *ty,
            None => {
                let ir_ty = match &ty {
                    parser::Ty::Null => self.allocator.alloc(ir::Ty::Null),
                    parser::Ty::Void => self.allocator.alloc(ir::Ty::Void),
                    parser::Ty::Bool => self.allocator.alloc(ir::Ty::Bool),
                    parser::Ty::Int(ty) => match ty {
                        parser::IntTy::I8 => self.allocator.alloc(ir::Ty::Int(ir::IntTy::I8)),
                        parser::IntTy::I16 => self.allocator.alloc(ir::Ty::Int(ir::IntTy::I16)),
                        parser::IntTy::I32 => self.allocator.alloc(ir::Ty::Int(ir::IntTy::I32)),
                        parser::IntTy::I64 => self.allocator.alloc(ir::Ty::Int(ir::IntTy::I64)),
                        parser::IntTy::Isize => self.allocator.alloc(ir::Ty::Int(ir::IntTy::Isize)),
                    },
                    parser::Ty::UInt(ty) => match ty {
                        parser::UintTy::U8 => self.allocator.alloc(ir::Ty::UInt(ir::UintTy::U8)),
                        parser::UintTy::U16 => self.allocator.alloc(ir::Ty::UInt(ir::UintTy::U16)),
                        parser::UintTy::U32 => self.allocator.alloc(ir::Ty::UInt(ir::UintTy::U32)),
                        parser::UintTy::U64 => self.allocator.alloc(ir::Ty::UInt(ir::UintTy::U64)),
                        parser::UintTy::Usize => {
                            self.allocator.alloc(ir::Ty::UInt(ir::UintTy::Usize))
                        }
                    },
                    parser::Ty::Ptr(ref ty) => self
                        .allocator
                        .alloc(ir::Ty::Ptr(self.lower_ty(*ty.clone()))),
                    parser::Ty::Array(parser::TyArray { ref ty, len }) => {
                        self.allocator.alloc(ir::Ty::Array(ir::TyArray {
                            len: *len,
                            ty: self.lower_ty(*ty.clone()),
                        }))
                    }
                    parser::Ty::Fn(ref params, ref ret_ty) => {
                        let mut alloced_params = Vec::new();

                        for ty in params {
                            alloced_params.push(self.lower_ty(ty.clone()));
                        }

                        let params = &*self.allocator.alloc_slice_copy(&alloced_params);

                        self.allocator
                            .alloc(ir::Ty::Fn(params, self.lower_ty(*ret_ty.clone())))
                    }
                    parser::Ty::Ident(ident) => {
                        return self.scopes.get_type(ident).unwrap();
                    }
                    parser::Ty::Infer => self.allocator.alloc(ir::Ty::Infer),
                };

                self.types.insert(ty, ir_ty);

                ir_ty
            }
        }
    }

    fn expr_ty(&mut self, expr: &parser::Expr) -> &'ir ir::Ty<'ir> {
        match expr {
            parser::Expr::Binary(_) | parser::Expr::Lit(_) => self.allocator.alloc(ir::Ty::Infer),
            parser::Expr::Ident(parser::ExprIdent(ident)) => {
                let id = self.scopes.get_symbol(ident).unwrap();

                match self.nodes_map.get(&id).unwrap() {
                    ir::Node::Stmt(stmt) => match stmt {
                        Stmt::Local(stmt) => stmt.ty(),
                        _ => unreachable!(),
                    },
                    _ => panic!("nono"),
                }
            }
            _ => todo!(),
        }
    }
}
