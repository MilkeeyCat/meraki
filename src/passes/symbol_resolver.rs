use crate::{
    codegen::Offset,
    parser::{Block, Expr, Expression, ParserError, Stmt},
    scope::Scope,
    symbol_table::{Symbol, SymbolFunction, SymbolGlobal, SymbolLocal, SymbolParam},
    type_table as tt,
    types::{Type, TypeError},
};

use super::pass::Pass;

type Result = std::result::Result<(), ParserError>;
pub struct SymbolResolver;

impl Pass for SymbolResolver {
    type State = ();
    type Output = Result;

    fn new(_: Self::State) -> Self {
        Self {}
    }

    fn run_pass(self, stmts: &mut Vec<Stmt>, scope: &mut Scope) -> Self::Output {
        // Resolve globals
        for stmt in &mut *stmts {
            match stmt {
                Stmt::Function(stmt) => {
                    stmt.params
                        .iter()
                        .enumerate()
                        .for_each(|(i, (name, type_))| {
                            stmt.block
                                .scope
                                .symbol_table
                                .push(Symbol::Param(SymbolParam {
                                    name: name.to_owned(),
                                    preceding: stmt.params[..i]
                                        .iter()
                                        .map(|(_, type_)| type_.to_owned())
                                        .collect(),
                                    type_: type_.to_owned(),
                                    offset: Offset::default(),
                                }))
                                .unwrap();
                        });

                    scope
                        .symbol_table_mut()
                        .push(Symbol::Function(SymbolFunction {
                            name: stmt.name.clone(),
                            return_type: stmt.return_type.clone(),
                            parameters: stmt
                                .params
                                .clone()
                                .into_iter()
                                .map(|(_, type_)| type_)
                                .collect(),
                        }))?;
                }
                Stmt::VarDecl(stmt) => {
                    scope.symbol_table_mut().push(Symbol::Global(SymbolGlobal {
                        name: stmt.name.clone(),
                        type_: stmt.type_.clone(),
                    }))?;
                }
                _ => unreachable!(),
            }
        }

        // Resolve locals
        for stmt in stmts {
            if let Stmt::Function(stmt) = stmt {
                Self::resolve_block(&mut stmt.block, scope)?;
            }
        }

        Ok(())
    }
}

impl SymbolResolver {
    fn resolve_block(block: &mut Block, scope: &mut Scope) -> Result {
        scope.enter(block.scope.clone());

        for stmt in &mut block.statements {
            Self::resolve_stmt(stmt, scope)?;
        }

        block.scope = scope.leave();

        Ok(())
    }

    fn resolve_stmt(stmt: &mut Stmt, scope: &mut Scope) -> Result {
        Ok(match stmt {
            Stmt::VarDecl(stmt) => {
                scope.symbol_table_mut().push(Symbol::Local(SymbolLocal {
                    name: stmt.name.clone(),
                    type_: stmt.type_.clone(),
                    offset: Offset::default(),
                }))?;
            }
            Stmt::Expr(expr) => {
                Self::resolve_expr(expr, scope)?;
            }
            Stmt::Function(_) => unreachable!(),
            Stmt::Return(stmt) => {
                if let Some(expr) = &mut stmt.expr {
                    Self::resolve_expr(expr, scope)?;
                }
            }
            Stmt::If(stmt) => {
                Self::resolve_expr(&mut stmt.condition, scope)?;
                Self::resolve_block(&mut stmt.consequence, scope)?;
                if let Some(alternative) = &mut stmt.alternative {
                    Self::resolve_block(alternative, scope)?;
                }
            }
            Stmt::While(stmt) => {
                Self::resolve_expr(&mut stmt.condition, scope)?;
                Self::resolve_block(&mut stmt.block, scope)?;
            }
            Stmt::For(stmt) => {
                scope.enter(stmt.block.scope.clone());

                if let Some(initializer) = &mut stmt.initializer {
                    Self::resolve_stmt(initializer, scope)?;
                }
                if let Some(condition) = &mut stmt.condition {
                    Self::resolve_expr(condition, scope)?;
                }
                if let Some(increment) = &mut stmt.increment {
                    Self::resolve_expr(increment, scope)?;
                }

                stmt.block.scope = scope.leave();
                Self::resolve_block(&mut stmt.block, scope)?;
            }
            Stmt::Continue | Stmt::Break => (),
        })
    }

    fn resolve_expr(expr: &Expr, scope: &Scope) -> Result {
        Ok(match expr {
            Expr::Binary(expr) => {
                Self::resolve_expr(expr.left.as_ref(), scope)?;
                Self::resolve_expr(expr.right.as_ref(), scope)?;
            }
            Expr::Unary(expr) => {
                Self::resolve_expr(expr.expr.as_ref(), scope)?;
            }
            Expr::Cast(expr) => {
                Self::resolve_expr(expr.expr.as_ref(), scope)?;
            }
            Expr::Lit(_) => (),
            Expr::Ident(expr) => {
                scope
                    .find_symbol(&expr.0)
                    .ok_or(TypeError::IdentNotFound(expr.0.clone()))?;
            }
            Expr::Struct(expr) => {
                let type_struct = match scope
                    .find_type(&expr.name)
                    .ok_or(TypeError::Nonexistent(expr.name.clone()))?
                {
                    tt::Type::Struct(s) => s,
                };

                for (field, field_expr) in &expr.fields {
                    if !type_struct.contains(&field) {
                        panic!("Field {} doesn't exist in struct {}", field, expr.name);
                    }

                    Self::resolve_expr(&field_expr, scope)?;
                }
            }
            Expr::Array(expr) => {
                for expr in &expr.0 {
                    Self::resolve_expr(expr, scope)?;
                }
            }
            Expr::StructAccess(expr) => {
                Self::resolve_expr(expr.expr.as_ref(), scope)?;

                let type_name = match expr.expr.type_(scope)? {
                    Type::Custom(name) => name,
                    _ => unreachable!(),
                };

                match scope
                    .find_type(&type_name)
                    .ok_or(TypeError::Nonexistent(type_name))?
                {
                    tt::Type::Struct(s) => {
                        if !s.contains(&expr.field) {
                            panic!("no such field bitch");
                        }
                    }
                }
            }
            Expr::StructMethod(expr) => {
                Self::resolve_expr(&expr.expr, scope)?;

                let type_name = match expr.expr.type_(scope)? {
                    Type::Custom(name) => name,
                    _ => unreachable!(),
                };

                match scope
                    .find_type(&type_name)
                    .ok_or(TypeError::Nonexistent(type_name))?
                {
                    tt::Type::Struct(s) => {
                        if s.find_method(&expr.method).is_none() {
                            panic!("Method {} doesn't exist", expr.method);
                        }
                    }
                }
            }
            Expr::ArrayAccess(expr) => {
                Self::resolve_expr(&expr.expr, scope)?;
                Self::resolve_expr(&expr.index, scope)?;
            }
            Expr::FunctionCall(expr) => {
                Self::resolve_expr(&expr.expr, scope)?;

                for expr in &expr.arguments {
                    Self::resolve_expr(expr, scope)?;
                }
            }
            Expr::MacroCall(_) => unreachable!(),
        })
    }
}
