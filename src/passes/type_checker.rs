use super::Pass;
use crate::{
    parser::{Block, Expr, Expression, ParserError, Stmt, UnOp},
    scope::Scope,
    type_table as tt,
    type_table::TypeTable,
    types::{Type, TypeError},
};
use std::collections::HashSet;

type Result<T> = std::result::Result<T, ParserError>;
pub struct TypeChecker;

impl Pass for TypeChecker {
    type Output = Result<()>;

    fn proccess(stmts: &mut Vec<Stmt>, scope: &mut Scope) -> Self::Output {
        Self::check_type_table(scope.type_table(), scope)?;
        for stmt in stmts {
            Self::check_stmt(stmt, scope)?;
        }

        Ok(())
    }
}

impl TypeChecker {
    fn check_block(block: &Block, scope: &mut Scope) -> Result<()> {
        scope.enter(block.scope.clone());
        Self::check_type_table(scope.type_table(), scope)?;
        for stmt in &block.statements {
            Self::check_stmt(stmt, scope)?;
        }
        scope.leave();

        Ok(())
    }

    fn check_stmt(stmt: &Stmt, scope: &mut Scope) -> Result<()> {
        Ok(match stmt {
            Stmt::VarDecl(stmt) => {
                Self::check_type(&stmt.type_, scope)?;

                if let Some(expr) = &stmt.value {
                    Self::check_expr(expr, scope)?;
                    Self::check_if_expr_assignable_to_type(expr, scope, stmt.type_.clone())?;
                }
            }
            Stmt::Expr(expr) => {
                Self::check_expr(expr, scope)?;
            }
            Stmt::Function(stmt) => {
                Self::check_type(&stmt.return_type, scope)?;
                for (_, type_) in &stmt.params {
                    Self::check_type(type_, scope)?;
                }
                Self::check_block(&stmt.block, scope)?;
            }
            Stmt::Return(stmt) => {
                if let Some(expr) = &stmt.expr {
                    Self::check_if_expr_assignable_to_type(
                        expr,
                        scope,
                        scope.context().unwrap().to_owned().1,
                    )?;
                }
            }
            Stmt::If(stmt) => {
                Self::bool_expr(&stmt.condition, scope)?;
                Self::check_block(&stmt.consequence, scope)?;
                if let Some(alternative) = &stmt.alternative {
                    Self::check_block(alternative, scope)?;
                }
            }
            Stmt::While(stmt) => {
                Self::bool_expr(&stmt.condition, scope)?;
                Self::check_block(&stmt.block, scope)?;
            }
            Stmt::For(stmt) => {
                if let Some(condition) = &stmt.condition {
                    Self::bool_expr(&condition, scope)?;
                }
                Self::check_block(&stmt.block, scope)?;
            }
        })
    }

    fn check_expr(expr: &Expr, scope: &Scope) -> Result<()> {
        Ok(match expr {
            Expr::Binary(expr) => {}
            Expr::Unary(expr) => match expr.op {
                UnOp::Deref => {
                    let type_ = expr.expr.type_(scope)?;
                    if !matches!(type_, Type::Ptr(..)) {
                        panic!("Can't dereference an expression of type {type_}")
                    }
                }
                _ => {}
            },
            Expr::Cast(expr) => {
                Self::check_type(&expr.type_, scope)?;
                Self::check_expr(&expr.expr, scope)?;
            }
            Expr::Lit(expr) => {}
            Expr::Ident(expr) => {}
            Expr::Struct(expr) => {
                Self::check_type(&expr.type_(scope)?, scope)?;

                if scope.find_type(&expr.name).is_none() {
                    return Err(ParserError::Type(TypeError::Nonexistent(expr.name.clone())));
                }
            }
            Expr::Array(expr) => {
                let unique = expr
                    .0
                    .iter()
                    .map(|item| item.type_(scope).unwrap())
                    .collect::<HashSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();

                if unique.len() != 1 {
                    panic!("Types only of the same type allowed in an array expression")
                }
            }
            Expr::StructAccess(expr) => {}
            Expr::StructMethod(expr) => {}
            Expr::ArrayAccess(expr) => {}
            Expr::FunctionCall(expr) => {
                let symbol = scope.find_symbol(&expr.name).unwrap().function_unchecked();
                let args_types = expr
                    .arguments
                    .iter()
                    .map(|expr| expr.type_(scope))
                    .collect::<std::result::Result<Vec<_>, _>>()?;

                for (param_type, arg_type) in symbol.parameters.iter().zip(&args_types) {
                    if let Err(_) = param_type.to_owned().assign(arg_type.to_owned()) {
                        return Err(ParserError::FunctionArguments(
                            expr.name.clone(),
                            symbol.parameters.to_owned(),
                            args_types,
                        ));
                    }
                }
            }
        })
    }

    fn bool_expr(expr: &Expr, scope: &Scope) -> Result<()> {
        let type_ = expr.type_(scope)?;

        if type_ != Type::Bool {
            Err(ParserError::Type(TypeError::Mismatched(Type::Bool, type_)))
        } else {
            Ok(())
        }
    }

    fn check_if_expr_assignable_to_type(expr: &Expr, scope: &Scope, target: Type) -> Result<()> {
        let type_ = expr.type_(scope)?;
        if type_ == target {
            return Ok(());
        }

        if Expr::int_lit_only(expr) {
            type_.clone().assign(target.clone())?;
        }

        Err(ParserError::Type(TypeError::Mismatched(type_, target)))
    }

    fn check_type(type_: &Type, scope: &Scope) -> Result<()> {
        Ok(match type_ {
            Type::Struct(name) => {
                if scope.find_type(name).is_none() {
                    return Err(ParserError::Type(TypeError::Nonexistent(name.to_owned())));
                }
            }
            _ => {}
        })
    }

    fn check_type_table(type_table: &TypeTable, scope: &Scope) -> Result<()> {
        for type_ in &type_table.0 {
            match type_ {
                tt::Type::Struct(type_struct) => {
                    for (_, type_) in &type_struct.fields {
                        Self::check_type(type_, scope)?;
                        if type_ == &Type::Struct(type_struct.name.clone()) {
                            panic!("Recursize type {type_} has infinite size");
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
