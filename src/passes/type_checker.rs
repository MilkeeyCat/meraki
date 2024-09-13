use super::Pass;
use crate::{
    parser::{BinOp, Block, Expr, ExprBinary, Expression, ParserError, Stmt, UnOp},
    scope::Scope,
    type_table::{self as tt, TypeTable},
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
                    Self::check_assign(stmt.type_.clone(), expr, scope)?;
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
            Expr::Binary(ExprBinary {
                op: BinOp::Assign,
                left,
                right,
            }) => {
                Self::check_expr(left, scope)?;
                Self::check_assign(left.type_(scope)?, right, scope)?;
            }
            Expr::Binary(expr) => {
                Self::check_bin(&expr.op, &expr.left, &expr.right, scope)?;
            }
            Expr::Unary(expr) => match expr.op {
                UnOp::Deref => {
                    let type_ = expr.expr.type_(scope)?;
                    if !matches!(type_, Type::Ptr(..)) {
                        panic!("Can't dereference an expression of type {type_}")
                    }
                }
                UnOp::Address => {
                    if !expr.expr.lvalue() {
                        panic!("Can't get address of {expr:?}");
                    }
                }
                _ => {}
            },
            Expr::Cast(expr) => {
                Self::check_type(&expr.type_, scope)?;
                Self::check_expr(&expr.expr, scope)?;
            }
            Expr::Lit(_) => {}
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
                let fn_name = &expr.name;
                let symbol = scope.find_symbol(&expr.name).unwrap().function_unchecked();
                let args_types = expr
                    .arguments
                    .iter()
                    .map(|expr| expr.type_(scope))
                    .collect::<std::result::Result<Vec<_>, _>>()?;

                for (expr, type_) in expr.arguments.iter().zip(&symbol.parameters) {
                    if let Err(_) =
                        Self::check_if_expr_assignable_to_type(expr, scope, type_.to_owned())
                    {
                        return Err(ParserError::FunctionArguments(
                            fn_name.clone(),
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
            Type::promote(type_.clone(), target.clone())?;
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

    pub fn check_bin(op: &BinOp, left: &Expr, right: &Expr, scope: &Scope) -> Result<Type> {
        Self::check_expr(left, scope)?;
        Self::check_expr(right, scope)?;

        let left_type = left.type_(scope)?;
        let right_type = right.type_(scope)?;

        let type_ = match op {
            BinOp::Add | BinOp::Sub => {
                if left_type.int() && right_type.int() {
                    if Expr::int_lit_only(left) || Expr::int_lit_only(right) {
                        if left_type > right_type {
                            Type::promote(right_type, left_type)?
                        } else {
                            Type::promote(left_type, right_type)?
                        }
                    } else {
                        assert_eq!(left_type, right_type);

                        left_type
                    }
                } else if left_type.ptr() && right_type.int() || left_type.int() && right_type.ptr()
                {
                    let (ptr, int, int_expr) = if left_type.ptr() {
                        (left_type, right_type, right)
                    } else {
                        (right_type, left_type, left)
                    };

                    if int != Type::Usize && !Expr::int_lit_only(int_expr) {
                        panic!("Only interger of type usize is allowed in pointer arithmetic");
                    }

                    if Expr::int_lit_only(int_expr) {
                        Type::promote(int, Type::Usize)?;
                    }

                    ptr
                } else {
                    panic!("Go fuck yourself");
                }
            }
            BinOp::LogicalAnd | BinOp::LogicalOr => {
                assert_eq!(left_type, Type::Bool);
                assert_eq!(right_type, Type::Bool);
                Type::Bool
            }
            BinOp::Assign => unreachable!(),
            _ => {
                assert!(
                    left_type.int() && right_type.int(),
                    "Math operations can be applied only to integers"
                );
                if Expr::int_lit_only(left) || Expr::int_lit_only(right) {
                    if left_type > right_type {
                        Type::promote(right_type, left_type)?
                    } else {
                        Type::promote(left_type, right_type)?
                    }
                } else {
                    assert_eq!(left_type, right_type);

                    left_type
                }
            }
        };

        Ok(type_)
    }

    fn check_assign(left_type: Type, right: &Expr, scope: &Scope) -> Result<()> {
        Self::check_expr(right, scope)?;

        let right_type = right.type_(scope)?;

        if left_type.int() {
            Type::promote(right_type, left_type)?;
        } else {
            assert_eq!(left_type, right_type);
        }

        Ok(())
    }
}
