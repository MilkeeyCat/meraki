use super::Pass;
use crate::{
    lexer::{self, LexerError},
    macros::{self, symbol_to_macros, Macro, MacroFn, Slice},
    parser::{Block, Expr, Parser, Precedence, Stmt},
    scope::Scope,
};
use libloading::Library;
use std::ffi::OsStr;

pub struct MacroExpansion {
    libs: Vec<Library>,
}

impl Pass for MacroExpansion {
    type State = Vec<String>;
    type Output = ();

    fn new(paths: Self::State) -> Self {
        Self {
            libs: paths
                .into_iter()
                .map(|path| unsafe { libloading::Library::new(OsStr::new(&path)).unwrap() })
                .collect(),
        }
    }

    fn run_pass(self, stmts: &mut Vec<Stmt>, _: &mut Scope) -> Self::Output {
        (0..stmts.len()).for_each(|i| self.check_stmt(stmts, i));
    }
}

impl MacroExpansion {
    const MACROS_SYMBOL_NAME: &'static str = "macros\0";

    fn get_macro(&self, name: &str) -> Option<&MacroFn> {
        for lib in &self.libs {
            let symbol = unsafe {
                lib.get::<*const Slice<Macro>>(Self::MACROS_SYMBOL_NAME.as_bytes())
                    .unwrap()
            };

            if let Some(mr) = symbol_to_macros(symbol).into_iter().find_map(|mr| {
                if mr.name() == name {
                    Some(mr)
                } else {
                    None
                }
            }) {
                return Some(mr.func());
            }
        }

        None
    }

    fn call_macro(
        &self,
        func: &MacroFn,
        tokens: &mut Vec<lexer::Token>,
    ) -> Vec<Result<lexer::Token, LexerError>> {
        let token_tree = std::mem::take(tokens)
            .into_iter()
            .map(|token| token.into())
            .collect::<Vec<macros::Token>>()
            .leak();
        let token_tree = func(Slice {
            ptr: token_tree.as_ptr(),
            len: token_tree.len(),
        });

        unsafe {
            Vec::from_raw_parts(
                token_tree.ptr as *mut macros::Token,
                token_tree.len,
                token_tree.len,
            )
        }
        .into_iter()
        .map(|token_tree| Ok(token_tree.into()))
        .collect()
    }

    fn check_stmt(&self, stmts: &mut Vec<Stmt>, i: usize) {
        match &mut stmts[i] {
            Stmt::VarDecl(stmt) => {
                if let Some(expr) = &mut stmt.value {
                    self.check_expr(expr);
                }
            }
            Stmt::Expr(expr) => match expr {
                Expr::MacroCall(call) => match self.get_macro(&call.name) {
                    Some(func) => {
                        let tokens = self.call_macro(func, &mut call.tokens);
                        let mut parser = Parser::new(tokens.into_iter()).unwrap();
                        // TODO: assert that all tokens were parsed
                        let parsed_stmts = parser.parse_stmts().unwrap();

                        stmts.splice(i..=i, parsed_stmts);
                    }
                    None => panic!("Macro {} doesn't exist", &call.name),
                },
                expr => self.check_expr(expr),
            },
            Stmt::Function(stmt) => self.check_block(&mut stmt.block),
            Stmt::Return(stmt) => {
                if let Some(expr) = &mut stmt.expr {
                    self.check_expr(expr)
                }
            }
            Stmt::If(stmt) => {
                self.check_expr(&mut stmt.condition);
                self.check_block(&mut stmt.consequence);
                if let Some(block) = &mut stmt.alternative {
                    self.check_block(block);
                }
            }
            Stmt::While(stmt) => {
                self.check_expr(&mut stmt.condition);
                self.check_block(&mut stmt.block);
            }
            Stmt::For(stmt) => {
                // What to do here?
                if let Some(stmt) = &mut stmt.initializer {}
                if let Some(expr) = &mut stmt.condition {
                    self.check_expr(expr);
                }
                if let Some(expr) = &mut stmt.increment {
                    self.check_expr(expr);
                }
                self.check_block(&mut stmt.block);
            }
            Stmt::Continue | Stmt::Break => (),
        };
    }

    fn check_expr(&self, expr: &mut Expr) {
        match expr {
            Expr::Binary(expr) => {
                self.check_expr(&mut expr.left);
                self.check_expr(&mut expr.right);
            }
            Expr::Unary(expr) => self.check_expr(&mut expr.expr),
            Expr::Cast(expr) => self.check_expr(&mut expr.expr),
            Expr::Lit(_) | Expr::Ident(_) => (),
            Expr::Struct(expr) => expr
                .fields
                .iter_mut()
                .for_each(|(_, expr)| self.check_expr(expr)),
            Expr::Array(expr) => expr.0.iter_mut().for_each(|expr| self.check_expr(expr)),
            Expr::StructAccess(expr) => self.check_expr(&mut expr.expr),
            Expr::StructMethod(expr) => {
                self.check_expr(&mut expr.expr);
                expr.arguments
                    .iter_mut()
                    .for_each(|expr| self.check_expr(expr));
            }
            Expr::ArrayAccess(expr) => {
                self.check_expr(&mut expr.expr);
                self.check_expr(&mut expr.index);
            }
            Expr::FunctionCall(expr) => {
                self.check_expr(&mut expr.expr);
                expr.arguments
                    .iter_mut()
                    .for_each(|expr| self.check_expr(expr));
            }
            Expr::MacroCall(call) => match self.get_macro(&call.name) {
                Some(func) => {
                    let tokens = self.call_macro(func, &mut call.tokens);
                    let mut parser = Parser::new(tokens.into_iter()).unwrap();
                    // TODO: assert that all tokens were parsed
                    let parsed_expr = parser.expr(Precedence::default()).unwrap();

                    *expr = parsed_expr;
                }
                None => panic!("Macro {} doesn't exist", &call.name),
            },
        }
    }

    fn check_block(&self, block: &mut Block) {
        (0..block.statements.len()).for_each(|i| self.check_stmt(&mut block.statements, i));
    }
}
