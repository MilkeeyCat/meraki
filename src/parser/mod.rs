mod diagnostics;
mod precedence;

use crate::{
    ast::{
        BinOp, Block, Expr, ExprKind, ExprLit, IntTy, Item, ItemKind, Stmt, StmtKind, Ty, UintTy,
        UnOp, Variable,
    },
    diagnostics::{Diagnostic, Diagnostics},
    lexer::{Token, TokenKind, span::Span},
};
pub use precedence::Precedence;
use std::collections::HashMap;

type PrefixFn<'a, 'src, T> = fn(&mut Parser<'a, 'src, T>) -> Result<Expr, ()>;
type InfixFn<'a, 'src, T> = fn(&mut Parser<'a, 'src, T>, left: Expr) -> Result<Expr, ()>;

pub struct Parser<'a, 'src, T: Iterator<Item = Result<Token, Span>>> {
    lexer: T,
    diag: &'a mut Diagnostics<'src>,
    prev_token: Option<Token>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    prefix_fns: HashMap<TokenKind, PrefixFn<'a, 'src, T>>,
    infix_fns: HashMap<TokenKind, InfixFn<'a, 'src, T>>,
}

impl<'a, 'src, T: Iterator<Item = Result<Token, Span>>> Parser<'a, 'src, T> {
    pub fn new(lexer: T, diag: &'a mut Diagnostics<'src>) -> Self {
        let mut parser = Self {
            prev_token: None,
            cur_token: None,
            peek_token: None,
            lexer,
            diag,
            prefix_fns: HashMap::from([
                (
                    TokenKind::Ident(Default::default()),
                    Self::parse_ident_expr as PrefixFn<T>,
                ),
                (
                    TokenKind::String(Default::default()),
                    Self::parse_string_lit_expr,
                ),
                (
                    TokenKind::Integer(Default::default()),
                    Self::parse_int_lit_expr,
                ),
                (TokenKind::Null, Self::parse_null_expr),
                (TokenKind::True, Self::parse_bool_expr),
                (TokenKind::False, Self::parse_bool_expr),
                (TokenKind::Minus, Self::parse_unary_expr),
                (TokenKind::Bang, Self::parse_unary_expr),
                (TokenKind::LParen, Self::parse_grouped_expr),
                (TokenKind::Ampersand, Self::parse_unary_expr),
                (TokenKind::Asterisk, Self::parse_unary_expr),
                (TokenKind::Tilde, Self::parse_unary_expr),
                (TokenKind::LBracket, Self::parse_array_expr),
            ]),
            infix_fns: HashMap::from([
                (TokenKind::Plus, Self::parse_bin_expr as InfixFn<T>),
                (TokenKind::Minus, Self::parse_bin_expr),
                (TokenKind::Asterisk, Self::parse_bin_expr),
                (TokenKind::Slash, Self::parse_bin_expr),
                (TokenKind::Assign, Self::parse_bin_expr),
                (TokenKind::LessThan, Self::parse_bin_expr),
                (TokenKind::LessEqual, Self::parse_bin_expr),
                (TokenKind::GreaterThan, Self::parse_bin_expr),
                (TokenKind::GreaterEqual, Self::parse_bin_expr),
                (TokenKind::Equal, Self::parse_bin_expr),
                (TokenKind::NotEqual, Self::parse_bin_expr),
                (TokenKind::And, Self::parse_bin_expr),
                (TokenKind::Or, Self::parse_bin_expr),
                (TokenKind::LParen, Self::parse_bin_expr),
                (TokenKind::Ampersand, Self::parse_bin_expr),
                (TokenKind::Bar, Self::parse_bin_expr),
                (TokenKind::Shl, Self::parse_bin_expr),
                (TokenKind::Shr, Self::parse_bin_expr),
                (TokenKind::Arrow, Self::parse_pointer_access_expr),
                (TokenKind::Period, Self::parse_struct_access_expr),
                (TokenKind::LBracket, Self::parse_array_access_expr),
                (TokenKind::As, Self::parse_cast_expr),
                (TokenKind::LParen, Self::parse_call_expr),
                (TokenKind::Bang, Self::parse_macro_call_expr),
            ]),
        };

        parser.bump();
        parser.bump();

        parser
    }

    fn bump(&mut self) {
        match self.lexer.next().transpose() {
            Ok(mut token) => {
                std::mem::swap(&mut self.prev_token, &mut self.cur_token);
                std::mem::swap(&mut self.cur_token, &mut self.peek_token);
                std::mem::swap(&mut token, &mut self.peek_token);
            }
            Err(span) => {
                self.diag.error(Diagnostic::UnknownChar, span);

                self.bump()
            }
        }
    }

    fn cur_token_is(&self, kind: &TokenKind) -> bool {
        self.cur_token.as_ref().map(|token| &token.kind) == Some(kind)
    }

    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        self.peek_token.as_ref().map(|token| &token.kind) == Some(kind)
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Span, ()> {
        match &self.cur_token {
            Some(cur) if &cur.kind == kind => {
                let span = cur.span.clone();
                self.bump();

                Ok(span)
            }
            _ => {
                self.expected(&[kind]);

                Err(())
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Item>, ()> {
        let mut items = Vec::new();

        while let Some(token) = &self.cur_token {
            let item = match token.kind {
                TokenKind::Struct => self.parse_struct_item().ok(),
                TokenKind::Let => self.parse_global_item().ok(),
                TokenKind::Fn => self.parse_function_item(true).ok(),
                _ => {
                    self.expected(&[&TokenKind::Struct, &TokenKind::Let, &TokenKind::Fn]);
                    self.bump();

                    None
                }
            };

            if let Some(item) = item {
                items.push(item);
            }
        }

        Ok(items)
    }

    pub fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ()> {
        let token = self.cur_token_unchecked();

        let mut left = match self.prefix_fns.get(&token.kind) {
            Some(func) => func(self),
            None => {
                self.diag
                    .error(Diagnostic::ExpressionPrefix(token.kind), token.span);

                return Err(());
            }
        };

        while !self.cur_token_is(&TokenKind::Semicolon)
            && self.cur_token.is_some()
            && precedence < Precedence::from(&self.cur_token_ref_unchecked().kind)
        {
            left = match self.infix_fns.get(&self.cur_token_ref_unchecked().kind) {
                Some(func) => func(self, left?),
                None => {
                    let Token { kind, span } = self.cur_token_unchecked();

                    self.diag.error(Diagnostic::ExpressionInfix(kind), span);

                    return Err(());
                }
            };
        }

        left
    }

    fn parse_struct_item(&mut self) -> Result<Item, ()> {
        self.expect(&TokenKind::Struct)?;

        let (name, _) = self.parse_ident()?;

        self.expect(&TokenKind::LBrace)?;

        let mut fields = Vec::new();

        while !self.cur_token_is(&TokenKind::RBrace) {
            let (name, span) = self.parse_ident()?;
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;

            if fields.iter().any(|(field_name, _)| field_name == &name) {
                self.diag.error(Diagnostic::RepeatingField(name), span);
            } else {
                fields.push((name, ty));
            }

            if !self.cur_token_is(&TokenKind::RBrace) {
                self.expect(&TokenKind::Semicolon)?;
            }
        }

        self.expect(&TokenKind::RBrace)?;

        Ok(Item::new(ItemKind::Struct { name, fields }))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ()> {
        match self.cur_token.as_ref().map(|token| &token.kind) {
            Some(TokenKind::Return) => self.parse_return_stmt(),
            Some(TokenKind::If) => self.parse_if_stmt(),
            Some(TokenKind::While) => self.parse_while_stmt(),
            Some(TokenKind::For) => self.parse_for_stmt(),
            Some(TokenKind::Let) => self.parse_local_stmt(),
            Some(TokenKind::Continue) => {
                self.expect(&TokenKind::Continue)?;
                self.expect(&TokenKind::Semicolon)?;

                Ok(Stmt::new(StmtKind::Continue))
            }
            Some(TokenKind::Break) => {
                self.expect(&TokenKind::Break)?;
                self.expect(&TokenKind::Semicolon)?;

                Ok(Stmt::new(StmtKind::Break))
            }
            Some(TokenKind::Fn) => Ok(Stmt::new(StmtKind::Item(self.parse_function_item(false)?))),
            Some(_) => {
                let expr = Stmt::new(StmtKind::Expr(self.parse_expr(Precedence::default())?));

                self.expect(&TokenKind::Semicolon)?;

                Ok(expr)
            }
            None => {
                self.expected(&[
                    &TokenKind::Return,
                    &TokenKind::If,
                    &TokenKind::While,
                    &TokenKind::For,
                    &TokenKind::Let,
                    &TokenKind::Continue,
                    &TokenKind::Break,
                ]);

                Err(())
            }
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Block, ()> {
        let mut stmts = Vec::new();

        let open_brace = self.expect(&TokenKind::LBrace)?;

        while !self.cur_token_is(&TokenKind::RBrace) {
            if let Ok(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }
        }

        let close_brace = self.expect(&TokenKind::RBrace)?;

        Ok(Block {
            open_brace,
            stmts,
            close_brace,
        })
    }

    // This function is used only by macro expansion
    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();

        while self.cur_token.is_some() {
            if let Ok(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            }
        }

        Ok(stmts)
    }

    fn parse_type(&mut self) -> Result<Ty, ()> {
        let ty = match self.cur_token.as_ref().map(|token| &token.kind) {
            Some(TokenKind::Asterisk) => Ty::Ptr(Box::new(self.parse_type()?)),
            Some(TokenKind::LBracket) => {
                self.bump();

                match &self.cur_token {
                    Some(Token {
                        kind: TokenKind::Integer(int),
                        span,
                    }) => {
                        let length: usize = match str::parse(int) {
                            Ok(value) => value,
                            Err(_) => {
                                let span = span.clone();

                                self.bump();
                                self.diag.error(Diagnostic::IntegerLitralTooLong, span);

                                return Err(());
                            }
                        };

                        self.bump();
                        self.expect(&TokenKind::RBracket)?;

                        return Ok(Ty::Array {
                            ty: Box::new(self.parse_type()?),
                            len: length,
                        });
                    }
                    _ => {
                        self.expected(&[&TokenKind::Integer(Default::default())]);
                        self.bump();

                        return Err(())?;
                    }
                }
            }
            Some(TokenKind::U8) => Ty::UInt(UintTy::U8),
            Some(TokenKind::U16) => Ty::UInt(UintTy::U16),
            Some(TokenKind::U32) => Ty::UInt(UintTy::U32),
            Some(TokenKind::U64) => Ty::UInt(UintTy::U64),
            Some(TokenKind::I8) => Ty::Int(IntTy::I8),
            Some(TokenKind::I16) => Ty::Int(IntTy::I16),
            Some(TokenKind::I32) => Ty::Int(IntTy::I32),
            Some(TokenKind::I64) => Ty::Int(IntTy::I64),
            Some(TokenKind::Usize) => Ty::UInt(UintTy::Usize),
            Some(TokenKind::Isize) => Ty::Int(IntTy::Isize),
            Some(TokenKind::Bool) => Ty::Bool,
            Some(TokenKind::Void) => Ty::Void,
            Some(TokenKind::Ident(ident)) => Ty::Ident(ident.clone()),
            Some(TokenKind::Fn) => {
                self.bump();
                self.expect(&TokenKind::LParen)?;

                let mut params = Vec::new();

                while !self.cur_token_is(&TokenKind::RParen) {
                    params.push(self.parse_type()?);

                    if !self.cur_token_is(&TokenKind::RParen) {
                        self.expect(&TokenKind::Comma)?;
                    }
                }

                self.expect(&TokenKind::RParen)?;
                self.expect(&TokenKind::Arrow)?;

                return Ok(Ty::Fn(params, Box::new(self.parse_type()?)));
            }
            _ => {
                self.diag.error(
                    Diagnostic::ParseExpected("type".to_string()),
                    self.cur_token
                        .as_ref()
                        .map(|token| &token.span)
                        .unwrap_or_else(|| {
                            self.prev_token.as_ref().map(|token| &token.span).unwrap()
                        })
                        .clone(),
                );
                self.bump();

                return Err(());
            }
        };

        self.bump();

        Ok(ty)
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::Return)?;

        let expr = if !self.cur_token_is(&TokenKind::Semicolon) {
            Some(self.parse_expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::new(StmtKind::Return(expr)))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::If)?;

        let condition = self.parse_expr(Precedence::default())?;
        let consequence = self.parse_block_stmt()?;
        let alternative = if self.cur_token_is(&TokenKind::Else) {
            self.expect(&TokenKind::Else)?;

            Some(self.parse_block_stmt()?)
        } else {
            None
        };

        Ok(Stmt::new(StmtKind::If {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::While)?;

        let condition = self.parse_expr(Precedence::default())?;
        let block = self.parse_block_stmt()?;

        Ok(Stmt::new(StmtKind::While { condition, block }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::For)?;

        let initializer = if self.cur_token_is(&TokenKind::Semicolon) {
            None
        } else {
            let stmt = if self.cur_token_is(&TokenKind::Let) {
                self.parse_local_stmt()?
            } else {
                Stmt::new(StmtKind::Expr(self.parse_expr(Precedence::default())?))
            };

            Some(stmt)
        };

        let condition = if self.cur_token_is(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expr(Precedence::default())?)
        };
        self.expect(&TokenKind::Semicolon)?;

        let increment = if self.cur_token_is(&TokenKind::LBrace) {
            None
        } else {
            Some(self.parse_expr(Precedence::default())?)
        };

        let block = self.parse_block_stmt()?;

        Ok(Stmt::new(StmtKind::For {
            initializer: initializer.map(|initializer| Box::new(initializer)),
            condition,
            increment,
            block,
        }))
    }

    fn parse_local_stmt(&mut self) -> Result<Stmt, ()> {
        self.expect(&TokenKind::Let)?;

        let (name, _) = self.parse_ident()?;
        let ty = if self.cur_token_is(&TokenKind::Colon) {
            self.expect(&TokenKind::Colon)?;

            self.parse_type()?
        } else {
            Ty::Infer
        };

        let expr = if self.cur_token_is(&TokenKind::Assign) {
            self.expect(&TokenKind::Assign)?;

            Some(self.parse_expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::new(StmtKind::Local(Variable {
            name,
            ty,
            value: expr,
        })))
    }

    fn parse_global_item(&mut self) -> Result<Item, ()> {
        self.expect(&TokenKind::Let)?;

        let (name, _) = self.parse_ident()?;
        self.expect(&TokenKind::Colon)?;

        let ty = self.parse_type()?;

        let expr = if self.cur_token_is(&TokenKind::Assign) {
            self.expect(&TokenKind::Assign)?;

            Some(self.parse_expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&TokenKind::Semicolon)?;

        Ok(Item::new(ItemKind::Global(Variable {
            name,
            ty,
            value: expr,
        })))
    }

    fn parse_function_item(&mut self, func_definition: bool) -> Result<Item, ()> {
        self.expect(&TokenKind::Fn)?;
        let (name, _) = self.parse_ident()?;
        self.expect(&TokenKind::LParen)?;
        let params = self.parse_params(TokenKind::Comma, TokenKind::RParen)?;
        self.expect(&TokenKind::Arrow)?;

        let ty = self.parse_type()?;
        let block = if self.cur_token_is(&TokenKind::LBrace) {
            Some(self.parse_block_stmt()?)
        } else {
            None
        };

        if let Some(block) = &block {
            if !func_definition {
                self.diag.error(
                    Diagnostic::IllegalFunctionDefinition,
                    block.open_brace.clone(),
                );

                return Err(());
            }
        } else {
            self.expect(&TokenKind::Semicolon)?;
        }

        Ok(Item::new(ItemKind::Fn {
            ret_ty: ty,
            name,
            params,
            block,
        }))
    }

    fn parse_params(&mut self, delim: TokenKind, end: TokenKind) -> Result<Vec<(String, Ty)>, ()> {
        let mut params = Vec::new();

        while !self.cur_token_is(&end) {
            let (name, span) = self.parse_ident()?;
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;

            if params.iter().any(|(field_name, _)| field_name == &name) {
                self.diag.error(Diagnostic::RepeatingParam(name), span);
            } else {
                params.push((name, ty));
            }

            if !self.cur_token_is(&end) {
                self.expect(&delim)?;
            }
        }

        self.expect(&end)?;

        Ok(params)
    }

    fn parse_ident_expr(&mut self) -> Result<Expr, ()> {
        match self.peek_token {
            Some(Token {
                kind: TokenKind::LBrace,
                ..
            }) => self.parse_struct_expr(),
            _ => {
                let (ident, span) = self.parse_ident()?;

                Ok(Expr::new(ExprKind::Ident(ident), span))
            }
        }
    }

    fn parse_struct_expr(&mut self) -> Result<Expr, ()> {
        let (name, start) = self.parse_ident()?;
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();

        while !self.cur_token_is(&TokenKind::RBrace) {
            let (field, _) = self.parse_ident()?;
            self.expect(&TokenKind::Colon)?;
            let expr = self.parse_expr(Precedence::Lowest)?;

            if !self.cur_token_is(&TokenKind::RBrace) {
                self.expect(&TokenKind::Comma)?;
            }

            fields.push((field, expr));
        }

        let end = self.expect(&TokenKind::RBrace)?;

        Ok(Expr::new(ExprKind::Struct { name, fields }, start.to(end)))
    }

    fn parse_string_lit_expr(&mut self) -> Result<Expr, ()> {
        match self.cur_token.clone() {
            Some(Token {
                kind: TokenKind::String(literal),
                span,
            }) => {
                self.bump();

                Ok(Expr::new(ExprKind::Lit(ExprLit::String(literal)), span))
            }
            _ => {
                self.expected(&[&TokenKind::String(Default::default())]);
                self.bump();

                Err(())
            }
        }
    }

    fn parse_int_lit_expr(&mut self) -> Result<Expr, ()> {
        let (lit, span) = self.parse_int_lit()?;

        Ok(Expr::new(ExprKind::Lit(ExprLit::UInt(lit)), span))
    }

    fn parse_null_expr(&mut self) -> Result<Expr, ()> {
        let span = self.expect(&TokenKind::Null)?;

        Ok(Expr::new(ExprKind::Lit(ExprLit::Null), span))
    }

    fn parse_bool_expr(&mut self) -> Result<Expr, ()> {
        match self.cur_token.as_ref().map(|token| &token.kind) {
            Some(TokenKind::True) => {
                let span = self.expect(&TokenKind::True)?;

                Ok(Expr::new(ExprKind::Lit(ExprLit::Bool(true)), span))
            }
            Some(TokenKind::False) => {
                let span = self.expect(&TokenKind::True)?;

                Ok(Expr::new(ExprKind::Lit(ExprLit::Bool(false)), span))
            }
            _ => {
                self.expected(&[&TokenKind::True, &TokenKind::False]);
                self.bump();

                Err(())
            }
        }
    }

    fn parse_call_expr(&mut self, left: Expr) -> Result<Expr, ()> {
        self.expect(&TokenKind::LParen)?;

        let (args, end) = self.parse_expr_list()?;
        let span = left.span.clone();

        Ok(Expr::new(
            ExprKind::FunctionCall {
                expr: Box::new(left),
                arguments: args,
            },
            span.clone().to(end),
        ))
    }

    fn parse_macro_call_expr(&mut self, left: Expr) -> Result<Expr, ()> {
        let (name, start) = match left {
            Expr {
                kind: ExprKind::Ident(expr),
                span,
                ..
            } => (expr, span),
            _ => panic!("Macro name can only be a string"),
        };

        self.expect(&TokenKind::Bang)?;
        self.expect(&TokenKind::LParen)?;

        let mut tokens = Vec::new();

        while !self.cur_token_is(&TokenKind::RParen) {
            tokens.push(self.cur_token.clone().ok_or_else(|| {
                self.expected(&[&TokenKind::RParen]);

                ()
            })?);
            self.bump();
        }

        let end = self.expect(&TokenKind::RParen)?;

        Ok(Expr::new(
            ExprKind::MacroCall { name, tokens },
            start.to(end),
        ))
    }

    fn parse_bin_expr(&mut self, left: Expr) -> Result<Expr, ()> {
        let Token { kind, span } = self.cur_token_unchecked();
        self.bump();

        // NOTE: assignment expression is right-associative
        let precedence = if let &TokenKind::Assign = &kind {
            Precedence::from(&kind).lower()
        } else {
            Precedence::from(&kind)
        };
        let right = self.parse_expr(precedence)?;
        let right_span = right.span.clone();
        let op = BinOp::try_from(&kind).map_err(|_| {
            self.diag
                .error(Diagnostic::ExpressionInfix(kind), span.clone());
        })?;

        Ok(Expr::new(
            ExprKind::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            },
            span.to(right_span.clone()),
        ))
    }

    fn parse_pointer_access_expr(&mut self, left: Expr) -> Result<Expr, ()> {
        let start = self.expect(&TokenKind::Arrow)?;

        let (field, end) = self.parse_ident()?;

        Ok(Expr::new(
            ExprKind::Field {
                expr: Box::new(Expr::new(
                    ExprKind::Unary {
                        op: UnOp::Deref,
                        expr: Box::new(left),
                    },
                    Span::DUMMY,
                )),
                field,
            },
            start.to(end),
        ))
    }

    fn parse_struct_access_expr(&mut self, expr: Expr) -> Result<Expr, ()> {
        let span = expr.span.clone();
        self.expect(&TokenKind::Period)?;

        if self.peek_token_is(&TokenKind::LParen) {
            let (method, _) = self.parse_ident()?;

            self.expect(&TokenKind::LParen)?;
            let (arguments, end) = self.parse_expr_list()?;

            Ok(Expr::new(
                ExprKind::StructMethod {
                    expr: Box::new(expr),
                    method,
                    arguments,
                },
                span.clone().to(end),
            ))
        } else {
            let (ident, end) = self.parse_ident()?;

            Ok(Expr::new(
                ExprKind::Field {
                    expr: Box::new(expr),
                    field: ident,
                },
                span.clone().to(end),
            ))
        }
    }

    fn parse_array_access_expr(&mut self, expr: Expr) -> Result<Expr, ()> {
        self.expect(&TokenKind::LBracket)?;
        let index = self.parse_expr(Precedence::Access)?;
        let end = self.expect(&TokenKind::RBracket)?;
        let span = expr.span.clone();

        Ok(Expr::new(
            ExprKind::ArrayAccess {
                expr: Box::new(expr),
                index: Box::new(index),
            },
            span.clone().to(end),
        ))
    }

    fn parse_cast_expr(&mut self, expr: Expr) -> Result<Expr, ()> {
        let dummy = self.expect(&TokenKind::As)?;

        Ok(Expr::new(
            ExprKind::Cast {
                expr: Box::new(expr),
                //FIXME: types should also return span
                ty: self.parse_type()?,
            },
            dummy,
        ))
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ()> {
        let Token { kind, span } = self.cur_token_unchecked();
        self.bump();
        let op = UnOp::try_from(&kind).map_err(|_| {
            self.diag
                .error(Diagnostic::ExpressionInfix(kind), span.clone());
        })?;
        let expr = self.parse_expr(Precedence::Prefix)?;
        let expr_span = expr.span.clone();

        Ok(Expr::new(
            ExprKind::Unary {
                op,
                expr: Box::new(expr),
            },
            span.to(expr_span.clone()),
        ))
    }

    fn parse_expr_list(&mut self) -> Result<(Vec<Expr>, Span), ()> {
        let mut exprs = Vec::new();

        while !self.cur_token_is(&TokenKind::RParen) {
            exprs.push(self.parse_expr(Precedence::default())?);
            if !self.cur_token_is(&TokenKind::RParen) {
                self.expect(&TokenKind::Comma)?;
            }
        }

        let end = self.expect(&TokenKind::RParen)?;

        Ok((exprs, end))
    }

    fn parse_grouped_expr(&mut self) -> Result<Expr, ()> {
        self.expect(&TokenKind::LParen)?;
        let expr = self.parse_expr(Precedence::default())?;
        self.expect(&TokenKind::RParen)?;

        Ok(expr)
    }

    fn parse_array_expr(&mut self) -> Result<Expr, ()> {
        let start = self.expect(&TokenKind::LBracket)?;
        let mut items = Vec::new();

        while !self.cur_token_is(&TokenKind::RBracket) {
            items.push(self.parse_expr(Precedence::default())?);

            if !self.cur_token_is(&TokenKind::RBracket) {
                self.expect(&TokenKind::Comma)?;
            }
        }

        let end = self.expect(&TokenKind::RBracket)?;

        Ok(Expr::new(ExprKind::Array(items), start.to(end)))
    }

    fn parse_int_lit(&mut self) -> Result<(u64, Span), ()> {
        match &self.cur_token {
            Some(Token {
                kind: TokenKind::Integer(num_str),
                span,
            }) => {
                let span = span.clone();
                let lit = match num_str.parse() {
                    Ok(value) => value,
                    Err(_) => {
                        let span = span.clone();

                        self.bump();
                        self.diag.error(Diagnostic::IntegerLitralTooLong, span);

                        return Err(());
                    }
                };
                self.bump();

                Ok((lit, span))
            }
            _ => {
                self.expected(&[&TokenKind::Integer(Default::default())]);
                self.bump();

                Err(())
            }
        }
    }

    fn parse_ident(&mut self) -> Result<(String, Span), ()> {
        match self.cur_token.clone() {
            Some(Token {
                kind: TokenKind::Ident(ident),
                span,
            }) => {
                self.bump();

                Ok((ident, span))
            }
            _ => {
                self.expected(&[&TokenKind::Ident(Default::default())]);
                self.bump();

                Err(())
            }
        }
    }

    #[inline]
    fn cur_token_unchecked(&self) -> Token {
        self.cur_token.clone().unwrap()
    }

    #[inline]
    fn cur_token_ref_unchecked(&self) -> &Token {
        self.cur_token.as_ref().unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        ast::{BinOp, ExprKind, ExprLit, IntTy, Stmt, Ty, UintTy, UnOp, Variable},
        diagnostics::Diagnostics,
        lexer::Lexer,
    };

    #[test]
    fn parse_arithmetic_expression() {
        //let tests = [
        //    (
        //        "
        //        {
        //            1 * 2 + 3 / (4 + 1 as u8);
        //        }
        //        ",
        //        vec![Stmt::Expr(ExprKind::Binary {
        //            op: BinOp::Add,
        //            left: Box::new(ExprKind::Binary {
        //                op: BinOp::Mul,
        //                left: Box::new(ExprKind::Lit(ExprLit::UInt(1))),
        //                right: Box::new(ExprKind::Lit(ExprLit::UInt(2))),
        //            }),
        //            right: Box::new(ExprKind::Binary {
        //                op: BinOp::Div,
        //                left: Box::new(ExprKind::Lit(ExprLit::UInt(3))),
        //                right: Box::new(ExprKind::Binary {
        //                    op: BinOp::Add,
        //                    left: Box::new(ExprKind::Lit(ExprLit::UInt(4))),
        //                    right: Box::new(ExprKind::Cast {
        //                        ty: Ty::UInt(UintTy::U8),
        //                        expr: Box::new(ExprKind::Lit(ExprLit::UInt(1))),
        //                    }),
        //                }),
        //            }),
        //        })],
        //    ),
        //    (
        //        "
        //        {
        //            let foo: u8;
        //            foo = -1 as u8 + 5;
        //        }
        //        ",
        //        vec![
        //            Stmt::Local(Variable {
        //                name: "foo".to_owned(),
        //                ty: Ty::UInt(UintTy::U8),
        //                value: None,
        //            }),
        //            Stmt::Expr(ExprKind::Binary {
        //                op: BinOp::Assign,
        //                left: Box::new(ExprKind::Ident("foo".to_owned())),
        //                right: Box::new(ExprKind::Binary {
        //                    op: BinOp::Add,
        //                    left: Box::new(ExprKind::Cast {
        //                        ty: Ty::UInt(UintTy::U8),
        //                        expr: Box::new(ExprKind::Unary {
        //                            op: UnOp::Negative,
        //                            expr: Box::new(ExprKind::Lit(ExprLit::UInt(1))),
        //                        }),
        //                    }),
        //                    right: Box::new(ExprKind::Lit(ExprLit::UInt(5))),
        //                }),
        //            }),
        //        ],
        //    ),
        //    (
        //        "
        //        {
        //            let foo: u8;
        //            let bar: i8;
        //            bar = foo as i8 + 5 / 10;
        //        }
        //        ",
        //        vec![
        //            Stmt::Local(Variable {
        //                name: "foo".to_owned(),
        //                ty: Ty::UInt(UintTy::U8),
        //                value: None,
        //            }),
        //            Stmt::Local(Variable {
        //                name: "bar".to_owned(),
        //                ty: Ty::Int(IntTy::I8),
        //                value: None,
        //            }),
        //            Stmt::Expr(ExprKind::Binary {
        //                op: BinOp::Assign,
        //                left: Box::new(ExprKind::Ident("bar".to_owned())),
        //                right: Box::new(ExprKind::Binary {
        //                    op: BinOp::Add,
        //                    left: Box::new(ExprKind::Cast {
        //                        ty: Ty::Int(IntTy::I8),
        //                        expr: Box::new(ExprKind::Ident("foo".to_owned())),
        //                    }),
        //                    right: Box::new(ExprKind::Binary {
        //                        op: BinOp::Div,
        //                        left: Box::new(ExprKind::Lit(ExprLit::UInt(5))),
        //                        right: Box::new(ExprKind::Lit(ExprLit::UInt(10))),
        //                    }),
        //                }),
        //            }),
        //        ],
        //    ),
        //    (
        //        "
        //        {
        //            1 as i8 + 2 / 3;
        //        }
        //        ",
        //        vec![Stmt::Expr(ExprKind::Binary {
        //            op: BinOp::Add,
        //            left: Box::new(ExprKind::Cast {
        //                ty: Ty::Int(IntTy::I8),
        //                expr: Box::new(ExprKind::Lit(ExprLit::UInt(1))),
        //            }),
        //            right: Box::new(ExprKind::Binary {
        //                op: BinOp::Div,
        //                left: Box::new(ExprKind::Lit(ExprLit::UInt(2))),
        //                right: Box::new(ExprKind::Lit(ExprLit::UInt(3))),
        //            }),
        //        })],
        //    ),
        //    (
        //        "
        //        {
        //            let a: u8;
        //            let b: u8;

        //            a = b = 69;
        //        }
        //        ",
        //        vec![
        //            Stmt::Local(Variable {
        //                name: "a".to_owned(),
        //                ty: Ty::UInt(UintTy::U8),
        //                value: None,
        //            }),
        //            Stmt::Local(Variable {
        //                name: "b".to_owned(),
        //                ty: Ty::UInt(UintTy::U8),
        //                value: None,
        //            }),
        //            Stmt::Expr(ExprKind::Binary {
        //                op: BinOp::Assign,
        //                left: Box::new(ExprKind::Ident("a".to_owned())),
        //                right: Box::new(ExprKind::Binary {
        //                    op: BinOp::Assign,
        //                    left: Box::new(ExprKind::Ident("b".to_owned())),
        //                    right: Box::new(ExprKind::Lit(ExprLit::UInt(69))),
        //                }),
        //            }),
        //        ],
        //    ),
        //];

        //for (input, expected) in tests {
        //    let mut diagnostics = Diagnostics::new(input);
        //    let mut parser = Parser::new(Lexer::new(input), &mut diagnostics);
        //    let ast = parser.parse_block_stmt().unwrap();

        //    assert_eq!(
        //        &ast.stmts, &expected,
        //        "expected: {:?}, got: {:?}",
        //        expected, ast
        //    );
        //}
    }
}
