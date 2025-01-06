mod error;
mod item;
mod op;
mod precedence;
mod stmt;
mod types;

pub mod expr;

use crate::{
    diagnostics::{Diagnostic, Diagnostics},
    lexer::{span::Span, Token},
};
pub use error::{Error, TyError};
pub use expr::*;
pub use item::{Item, ItemFn, ItemStruct};
pub use op::{BinOp, BitwiseOp, CmpOp, OpParseError, UnOp};
pub use precedence::Precedence;
use std::collections::HashMap;
pub use stmt::{Stmt, StmtFor, StmtIf, StmtReturn, StmtWhile};
pub use types::{IntTy, Ty, UintTy};

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: Ty,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Stmt>);

type PrefixFn<'a, 'src, T> = fn(&mut Parser<'a, 'src, T>) -> Result<Expr, Error>;
type InfixFn<'a, 'src, T> = fn(&mut Parser<'a, 'src, T>, left: Expr) -> Result<Expr, Error>;

pub struct Parser<'a, 'src, T: Iterator<Item = Result<Token, Span>>> {
    lexer: T,
    diag: &'a mut Diagnostics<'src>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    prefix_fns: HashMap<Token, PrefixFn<'a, 'src, T>>,
    infix_fns: HashMap<Token, InfixFn<'a, 'src, T>>,
}

impl<'a, 'src, T: Iterator<Item = Result<Token, Span>>> Parser<'a, 'src, T> {
    pub fn new(lexer: T, diag: &'a mut Diagnostics<'src>) -> Result<Self, Error> {
        let mut parser = Self {
            cur_token: None,
            peek_token: None,
            lexer,
            diag,
            prefix_fns: HashMap::from([
                (Token::Ident(Default::default()), Self::ident as PrefixFn<T>),
                (Token::String(Default::default()), Self::string_lit),
                (Token::Integer(Default::default()), Self::int_lit),
                (Token::Null, Self::null),
                (Token::True, Self::bool),
                (Token::False, Self::bool),
                (Token::Minus, Self::unary_expr),
                (Token::Bang, Self::unary_expr),
                (Token::LParen, Self::grouped_expr),
                (Token::Ampersand, Self::unary_expr),
                (Token::Asterisk, Self::unary_expr),
                (Token::Tilde, Self::unary_expr),
                (Token::LBracket, Self::array_expr),
            ]),
            infix_fns: HashMap::from([
                (Token::Plus, Self::bin_expr as InfixFn<T>),
                (Token::Minus, Self::bin_expr),
                (Token::Asterisk, Self::bin_expr),
                (Token::Slash, Self::bin_expr),
                (Token::Assign, Self::bin_expr),
                (Token::LessThan, Self::bin_expr),
                (Token::LessEqual, Self::bin_expr),
                (Token::GreaterThan, Self::bin_expr),
                (Token::GreaterEqual, Self::bin_expr),
                (Token::Equal, Self::bin_expr),
                (Token::NotEqual, Self::bin_expr),
                (Token::And, Self::bin_expr),
                (Token::Or, Self::bin_expr),
                (Token::LParen, Self::bin_expr),
                (Token::Ampersand, Self::bin_expr),
                (Token::Bar, Self::bin_expr),
                (Token::Shl, Self::bin_expr),
                (Token::Shr, Self::bin_expr),
                (Token::Arrow, Self::pointer_access),
                (Token::Period, Self::struct_access),
                (Token::LBracket, Self::array_access),
                (Token::As, Self::cast_expr),
                (Token::LParen, Self::func_call_expr),
                (Token::Bang, Self::macro_call_expr),
            ]),
        };

        parser.bump()?;
        parser.bump()?;

        Ok(parser)
    }

    fn bump(&mut self) -> Result<(), Error> {
        match self.lexer.next().transpose() {
            Ok(mut token) => {
                std::mem::swap(&mut self.cur_token, &mut self.peek_token);
                std::mem::swap(&mut token, &mut self.peek_token);

                Ok(())
            }
            Err(span) => {
                self.diag.error(Diagnostic::UnknownChar, span);

                self.bump()
            }
        }
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        self.cur_token.as_ref() == Some(token)
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        self.peek_token.as_ref() == Some(token)
    }

    fn expect(&mut self, token: &Token) -> Result<(), Error> {
        match &self.cur_token {
            Some(cur) if cur == token => {
                self.bump()?;

                Ok(())
            }
            Some(cur) => Err(Error::UnexpectedToken(token.to_owned(), cur.clone())),
            None => Err(Error::Expected(token.to_owned())),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();

        while let Some(token) = &self.cur_token {
            let item = match token {
                Token::Struct => self.parse_struct()?,
                Token::Let => self.global()?,
                Token::Fn => self.function(true)?,
                _ => unreachable!(),
            };
            items.push(item);
        }

        Ok(items)
    }

    pub fn expr(&mut self, precedence: Precedence) -> Result<Expr, Error> {
        let token = self.cur_token.clone().unwrap();

        let mut left = match self.prefix_fns.get(&token) {
            Some(func) => func(self),
            None => {
                return Err(Error::Prefix(token));
            }
        };

        while !self.cur_token_is(&Token::Semicolon)
            && self.cur_token.is_some()
            && precedence < Precedence::from(self.cur_token.as_ref().unwrap())
        {
            left = match self.infix_fns.get(self.cur_token.as_ref().unwrap()) {
                Some(func) => func(self, left?),
                None => {
                    return Err(Error::Infix(self.cur_token.clone().unwrap()));
                }
            };
        }

        left
    }

    fn parse_struct(&mut self) -> Result<Item, Error> {
        self.expect(&Token::Struct)?;

        let name = self.parse_ident()?;

        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            if self.cur_token_is(&Token::Fn) {
                // Handle struct methods here
            } else {
                let name = self.parse_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;

                match fields.iter().find(|(field_name, _)| field_name == &name) {
                    Some(_) => todo!("Don't know yet what error to return"),
                    None => fields.push((name, ty)),
                };

                if !self.cur_token_is(&Token::RBrace) {
                    self.expect(&Token::Semicolon)?;
                }
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Item::Struct(ItemStruct { name, fields }))
    }

    fn stmt(&mut self) -> Result<Stmt, Error> {
        match self.cur_token.as_ref().unwrap() {
            Token::Return => self.parse_return(),
            Token::If => self.if_stmt(),
            Token::While => self.while_stmt(),
            Token::For => self.for_stmt(),
            Token::Let => self.local(),
            Token::Continue => {
                self.expect(&Token::Continue)?;
                self.expect(&Token::Semicolon)?;

                Ok(Stmt::Continue)
            }
            Token::Break => {
                self.expect(&Token::Break)?;
                self.expect(&Token::Semicolon)?;

                Ok(Stmt::Break)
            }
            Token::Fn => Ok(Stmt::Item(self.function(true)?)),
            _ => {
                let expr = Stmt::Expr(self.expr(Precedence::default())?);

                self.expect(&Token::Semicolon)?;

                Ok(expr)
            }
        }
    }

    fn compound_statement(&mut self) -> Result<Block, Error> {
        let mut stmts = Vec::new();

        self.expect(&Token::LBrace)?;

        while !self.cur_token_is(&Token::RBrace) {
            stmts.push(self.stmt()?);
        }

        self.expect(&Token::RBrace)?;

        Ok(Block(stmts))
    }

    // This function is used only by macro expansion
    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = Vec::new();

        while self.cur_token.is_some() {
            stmts.push(self.stmt()?);
        }

        Ok(stmts)
    }

    fn parse_type(&mut self) -> Result<Ty, Error> {
        let ty = match &self.cur_token {
            Some(Token::Asterisk) => Ty::Ptr(Box::new(self.parse_type()?)),
            Some(Token::LBracket) => {
                self.bump()?;

                match &self.cur_token {
                    Some(Token::Integer(int)) => {
                        let length: usize = str::parse(int).unwrap();
                        self.expect(&Token::RBracket)?;

                        Ty::Array {
                            ty: Box::new(self.parse_type()?),
                            len: length,
                        }
                    }
                    token => panic!("Expected integer, got {token:?}"),
                }
            }
            Some(Token::U8) => Ty::UInt(UintTy::U8),
            Some(Token::U16) => Ty::UInt(UintTy::U16),
            Some(Token::U32) => Ty::UInt(UintTy::U32),
            Some(Token::U64) => Ty::UInt(UintTy::U64),
            Some(Token::I8) => Ty::Int(IntTy::I8),
            Some(Token::I16) => Ty::Int(IntTy::I16),
            Some(Token::I32) => Ty::Int(IntTy::I32),
            Some(Token::I64) => Ty::Int(IntTy::I64),
            Some(Token::Usize) => Ty::UInt(UintTy::Usize),
            Some(Token::Isize) => Ty::Int(IntTy::Isize),
            Some(Token::Bool) => Ty::Bool,
            Some(Token::Void) => Ty::Void,
            Some(Token::Ident(ident)) => Ty::Ident(ident.clone()),
            Some(Token::Fn) => {
                self.bump()?;
                self.expect(&Token::LParen)?;

                let mut params = Vec::new();

                while !self.cur_token_is(&Token::RParen) {
                    params.push(self.parse_type()?);

                    if !self.cur_token_is(&Token::RParen) {
                        self.expect(&Token::Comma)?;
                    }
                }

                self.expect(&Token::RParen)?;
                self.expect(&Token::Arrow)?;

                return Ok(Ty::Fn(params, Box::new(self.parse_type()?)));
            }
            token => return Err(Error::ParseType(token.clone().unwrap())),
        };

        self.bump()?;

        Ok(ty)
    }

    fn parse_return(&mut self) -> Result<Stmt, Error> {
        self.expect(&Token::Return)?;

        let expr = if !self.cur_token_is(&Token::Semicolon) {
            Some(self.expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::Return(StmtReturn { expr }))
    }

    fn if_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(&Token::If)?;

        let condition = self.expr(Precedence::default())?;
        let consequence = self.compound_statement()?;
        let alternative = if self.cur_token_is(&Token::Else) {
            self.expect(&Token::Else)?;

            Some(self.compound_statement()?)
        } else {
            None
        };

        Ok(Stmt::If(StmtIf {
            condition,
            consequence,
            alternative,
        }))
    }

    fn while_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(&Token::While)?;

        let condition = self.expr(Precedence::default())?;
        let block = self.compound_statement()?;

        Ok(Stmt::While(StmtWhile { condition, block }))
    }

    fn for_stmt(&mut self) -> Result<Stmt, Error> {
        self.expect(&Token::For)?;

        let initializer = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            let stmt = if self.cur_token_is(&Token::Let) {
                self.local()?
            } else {
                Stmt::Expr(self.expr(Precedence::default())?)
            };

            Some(stmt)
        };

        let condition = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            Some(self.expr(Precedence::default())?)
        };
        self.expect(&Token::Semicolon)?;

        let increment = if self.cur_token_is(&Token::LBrace) {
            None
        } else {
            Some(self.expr(Precedence::default())?)
        };

        let block = self.compound_statement()?;

        Ok(Stmt::For(StmtFor {
            initializer: initializer.map(|initializer| Box::new(initializer)),
            condition,
            increment,
            block,
        }))
    }

    fn local(&mut self) -> Result<Stmt, Error> {
        self.expect(&Token::Let)?;

        let name = self.parse_ident()?;
        let ty = if self.cur_token_is(&Token::Colon) {
            self.expect(&Token::Colon)?;

            self.parse_type()?
        } else {
            Ty::Infer
        };

        let expr = if self.cur_token_is(&Token::Assign) {
            self.expect(&Token::Assign)?;

            Some(self.expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::Local(Variable {
            name,
            ty,
            value: expr,
        }))
    }

    fn global(&mut self) -> Result<Item, Error> {
        self.expect(&Token::Let)?;

        let name = self.parse_ident()?;
        self.expect(&Token::Colon)?;

        let ty = self.parse_type()?;

        let expr = if self.cur_token_is(&Token::Assign) {
            self.expect(&Token::Assign)?;

            Some(self.expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Ok(Item::Global(Variable {
            name,
            ty,
            value: expr,
        }))
    }

    fn function(&mut self, func_definition: bool) -> Result<Item, Error> {
        self.expect(&Token::Fn)?;

        let name = self.parse_ident()?;

        self.expect(&Token::LParen)?;

        let params = self.params(Token::Comma, Token::RParen)?;
        self.expect(&Token::Arrow)?;

        let type_ = self.parse_type()?;
        let block = if self.cur_token_is(&Token::LBrace) {
            Some(self.compound_statement()?)
        } else {
            None
        };

        if block.is_some() & !func_definition {
            panic!("Function definition is not supported here");
        }

        if block.is_none() {
            self.expect(&Token::Semicolon)?;
        }

        Ok(Item::Fn(ItemFn {
            ret_ty: type_,
            name,
            params,
            block,
        }))
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<Vec<(String, Ty)>, Error> {
        let mut params = Vec::new();

        while !self.cur_token_is(&end) {
            let name = self.parse_ident()?;
            self.expect(&Token::Colon)?;
            let type_ = self.parse_type()?;

            match params.iter().find(|(field_name, _)| field_name == &name) {
                Some(_) => todo!("Don't know yet what error to return"),
                None => params.push((name, type_)),
            };

            if !self.cur_token_is(&end) {
                self.expect(&delim)?;
            }
        }

        self.expect(&end)?;

        Ok(params)
    }

    fn ident(&mut self) -> Result<Expr, Error> {
        match self.peek_token {
            Some(Token::LBrace) => self.struct_expr(),
            _ => Ok(Expr::Ident(ExprIdent(self.parse_ident()?))),
        }
    }

    fn struct_expr(&mut self) -> Result<Expr, Error> {
        let name = self.parse_ident()?;

        self.expect(&Token::LBrace)?;
        let mut fields = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            match self.cur_token.clone() {
                Some(Token::Ident(field)) => {
                    self.bump()?;
                    self.expect(&Token::Colon)?;
                    let expr = self.expr(Precedence::Lowest)?;

                    if !self.cur_token_is(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }

                    fields.push((field, expr));
                }
                _ => todo!("Don't know what error to return yet"),
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Expr::Struct(ExprStruct { name, fields }))
    }

    fn string_lit(&mut self) -> Result<Expr, Error> {
        match &self.cur_token {
            Some(Token::String(literal)) => {
                let lit = literal.clone();
                self.bump()?;

                Ok(Expr::Lit(ExprLit::String(lit)))
            }
            _ => unreachable!(),
        }
    }

    fn int_lit(&mut self) -> Result<Expr, Error> {
        match &self.cur_token {
            Some(Token::Integer(num_str)) => {
                let lit = num_str.parse().unwrap();
                self.bump()?;

                Ok(Expr::Lit(ExprLit::UInt(lit)))
            }
            _ => unreachable!(),
        }
    }

    fn null(&mut self) -> Result<Expr, Error> {
        self.expect(&Token::Null)?;

        Ok(Expr::Lit(ExprLit::Null))
    }

    fn bool(&mut self) -> Result<Expr, Error> {
        match &self.cur_token {
            Some(Token::True) => {
                self.bump()?;

                Ok(Expr::Lit(ExprLit::Bool(true)))
            }
            Some(Token::False) => {
                self.bump()?;

                Ok(Expr::Lit(ExprLit::Bool(false)))
            }
            _ => unreachable!(),
        }
    }

    fn func_call_expr(&mut self, left: Expr) -> Result<Expr, Error> {
        self.expect(&Token::LParen)?;

        let args = self.expr_list()?;

        Ok(Expr::FunctionCall(ExprFunctionCall {
            expr: Box::new(left),
            arguments: args,
        }))
    }

    fn macro_call_expr(&mut self, left: Expr) -> Result<Expr, Error> {
        let name = match left {
            Expr::Ident(expr) => expr.0,
            _ => panic!("Macro name can only be a string"),
        };

        self.expect(&Token::Bang)?;
        self.expect(&Token::LParen)?;

        let mut tokens = Vec::new();

        while !self.cur_token_is(&Token::RParen) {
            tokens.push(
                self.cur_token
                    .clone()
                    .ok_or(Error::Expected(Token::RParen))?,
            );
            self.bump()?;
        }

        self.expect(&Token::RParen)?;

        Ok(Expr::MacroCall(MacroCall { name, tokens }))
    }

    fn bin_expr(&mut self, left: Expr) -> Result<Expr, Error> {
        let token = self.cur_token.clone().unwrap();
        self.bump()?;

        // NOTE: assignment expression is right-associative
        let precedence = if let &Token::Assign = &token {
            Precedence::from(&token).lower()
        } else {
            Precedence::from(&token)
        };
        let right = self.expr(precedence)?;
        let op = BinOp::try_from(&token).map_err(|e| Error::Operator(e))?;

        Ok(Expr::Binary(ExprBinary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    fn pointer_access(&mut self, left: Expr) -> Result<Expr, Error> {
        self.expect(&Token::Arrow)?;

        let field = self.parse_ident()?;

        Ok(Expr::Field(ExprField {
            expr: Box::new(Expr::Unary(ExprUnary {
                op: UnOp::Deref,
                expr: Box::new(left),
            })),
            field,
        }))
    }

    fn struct_access(&mut self, expr: Expr) -> Result<Expr, Error> {
        self.expect(&Token::Period)?;

        if self.peek_token_is(&Token::LParen) {
            let method = self.parse_ident()?;

            self.expect(&Token::LParen)?;
            let arguments = self.expr_list()?;

            Ok(Expr::StructMethod(ExprStructMethod {
                expr: Box::new(expr),
                method,
                arguments,
            }))
        } else {
            Ok(Expr::Field(ExprField {
                expr: Box::new(expr),
                field: self.parse_ident()?,
            }))
        }
    }

    fn array_access(&mut self, expr: Expr) -> Result<Expr, Error> {
        self.expect(&Token::LBracket)?;
        let index = self.expr(Precedence::Access)?;
        self.expect(&Token::RBracket)?;

        Ok(Expr::ArrayAccess(ExprArrayAccess {
            expr: Box::new(expr),
            index: Box::new(index),
        }))
    }

    fn cast_expr(&mut self, expr: Expr) -> Result<Expr, Error> {
        self.expect(&Token::As)?;

        Ok(Expr::Cast(ExprCast {
            expr: Box::new(expr),
            ty: self.parse_type()?,
        }))
    }

    fn unary_expr(&mut self) -> Result<Expr, Error> {
        let token = self.cur_token.clone().unwrap();
        self.bump()?;
        let op = UnOp::try_from(&token).map_err(|e| Error::Operator(e))?;
        let expr = self.expr(Precedence::Prefix)?;

        Ok(Expr::Unary(ExprUnary {
            op,
            expr: Box::new(expr),
        }))
    }

    fn expr_list(&mut self) -> Result<Vec<Expr>, Error> {
        let mut exprs = Vec::new();

        while !self.cur_token_is(&Token::RParen) {
            exprs.push(self.expr(Precedence::default())?);
            if !self.cur_token_is(&Token::RParen) {
                self.expect(&Token::Comma)?;
            }
        }

        self.expect(&Token::RParen)?;

        Ok(exprs)
    }

    fn grouped_expr(&mut self) -> Result<Expr, Error> {
        self.expect(&Token::LParen)?;

        let expr = self.expr(Precedence::default())?;
        self.expect(&Token::RParen)?;

        Ok(expr)
    }

    fn array_expr(&mut self) -> Result<Expr, Error> {
        self.expect(&Token::LBracket)?;
        let mut items = Vec::new();

        while !self.cur_token_is(&Token::RBracket) {
            items.push(self.expr(Precedence::default())?);

            if !self.cur_token_is(&Token::RBracket) {
                self.expect(&Token::Comma)?;
            }
        }

        self.expect(&Token::RBracket)?;

        Ok(Expr::Array(ExprArray(items)))
    }

    fn parse_ident(&mut self) -> Result<String, Error> {
        match self.cur_token.clone() {
            Some(Token::Ident(ident)) => {
                self.bump()?;

                Ok(ident)
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        diagnostics::Diagnostics,
        lexer::Lexer,
        parser::{
            BinOp, Error, Expr, ExprBinary, ExprCast, ExprIdent, ExprLit, ExprUnary, IntTy, Stmt,
            Ty, UintTy, UnOp, Variable,
        },
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), Error> {
        let tests = [
            (
                "
                {
                    1 * 2 + 3 / (4 + 1 as u8);
                }
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Binary(ExprBinary {
                        op: BinOp::Mul,
                        left: Box::new(Expr::Lit(ExprLit::UInt(1))),
                        right: Box::new(Expr::Lit(ExprLit::UInt(2))),
                    })),
                    right: Box::new(Expr::Binary(ExprBinary {
                        op: BinOp::Div,
                        left: Box::new(Expr::Lit(ExprLit::UInt(3))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Lit(ExprLit::UInt(4))),
                            right: Box::new(Expr::Cast(ExprCast {
                                ty: Ty::UInt(UintTy::U8),
                                expr: Box::new(Expr::Lit(ExprLit::UInt(1))),
                            })),
                        })),
                    })),
                }))],
            ),
            (
                "
                {
                    let foo: u8;
                    foo = -1 as u8 + 5;
                }
                ",
                vec![
                    Stmt::Local(Variable {
                        name: "foo".to_owned(),
                        ty: Ty::UInt(UintTy::U8),
                        value: None,
                    }),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                ty: Ty::UInt(UintTy::U8),
                                expr: Box::new(Expr::Unary(ExprUnary {
                                    op: UnOp::Negative,
                                    expr: Box::new(Expr::Lit(ExprLit::UInt(1))),
                                })),
                            })),
                            right: Box::new(Expr::Lit(ExprLit::UInt(5))),
                        })),
                    })),
                ],
            ),
            (
                "
                {
                    let foo: u8;
                    let bar: i8;
                    bar = foo as i8 + 5 / 10;
                }
                ",
                vec![
                    Stmt::Local(Variable {
                        name: "foo".to_owned(),
                        ty: Ty::UInt(UintTy::U8),
                        value: None,
                    }),
                    Stmt::Local(Variable {
                        name: "bar".to_owned(),
                        ty: Ty::Int(IntTy::I8),
                        value: None,
                    }),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("bar".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                ty: Ty::Int(IntTy::I8),
                                expr: Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                            })),
                            right: Box::new(Expr::Binary(ExprBinary {
                                op: BinOp::Div,
                                left: Box::new(Expr::Lit(ExprLit::UInt(5))),
                                right: Box::new(Expr::Lit(ExprLit::UInt(10))),
                            })),
                        })),
                    })),
                ],
            ),
            (
                "
                {
                    1 as i8 + 2 / 3;
                }
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary {
                    op: BinOp::Add,
                    left: Box::new(Expr::Cast(ExprCast {
                        ty: Ty::Int(IntTy::I8),
                        expr: Box::new(Expr::Lit(ExprLit::UInt(1))),
                    })),
                    right: Box::new(Expr::Binary(ExprBinary {
                        op: BinOp::Div,
                        left: Box::new(Expr::Lit(ExprLit::UInt(2))),
                        right: Box::new(Expr::Lit(ExprLit::UInt(3))),
                    })),
                }))],
            ),
            (
                "
                {
                    let a: u8;
                    let b: u8;

                    a = b = 69;
                }
                ",
                vec![
                    Stmt::Local(Variable {
                        name: "a".to_owned(),
                        ty: Ty::UInt(UintTy::U8),
                        value: None,
                    }),
                    Stmt::Local(Variable {
                        name: "b".to_owned(),
                        ty: Ty::UInt(UintTy::U8),
                        value: None,
                    }),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("a".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Assign,
                            left: Box::new(Expr::Ident(ExprIdent("b".to_owned()))),
                            right: Box::new(Expr::Lit(ExprLit::UInt(69))),
                        })),
                    })),
                ],
            ),
        ];

        for (input, expected) in tests {
            let mut diagnostics = Diagnostics::new(input);
            let mut parser = Parser::new(Lexer::new(input), &mut diagnostics).unwrap();
            let ast = parser.compound_statement().unwrap();

            assert_eq!(
                &ast.0, &expected,
                "expected: {:?}, got: {:?}",
                expected, ast
            );
        }

        Ok(())
    }
}
