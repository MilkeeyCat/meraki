use super::{
    expr::{ExprBinary, ExprLit, ExprUnary},
    precedence::Precedence,
    stmt::{StmtFor, StmtIf, StmtReturn, StmtWhile},
    BinOp, Block, Expr, ExprArray, ExprArrayAccess, ExprCast, ExprIdent, ExprStruct,
    ExprStructMethod, ParserError, Stmt, StmtFunction, StmtVarDecl, UIntLitRepr, UnOp,
};
use crate::{
    lexer::{Lexer, Token},
    parser::{ExprFunctionCall, ExprStructAccess},
    scope::Scope,
    symbol_table::{Symbol, SymbolFunction},
    type_table::{TypeStruct, TypeStructMethod},
    types::{Type, TypeArray, TypeError},
};
use std::collections::HashMap;

type PrefixFn = fn(&mut Parser) -> Result<Expr, ParserError>;
type InfixFn = fn(&mut Parser, left: Expr) -> Result<Expr, ParserError>;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    scope: Scope,
    global_stms: Vec<Stmt>,
    prefix_fns: HashMap<Token, PrefixFn>,
    infix_fns: HashMap<Token, InfixFn>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Result<Self, ParserError> {
        Ok(Self {
            cur_token: lexer.next_token()?,
            peek_token: lexer.next_token()?,
            lexer,
            scope: Scope::new(),
            global_stms: Vec::new(),
            prefix_fns: HashMap::from([
                (Token::Ident(Default::default()), Self::ident as PrefixFn),
                (
                    Token::String(Default::default()),
                    Self::string_lit as PrefixFn,
                ),
                (Token::Integer(Default::default()), Self::int_lit),
                (Token::True, Self::bool),
                (Token::False, Self::bool),
                (Token::Minus, Self::unary_expr),
                (Token::Bang, Self::unary_expr),
                (Token::LParen, Self::grouped_expr),
                (Token::Ampersand, Self::unary_expr),
                (Token::Asterisk, Self::unary_expr),
                (Token::LBracket, Self::array_expr),
            ]),
            infix_fns: HashMap::from([
                (Token::Plus, Self::bin_expr as InfixFn),
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
                (Token::Arrow, Self::pointer_access),
                (Token::Period, Self::struct_access),
                (Token::LBracket, Self::array_access),
                (Token::As, Self::cast_expr),
            ]),
        })
    }

    fn next_token(&mut self) -> Result<Token, ParserError> {
        let mut token = self.lexer.next_token()?;
        std::mem::swap(&mut self.cur_token, &mut self.peek_token);
        std::mem::swap(&mut token, &mut self.peek_token);

        Ok(token)
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        &self.cur_token == token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        &self.peek_token == token
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParserError> {
        if self.cur_token_is(token) {
            self.next_token()?;

            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                token.to_owned(),
                self.cur_token.clone(),
            ))
        }
    }

    pub fn into_parts(mut self) -> Result<(Vec<Stmt>, Scope), ParserError> {
        Ok((self.parse()?, self.scope))
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while !&self.cur_token_is(&Token::Eof) {
            match self.cur_token {
                Token::Struct => self.parse_struct()?,
                Token::Let => stmts.push(self.var_decl()?),
                Token::Fn => {
                    if let Some(stmt) = self.function(true)? {
                        stmts.push(stmt)
                    }
                }
                _ => unreachable!(),
            }
        }

        stmts.extend_from_slice(&self.global_stms);

        Ok(stmts)
    }

    fn expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let token = match &self.cur_token {
            Token::Ident(_) => Token::Ident(Default::default()),
            Token::Integer(_) => Token::Integer(Default::default()),
            Token::String(_) => Token::String(Default::default()),
            token => token.clone(),
        };

        let mut left = match self.prefix_fns.get(&token) {
            Some(func) => func(self),
            None => {
                return Err(ParserError::Prefix(self.cur_token.clone()));
            }
        };

        while !self.cur_token_is(&Token::Semicolon)
            && precedence < Precedence::from(&self.cur_token)
        {
            left = match self.infix_fns.get(&self.cur_token) {
                Some(func) => func(self, left?),
                None => {
                    return Err(ParserError::Infix(self.cur_token.clone()));
                }
            };
        }

        left
    }

    fn parse_struct(&mut self) -> Result<(), ParserError> {
        self.expect(&Token::Struct)?;

        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => todo!("Don't know what error to return yet"),
        };

        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            if self.cur_token_is(&Token::Fn) {
                self.expect(&Token::Fn)?;

                let method_name = match self.next_token()? {
                    Token::Ident(ident) => ident,
                    _ => todo!(),
                };
                self.expect(&Token::LParen)?;
                let mut params = self.params(Token::Comma, Token::RParen)?;
                params.insert(
                    0,
                    (
                        "this".to_owned(),
                        Type::Ptr(Box::new(Type::Struct(name.clone()))),
                    ),
                );
                self.expect(&Token::Arrow)?;

                let type_ = self.parse_type()?;
                let block = self.compound_statement((method_name.clone(), type_.clone()))?;

                methods.push(TypeStructMethod {
                    return_type: type_.clone(),
                    name: method_name.clone(),
                    params: params.clone(),
                });
                self.global_stms.push(Stmt::Function(StmtFunction {
                    return_type: type_,
                    name: format!("{name}__{method_name}"),
                    params,
                    block,
                }));
            } else {
                let name = match self.next_token()? {
                    Token::Ident(ident) => ident,
                    _ => todo!("Don't know what error to return yet"),
                };
                self.expect(&Token::Colon)?;
                let mut type_ = self.parse_type()?;
                self.array_type(&mut type_)?;

                match fields.iter().find(|(field_name, _)| field_name == &name) {
                    Some(_) => todo!("Don't know yet what error to return"),
                    None => fields.push((name, type_)),
                };

                if !self.cur_token_is(&Token::RBrace) {
                    self.expect(&Token::Semicolon)?;
                }
            }
        }

        self.expect(&Token::RBrace)?;

        self.scope
            .type_table_mut()
            .define(crate::type_table::Type::Struct(TypeStruct {
                name,
                fields,
                methods,
            }));

        Ok(())
    }

    fn compound_statement(&mut self, context: (String, Type)) -> Result<Block, ParserError> {
        let mut stmts = Vec::new();

        self.scope.enter_new(context);
        self.expect(&Token::LBrace)?;

        while !self.cur_token_is(&Token::RBrace) {
            match &self.cur_token {
                Token::Return => stmts.push(self.parse_return()?),
                Token::If => stmts.push(self.if_stmt()?),
                Token::While => stmts.push(self.while_stmt()?),
                Token::For => stmts.push(self.for_stmt()?),
                Token::Let => stmts.push(self.var_decl()?),
                Token::Fn => {
                    if let Some(stmt) = self.function(true)? {
                        stmts.push(stmt)
                    }
                }
                _ => {
                    let expr = Stmt::Expr(self.expr(Precedence::default())?);

                    self.expect(&Token::Semicolon)?;

                    stmts.push(expr);
                }
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Block {
            statements: stmts,
            scope: self.scope.leave(),
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut n = 0;
        while self.cur_token_is(&Token::Asterisk) {
            self.expect(&Token::Asterisk)?;
            n += 1;
        }

        let mut base = match self.next_token()? {
            Token::U8 => Ok(Type::U8),
            Token::U16 => Ok(Type::U16),
            Token::U32 => Ok(Type::U32),
            Token::U64 => Ok(Type::U64),
            Token::I8 => Ok(Type::I8),
            Token::I16 => Ok(Type::I16),
            Token::I32 => Ok(Type::I32),
            Token::I64 => Ok(Type::I64),
            Token::Usize => Ok(Type::Usize),
            Token::Isize => Ok(Type::Isize),
            Token::Bool => Ok(Type::Bool),
            Token::Void => Ok(Type::Void),
            Token::Ident(ident) => Ok(Type::Struct(ident)),
            token => Err(ParserError::ParseType(token)),
        }?;

        while n > 0 {
            base = Type::Ptr(Box::new(base));
            n -= 1;
        }

        Ok(base)
    }

    fn parse_return(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Return)?;

        let expr = if !self.cur_token_is(&Token::Semicolon) {
            Some(self.expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        let (name, _) = self.scope.context().unwrap();

        Ok(Stmt::Return(StmtReturn {
            expr,
            //TODO: it's not a good idea
            label: name.to_owned() + "_ret",
        }))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::If)?;

        let condition = self.expr(Precedence::default())?;
        let consequence = self.compound_statement(self.scope.context().unwrap().to_owned())?;
        let alternative = if self.cur_token_is(&Token::Else) {
            self.expect(&Token::Else)?;

            Some(self.compound_statement(self.scope.context().unwrap().to_owned())?)
        } else {
            None
        };

        Ok(Stmt::If(StmtIf {
            condition,
            consequence,
            alternative,
        }))
    }

    fn while_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::While)?;

        let condition = self.expr(Precedence::default())?;
        let block = self.compound_statement(self.scope.context().unwrap().to_owned())?;

        Ok(Stmt::While(StmtWhile { condition, block }))
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::For)?;

        let initializer = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            let stmt = if self.cur_token_is(&Token::Let) {
                self.var_decl()?
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

        let block = self.compound_statement(self.scope.context().unwrap().to_owned())?;

        Ok(Stmt::For(StmtFor {
            initializer: initializer.map(|initializer| Box::new(initializer)),
            condition,
            increment,
            block,
        }))
    }

    fn array_type(&mut self, type_: &mut Type) -> Result<(), ParserError> {
        if self.cur_token_is(&Token::LBracket) {
            self.expect(&Token::LBracket)?;

            match self.next_token()? {
                Token::Integer(int) => {
                    let length: usize = str::parse(&int).unwrap();
                    self.expect(&Token::RBracket)?;

                    *type_ = Type::Array(TypeArray {
                        type_: Box::new(type_.clone()),
                        length,
                    });
                }
                token => panic!("Expected integer, got {token}"),
            }
        }

        Ok(())
    }

    fn var_decl(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Let)?;

        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };
        self.expect(&Token::Colon)?;

        let mut type_ = self.parse_type()?;
        if let Type::Void = type_ {
            return Err(ParserError::Type(TypeError::VoidVariable));
        }

        self.array_type(&mut type_)?;

        let expr = if self.cur_token_is(&Token::Assign) {
            self.expect(&Token::Assign)?;

            Some(self.expr(Precedence::default())?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::VarDecl(StmtVarDecl::new(type_, name, expr)))
    }

    fn function(&mut self, func_definition: bool) -> Result<Option<Stmt>, ParserError> {
        self.expect(&Token::Fn)?;

        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        self.expect(&Token::LParen)?;

        let params = self.params(Token::Comma, Token::RParen)?;
        self.expect(&Token::Arrow)?;

        let type_ = self.parse_type()?;
        let block = if self.cur_token_is(&Token::LBrace) {
            Some(self.compound_statement((name.clone(), type_.clone()))?)
        } else {
            None
        };

        if block.is_some() & !func_definition {
            panic!("Function definition is not supported here");
        }

        if let Some(block) = block {
            Ok(Some(Stmt::Function(StmtFunction {
                return_type: type_,
                name,
                params,
                block,
            })))
        } else {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Function(SymbolFunction {
                    name: name.clone(),
                    return_type: type_.clone(),
                    parameters: params.clone().into_iter().map(|(_, type_)| type_).collect(),
                }))?;
            self.expect(&Token::Semicolon)?;

            Ok(None)
        }
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<Vec<(String, Type)>, ParserError> {
        let mut params = Vec::new();

        while !self.cur_token_is(&end) {
            let name = match self.next_token()? {
                Token::Ident(ident) => ident,
                _ => todo!("Don't know what error to return yet"),
            };
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

    fn ident(&mut self) -> Result<Expr, ParserError> {
        match self.peek_token {
            Token::LBrace => self.struct_expr(),
            _ => match self.next_token()? {
                Token::Ident(ident) => Ok(Expr::Ident(ExprIdent(ident))),
                token => Err(ParserError::ParseType(token)),
            },
        }
    }

    fn struct_expr(&mut self) -> Result<Expr, ParserError> {
        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => todo!("Don't know what error to return yet"),
        };

        self.expect(&Token::LBrace)?;
        let mut fields = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            match self.next_token()? {
                Token::Ident(field) => {
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

    fn string_lit(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Token::String(literal) => Ok(Expr::Lit(ExprLit::String(literal))),
            token => Err(ParserError::ParseType(token)),
        }
    }

    fn int_lit(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Token::Integer(num_str) => Ok(Expr::Lit(ExprLit::UInt(
                UIntLitRepr::try_from(&num_str[..]).map_err(|e| ParserError::Int(e))?,
            ))),
            token => Err(ParserError::ParseType(token)),
        }
    }

    fn bool(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Token::True => Ok(Expr::Lit(ExprLit::Bool(true))),
            Token::False => Ok(Expr::Lit(ExprLit::Bool(false))),
            token => Err(ParserError::UnexpectedToken(token.clone(), token)),
        }
    }

    fn bin_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Token::LParen => match left {
                Expr::Ident(ident) => {
                    let args = self.expr_list()?;

                    Ok(Expr::FunctionCall(ExprFunctionCall {
                        name: ident.0,
                        arguments: args,
                    }))
                }
                _ => todo!("Don't know what error to return yet"),
            },
            token => {
                let left = left;
                // NOTE: assignment expression is right-associative
                let precedence = if let &Token::Assign = &token {
                    Precedence::from(&token).lower()
                } else {
                    Precedence::from(&token)
                };
                let right = self.expr(precedence)?;
                let op = BinOp::try_from(&token).map_err(|e| ParserError::Operator(e))?;

                Ok(Expr::Binary(ExprBinary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }))
            }
        }
    }

    fn pointer_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::Arrow)?;

        let field = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => unreachable!(),
        };

        Ok(Expr::StructAccess(ExprStructAccess {
            expr: Box::new(Expr::Unary(ExprUnary {
                op: UnOp::Deref,
                expr: Box::new(left),
            })),
            field,
        }))
    }

    fn struct_access(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::Period)?;

        if self.peek_token_is(&Token::LParen) {
            let method = match self.next_token()? {
                Token::Ident(field) => field,
                _ => panic!("Struct field name should be of type string"),
            };

            self.expect(&Token::LParen)?;
            let arguments = self.expr_list()?;

            Ok(Expr::StructMethod(ExprStructMethod {
                expr: Box::new(expr),
                method,
                arguments,
            }))
        } else {
            match self.next_token()? {
                Token::Ident(field) => Ok(Expr::StructAccess(ExprStructAccess {
                    expr: Box::new(expr),
                    field,
                })),
                _ => panic!("Struct field name should be of type string"),
            }
        }
    }

    fn array_access(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::LBracket)?;
        let index = self.expr(Precedence::Access)?;
        self.expect(&Token::RBracket)?;

        Ok(Expr::ArrayAccess(ExprArrayAccess {
            expr: Box::new(expr),
            index: Box::new(index),
        }))
    }

    fn cast_expr(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::As)?;

        Ok(Expr::Cast(ExprCast {
            expr: Box::new(expr),
            type_: self.parse_type()?,
        }))
    }

    fn unary_expr(&mut self) -> Result<Expr, ParserError> {
        let op = UnOp::try_from(&self.next_token()?).map_err(|e| ParserError::Operator(e))?;
        let expr = self.expr(Precedence::Prefix)?;

        Ok(Expr::Unary(ExprUnary {
            op,
            expr: Box::new(expr),
        }))
    }

    fn expr_list(&mut self) -> Result<Vec<Expr>, ParserError> {
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

    fn grouped_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(&Token::LParen)?;

        let expr = self.expr(Precedence::default())?;
        self.expect(&Token::RParen)?;

        Ok(expr)
    }

    fn array_expr(&mut self) -> Result<Expr, ParserError> {
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
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        lexer::Lexer,
        parser::{
            BinOp, Expr, ExprBinary, ExprCast, ExprIdent, ExprLit, ExprUnary, ParserError, Stmt,
            StmtVarDecl, UIntLitRepr, UnOp,
        },
        types::Type,
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), ParserError> {
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
                        left: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                        right: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2)))),
                    })),
                    right: Box::new(Expr::Binary(ExprBinary {
                        op: BinOp::Div,
                        left: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3)))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(4)))),
                            right: Box::new(Expr::Cast(ExprCast {
                                type_: Type::U8,
                                expr: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
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
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                type_: Type::U8,
                                expr: Box::new(Expr::Unary(ExprUnary {
                                    op: UnOp::Negative,
                                    expr: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                                })),
                            })),
                            right: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5)))),
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
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::VarDecl(StmtVarDecl::new(Type::I8, "bar".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("bar".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                type_: Type::I8,
                                expr: Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                            })),
                            right: Box::new(Expr::Binary(ExprBinary {
                                op: BinOp::Div,
                                left: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5)))),
                                right: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(10)))),
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
                        type_: Type::I8,
                        expr: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                    })),
                    right: Box::new(Expr::Binary(ExprBinary {
                        op: BinOp::Div,
                        left: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2)))),
                        right: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3)))),
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
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "a".to_owned(), None)),
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "b".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("a".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Assign,
                            left: Box::new(Expr::Ident(ExprIdent("b".to_owned()))),
                            right: Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(69)))),
                        })),
                    })),
                ],
            ),
        ];

        for (input, expected) in tests {
            let mut parser = Parser::new(Lexer::new(input.to_string())).unwrap();
            let ast = parser
                .compound_statement(("".to_string(), Type::Void))
                .unwrap();

            assert_eq!(
                &ast.statements, &expected,
                "expected: {:?}, got: {:?}",
                expected, ast
            );
        }

        Ok(())
    }
}
