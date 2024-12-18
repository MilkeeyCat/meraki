use super::{
    expr::{ExprBinary, ExprLit, ExprUnary},
    precedence::Precedence,
    stmt::{StmtFor, StmtIf, StmtReturn, StmtWhile},
    BinOp, Block, Expr, ExprArray, ExprArrayAccess, ExprCast, ExprIdent, ExprStruct,
    ExprStructMethod, MacroCall, ParserError, Stmt, StmtFunction, StmtVarDecl, UIntLitRepr, UnOp,
};
use crate::{
    lexer::{LexerError, Token},
    parser::{ExprFunctionCall, ExprStructAccess},
    scope::{Scope, ScopeKind},
    symbol_table::{Symbol, SymbolFunction},
    type_table::{TypeStruct, TypeStructMethod},
    types::{IntType, Type, TypeArray, TypeError, UintType},
};
use std::collections::HashMap;

type PrefixFn<T> = fn(&mut Parser<T>) -> Result<Expr, ParserError>;
type InfixFn<T> = fn(&mut Parser<T>, left: Expr) -> Result<Expr, ParserError>;

pub struct Parser<T: Iterator<Item = Result<Token, LexerError>>> {
    lexer: T,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
    scope: Scope,
    global_stms: Vec<Stmt>,
    prefix_fns: HashMap<Token, PrefixFn<T>>,
    infix_fns: HashMap<Token, InfixFn<T>>,
}

impl<T: Iterator<Item = Result<Token, LexerError>>> Parser<T> {
    pub fn new(mut lexer: T) -> Result<Self, ParserError> {
        let mut scope = Scope::new();
        scope.enter_new(ScopeKind::Global);

        Ok(Self {
            cur_token: lexer.next().transpose()?,
            peek_token: lexer.next().transpose()?,
            lexer,
            scope,
            global_stms: Vec::new(),
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
        })
    }

    fn next_token(&mut self) -> Result<Option<Token>, ParserError> {
        let mut token = self.lexer.next().transpose()?;

        std::mem::swap(&mut self.cur_token, &mut self.peek_token);
        std::mem::swap(&mut token, &mut self.peek_token);

        Ok(token)
    }

    fn cur_token_is(&self, token: &Token) -> bool {
        self.cur_token.as_ref() == Some(token)
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        self.peek_token.as_ref() == Some(token)
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParserError> {
        match self.next_token()? {
            Some(ref cur) if cur == token => Ok(()),
            Some(cur) => Err(ParserError::UnexpectedToken(token.to_owned(), cur)),
            None => Err(ParserError::Expected(token.to_owned())),
        }
    }

    pub fn into_parts(mut self) -> Result<(Vec<Stmt>, Scope), ParserError> {
        Ok((self.parse()?, self.scope))
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while let Some(token) = &self.cur_token {
            match token {
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

    pub fn expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let token = match self.cur_token.as_ref().unwrap() {
            Token::Ident(_) => Token::Ident(Default::default()),
            Token::Integer(_) => Token::Integer(Default::default()),
            Token::String(_) => Token::String(Default::default()),
            token => token.clone(),
        };

        let mut left = match self.prefix_fns.get(&token) {
            Some(func) => func(self),
            None => {
                return Err(ParserError::Prefix(token));
            }
        };

        while !self.cur_token_is(&Token::Semicolon)
            && self.cur_token.is_some()
            && precedence < Precedence::from(self.cur_token.as_ref().unwrap())
        {
            left = match self.infix_fns.get(self.cur_token.as_ref().unwrap()) {
                Some(func) => func(self, left?),
                None => {
                    return Err(ParserError::Infix(self.cur_token.clone().unwrap()));
                }
            };
        }

        left
    }

    fn parse_struct(&mut self) -> Result<(), ParserError> {
        self.expect(&Token::Struct)?;

        let name = match self
            .next_token()?
            .ok_or(ParserError::Expected(Token::Ident(Default::default())))?
        {
            Token::Ident(ident) => Ok(ident),
            token => Err(ParserError::UnexpectedToken(
                Token::Ident(Default::default()),
                token,
            )),
        }?;

        self.expect(&Token::LBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            if self.cur_token_is(&Token::Fn) {
                self.expect(&Token::Fn)?;

                let method_name = match self.next_token()? {
                    Some(Token::Ident(ident)) => ident,
                    _ => todo!(),
                };
                self.expect(&Token::LParen)?;
                let mut params = self.params(Token::Comma, Token::RParen)?;
                params.insert(
                    0,
                    (
                        "this".to_owned(),
                        Type::Ptr(Box::new(Type::Custom(name.clone()))),
                    ),
                );
                self.expect(&Token::Arrow)?;

                let type_ = self.parse_type()?;
                let block = self.compound_statement(ScopeKind::Function(type_.clone()))?;

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
                    Some(Token::Ident(ident)) => ident,
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

    fn stmt(&mut self) -> Result<Option<Stmt>, ParserError> {
        match self.cur_token.as_ref().unwrap() {
            Token::Return => Ok(Some(self.parse_return()?)),
            Token::If => Ok(Some(self.if_stmt()?)),
            Token::While => Ok(Some(self.while_stmt()?)),
            Token::For => Ok(Some(self.for_stmt()?)),
            Token::Let => Ok(Some(self.var_decl()?)),
            Token::Continue => {
                self.expect(&Token::Continue)?;
                self.expect(&Token::Semicolon)?;

                Ok(Some(Stmt::Continue))
            }
            Token::Break => {
                self.expect(&Token::Break)?;
                self.expect(&Token::Semicolon)?;

                Ok(Some(Stmt::Break))
            }
            Token::Fn => self.function(true),
            _ => {
                let expr = Stmt::Expr(self.expr(Precedence::default())?);

                self.expect(&Token::Semicolon)?;

                Ok(Some(expr))
            }
        }
    }

    fn compound_statement(&mut self, scope_kind: ScopeKind) -> Result<Block, ParserError> {
        let mut stmts = Vec::new();

        self.scope.enter_new(scope_kind);
        self.expect(&Token::LBrace)?;

        while !self.cur_token_is(&Token::RBrace) {
            if let Some(stmt) = self.stmt()? {
                stmts.push(stmt);
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Block {
            statements: stmts,
            scope: self.scope.leave(),
        })
    }

    // This function is used only by macro expansion
    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while self.cur_token.is_some() {
            if let Some(stmt) = self.stmt()? {
                stmts.push(stmt);
            }
        }

        Ok(stmts)
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut n = 0;
        while self.cur_token_is(&Token::Asterisk) {
            self.expect(&Token::Asterisk)?;
            n += 1;
        }

        let mut base = match self.next_token()?.unwrap() {
            Token::U8 => Ok(Type::UInt(UintType::U8)),
            Token::U16 => Ok(Type::UInt(UintType::U16)),
            Token::U32 => Ok(Type::UInt(UintType::U32)),
            Token::U64 => Ok(Type::UInt(UintType::U64)),
            Token::I8 => Ok(Type::Int(IntType::I8)),
            Token::I16 => Ok(Type::Int(IntType::I16)),
            Token::I32 => Ok(Type::Int(IntType::I32)),
            Token::I64 => Ok(Type::Int(IntType::I64)),
            Token::Usize => Ok(Type::UInt(UintType::Usize)),
            Token::Isize => Ok(Type::Int(IntType::Isize)),
            Token::Bool => Ok(Type::Bool),
            Token::Void => Ok(Type::Void),
            Token::Ident(ident) => Ok(Type::Custom(ident)),
            Token::Fn => {
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

                Ok(Type::Fn(params, Box::new(self.parse_type()?)))
            }
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

        Ok(Stmt::Return(StmtReturn { expr }))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::If)?;

        let condition = self.expr(Precedence::default())?;
        let consequence = self.compound_statement(ScopeKind::Local)?;
        let alternative = if self.cur_token_is(&Token::Else) {
            self.expect(&Token::Else)?;

            Some(self.compound_statement(ScopeKind::Local)?)
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
        let block = self.compound_statement(ScopeKind::Loop)?;

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

        let block = self.compound_statement(ScopeKind::Loop)?;

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

            match self.next_token()?.unwrap() {
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

        let name = match self.next_token()?.unwrap() {
            Token::Ident(ident) => ident,
            token => {
                return Err(ParserError::ParseType(token));
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

        let name = match self.next_token()?.unwrap() {
            Token::Ident(ident) => ident,
            token => {
                return Err(ParserError::ParseType(token));
            }
        };

        self.expect(&Token::LParen)?;

        let params = self.params(Token::Comma, Token::RParen)?;
        self.expect(&Token::Arrow)?;

        let type_ = self.parse_type()?;
        let block = if self.cur_token_is(&Token::LBrace) {
            Some(self.compound_statement(ScopeKind::Function(type_.clone()))?)
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
            self.scope.symbol_table_mut().push(
                name.clone(),
                Symbol::Function(SymbolFunction {
                    return_type: type_.clone(),
                    parameters: params.clone().into_iter().map(|(_, type_)| type_).collect(),
                }),
            )?;
            self.expect(&Token::Semicolon)?;

            Ok(None)
        }
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<Vec<(String, Type)>, ParserError> {
        let mut params = Vec::new();

        while !self.cur_token_is(&end) {
            let name = match self.next_token()? {
                Some(Token::Ident(ident)) => ident,
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
            Some(Token::LBrace) => self.struct_expr(),
            _ => match self
                .next_token()?
                .ok_or(ParserError::Expected(Token::Ident(Default::default())))?
            {
                Token::Ident(ident) => Ok(Expr::Ident(ExprIdent(ident))),
                token => Err(ParserError::ParseType(token)),
            },
        }
    }

    fn struct_expr(&mut self) -> Result<Expr, ParserError> {
        let name = match self.next_token()? {
            Some(Token::Ident(ident)) => ident,
            _ => todo!("Don't know what error to return yet"),
        };

        self.expect(&Token::LBrace)?;
        let mut fields = Vec::new();

        while !self.cur_token_is(&Token::RBrace) {
            match self.next_token()? {
                Some(Token::Ident(field)) => {
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
            Some(Token::String(literal)) => Ok(Expr::Lit(ExprLit::String(literal))),
            Some(_) | None => unreachable!(),
        }
    }

    fn int_lit(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Some(Token::Integer(num_str)) => Ok(Expr::Lit(ExprLit::UInt(
                UIntLitRepr::try_from(&num_str[..]).map_err(|e| ParserError::Int(e))?,
            ))),
            Some(_) | None => unreachable!(),
        }
    }

    fn null(&mut self) -> Result<Expr, ParserError> {
        self.expect(&Token::Null)?;

        Ok(Expr::Lit(ExprLit::Null))
    }

    fn bool(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Some(Token::True) => Ok(Expr::Lit(ExprLit::Bool(true))),
            Some(Token::False) => Ok(Expr::Lit(ExprLit::Bool(false))),
            Some(_) | None => unreachable!(),
        }
    }

    fn func_call_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::LParen)?;

        let args = self.expr_list()?;

        Ok(Expr::FunctionCall(ExprFunctionCall {
            expr: Box::new(left),
            arguments: args,
        }))
    }

    fn macro_call_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let name = match left {
            Expr::Ident(expr) => expr.0,
            _ => panic!("Macro name can only be a string"),
        };

        self.expect(&Token::Bang)?;
        self.expect(&Token::LParen)?;

        let mut tokens = Vec::new();

        while !self.cur_token_is(&Token::RParen) {
            tokens.push(
                self.next_token()?
                    .ok_or(ParserError::Expected(Token::RParen))?,
            );
        }

        self.expect(&Token::RParen)?;

        Ok(Expr::MacroCall(MacroCall { name, tokens }))
    }

    fn bin_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let token = self.next_token()?.unwrap();

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

    fn pointer_access(&mut self, left: Expr) -> Result<Expr, ParserError> {
        self.expect(&Token::Arrow)?;

        let field = match self.next_token()? {
            Some(Token::Ident(ident)) => ident,
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
                Some(Token::Ident(field)) => field,
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
                Some(Token::Ident(field)) => Ok(Expr::StructAccess(ExprStructAccess {
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
        let op =
            UnOp::try_from(&self.next_token()?.unwrap()).map_err(|e| ParserError::Operator(e))?;
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
        scope::ScopeKind,
        types::{IntType, Type, UintType},
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
                                type_: Type::UInt(UintType::U8),
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
                    Stmt::VarDecl(StmtVarDecl::new(
                        Type::UInt(UintType::U8),
                        "foo".to_owned(),
                        None,
                    )),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                type_: Type::UInt(UintType::U8),
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
                    Stmt::VarDecl(StmtVarDecl::new(
                        Type::UInt(UintType::U8),
                        "foo".to_owned(),
                        None,
                    )),
                    Stmt::VarDecl(StmtVarDecl::new(
                        Type::Int(IntType::I8),
                        "bar".to_owned(),
                        None,
                    )),
                    Stmt::Expr(Expr::Binary(ExprBinary {
                        op: BinOp::Assign,
                        left: Box::new(Expr::Ident(ExprIdent("bar".to_owned()))),
                        right: Box::new(Expr::Binary(ExprBinary {
                            op: BinOp::Add,
                            left: Box::new(Expr::Cast(ExprCast {
                                type_: Type::Int(IntType::I8),
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
                        type_: Type::Int(IntType::I8),
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
                    Stmt::VarDecl(StmtVarDecl::new(
                        Type::UInt(UintType::U8),
                        "a".to_owned(),
                        None,
                    )),
                    Stmt::VarDecl(StmtVarDecl::new(
                        Type::UInt(UintType::U8),
                        "b".to_owned(),
                        None,
                    )),
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
            let ast = parser.compound_statement(ScopeKind::Global).unwrap();

            assert_eq!(
                &ast.statements, &expected,
                "expected: {:?}, got: {:?}",
                expected, ast
            );
        }

        Ok(())
    }
}
