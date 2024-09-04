use super::{
    expr::{ExprBinary, ExprLit, ExprUnary},
    precedence::Precedence,
    stmt::{StmtFor, StmtIf, StmtReturn, StmtWhile},
    BinOp, Block, Expr, ExprArray, ExprArrayAccess, ExprCast, ExprIdent, ExprStruct, Expression,
    ParserError, Stmt, StmtFunction, StmtVarDecl, UIntLitRepr, UnOp,
};
use crate::{
    codegen::Offset,
    lexer::{Lexer, Token},
    parser::{ExprFunctionCall, ExprStructAccess},
    scope::Scope,
    symbol_table::{Symbol, SymbolFunction, SymbolGlobal, SymbolLocal, SymbolParam},
    type_table::{self, TypeStruct},
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
                (Token::Ampersand, Self::addr_expr),
                (Token::Asterisk, Self::deref_expr),
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
                (Token::Period, Self::struct_access),
                (Token::LBracket, Self::array_access),
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
                _ => {
                    let type_ = self.parse_type()?;

                    if let Some(stmt) = self.parse_symbol(type_, true)? {
                        stmts.push(stmt);
                    }
                }
            }
        }

        Ok(stmts)
    }

    fn parse_symbol(
        &mut self,
        type_: Type,
        func_definition: bool,
    ) -> Result<Option<Stmt>, ParserError> {
        match &self.peek_token {
            Token::LParen => self.function(type_, func_definition),
            _ => Ok(Some(self.var_decl(type_)?)),
        }
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

        let fields = self.params(Token::Semicolon, Token::RBrace)?;

        self.scope
            .type_table_mut()
            .define(crate::type_table::Type::Struct(TypeStruct { name, fields }));

        Ok(())
    }

    fn compound_statement(
        &mut self,
        context: Option<(String, Type)>,
        predefined_symbols: Option<Vec<Symbol>>,
    ) -> Result<Block, ParserError> {
        let mut stmts = Vec::new();

        self.scope
            .enter_new(context.unwrap_or_else(|| self.scope.context().unwrap().clone()));
        self.expect(&Token::LBrace)?;

        if let Some(symbols) = predefined_symbols {
            for symbol in symbols {
                self.scope.symbol_table_mut().push(symbol)?;
            }
        }

        while !self.cur_token_is(&Token::RBrace) {
            if self.cur_token.is_type(&self.scope) && !self.peek_token_is(&Token::LBrace) {
                let type_ = self.parse_type()?;

                if let Some(stmt) = self.parse_symbol(type_, false)? {
                    stmts.push(stmt);
                }
            } else {
                match &self.cur_token {
                    Token::Return => stmts.push(self.parse_return()?),
                    Token::If => stmts.push(self.if_stmt()?),
                    Token::While => stmts.push(self.while_stmt()?),
                    Token::For => stmts.push(self.for_stmt()?),
                    _ => {
                        let expr = self.expr(Precedence::default())?;
                        expr.type_(&self.scope)?;
                        let expr = Stmt::Expr(expr);

                        self.expect(&Token::Semicolon)?;

                        stmts.push(expr);
                    }
                }
            }
        }

        self.expect(&Token::RBrace)?;
        let scope_impl = self.scope.leave();

        Ok(Block {
            statements: stmts,
            scope: scope_impl,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
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
            Token::Ident(ident) => Ok(
                match self
                    .scope
                    .find_type(&ident)
                    .ok_or(TypeError::Nonexistent(ident.clone()))?
                {
                    type_table::Type::Struct(_) => Type::Struct(ident),
                },
            ),
            token => Err(ParserError::ParseType(token)),
        }?;

        while self.cur_token_is(&Token::Asterisk) {
            self.expect(&Token::Asterisk)?;

            base = Type::Ptr(Box::new(base));
        }

        Ok(base)
    }

    fn parse_return(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::Return)?;

        let mut expr = None;
        let type_;
        if !self.cur_token_is(&Token::Semicolon) {
            expr = Some(self.expr(Precedence::default())?);
            type_ = expr.as_ref().unwrap().type_(&self.scope)?;
        } else {
            type_ = Type::Void;
        }

        self.expect(&Token::Semicolon)?;

        let (name, return_type) = self.scope.context().unwrap();
        return_type.to_owned().assign(type_).map_err(|e| match e {
            TypeError::Assignment(left, right) => TypeError::Return(left, right),
            e => e,
        })?;

        Ok(Stmt::Return(StmtReturn {
            expr,
            //TODO: it's not a good idea
            label: name.to_owned() + "_ret",
        }))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::If)?;

        let condition = self.expr(Precedence::default())?;
        match condition.type_(&self.scope)? {
            Type::Bool => {}
            type_ => return Err(ParserError::Type(TypeError::Mismatched(Type::Bool, type_))),
        }
        let consequence = self.compound_statement(None, None)?;
        let alternative = if self.cur_token_is(&Token::Else) {
            self.expect(&Token::Else)?;

            Some(self.compound_statement(None, None)?)
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
        match condition.type_(&self.scope)? {
            Type::Bool => {}
            type_ => return Err(ParserError::Type(TypeError::Mismatched(Type::Bool, type_))),
        }
        let block = self.compound_statement(None, None)?;

        Ok(Stmt::While(StmtWhile { condition, block }))
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect(&Token::For)?;

        self.scope
            .enter_new(self.scope.context().unwrap().to_owned());
        let initializer = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            let stmt = if self.cur_token.is_type(&self.scope) {
                let type_ = self.parse_type()?;
                self.var_decl(type_)?
            } else {
                Stmt::Expr(self.expr(Precedence::default())?)
            };

            Some(stmt)
        };
        let symbols = self.scope.leave().symbol_table.0;

        let condition = if self.cur_token_is(&Token::Semicolon) {
            None
        } else {
            let condition = self.expr(Precedence::default())?;
            match condition.type_(&self.scope)? {
                Type::Bool => {}
                type_ => return Err(ParserError::Type(TypeError::Mismatched(Type::Bool, type_))),
            }

            Some(condition)
        };
        self.expect(&Token::Semicolon)?;

        let increment = if self.cur_token_is(&Token::LBrace) {
            None
        } else {
            Some(self.expr(Precedence::default())?)
        };

        let block = self.compound_statement(None, Some(symbols))?;

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

    fn var_decl(&mut self, mut type_: Type) -> Result<Stmt, ParserError> {
        if let Type::Void = type_ {
            return Err(ParserError::Type(TypeError::VoidVariable));
        }
        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        self.array_type(&mut type_)?;

        if self.scope.local() {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Local(SymbolLocal {
                    name: name.clone(),
                    type_: type_.clone(),
                    offset: Offset::default(),
                }))?;
        } else {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Global(SymbolGlobal {
                    name: name.clone(),
                    type_: type_.clone(),
                }))?;
        }

        let expr = if self.cur_token_is(&Token::Assign) {
            self.expect(&Token::Assign)?;
            let expr = self.expr(Precedence::default())?;

            self.scope
                .find_symbol(&name)
                .unwrap()
                .type_()
                .assign(expr.type_(&self.scope)?)?;

            Some(expr)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::VarDecl(StmtVarDecl::new(type_, name, expr)))
    }

    fn function(
        &mut self,
        type_: Type,
        func_definition: bool,
    ) -> Result<Option<Stmt>, ParserError> {
        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        self.expect(&Token::LParen)?;

        let params = self.params(Token::Comma, Token::RParen)?;
        let parameters: Vec<Symbol> = params
            .iter()
            .enumerate()
            .map(|(i, (name, type_))| {
                Symbol::Param(SymbolParam {
                    name: name.to_owned(),
                    preceding: params[..i]
                        .iter()
                        .map(|(_, type_)| type_.to_owned())
                        .collect(),
                    type_: type_.to_owned(),
                    offset: Offset::default(),
                })
            })
            .collect();
        let block = if self.cur_token_is(&Token::LBrace) {
            Some(self.compound_statement(Some((name.clone(), type_.clone())), Some(parameters))?)
        } else {
            None
        };
        self.scope
            .symbol_table_mut()
            .push(Symbol::Function(SymbolFunction {
                name: name.clone(),
                return_type: type_.clone(),
                parameters: params.clone().into_iter().map(|(_, type_)| type_).collect(),
            }))?;

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
            self.expect(&Token::Semicolon)?;
            Ok(None)
        }
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<Vec<(String, Type)>, ParserError> {
        let mut params = Vec::new();

        while !self.cur_token_is(&end) {
            let type_ = self.parse_type()?;
            let name = match self.next_token()? {
                Token::Ident(ident) => ident,
                _ => todo!("Don't know what error to return yet"),
            };

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
                    let type_ = match self
                        .scope
                        .find_type(&name)
                        .ok_or(TypeError::Nonexistent(name.clone()))?
                    {
                        type_table::Type::Struct(type_struct) => {
                            match type_struct.fields.iter().find(|(name, _)| name == &field) {
                                Some((_, type_)) => type_.clone(),
                                _ => todo!("Field {field} doesn't exist in struct {name}"),
                            }
                        }
                    };

                    self.expect(&Token::Colon)?;
                    let expr = self.expr(Precedence::Lowest)?;
                    type_.assign(expr.type_(&self.scope)?)?;

                    if !self.cur_token_is(&Token::RBrace) {
                        self.expect(&Token::Comma)?;
                    }
                    fields.push((field, expr));
                }
                _ => todo!("Don't know what error to return yet"),
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Expr::Struct(ExprStruct::new(name, fields)))
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
                    if let Some(Symbol::Function(function)) = self.scope.find_symbol(&ident.0) {
                        let function_name = function.name.to_owned();
                        let function_params = function.parameters.to_owned();
                        let args = self.expr_list()?;
                        let args_types = args
                            .iter()
                            .map(|expr| expr.type_(&self.scope))
                            .collect::<Result<Vec<_>, _>>()?;
                        for (param_type, arg_type) in function_params.iter().zip(&args_types) {
                            if let Err(_) = param_type.to_owned().assign(arg_type.to_owned()) {
                                return Err(ParserError::FunctionArguments(
                                    function_name,
                                    function_params,
                                    args_types,
                                ));
                            }
                        }

                        Ok(Expr::FunctionCall(ExprFunctionCall::new(ident.0, args)))
                    } else {
                        return Err(ParserError::UndeclaredFunction(ident.0));
                    }
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

                Ok(Expr::Binary(ExprBinary::new(op, left, right, &self.scope)?))
            }
        }
    }

    fn struct_access(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        let token = self.next_token()?;

        match self.expr(Precedence::from(&token))? {
            Expr::Ident(field) => match expr.type_(&self.scope)? {
                Type::Struct(struct_type) => {
                    match self
                        .scope
                        .find_type(&struct_type)
                        .ok_or(TypeError::Nonexistent(struct_type))?
                    {
                        type_table::Type::Struct(s) => {
                            if s.contains(&field.0) {
                                Ok(Expr::StructAccess(ExprStructAccess {
                                    expr: Box::new(expr),
                                    field: field.0,
                                }))
                            } else {
                                panic!("no such field bitch");
                            }
                        }
                    }
                }
                type_ => panic!("Expected struct type, got {type_}"),
            },
            _ => panic!("Struct field name should be of type string"),
        }
    }

    fn array_access(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        match expr.type_(&self.scope)? {
            Type::Array(_) => {
                self.expect(&Token::LBracket)?;
                let index = self.expr(Precedence::Access)?;
                self.expect(&Token::RBracket)?;

                Ok(Expr::ArrayAccess(ExprArrayAccess {
                    expr: Box::new(expr),
                    index: Box::new(index),
                }))
            }
            type_ => panic!("Cannot use [] syntax for type {type_}"),
        }
    }

    fn unary_expr(&mut self) -> Result<Expr, ParserError> {
        let op = UnOp::try_from(&self.next_token()?).map_err(|e| ParserError::Operator(e))?;
        let expr = self.expr(Precedence::Prefix)?;

        Ok(Expr::Unary(ExprUnary::new(op, Box::new(expr))))
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

        if self.cur_token.is_type(&self.scope) {
            let type_ = self.parse_type()?;
            self.expect(&Token::RParen)?;
            let expr = self.expr(Precedence::Cast)?;

            Ok(Expr::Cast(ExprCast::new(type_, Box::new(expr))))
        } else {
            let expr = self.expr(Precedence::default())?;
            self.expect(&Token::RParen)?;

            Ok(expr)
        }
    }

    fn addr_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(&Token::Ampersand)?;

        let expr = self.expr(Precedence::Prefix)?;
        if !expr.lvalue() {
            panic!("Can't get address of {expr:?}");
        }

        Ok(Expr::Unary(ExprUnary::new(UnOp::Address, Box::new(expr))))
    }

    fn deref_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(&Token::Asterisk)?;

        let expr = self.expr(Precedence::Prefix)?;
        match expr.type_(&self.scope)? {
            Type::Ptr(_) => {}
            type_ => panic!("Can't dereference an expression of type {type_}"),
        };

        Ok(Expr::Unary(ExprUnary::new(UnOp::Deref, Box::new(expr))))
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

        Ok(Expr::Array(ExprArray::new(items, &self.scope)))
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
        scope::Scope,
        symbol_table::{Symbol, SymbolLocal},
        types::Type,
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), ParserError> {
        let tests = [
            (
                "
                {
                    1 * 2 + 3 / (4 + (u8)1);
                }
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary::new(
                    BinOp::Add,
                    Expr::Binary(ExprBinary::new(
                        BinOp::Mul,
                        Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1))),
                        Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2))),
                        &Scope::new(),
                    )?),
                    Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3))),
                        Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Expr::Lit(ExprLit::UInt(UIntLitRepr::new(4))),
                            Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                            )),
                            &Scope::new(),
                        )?),
                        &Scope::new(),
                    )?),
                    &Scope::new(),
                )?))],
            ),
            (
                "
                {
                    u8 foo;
                    foo = (u8)-1 + 5;
                }
                ",
                vec![
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary::new(
                        BinOp::Assign,
                        Expr::Ident(ExprIdent("foo".to_owned())),
                        Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Unary(ExprUnary::new(
                                    UnOp::Negative,
                                    Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                                ))),
                            )),
                            Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5))),
                            &Scope::new(),
                        )?),
                        &Scope::from(vec![Symbol::Local(SymbolLocal {
                            name: "foo".to_string(),
                            type_: Type::U8,
                            offset: Default::default(),
                        })]),
                    )?)),
                ],
            ),
            (
                "
                {
                    u8 foo;
                    i8 bar;
                    bar = (i8)foo + 5 / 10;
                }
                ",
                vec![
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::VarDecl(StmtVarDecl::new(Type::I8, "bar".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary::new(
                        BinOp::Assign,
                        Expr::Ident(ExprIdent("bar".to_owned())),
                        Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Expr::Cast(ExprCast::new(
                                Type::I8,
                                Box::new(Expr::Ident(ExprIdent("foo".to_owned()))),
                            )),
                            Expr::Binary(ExprBinary::new(
                                BinOp::Div,
                                Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5))),
                                Expr::Lit(ExprLit::UInt(UIntLitRepr::new(10))),
                                &Scope::new(),
                            )?),
                            &Scope::from(vec![Symbol::Local(SymbolLocal {
                                name: "foo".to_string(),
                                type_: Type::U8,
                                offset: Default::default(),
                            })]),
                        )?),
                        &Scope::from(vec![
                            Symbol::Local(SymbolLocal {
                                name: "bar".to_string(),
                                type_: Type::I8,
                                offset: Default::default(),
                            }),
                            Symbol::Local(SymbolLocal {
                                name: "foo".to_string(),
                                type_: Type::U8,
                                offset: Default::default(),
                            }),
                        ]),
                    )?)),
                ],
            ),
            (
                "
                {
                    (i8)1 + 2 / 3;
                }
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary::new(
                    BinOp::Add,
                    Expr::Cast(ExprCast::new(
                        Type::I8,
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                    )),
                    Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2))),
                        Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3))),
                        &Scope::new(),
                    )?),
                    &Scope::new(),
                )?))],
            ),
            (
                "
                {
                    u8 a;
                    u8 b;

                    a = b = 69;
                }
                ",
                vec![
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "a".to_owned(), None)),
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "b".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary::new(
                        BinOp::Assign,
                        Expr::Ident(ExprIdent("a".to_owned())),
                        Expr::Binary(ExprBinary::new(
                            BinOp::Assign,
                            Expr::Ident(ExprIdent("b".to_owned())),
                            Expr::Lit(ExprLit::UInt(UIntLitRepr::new(69))),
                            &Scope::from(vec![Symbol::Local(SymbolLocal {
                                name: "b".to_string(),
                                type_: Type::U8,
                                offset: Default::default(),
                            })]),
                        )?),
                        &Scope::from(vec![
                            Symbol::Local(SymbolLocal {
                                name: "a".to_string(),
                                type_: Type::U8,
                                offset: Default::default(),
                            }),
                            Symbol::Local(SymbolLocal {
                                name: "b".to_string(),
                                type_: Type::U8,
                                offset: Default::default(),
                            }),
                        ]),
                    )?)),
                ],
            ),
        ];
        dbg!("GOTTEM");

        for (input, expected) in tests {
            let mut parser = Parser::new(Lexer::new(input.to_string())).unwrap();
            let ast = parser
                .compound_statement(Some(("".to_string(), Type::Void)), None)
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
