use super::{
    expr::{ExprBinary, ExprLit, ExprUnary, IntLitRepr},
    precedence::Precedence,
    stmt::StmtReturn,
    type_::{Type, TypeError},
    BinOp, Expr, ExprCast, IntLitReprError, OpParseError, Stmt, StmtFunction, StmtVarDecl, UnOp,
};
use crate::{
    lexer::{Lexer, Token},
    scope::Scope,
    symtable::{Symbol, SymbolGlobalVar, SymbolLocalVar, SymbolTable, SymbolTableError},
};

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, Token),
    ParseType(Token),
    Prefix(Token),
    Infix(Token),
    Type(TypeError),
    Operator(OpParseError),
    Int(IntLitReprError),
    SymbolTable(SymbolTableError),
}

impl std::error::Error for ParserError {}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken(expected, actual) => {
                write!(f, "Expected token {}, got {}", expected, actual)
            }
            Self::ParseType(token) => write!(f, "Failed to parse type, found {}", token),
            Self::Prefix(token) => write!(f, "Failed to parse prefix token {}", token),
            Self::Infix(token) => write!(f, "Failed to parse infix token {}", token),
            Self::Type(e) => write!(f, "{}", e),
            Self::Operator(e) => write!(f, "{}", e),
            Self::Int(e) => write!(f, "{}", e),
            Self::SymbolTable(e) => write!(f, "{}", e),
        }
    }
}

impl From<TypeError> for ParserError {
    fn from(value: TypeError) -> Self {
        ParserError::Type(value)
    }
}

impl From<SymbolTableError> for ParserError {
    fn from(value: SymbolTableError) -> Self {
        ParserError::SymbolTable(value)
    }
}

pub struct Parser {
    lexer: Lexer,
    symtable: SymbolTable,
    cur_token: Token,
    peek_token: Token,
    scope: Scope,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token().unwrap(),
            peek_token: lexer.next_token().unwrap(),
            symtable: SymbolTable::new(),
            lexer,
            scope: Scope::default(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().unwrap();
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn expect_peek(&mut self, token: Token) -> Result<(), ParserError> {
        if self.peek_token_is(token.clone()) {
            self.next_token();

            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                token,
                self.peek_token.to_owned(),
            ))
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), ParserError> {
        if self.cur_token_is(token.clone()) {
            self.next_token();

            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                token,
                self.cur_token.to_owned(),
            ))
        }
    }

    pub fn into_parts(mut self) -> Result<(Vec<Stmt>, SymbolTable), ParserError> {
        Ok((self.parse()?, self.symtable))
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while !&self.cur_token_is(Token::Eof) {
            stmts.push(self.stmt()?);
        }

        Ok(stmts)
    }

    fn expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let mut left = match &self.cur_token {
            Token::Ident(_) => self.ident(),
            Token::Integer(_) => self.int_lit(),
            Token::True | Token::False => self.bool(),
            Token::Minus | Token::Bang => self.unary_expr(),
            Token::LParen => self.grouped_expr(),
            token => Err(ParserError::Prefix(token.to_owned())),
        };

        while !self.peek_token_is(Token::Semicolon)
            && precedence < Precedence::from(&self.peek_token)
        {
            self.next_token();

            left = match &self.cur_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Assign
                | Token::LessThan
                | Token::LessEqual
                | Token::GreaterThan
                | Token::GreaterEqual
                | Token::Equal
                | Token::NotEqual => self.bin_expr(left?),
                token => {
                    return Err(ParserError::Infix(token.to_owned()));
                }
            }
        }

        left
    }

    fn stmt(&mut self) -> Result<Stmt, ParserError> {
        match &self.cur_token {
            Token::U8 | Token::U16 | Token::I8 | Token::I16 | Token::Bool | Token::Void => {
                let type_ = self.parse_type()?;

                if self.peek_token_is(Token::Semicolon) {
                    self.var_decl(type_)
                } else {
                    self.function(type_)
                }
            }
            Token::Return => self.parse_return(),
            _ => {
                let expr = self.expr(Precedence::default())?;
                expr.type_(&self.symtable)?;
                let expr = Stmt::Expr(expr);

                self.next_token();
                self.expect(Token::Semicolon)?;

                Ok(expr)
            }
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let token = self.cur_token.clone();
        self.next_token();

        match token {
            Token::U8 => Ok(Type::U8),
            Token::U16 => Ok(Type::U16),
            Token::I8 => Ok(Type::I8),
            Token::I16 => Ok(Type::I16),
            Token::Bool => Ok(Type::Bool),
            Token::Void => Ok(Type::Void),
            token => Err(ParserError::ParseType(token)),
        }
    }

    fn parse_return(&mut self) -> Result<Stmt, ParserError> {
        self.expect(Token::Return)?;

        let mut expr = None;
        let type_;
        if !self.cur_token_is(Token::Semicolon) {
            expr = Some(self.expr(Precedence::default())?);
            self.next_token();
            type_ = expr.as_ref().unwrap().type_(&self.symtable)?;
        } else {
            type_ = Type::Void;
        }

        self.expect(Token::Semicolon)?;

        if let Scope::Local(name, return_type) = &self.scope {
            type_.assign(return_type.clone()).map_err(|e| match e {
                TypeError::Assignment(left, right) => TypeError::Return(left, right),
                e => e,
            })?;

            Ok(Stmt::Return(StmtReturn {
                expr,
                //TODO: it's not a good idea
                label: name.to_owned() + "_ret",
            }))
        } else {
            panic!("wat");
        }
    }

    fn var_decl(&mut self, type_: Type) -> Result<Stmt, ParserError> {
        if let Type::Void = type_ {
            return Err(ParserError::Type(TypeError::VoidVariable));
        }

        let name = match &self.cur_token {
            Token::Ident(ident) => ident.to_owned(),
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        if let Scope::Local(_, _) = self.scope {
            self.symtable.push(Symbol::LocalVar(SymbolLocalVar {
                name: name.clone(),
                type_: type_.clone(),
                offset: 0,
            }))?;
        } else {
            self.symtable.push(Symbol::GlobalVar(SymbolGlobalVar {
                name: name.clone(),
                type_: type_.clone(),
            }))?;
        }

        self.next_token();
        self.expect(Token::Semicolon)?;

        Ok(Stmt::VarDecl(StmtVarDecl::new(type_, name, None)))
    }

    fn function(&mut self, type_: Type) -> Result<Stmt, ParserError> {
        let name = match &self.cur_token {
            Token::Ident(ident) => ident.to_owned(),
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        self.next_token();
        self.expect(Token::LParen)?;
        self.symtable.enter(Box::new(SymbolTable::new()));

        let params = self.params(Token::Comma, Token::RParen)?;
        self.scope = Scope::Local(name.clone(), type_.clone());
        let body = self.function_body()?;
        self.scope = Scope::Global;

        Ok(Stmt::Function(StmtFunction {
            return_type: type_,
            name,
            params,
            body,
            symtable: self.symtable.inner(),
        }))
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<Vec<(String, Type)>, ParserError> {
        let mut params = Vec::new();

        while !self.cur_token_is(end.clone()) {
            let type_ = self.parse_type()?;
            let name = match &self.cur_token {
                Token::Ident(ident) => ident.to_owned(),
                _ => panic!("jkasdlj"),
            };

            self.next_token();
            params.push((name, type_));

            if !self.cur_token_is(end.clone()) {
                self.expect(delim.clone())?;
            }
        }

        self.expect(end)?;

        Ok(params)
    }

    fn function_body(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        self.expect(Token::LBrace)?;

        while !self.cur_token_is(Token::RBrace) {
            stmts.push(self.stmt()?);
        }

        self.expect(Token::RBrace)?;

        Ok(stmts)
    }

    fn ident(&mut self) -> Result<Expr, ParserError> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(Expr::Ident(ident.to_owned())),
            _ => Err(ParserError::ParseType(self.cur_token.to_owned())),
        }
    }

    fn int_lit(&self) -> Result<Expr, ParserError> {
        match &self.cur_token {
            Token::Integer(num_str) => Ok(Expr::Lit(ExprLit::Int(
                IntLitRepr::try_from(&num_str[..]).map_err(|e| ParserError::Int(e))?,
            ))),
            _ => Err(ParserError::ParseType(self.cur_token.to_owned())),
        }
    }

    fn bool(&self) -> Result<Expr, ParserError> {
        match &self.cur_token {
            Token::True => Ok(Expr::Lit(ExprLit::Bool(true))),
            Token::False => Ok(Expr::Lit(ExprLit::Bool(false))),
            token => Err(ParserError::UnexpectedToken(token.clone(), token.clone())),
        }
    }

    fn bin_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let token = self.cur_token.clone();
        self.next_token();

        let left = Box::new(left);
        let right = Box::new(self.expr(Precedence::from(&token))?);
        let op = BinOp::try_from(&token).map_err(|e| ParserError::Operator(e))?;

        Ok(Expr::Binary(ExprBinary::new(op, left, right)))
    }

    fn unary_expr(&mut self) -> Result<Expr, ParserError> {
        let op_token = self.cur_token.clone();
        self.next_token();

        let expr = Expr::Unary(ExprUnary::new(
            UnOp::try_from(&op_token).unwrap(),
            Box::new(self.expr(Precedence::Prefix)?),
        ));

        Ok(expr)
    }

    fn grouped_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(Token::LParen)?;

        match &self.cur_token {
            Token::U8 | Token::I8 | Token::U16 | Token::I16 | Token::Bool | Token::Void => {
                let type_ = self.parse_type()?;
                self.expect(Token::RParen)?;
                let expr = self.expr(Precedence::Highest)?;

                Ok(Expr::Cast(ExprCast::new(type_, Box::new(expr))))
            }
            _ => {
                let expr = self.expr(Precedence::default());
                self.expect_peek(Token::RParen)?;

                expr
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        lexer::Lexer,
        parser::{
            BinOp, Expr, ExprBinary, ExprCast, ExprLit, ExprUnary, IntLitRepr, IntLitReprError,
            Stmt, StmtVarDecl, Type, UnOp,
        },
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), IntLitReprError> {
        let tests = [
            (
                "1 * 2 + 3 / (4 + (u8)1);",
                vec![Stmt::Expr(Expr::Binary(ExprBinary::new(
                    BinOp::Add,
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Mul,
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("1")?))),
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("2")?))),
                    ))),
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("3")?))),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("4")?))),
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("1")?))),
                            ))),
                        ))),
                    ))),
                )))],
            ),
            (
                "
                u8 foo;
                foo = (u8)-1 + 5;
                ",
                vec![
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary::new(
                        BinOp::Assign,
                        Box::new(Expr::Ident("foo".to_owned())),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Unary(ExprUnary::new(
                                    UnOp::Negative,
                                    Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("1")?))),
                                ))),
                            ))),
                            Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("5")?))),
                        ))),
                    ))),
                ],
            ),
            (
                "
                u8 foo;
                i8 bar;
                bar = (i8)foo + 5 / 10;
                ",
                vec![
                    Stmt::VarDecl(StmtVarDecl::new(Type::U8, "foo".to_owned(), None)),
                    Stmt::VarDecl(StmtVarDecl::new(Type::I8, "bar".to_owned(), None)),
                    Stmt::Expr(Expr::Binary(ExprBinary::new(
                        BinOp::Assign,
                        Box::new(Expr::Ident("bar".to_owned())),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::I8,
                                Box::new(Expr::Ident("foo".to_owned())),
                            ))),
                            Box::new(Expr::Binary(ExprBinary::new(
                                BinOp::Div,
                                Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("5")?))),
                                Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("10")?))),
                            ))),
                        ))),
                    ))),
                ],
            ),
            (
                "
                (i8)1 + 2 / 3;
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary::new(
                    BinOp::Add,
                    Box::new(Expr::Cast(ExprCast::new(
                        Type::I8,
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("1")?))),
                    ))),
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("2")?))),
                        Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("3")?))),
                    ))),
                )))],
            ),
        ];

        for (input, expected) in tests {
            let mut parser = Parser::new(Lexer::new(input.to_string()));
            assert_eq!(parser.parse().unwrap(), expected);
        }

        Ok(())
    }
}
