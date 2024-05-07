use super::{
    BinOp, ExprBinary, ExprLit, ExprUnary, IntLitRepr, Precedence, Stmt, StmtFunction, StmtReturn,
    Type, UnOp,
};
use crate::{
    lexer::{Lexer, Token},
    parser::{Expr, StmtVarDecl},
    symtable::{Symbol, SymbolFunction, SymbolTable},
};

pub struct Parser<'a> {
    lexer: Lexer,
    pub symtable: SymbolTable<'a>,
    pub cur_token: Token,
    pub peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token().unwrap(),
            peek_token: lexer.next_token().unwrap(),
            symtable: SymbolTable::new(),
            lexer,
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

    fn expect_peek(&mut self, token: Token) {
        if self.peek_token_is(token.clone()) {
            self.next_token();
        } else {
            panic!("expected {:?}, got: {:?}", token, self.peek_token);
        }
    }

    fn parse_type(&mut self) -> Type {
        let mut type_ = match &self.cur_token {
            Token::I8 => Type::I8,
            Token::I16 => Type::I16,
            Token::I32 => Type::I32,
            Token::I64 => Type::I64,
            Token::U8 => Type::U8,
            Token::U16 => Type::U16,
            Token::U32 => Type::U32,
            Token::U64 => Type::U64,
            Token::Char => Type::Char,
            Token::Bool => Type::Bool,
            Token::Void => Type::Void,
            token => panic!("token {:?} cant be converted to a type", token),
        };

        while self.peek_token_is(Token::Asterisk) {
            self.next_token();
            type_ = Type::Ptr(Box::new(type_));
        }

        self.next_token();

        type_
    }

    pub fn parse_statements(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();

        while !self.cur_token_is(Token::Eof) {
            let type_ = self.parse_type();

            match &self.cur_token {
                Token::Ident(_) => {
                    if self.peek_token_is(Token::LParen) {
                        stmts.push(self.parse_function(type_));
                    } else {
                        stmts.push(self.parse_variable_declaration(type_));
                    }
                }
                token => panic!("expected ident, got: {:?}", token),
            }
        }

        stmts
    }

    fn parse_function(&mut self, type_: Type) -> Stmt {
        let func_name = match &self.cur_token {
            Token::Ident(ident) => ident.clone(),
            _ => unreachable!(),
        };

        self.expect_peek(Token::LParen);
        let args = self.parse_variables_list(Token::Comma, Token::RParen);

        self.expect_peek(Token::LBrace);
        let block = self.parse_block_statement();

        self.symtable.push(Symbol::Function(SymbolFunction::new(
            func_name.to_string(),
            args.clone(),
            type_.clone(),
        )));

        let args = args.into_iter().map(|(_, type_)| type_).collect();

        Stmt::Function(StmtFunction::new(func_name.to_string(), type_, args, block))
    }

    fn parse_variables_list(&mut self, delim: Token, end_token: Token) -> Vec<(String, Type)> {
        let mut variables: Vec<(String, Type)> = Vec::new();
        self.next_token();

        while !self.cur_token_is(end_token.clone()) {
            let type_ = self.parse_type();

            match &self.cur_token {
                Token::Ident(ident) => {
                    variables.push((ident.clone(), type_));

                    if self.peek_token_is(delim.clone()) {
                        self.next_token();
                    }

                    self.next_token();
                }
                token => panic!("expected ident, got: {:?}", token),
            }
        }

        variables
    }

    fn parse_block_statement(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();

        self.next_token();

        while !self.peek_token_is(Token::RBrace) {
            match &self.cur_token {
                Token::Return => stmts.push(self.parse_return_statement()),
                _ => stmts.push(self.parse_expression_statement()),
            }
        }

        self.expect_peek(Token::RBrace);
        self.next_token();

        stmts
    }

    fn parse_variable_declaration(&mut self, type_: Type) -> Stmt {
        match &self.cur_token {
            Token::Ident(ident) => {
                let name = ident.to_string();
                let mut value = None;
                self.next_token();

                if self.cur_token_is(Token::Assign) {
                    value = Some(self.parse_expression(Precedence::default()));
                } else if !self.cur_token_is(Token::Semicolon) {
                    panic!("expected semicolon, got: {:?}", self.cur_token);
                }
                self.next_token();

                Stmt::VarDecl(StmtVarDecl::new(type_, name, value))
            }
            token => panic!("expected ident, got: {:?}", token),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expr {
        let mut left = match &self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Integer(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_unary(),
            Token::LParen => self.parse_grouped_binary(),
            token => {
                panic!("failed to parse prefix token: {:?}", token);
            }
        };

        while !self.peek_token_is(Token::Semicolon)
            && precedence < Precedence::from(&self.peek_token)
        {
            self.next_token();
            left = match &self.cur_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::GreaterThan
                | Token::LessEqual
                | Token::GreaterEqual => self.parse_binary(left),
                token => {
                    panic!("failed to parse infix token: {:?}", token);
                }
            };
        }

        left
    }

    fn parse_return_statement(&mut self) -> Stmt {
        let stmt;
        self.next_token();

        if self.cur_token_is(Token::Semicolon) {
            stmt = Stmt::Return(StmtReturn::new(None));
        } else {
            stmt = Stmt::Return(StmtReturn::new(Some(Box::new(
                self.parse_expression(Precedence::default()),
            ))));
            self.expect_peek(Token::Semicolon);
        }

        stmt
    }

    fn parse_expression_statement(&mut self) -> Stmt {
        let stmt = Stmt::Expr(self.parse_expression(Precedence::default()));
        self.expect_peek(Token::Semicolon);

        stmt
    }

    fn parse_identifier(&self) -> Expr {
        match &self.cur_token {
            Token::Ident(ident) => Expr::Ident(ident.to_owned()),
            token => {
                panic!("wrong value {:?}", token);
            }
        }
    }

    fn parse_integer_literal(&self) -> Expr {
        match &self.cur_token {
            Token::Integer(int) => Expr::Lit(ExprLit::Int(IntLitRepr::from(int.clone()))),
            token => {
                panic!("wrong value {:?}", token);
            }
        }
    }

    fn parse_unary(&mut self) -> Expr {
        let token = self.cur_token.clone();
        self.next_token();

        Expr::Unary(ExprUnary::new(
            UnOp::from(&token),
            Box::new(self.parse_expression(Precedence::Prefix)),
        ))
    }

    fn parse_grouped_binary(&mut self) -> Expr {
        self.next_token();

        let expr = self.parse_expression(Precedence::default());
        self.expect_peek(Token::RParen);

        expr
    }

    fn parse_binary(&mut self, left: Expr) -> Expr {
        let token = self.cur_token.clone();
        self.next_token();

        Expr::Binary(ExprBinary::new(
            BinOp::from(&token),
            Some(Box::new(left)),
            Some(Box::new(self.parse_expression(Precedence::from(&token)))),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        lexer::Lexer,
        parser::{BinOp, Expr, ExprBinary, ExprLit, IntLitRepr, Precedence},
    };

    #[test]
    fn parse_arithmetic_expression() {
        let input = "1 * 2 + 3 / (4 + 1);";
        let mut parser = Parser::new(Lexer::new(input.to_string()));

        assert_eq!(
            parser.parse_expression(Precedence::default()),
            Expr::Binary(ExprBinary::new(
                BinOp::Add,
                Some(Box::new(Expr::Binary(ExprBinary::new(
                    BinOp::Mul,
                    Some(Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::from_str(
                        "1".to_string()
                    ))))),
                    Some(Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::from_str(
                        "2".to_string()
                    )))))
                )))),
                Some(Box::new(Expr::Binary(ExprBinary::new(
                    BinOp::Div,
                    Some(Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::from_str(
                        "3".to_string()
                    ))))),
                    Some(Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Add,
                        Some(Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::from_str(
                            "4".to_string()
                        ))))),
                        Some(Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::from_str(
                            "1".to_string()
                        ))))),
                    ))))
                ))))
            ))
        );
    }
}
