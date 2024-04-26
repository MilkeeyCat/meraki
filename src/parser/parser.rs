use super::{BinOp, ExprBinary, ExprLit, ExprUnary, Precedence, Stmt, StmtReturn, UnOp};
use crate::{
    lexer::{Lexer, Token},
    parser::Expr,
    symtable::SymbolTable,
};

pub struct Parser {
    lexer: Lexer,
    pub symtable: SymbolTable,
    pub cur_token: Token,
    pub peek_token: Token,
}

impl Parser {
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

    pub fn parse_statements(&mut self) -> Vec<Stmt> {
        let mut nodes = Vec::new();

        loop {
            match &self.cur_token {
                Token::Eof => {
                    break;
                }
                Token::Return => nodes.push(self.parse_return_statement()),
                _ => nodes.push(self.parse_expression_statement()),
            }

            self.next_token();
        }

        nodes
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expr {
        let mut left = match &self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Integer(_) => self.parse_ingeter_literal(),
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
                self.parse_expression(Precedence::Lowest),
            ))));
            self.expect_peek(Token::Semicolon);
        }

        stmt
    }

    fn parse_expression_statement(&mut self) -> Stmt {
        let stmt = Stmt::Expr(self.parse_expression(Precedence::Lowest));
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

    fn parse_ingeter_literal(&self) -> Expr {
        match &self.cur_token {
            Token::Integer(int) => Expr::Lit(ExprLit::Int(int.to_owned().parse().unwrap())),
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

        let expr = self.parse_expression(Precedence::Lowest);
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
        parser::{BinOp, Expr, ExprBinary, ExprLit, Stmt},
    };

    #[test]
    fn parse_arithmetic_expression() {
        let input = "1 * 2 + 3 / (4 + 1);";
        let mut parser = Parser::new(Lexer::new(input.to_string()));
        dbg!(
            parser.parse_statements(),
            [Stmt::Expr(Expr::Binary(ExprBinary::new(
                BinOp::Add,
                Some(Box::new(Expr::Binary(ExprBinary::new(
                    BinOp::Mul,
                    Some(Box::new(Expr::Lit(ExprLit::Int(1)))),
                    Some(Box::new(Expr::Lit(ExprLit::Int(2))))
                )))),
                Some(Box::new(Expr::Binary(ExprBinary::new(
                    BinOp::Div,
                    Some(Box::new(Expr::Lit(ExprLit::Int(3)))),
                    Some(Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Add,
                        Some(Box::new(Expr::Lit(ExprLit::Int(4)))),
                        Some(Box::new(Expr::Lit(ExprLit::Int(1)))),
                    ))))
                ))))
            )))]
        );
    }
}
