use super::{BinOp, ExprBinary, ExprLit, ExprUnary, Precedence, Stmt, StmtReturn, UnOp};
use crate::{
    lexer::{Lexer, Token},
    parser::Expr,
    symtable::SymbolTable,
};
use std::collections::HashMap;

type PrefixParseFn = fn(&mut Parser) -> Expr;
type InfixParseFn = fn(&mut Parser, Expr) -> Expr;

pub struct Parser {
    lexer: Lexer,
    pub symtable: SymbolTable,
    pub cur_token: Token,
    pub peek_token: Token,
    pub prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    pub infix_parse_fns: HashMap<Token, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            cur_token: lexer.next_token().unwrap(),
            peek_token: lexer.next_token().unwrap(),
            symtable: SymbolTable::new(),
            lexer,
            prefix_parse_fns: HashMap::from([
                (
                    Token::Ident("".to_string()),
                    parse_identifier as PrefixParseFn,
                ),
                (
                    Token::Integer("".to_string()),
                    parse_ingeter_literal as PrefixParseFn,
                ),
                (Token::Bang, parse_unary as PrefixParseFn),
                (Token::Minus, parse_unary as PrefixParseFn),
                (Token::LParen, parse_grouped_binary as PrefixParseFn),
            ]),
            infix_parse_fns: HashMap::from([
                (Token::Plus, parse_binary as InfixParseFn),
                (Token::Minus, parse_binary as InfixParseFn),
                (Token::Slash, parse_binary as InfixParseFn),
                (Token::Asterisk, parse_binary as InfixParseFn),
                (Token::Equal, parse_binary as InfixParseFn),
                (Token::NotEqual, parse_binary as InfixParseFn),
                (Token::LessThan, parse_binary as InfixParseFn),
                (Token::GreaterThan, parse_binary as InfixParseFn),
                (Token::LessEqual, parse_binary as InfixParseFn),
                (Token::GreaterEqual, parse_binary as InfixParseFn),
            ]),
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
        let empty_token = match self.cur_token.clone() {
            Token::Ident(_) => Token::Ident("".to_string()),
            Token::Integer(_) => Token::Integer("".to_string()),
            Token::String(_) => Token::String("".to_string()),
            token => token,
        };

        let mut left = match self.prefix_parse_fns.get(&empty_token) {
            Some(func) => func(self),
            None => {
                panic!("coudn't find prefix parse fn for {:?}", empty_token);
            }
        };

        while !self.peek_token_is(Token::Semicolon)
            && precedence < Precedence::from(&self.peek_token)
        {
            let empty_token = match self.peek_token.clone() {
                Token::Ident(_) => Token::Ident("".to_string()),
                Token::Integer(_) => Token::Integer("".to_string()),
                Token::String(_) => Token::String("".to_string()),
                token => token,
            };
            self.next_token();

            match self.infix_parse_fns.get(&empty_token) {
                Some(func) => {
                    left = func(self, left);
                }
                None => {
                    panic!("coudn't find infix parse fn for {:?}", empty_token);
                    //return left;
                }
            }
        }

        left
    }

    fn parse_return_statement(&mut self) -> Stmt {
        self.next_token();
        let stmt = Stmt::Return(StmtReturn::new(Box::new(
            self.parse_expression(Precedence::Lowest),
        )));
        self.expect_peek(Token::Semicolon);

        stmt
    }

    fn parse_expression_statement(&mut self) -> Stmt {
        let stmt = Stmt::Expr(self.parse_expression(Precedence::Lowest));
        self.expect_peek(Token::Semicolon);

        stmt
    }
}

fn parse_identifier(parser: &mut Parser) -> Expr {
    match &parser.cur_token {
        Token::Ident(ident) => Expr::Ident(ident.to_owned()),
        token => {
            panic!("wrong value {:?}", token);
        }
    }
}

fn parse_ingeter_literal(parser: &mut Parser) -> Expr {
    match &parser.cur_token {
        Token::Integer(int) => Expr::Lit(ExprLit::Int(int.to_owned().parse().unwrap())),
        token => {
            panic!("wrong value {:?}", token);
        }
    }
}

fn parse_unary(parser: &mut Parser) -> Expr {
    let token = parser.cur_token.clone();
    parser.next_token();

    Expr::Unary(ExprUnary::new(
        UnOp::from(&token),
        Box::new(parser.parse_expression(Precedence::Prefix)),
    ))
}

fn parse_binary(parser: &mut Parser, left: Expr) -> Expr {
    let token = parser.cur_token.clone();
    parser.next_token();

    Expr::Binary(ExprBinary::new(
        BinOp::from(&token),
        Some(Box::new(left)),
        Some(Box::new(parser.parse_expression(Precedence::from(&token)))),
    ))
}

fn parse_grouped_binary(parser: &mut Parser) -> Expr {
    parser.next_token();

    let expr = parser.parse_expression(Precedence::Lowest);
    parser.expect_peek(Token::RParen);

    expr
}
