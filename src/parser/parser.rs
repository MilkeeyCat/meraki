use crate::lexer::{Lexer, Token, TokenType};

#[derive(Debug)]
pub enum ASTNodeType {
    Add,
    Sub,
    Mult,
    Div,
    IntLit(String),
}

impl From<&TokenType> for ASTNodeType {
    fn from(value: &TokenType) -> Self {
        return match value {
            TokenType::Asterisk => Self::Mult,
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Slash => Self::Div,
            TokenType::Int(int) => Self::IntLit(int.to_owned()),
            t => unreachable!("unknown token type {:?}", t),
        };
    }
}

#[derive(Debug)]
pub struct ASTNode {
    op: ASTNodeType,
    left: Option<Box<ASTNode>>,
    right: Option<Box<ASTNode>>,
}

impl ASTNode {
    pub fn new(op: ASTNodeType, left: Option<Box<ASTNode>>, right: Option<Box<ASTNode>>) -> Self {
        return Self { op, left, right };
    }
}

pub struct Parser {
    lexer: Lexer,

    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        return Self {
            cur_token: lexer.next_token().unwrap(),
            peek_token: lexer.next_token().unwrap(),
            lexer,
        };
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().unwrap();
    }

    fn cur_token_is(&self, token: TokenType) -> bool {
        return self.cur_token.token_type == token;
    }

    fn peek_token_is(&self, token: TokenType) -> bool {
        return self.peek_token.token_type == token;
    }

    fn primary(&mut self) -> ASTNode {
        let token = self.cur_token.clone();
        self.next_token();

        return match token.token_type {
            TokenType::Int(int) => ASTNode::new(ASTNodeType::IntLit(int), None, None),
            _ => unreachable!("syntax error on {:?}", token.span),
        };
    }

    pub fn bin_expr(&mut self) -> ASTNode {
        let left = self.primary();

        if self.cur_token_is(TokenType::Eof) {
            return left;
        }

        let op = ASTNodeType::from(&self.cur_token.token_type);

        self.next_token();

        let right = self.bin_expr();

        return ASTNode::new(op, Some(Box::new(left)), Some(Box::new(right)));
    }
}
