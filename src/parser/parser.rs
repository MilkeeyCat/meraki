use crate::lexer::{Lexer, Token, TokenType};

#[derive(Debug, Clone)]
pub enum ASTNodeType {
    Add,
    Sub,
    Mult,
    Div,
    IntLit(String),
}

impl TokenType {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Int(_) | Self::Eof => {
                panic!("cant use it {:?}", self);
            }
            Self::Plus => 1,
            Self::Minus => 1,
            Self::Asterisk => 2,
            Self::Slash => 2,
            t => unreachable!("unknown type type {:?}", t),
        }
    }
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

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub op: ASTNodeType,
    pub left: Option<Box<ASTNode>>,
    pub right: Option<Box<ASTNode>>,
}

impl ASTNode {
    pub fn new(op: ASTNodeType, left: Option<Box<ASTNode>>, right: Option<Box<ASTNode>>) -> Self {
        return Self { op, left, right };
    }
}

pub struct Parser {
    lexer: Lexer,

    pub cur_token: Token,
    pub peek_token: Token,
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

    pub fn bin_expr(&mut self, precedence: u8) -> ASTNode {
        let mut left = self.primary();

        if self.cur_token_is(TokenType::Eof) {
            return left;
        }

        // opration token: + - / * etc.
        let mut token = self.cur_token.clone();

        while token.token_type.precedence() > precedence {
            self.next_token();
            let right = self.bin_expr(token.token_type.precedence());

            left = ASTNode::new(
                ASTNodeType::from(&token.token_type),
                Some(Box::new(left)),
                Some(Box::new(right)),
            );

            if self.cur_token_is(TokenType::Eof) {
                return left;
            }

            token = self.cur_token.clone();
        }

        return left;
    }
}

pub fn interpret_ast(node: ASTNode) -> i32 {
    let mut left: i32 = 0;
    let mut right: i32 = 0;

    if node.left.is_some() {
        left = interpret_ast(*node.left.unwrap());
    }

    if node.right.is_some() {
        right = interpret_ast(*node.right.unwrap());
    }

    return match node.op {
        ASTNodeType::Add => left + right,
        ASTNodeType::Sub => left - right,
        ASTNodeType::Mult => left * right,
        ASTNodeType::Div => left / right,
        ASTNodeType::IntLit(int) => int.parse().unwrap(),
    };
}
