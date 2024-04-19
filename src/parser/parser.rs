use crate::{
    codegen::CodeGen,
    lexer::{Lexer, Token},
    symtable::SymbolTable,
};

#[derive(Debug, Clone)]
pub enum ASTNodeType {
    Add,
    Sub,
    Mult,
    Div,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Assign,
    IntLit(String),
    Ident(usize),
    LvIdent(usize),
    DeclareVariable(String),
}

impl Token {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Integer(_) | Self::Eof => {
                panic!("cant use it {:?}", self);
            }
            Self::Plus | Self::Minus => 1,
            Self::Asterisk | Self::Slash => 2,
            Self::Equal | Self::NotEqual => 3,
            Self::LessThan | Self::GreaterThan | Self::LessEqual | Self::GreaterEqual => 4,
            t => unreachable!("unknown type type {:?}", t),
        }
    }
}

impl From<&Token> for ASTNodeType {
    fn from(value: &Token) -> Self {
        use Token::*;
        match value {
            Asterisk => Self::Mult,
            Plus => Self::Add,
            Minus => Self::Sub,
            Slash => Self::Div,
            Equal => Self::Equal,
            NotEqual => Self::NotEqual,
            LessThan => Self::LessThan,
            GreaterThan => Self::GreaterThan,
            LessEqual => Self::LessEqual,
            GreaterEqual => Self::GreaterEqual,
            Integer(int) => Self::IntLit(int.to_owned()),
            t => unreachable!("unknown token type {:?}", t),
        }
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
        Self { op, left, right }
    }
}

pub struct Parser {
    lexer: Lexer,
    symtable: SymbolTable,

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

    fn primary(&mut self) -> ASTNode {
        let token = self.cur_token.clone();
        self.next_token();

        match token {
            Token::Integer(int) => ASTNode::new(ASTNodeType::IntLit(int), None, None),
            Token::Ident(ident) => ASTNode::new(
                ASTNodeType::Ident(self.symtable.find(&ident).unwrap()),
                None,
                None,
            ),
            t => unreachable!("syntax error, token: {:?}", t),
        }
    }

    fn expect_peek(&mut self, token: Token) {
        if self.peek_token_is(token.clone()) {
            self.next_token();
        } else {
            panic!("expected {:?}, got: {:?}", token, self.peek_token);
        }
    }

    pub fn bin_expr(&mut self, precedence: u8) -> ASTNode {
        let mut left = self.primary();

        if self.cur_token_is(Token::Semicolon) {
            return left;
        }

        // opration token: + - / * etc.
        let mut token = self.cur_token.clone();

        while token.precedence() > precedence {
            self.next_token();
            let right = self.bin_expr(token.precedence());

            left = ASTNode::new(
                ASTNodeType::from(&token),
                Some(Box::new(left)),
                Some(Box::new(right)),
            );

            if self.cur_token_is(Token::Semicolon) {
                return left;
            }

            token = self.cur_token.clone();
        }

        left
    }

    pub fn statements(&mut self) {
        let mut nodes: Vec<ASTNode> = vec![];

        loop {
            match self.cur_token.clone() {
                Token::Eof => break,
                Token::Ident(ident) => {
                    if ident == "print" {
                        self.print_statement(&mut nodes);
                    } else {
                        self.assign_statement(&mut nodes);
                    }
                }
                Token::Int => {
                    self.var_declaration(&mut nodes);
                }
                t => {
                    panic!("unknown token: {:?}", t);
                }
            }
        }

        dbg!(&nodes);

        CodeGen::new("./nasm/main.nasm", nodes, self.symtable.clone()).generate();
    }

    fn print_statement(&mut self, nodes: &mut Vec<ASTNode>) {
        if !self.cur_token_is(Token::Ident(String::from("print"))) {
            panic!("expected print, got: {:?}", self.peek_token);
        }
        self.next_token();

        nodes.push(self.bin_expr(0));

        //self.expect_peek(Token::Semicolon);
        if !self.cur_token_is(Token::Semicolon) {
            panic!("expected semi, got: {:?}", self.peek_token);
        }
        self.next_token();

        if self.cur_token_is(Token::Eof) {
            return;
        }
    }

    fn var_declaration(&mut self, nodes: &mut Vec<ASTNode>) {
        if !self.cur_token_is(Token::Int) {
            panic!("expected int, got: {:?}", self.peek_token);
        }
        self.next_token();

        if let Token::Ident(ident) = self.cur_token.clone() {
            self.symtable.push(ident.clone());
            self.next_token();

            let node = ASTNode::new(ASTNodeType::DeclareVariable(ident), None, None);

            nodes.push(node);
        } else {
            panic!("expected ident, got: {:?}", self.peek_token);
        }

        if !self.cur_token_is(Token::Semicolon) {
            panic!("expected semi, got: {:?}", self.peek_token);
        }
        self.next_token();
    }

    fn assign_statement(&mut self, nodes: &mut Vec<ASTNode>) {
        if let Token::Ident(ident) = self.cur_token.clone() {
            let id = self.symtable.find(&ident).unwrap();

            let right = ASTNode::new(ASTNodeType::LvIdent(id), None, None);

            self.expect_peek(Token::Assign);
            self.next_token();

            let left = self.bin_expr(0);

            let node = ASTNode::new(
                ASTNodeType::Assign,
                Some(Box::new(left)),
                Some(Box::new(right)),
            );

            if let Token::Semicolon = self.cur_token {
                self.next_token()
            } else {
                panic!("expected semmi");
            }

            nodes.push(node);
        } else {
            panic!("expected ident, found: {:?}", self.cur_token);
        }
    }
}
