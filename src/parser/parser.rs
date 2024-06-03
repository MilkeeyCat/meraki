use std::fmt::Display;

use super::{
    expr::{BinOp, ExprBinary, ExprLit, ExprUnary, IntLitRepr, UnOp},
    precedence::Precedence,
    type_::{Type, TypeError},
    Expr, ExprCast, IntLitReprError, OpParseError, Stmt, StmtVarDecl,
};
use crate::{
    lexer::{Lexer, Token},
    symtable::{Symbol, SymbolGlobalVar, SymbolTable},
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
}

impl Display for ParserError {
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
        }
    }
}

impl From<TypeError> for ParserError {
    fn from(value: TypeError) -> Self {
        ParserError::Type(value)
    }
}

pub struct Parser {
    lexer: Lexer,
    symtable: SymbolTable,
    cur_token: Token,
    peek_token: Token,
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
                self.peek_token.to_owned(),
            ))
        }
    }

    pub fn into_parts(mut self) -> (Result<Vec<Stmt>, ParserError>, SymbolTable) {
        (self.parse(), self.symtable)
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        while !&self.cur_token_is(Token::Eof) {
            stmts.push(self.stmt()?);
            self.next_token();
        }

        Ok(stmts)
    }

    fn expr(&mut self, precedence: u8) -> Result<Expr, ParserError> {
        let mut left = match &self.cur_token {
            Token::Ident(_) => self.ident(),
            Token::Integer(_) => self.int_lit(),
            Token::Minus | Token::Bang => self.unary_expr(),
            Token::LParen => self.grouped_expr(),
            token => Err(ParserError::Prefix(token.to_owned())),
        };

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_token.precedence() {
            self.next_token();

            left = match &self.cur_token {
                Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Assign => {
                    self.bin_expr(left?)
                }
                token => {
                    return Err(ParserError::Infix(token.to_owned()));
                }
            }
        }

        left
    }

    fn stmt(&mut self) -> Result<Stmt, ParserError> {
        match &self.cur_token {
            Token::U8 | Token::I8 => {
                let type_ = self.parse_type()?;

                self.var_decl(type_)
            }
            token => {
                let expr = self.expr(token.precedence())?;
                _ = expr.type_(&self.symtable);
                let expr = Stmt::Expr(expr);

                self.expect_peek(Token::Semicolon)?;

                Ok(expr)
            }
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let token = self.cur_token.clone();
        self.next_token();

        match token {
            Token::U8 => Ok(Type::U8),
            Token::I8 => Ok(Type::I8),
            token => Err(ParserError::ParseType(token)),
        }
    }

    fn var_decl(&mut self, type_: Type) -> Result<Stmt, ParserError> {
        let name;

        match &self.cur_token {
            Token::Ident(ident) => {
                name = ident.to_string();
            }
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        }

        self.next_token();
        self.symtable.push(Symbol::GlobalVar(SymbolGlobalVar {
            name: name.clone(),
            type_: type_.clone(),
        }));

        Ok(Stmt::VarDecl(StmtVarDecl::new(type_, name, None)))
    }

    fn ident(&mut self) -> Result<Expr, ParserError> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(Expr::Ident(ident.to_owned())),
            _ => Err(ParserError::ParseType(self.cur_token.to_owned())),
        }
    }

    fn int_lit(&mut self) -> Result<Expr, ParserError> {
        match &self.cur_token {
            Token::Integer(num_str) => Ok(Expr::Lit(ExprLit::Int(
                IntLitRepr::try_from(&num_str[..]).map_err(|e| ParserError::Int(e))?,
            ))),
            _ => Err(ParserError::ParseType(self.cur_token.to_owned())),
        }
    }

    fn bin_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let token = self.cur_token.clone();
        self.next_token();

        let left = Box::new(left);
        let right = Box::new(self.expr(token.precedence())?);
        let op = BinOp::try_from(&token).map_err(|e| ParserError::Operator(e))?;

        if op == BinOp::Assign {
            left.type_(&self.symtable)?
                .assign(right.type_(&self.symtable)?)?;
        }

        Ok(Expr::Binary(ExprBinary::new(op, left, right)))
    }

    fn unary_expr(&mut self) -> Result<Expr, ParserError> {
        let op_token = self.cur_token.clone();
        self.next_token();

        let expr = Expr::Unary(ExprUnary::new(
            UnOp::try_from(&op_token).unwrap(),
            Box::new(self.expr(Precedence::Prefix as u8)?),
        ));

        Ok(expr)
    }

    fn grouped_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect(Token::LParen)?;

        let expr = match &self.cur_token {
            Token::U8 | Token::I8 => {
                let type_ = self.parse_type()?;
                self.expect(Token::RParen)?;
                let expr = self.expr(self.cur_token.precedence())?;

                Ok(Expr::Cast(ExprCast::new(type_, Box::new(expr))))
            }
            _ => {
                let expr = self.expr(Token::LParen.precedence());
                self.expect_peek(Token::RParen)?;

                expr
            }
        };

        expr
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{
        lexer::Lexer,
        parser::{
            precedence::Precedence, BinOp, Expr, ExprBinary, ExprCast, ExprLit, IntLitRepr,
            IntLitReprError, Type,
        },
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), IntLitReprError> {
        let input = "1 * 2 + 3 / (4 + (u8)1);";
        let mut parser = Parser::new(Lexer::new(input.to_string()));

        assert_eq!(
            parser.expr(Precedence::default() as u8).unwrap(),
            Expr::Binary(ExprBinary::new(
                BinOp::Add,
                Box::new(Expr::Binary(ExprBinary::new(
                    BinOp::Mul,
                    Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("1")?))),
                    Box::new(Expr::Lit(ExprLit::Int(IntLitRepr::try_from("2")?)))
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
                    )))
                )))
            ))
        );

        Ok(())
    }
}
