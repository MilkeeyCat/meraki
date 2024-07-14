use super::{
    expr::{ExprBinary, ExprLit, ExprUnary, IntLitRepr},
    precedence::Precedence,
    stmt::StmtReturn,
    BinOp, Expr, ExprCast, ExprStruct, Expression, IntLitReprError, OpParseError, Stmt,
    StmtFunction, StmtVarDecl, UIntLitRepr, UnOp,
};
use crate::{
    lexer::{Lexer, LexerError, Token},
    parser::ExprFunctionCall,
    scope::Scope,
    symbol_table::{
        Symbol, SymbolFunction, SymbolGlobal, SymbolLocal, SymbolParam, SymbolTableError,
    },
    type_::{Type, TypeError},
    type_table::{self, TypeStruct},
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, Token),
    ParseType(Token),
    Prefix(Token),
    Infix(Token),
    Lexer(LexerError),
    Type(TypeError),
    Operator(OpParseError),
    Int(IntLitReprError),
    SymbolTable(SymbolTableError),
    UndeclaredFunction(String),
    FunctionArguments(String, Vec<Type>, Vec<Type>),
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
            Self::Lexer(e) => write!(f, "{}", e),
            Self::Type(e) => write!(f, "{}", e),
            Self::Operator(e) => write!(f, "{}", e),
            Self::Int(e) => write!(f, "{}", e),
            Self::SymbolTable(e) => write!(f, "{}", e),
            Self::UndeclaredFunction(name) => write!(f, "Call to undeclared function {name}"),
            Self::FunctionArguments(name, expected, actual) => write!(
                f,
                "Function {} has signature ({}), got called with ({})",
                name,
                expected
                    .iter()
                    .map(|type_| type_.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                actual
                    .iter()
                    .map(|type_| type_.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        ParserError::Lexer(value)
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    scope: Scope,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Result<Self, ParserError> {
        Ok(Self {
            cur_token: lexer.next_token()?,
            peek_token: lexer.next_token()?,
            lexer,
            scope: Scope::new(),
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

    fn expect_peek(&mut self, token: &Token) -> Result<(), ParserError> {
        if self.peek_token_is(token) {
            self.next_token()?;

            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(
                token.to_owned(),
                self.peek_token.clone(),
            ))
        }
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

                    if let Some(stmt) = self.parse_symbol(type_)? {
                        stmts.push(stmt);
                    }
                }
            }
        }

        Ok(stmts)
    }

    fn parse_symbol(&mut self, type_: Type) -> Result<Option<Stmt>, ParserError> {
        match self.peek_token {
            Token::Semicolon => Ok(Some(self.var_decl(type_)?)),
            _ => self.function(type_, true),
        }
    }

    fn expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let mut left = match &self.cur_token {
            Token::Ident(_) => match self.peek_token {
                Token::LBrace => self.struct_expr(),
                _ => self.ident(),
            },
            Token::Integer(_) => self.int_lit(),
            Token::True | Token::False => self.bool(),
            Token::Minus | Token::Bang => self.unary_expr(),
            Token::LParen => self.grouped_expr(),
            token => Err(ParserError::Prefix(token.to_owned())),
        };

        while !self.cur_token_is(&Token::Semicolon)
            && precedence < Precedence::from(&self.cur_token)
        {
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
                | Token::NotEqual
                | Token::LParen => self.bin_expr(left?),
                token => {
                    return Err(ParserError::Infix(token.to_owned()));
                }
            }
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

        dbg!(self.scope.type_table());

        Ok(())
    }

    fn compound_statement(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = Vec::new();

        self.expect(&Token::LBrace)?;

        while !self.cur_token_is(&Token::RBrace) {
            match &self.cur_token {
                Token::U8 | Token::U16 | Token::I8 | Token::I16 | Token::Bool | Token::Void => {
                    let type_ = self.parse_type()?;

                    if self.peek_token_is(&Token::Semicolon) {
                        stmts.push(self.var_decl(type_)?);
                    } else {
                        self.function(type_, false)?;
                    }
                }
                Token::Return => stmts.push(self.parse_return()?),
                _ => {
                    let expr = self.expr(Precedence::default())?;
                    expr.type_(&self.scope)?;
                    let expr = Stmt::Expr(expr);

                    self.expect(&Token::Semicolon)?;

                    stmts.push(expr);
                }
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(stmts)
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let token = self.next_token()?;

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

    fn var_decl(&mut self, type_: Type) -> Result<Stmt, ParserError> {
        if let Type::Void = type_ {
            return Err(ParserError::Type(TypeError::VoidVariable));
        }

        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => {
                return Err(ParserError::ParseType(self.cur_token.to_owned()));
            }
        };

        if self.scope.local() {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Local(SymbolLocal {
                    name: name.clone(),
                    type_: type_.clone(),
                    offset: 0,
                }))?;
        } else {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Global(SymbolGlobal {
                    name: name.clone(),
                    type_: type_.clone(),
                }))?;
        }

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::VarDecl(StmtVarDecl::new(type_, name, None)))
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
        self.scope.enter_new((name.clone(), type_.clone()));

        let params = self.params(Token::Comma, Token::RParen)?;
        for (i, (name, type_)) in params.iter().enumerate() {
            self.scope
                .symbol_table_mut()
                .push(Symbol::Param(SymbolParam {
                    name: name.to_owned(),
                    n: i,
                    type_: type_.to_owned(),
                }))?;
        }
        let mut has_body = false;
        let body = if self.cur_token_is(&Token::LBrace) {
            has_body = true;
            self.compound_statement()?
        } else {
            vec![]
        };
        let scope_impl = self.scope.leave();
        self.scope
            .symbol_table_mut()
            .push(Symbol::Function(SymbolFunction {
                name: name.clone(),
                return_type: type_.clone(),
                parameters: params.clone().into_values().collect(),
            }))?;

        if has_body & !func_definition {
            panic!("Function definition is not supported here");
        }

        if !has_body {
            self.expect(&Token::Semicolon)?;
            Ok(None)
        } else {
            Ok(Some(Stmt::Function(StmtFunction {
                return_type: type_,
                name,
                params,
                body,
                scope: Box::new(scope_impl),
            })))
        }
    }

    fn params(&mut self, delim: Token, end: Token) -> Result<HashMap<String, Type>, ParserError> {
        let mut params = HashMap::new();

        while !self.cur_token_is(&end) {
            let type_ = self.parse_type()?;
            let name = match self.next_token()? {
                Token::Ident(ident) => ident,
                _ => todo!("Don't know what error to return yet"),
            };

            match params.contains_key(&name) {
                true => todo!("Don't know yet what error to return"),
                false => params.insert(name, type_),
            };

            if !self.cur_token_is(&end) {
                self.expect(&delim)?;
            }
        }

        self.expect(&end)?;

        Ok(params)
    }

    fn ident(&mut self) -> Result<Expr, ParserError> {
        match self.next_token()? {
            Token::Ident(ident) => Ok(Expr::Ident(ident)),
            token => Err(ParserError::ParseType(token)),
        }
    }

    fn struct_expr(&mut self) -> Result<Expr, ParserError> {
        let name = match self.next_token()? {
            Token::Ident(ident) => ident,
            _ => todo!("Don't know what error to return yet"),
        };

        self.expect(&Token::LBrace)?;
        let mut fields = HashMap::new();

        while !self.cur_token_is(&Token::RBrace) {
            match self.next_token()? {
                Token::Ident(field) => {
                    if !match self
                        .scope
                        .type_table()
                        .find(&name)
                        .expect("Type doesn't exist")
                    {
                        type_table::Type::Struct(type_struct) => {
                            type_struct.fields.contains_key(&field)
                        }
                    } {
                        todo!("Field {field} doesn't exist in struct {name}");
                    }

                    self.expect(&Token::Colon)?;
                    let expr = self.expr(Precedence::Lowest)?;

                    self.expect(&Token::Comma)?;
                    fields.insert(field, expr);
                }
                _ => todo!("Don't know what error to return yet"),
            }
        }

        self.expect(&Token::RBrace)?;

        Ok(Expr::Struct(ExprStruct::new(name, fields)))
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
                    if let Some(Symbol::Function(function)) = self.scope.find_symbol(&ident) {
                        let function_name = function.name.to_owned();
                        let function_params = function.parameters.to_owned();
                        let args = self.expr_list()?;
                        let args_types = args
                            .iter()
                            .map(|expr| expr.type_(&self.scope).unwrap())
                            .collect::<Vec<Type>>();

                        if function_params != args_types {
                            return Err(ParserError::FunctionArguments(
                                function_name,
                                function_params,
                                args_types,
                            ));
                        }

                        Ok(Expr::FunctionCall(ExprFunctionCall::new(ident, args)))
                    } else {
                        return Err(ParserError::UndeclaredFunction(ident));
                    }
                }
                _ => todo!("Don't know what error to return yet"),
            },
            token => {
                let left = Box::new(left);
                let right = Box::new(self.expr(Precedence::from(&token))?);
                let op = BinOp::try_from(&token).map_err(|e| ParserError::Operator(e))?;

                Ok(Expr::Binary(ExprBinary::new(op, left, right)))
            }
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

        match &self.cur_token {
            Token::U8 | Token::I8 | Token::U16 | Token::I16 | Token::Bool | Token::Void => {
                let type_ = self.parse_type()?;
                self.expect(&Token::RParen)?;
                let expr = self.expr(Precedence::Highest)?;

                Ok(Expr::Cast(ExprCast::new(type_, Box::new(expr))))
            }
            _ => {
                let expr = self.expr(Precedence::default());
                self.expect(&Token::RParen)?;

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
            Stmt, StmtVarDecl, UIntLitRepr, UnOp,
        },
        type_::Type,
    };

    #[test]
    fn parse_arithmetic_expression() -> Result<(), IntLitReprError> {
        let tests = [
            (
                "
                {
                    1 * 2 + 3 / (4 + (u8)1);
                }
                ",
                vec![Stmt::Expr(Expr::Binary(ExprBinary::new(
                    BinOp::Add,
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Mul,
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2)))),
                    ))),
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3)))),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(4)))),
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                            ))),
                        ))),
                    ))),
                )))],
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
                        Box::new(Expr::Ident("foo".to_owned())),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::U8,
                                Box::new(Expr::Unary(ExprUnary::new(
                                    UnOp::Negative,
                                    Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                                ))),
                            ))),
                            Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5)))),
                        ))),
                    ))),
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
                        Box::new(Expr::Ident("bar".to_owned())),
                        Box::new(Expr::Binary(ExprBinary::new(
                            BinOp::Add,
                            Box::new(Expr::Cast(ExprCast::new(
                                Type::I8,
                                Box::new(Expr::Ident("foo".to_owned())),
                            ))),
                            Box::new(Expr::Binary(ExprBinary::new(
                                BinOp::Div,
                                Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(5)))),
                                Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(10)))),
                            ))),
                        ))),
                    ))),
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
                    Box::new(Expr::Cast(ExprCast::new(
                        Type::I8,
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(1)))),
                    ))),
                    Box::new(Expr::Binary(ExprBinary::new(
                        BinOp::Div,
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(2)))),
                        Box::new(Expr::Lit(ExprLit::UInt(UIntLitRepr::new(3)))),
                    ))),
                )))],
            ),
        ];

        for (input, expected) in tests {
            let mut parser = Parser::new(Lexer::new(input.to_string())).unwrap();
            assert_eq!(parser.compound_statement().unwrap(), expected);
        }

        Ok(())
    }
}
