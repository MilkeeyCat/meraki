use super::Parser;
use crate::{
    diagnostics::Diagnostic,
    lexer::{span::Span, Token, TokenKind},
};

impl<'a, 'src, T: Iterator<Item = Result<Token, Span>>> Parser<'a, 'src, T> {
    pub fn expected(&mut self, tokens: &[&TokenKind]) {
        let mut msg = match tokens {
            [token] => format!("`{token}`"),
            [token1, token2] => format!("`{token1}` or `{token2}`"),
            tokens => {
                tokens[..tokens.len() - 1]
                    .iter()
                    .map(|kind| format!("`{kind}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
                    + &format!(" or `{}`", tokens.last().unwrap())
            }
        };

        let span = if let Some(Token { span, .. }) = &self.cur_token {
            span
        } else {
            let Token { kind, span } = self.prev_token.as_ref().unwrap();

            msg.push_str(&format!(" after `{kind}`"));

            span
        };

        self.diag
            .error(Diagnostic::ParseExpected(msg), span.clone())
    }
}
