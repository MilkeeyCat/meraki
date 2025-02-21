use crate::lexer::{TokenKind, span::Span};
use derive_more::derive::Display;

#[derive(Debug, Display)]
pub enum Diagnostic {
    #[display("syntax error: unknown character")]
    UnknownChar,
    #[display("syntax error: expected {_0}")]
    ParseExpected(String),
    #[display("syntax error: {_0} is not a valid prefix operator")]
    ExpressionPrefix(TokenKind),
    #[display("syntax error: {_0} is not a valid infix operator")]
    ExpressionInfix(TokenKind),
    #[display("field `{_0}` is already declared")]
    RepeatingField(String),
    #[display("parameter `{_0}` is already declared")]
    RepeatingParam(String),
    #[display("integer literal is too large value exceeds limit of `{}`", u64::MAX)]
    IntegerLitralTooLong,
    #[display("function definition is not allowed here")]
    IllegalFunctionDefinition,
    #[display("type mismatch: `{_0}` expected, `{_1}` found")]
    TypeMismatch(String, String),
    #[display("type hint required")]
    TypeHintRequired,
    #[display("{_0} is not a pointer type")]
    NotAPointer(String),
    #[display("cannot cast `{_0}` into `{_1}`")]
    InvalidCast(String, String),
}

#[derive(Debug, Eq, PartialEq, Display)]
enum Level {
    #[display("error")]
    Error,
    #[display("warning")]
    Warning,
}

#[derive(Debug)]
struct Message {
    level: Level,
    diag: Diagnostic,
    span: Span,
}

#[derive(Debug)]
pub struct Diagnostics<'src> {
    source: &'src str,
    messages: Vec<Message>,
}

impl<'src> Diagnostics<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            messages: Vec::new(),
        }
    }

    pub fn error(&mut self, diag: Diagnostic, span: Span) {
        self.messages.push(Message {
            level: Level::Error,
            diag,
            span,
        })
    }

    pub fn warning(&mut self, diag: Diagnostic, span: Span) {
        self.messages.push(Message {
            level: Level::Warning,
            diag,
            span,
        })
    }

    pub fn has_errors(&self) -> bool {
        self.messages.iter().any(|msg| msg.level == Level::Error)
    }

    fn row(&self, col: usize) -> usize {
        self.source[..col].chars().filter(|ch| ch == &'\n').count()
    }

    fn column(&self, row: usize) -> usize {
        self.source[..row]
            .chars()
            .rev()
            .enumerate()
            .find_map(|(i, ch)| if ch == '\n' { Some(i) } else { None })
            .unwrap_or(row)
    }

    fn lines(&self, span: &Span) -> usize {
        self.source[span.start..span.end]
            .chars()
            .filter(|ch| ch == &'\n')
            .count()
            + 1
    }
}

impl std::fmt::Display for Diagnostics<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const RED_FG: &'static str = "\x1b[1;31m";
        const YELLOW_FG: &'static str = "\x1b[1;33m";
        const BLUE_FG: &'static str = "\x1b[1;34m";
        const RESET: &'static str = "\x1b[0m";

        for message in &self.messages {
            let color = match message.level {
                Level::Error => RED_FG,
                Level::Warning => YELLOW_FG,
            };

            writeln!(f, "{color}{}{RESET}: {}", message.level, message.diag)?;

            let col = self.row(message.span.start);
            let row = self.column(message.span.start);

            writeln!(f, "notarealfilename.rs:{}:{}", col + 1, row + 1)?;

            if self.lines(&message.span) > 1 {
                todo!("Dunno how to handle multiline diagnostics");
            } else {
                writeln!(
                    f,
                    "{BLUE_FG}{} |{RESET} {}",
                    col + 1,
                    self.source.lines().nth(col).unwrap()
                )?;
                writeln!(
                    f,
                    "{}{}",
                    (0..4).into_iter().map(|_| " ").collect::<String>(),
                    (0..self.column(message.span.end))
                        .into_iter()
                        .map(|i| {
                            if (self.column(message.span.start)..self.column(message.span.end))
                                .contains(&i)
                            {
                                format!("{color}^{RESET}")
                            } else {
                                " ".into()
                            }
                        })
                        .collect::<String>()
                )?;
            }
        }

        Ok(())
    }
}
