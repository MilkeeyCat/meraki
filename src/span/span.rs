#[derive(Debug, Clone, PartialEq)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

impl Default for LineColumn {
    fn default() -> Self {
        return Self { line: 0, column: 0 };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    start: LineColumn,
    end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        return Self { start, end };
    }
}

impl Default for Span {
    fn default() -> Self {
        return Self {
            start: LineColumn::default(),
            end: LineColumn::default(),
        };
    }
}
