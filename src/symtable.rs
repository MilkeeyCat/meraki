use crate::parser::Type;

const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone)]
pub enum Symbol<'a> {
    Variable(SymbolVariable<'a>),
    Function(SymbolFunction),
}

impl<'a> Symbol<'a> {
    fn name(&self) -> &str {
        match self {
            Self::Variable(variable) => &variable.name,
            Self::Function(function) => &function.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolVariable<'a> {
    name: String,
    type_: &'a Type,
}

#[derive(Debug, Clone)]
pub struct SymbolFunction {
    name: String,
    arguments: Vec<(String, Type)>,
    return_type: Type,
}

impl SymbolFunction {
    pub fn new(name: String, args: Vec<(String, Type)>, ret: Type) -> Self {
        Self {
            name,
            return_type: ret,
            arguments: args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable<'a>(Vec<Symbol<'a>>);

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        Self(Vec::with_capacity(MAX_SYMBOLS))
    }

    pub fn get(&self, id: usize) -> &Symbol {
        &self.0[id]
    }

    pub fn find(&self, name: &str) -> Option<&Symbol> {
        self.0
            .iter()
            .enumerate()
            .find(|(_, symbol)| symbol.name() == name)
            .map(|(_, symbol)| symbol)
    }

    pub fn push(&mut self, symbol: Symbol<'a>) {
        if self.0.len() >= MAX_SYMBOLS {
            panic!("too many symbols");
        }

        match self.find(symbol.name()) {
            Some(_) => {
                return;
            }
            None => {
                self.0.push(symbol);
            }
        }
    }
}
