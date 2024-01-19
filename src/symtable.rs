const MAX_SYMBOLS: usize = 512;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        return Self { symbols: vec![] };
    }

    pub fn get(&self, id: usize) -> Option<String> {
        return self
            .symbols
            .iter()
            .enumerate()
            .find(|(i, _)| *i == id)
            .map(|(_, val)| val.to_owned());
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        return self
            .symbols
            .iter()
            .enumerate()
            .find(|(_, val)| *val == name)
            .map(|(i, _)| i);
    }

    pub fn push(&mut self, name: String) -> usize {
        let len = self.symbols.len();

        if len >= MAX_SYMBOLS {
            panic!("too many symbols");
        }

        if let Some(i) = self.find(&name) {
            return i;
        }

        self.symbols.push(name);

        return len;
    }
}
