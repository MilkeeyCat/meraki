use crate::{
    symbol_table::{Symbol, SymbolTable},
    type_table::{self as tt, TypeTable},
    types::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeKind {
    Global,
    Local,
    Loop,
    Function(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeImpl {
    pub type_table: TypeTable,
    pub symbol_table: SymbolTable,
    pub kind: ScopeKind,
}

impl ScopeImpl {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            type_table: TypeTable::new(),
            symbol_table: SymbolTable::new(),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope(Vec<ScopeImpl>);

impl Scope {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn enter_new(&mut self, kind: ScopeKind) {
        self.0.push(ScopeImpl::new(kind));
    }

    pub fn enter(&mut self, scope_impl: ScopeImpl) {
        self.0.push(scope_impl);
    }

    pub fn leave(&mut self) -> ScopeImpl {
        if self.0.len() > 1 {
            self.0.pop().unwrap()
        } else {
            panic!("Can't leave outermost scope");
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in &self.0 {
            let symbol = scope.symbol_table.find(name);

            if symbol.is_some() {
                return symbol;
            }
        }

        return None;
    }

    pub fn find_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in &mut self.0 {
            let symbol = scope.symbol_table.find_mut(name);

            if symbol.is_some() {
                return symbol;
            }
        }

        return None;
    }

    pub fn find_type(&self, name: &str) -> Option<&tt::Type> {
        for scope in &self.0 {
            let type_ = scope.type_table.find(name);

            if type_.is_some() {
                return type_;
            }
        }

        return None;
    }

    pub fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        &mut self.0.last_mut().unwrap().symbol_table
    }

    pub fn type_table(&self) -> &TypeTable {
        &self.0.last().unwrap().type_table
    }

    pub fn type_table_mut(&mut self) -> &mut TypeTable {
        &mut self.0.last_mut().unwrap().type_table
    }

    pub fn kind(&self) -> &ScopeKind {
        &self.0.last().unwrap().kind
    }

    pub fn local(&self) -> bool {
        self.0.len() > 1
    }

    pub fn return_type(&self) -> Option<&Type> {
        self.0.iter().rev().find_map(|scope_impl| {
            if let ScopeKind::Function(type_) = &scope_impl.kind {
                return Some(type_);
            } else {
                None
            }
        })
    }
}

impl From<Vec<Symbol>> for Scope {
    fn from(value: Vec<Symbol>) -> Self {
        let mut scope = Self::new();
        let mut scope_impl = ScopeImpl::new(ScopeKind::Global);
        scope_impl.symbol_table.0 = value;
        scope.enter(scope_impl);

        scope
    }
}
