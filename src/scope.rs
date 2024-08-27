use crate::{
    symbol_table::{Symbol, SymbolTable},
    type_table::TypeTable,
    types::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeImpl {
    pub type_table: TypeTable,
    pub symbol_table: SymbolTable,
    pub context: Option<(String, Type)>,
}

impl ScopeImpl {
    pub fn new(context: Option<(String, Type)>) -> Self {
        Self {
            type_table: TypeTable::new(),
            symbol_table: SymbolTable::new(),
            context,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope(Vec<ScopeImpl>);

impl Scope {
    pub fn new() -> Self {
        Self(vec![ScopeImpl::new(None)])
    }

    pub fn enter_new(&mut self, context: (String, Type)) {
        self.0.push(ScopeImpl::new(Some(context)));
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

    pub fn find_type(&self, name: &str) -> Option<&crate::type_table::Type> {
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

    pub fn type_table_mut(&mut self) -> &mut TypeTable {
        &mut self.0.last_mut().unwrap().type_table
    }

    pub fn context(&self) -> Option<&(String, Type)> {
        self.0.last().unwrap().context.as_ref()
    }

    pub fn local(&self) -> bool {
        self.0.len() > 1
    }
}
