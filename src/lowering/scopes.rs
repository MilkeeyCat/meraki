use crate::ir::{Id, Ty};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope<'ir> {
    pub type_table: HashMap<String, &'ir Ty<'ir>>,
    pub symbol_table: HashMap<String, Id>,
}

impl<'ir> Scope<'ir> {
    pub fn new() -> Self {
        Self {
            type_table: HashMap::new(),
            symbol_table: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Scopes<'ir>(Vec<Scope<'ir>>);

impl<'ir> Scopes<'ir> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn enter(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn leave(&mut self) {
        self.0.pop();
    }

    pub fn find<T, O>(&self, f: T) -> Option<O>
    where
        T: Fn(&Scope<'ir>) -> Option<O>,
    {
        for scope in &self.0 {
            if let item @ Some(_) = f(scope) {
                return item;
            }
        }

        None
    }

    pub fn insert_type(&mut self, name: String, ty: &'ir Ty<'ir>) {
        self.0.last_mut().unwrap().type_table.insert(name, ty);
    }

    pub fn get_type(&self, name: &str) -> Option<&'ir Ty<'ir>> {
        self.find(|scope| scope.type_table.get(name).map(|&ty| ty))
    }

    pub fn insert_symbol(&mut self, name: String, id: Id) {
        self.0.last_mut().unwrap().symbol_table.insert(name, id);
    }

    pub fn get_symbol(&self, name: &str) -> Option<Id> {
        self.find(|scope| scope.symbol_table.get(name).map(|&id| id))
    }

    pub fn is_global(&self) -> bool {
        self.0.len() < 1
    }
}
