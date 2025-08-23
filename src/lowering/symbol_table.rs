use crate::ir::{Id, Ty};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope<'ir> {
    types: HashMap<String, &'ir Ty<'ir>>,
    symbols: HashMap<String, Id>,
}

impl<'ir> Scope<'ir> {
    fn new() -> Self {
        Self {
            types: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'ir>(Vec<Scope<'ir>>);

impl<'ir> SymbolTable<'ir> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn enter(&mut self) {
        self.0.push(Scope::new());
    }

    pub fn leave(&mut self) {
        self.0.pop();
    }

    fn find<'a, F, O>(&'a self, f: F) -> Option<O>
    where
        F: Fn(&'a Scope<'ir>) -> Option<O>,
    {
        self.0.iter().rev().find_map(|scope| f(scope))
    }

    fn find_mut<F, O>(&mut self, f: F) -> Option<&mut O>
    where
        F: for<'s> Fn(&'s mut Scope<'ir>) -> Option<&'s mut O>,
    {
        self.0.iter_mut().rev().find_map(|scope| f(scope))
    }

    pub fn find_symbol(&self, name: &str) -> Option<Id> {
        self.find(|scope| {
            scope
                .symbols
                .iter()
                .find(|(symbol_name, _)| symbol_name == &name)
                .map(|(_, node_id)| *node_id)
        })
    }

    pub fn find_symbol_mut(&mut self, name: &str) -> Option<&mut Id> {
        self.find_mut(|scope| {
            scope
                .symbols
                .iter_mut()
                .find(|(symbol_name, _)| symbol_name == &name)
                .map(|(_, node_id)| node_id)
        })
    }

    pub fn insert_symbol(&mut self, name: String, node_id: Id) -> bool {
        self.0
            .last_mut()
            .unwrap()
            .symbols
            .insert(name, node_id)
            .is_none()
    }

    pub fn find_ty(&self, name: &str) -> Option<&'ir Ty<'ir>> {
        self.find(|scope| {
            scope
                .types
                .iter()
                .find(|(ty_name, _)| ty_name == &name)
                .map(|(_, ty)| *ty)
        })
    }

    pub fn insert_ty(&mut self, name: String, ty: &'ir Ty<'ir>) -> bool {
        self.0.last_mut().unwrap().types.insert(name, ty).is_none()
    }
}
