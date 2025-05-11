use crate::{
    Context,
    ast::node_id::NodeId,
    ir::{self, FunctionIdx, ty::Ty},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Variable<'ir> {
    pub kind: VariableKind,
    pub ty: &'ir Ty<'ir>,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Local(ir::LocalIdx),
    Global(ir::GlobalIdx),
}

#[derive(Debug)]
struct FnSig<'ir> {
    pub params: Vec<&'ir Ty<'ir>>,
    pub ret_ty: &'ir Ty<'ir>,
}

#[derive(Debug)]
pub struct Function<'ir> {
    pub idx: FunctionIdx,
    pub signature: FnSig<'ir>,
}

#[derive(Debug)]
pub struct Scope<'ir> {
    types: HashMap<String, &'ir Ty<'ir>>,
    variables: HashMap<String, Variable<'ir>>,
    functions: HashMap<String, Function<'ir>>,
}

impl<'ir> Scope<'ir> {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ScopeTable<'ir> {
    scopes: Vec<Scope<'ir>>,
    node_id_to_scope_idx: HashMap<NodeId, usize>,
    stack: Vec<usize>,
}

impl<'ir> ScopeTable<'ir> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            node_id_to_scope_idx: HashMap::new(),
            stack: vec![0],
        }
    }

    pub fn enter_new(&mut self, node_id: NodeId) {
        assert!(self.node_id_to_scope_idx.get(&node_id).is_none());
        let idx = self.scopes.len();

        self.scopes.push(Scope::new());
        self.node_id_to_scope_idx.insert(node_id, idx);
        self.stack.push(idx);
    }

    pub fn enter(&mut self, node_id: NodeId) {
        self.stack.push(self.node_id_to_scope_idx[&node_id]);
    }

    pub fn leave(&mut self) {
        self.stack.pop();
    }

    fn find<'a, F, O>(&'a self, f: F) -> Option<O>
    where
        F: Fn(&'a Scope<'ir>) -> Option<O>,
    {
        self.stack
            .iter()
            .rev()
            .find_map(|&idx| f(&self.scopes[idx]))
    }

    fn find_mut<F, O>(&mut self, f: F) -> Option<&mut O>
    where
        F: for<'s> Fn(&'s mut Scope<'ir>) -> Option<&'s mut O>,
    {
        let mut scope_idx = None;

        for &idx in &self.stack {
            if f(&mut self.scopes[idx]).is_some() {
                scope_idx = Some(idx);

                break;
            }
        }

        scope_idx.and_then(|idx| f(&mut self.scopes[idx]))
    }

    pub fn insert_ty(&mut self, name: String, ty: &'ir Ty<'ir>) {
        self.scopes[*self.stack.last().unwrap()]
            .types
            .insert(name, ty);
    }

    pub fn get_ty(&self, name: &str) -> Option<&'ir Ty<'ir>> {
        self.find(|scope| scope.types.get(name).map(|ty| *ty))
    }

    pub fn insert_local(&mut self, name: String, ty: &'ir Ty<'ir>, local_idx: ir::LocalIdx) {
        self.scopes[*self.stack.last().unwrap()].variables.insert(
            name,
            Variable {
                kind: VariableKind::Local(local_idx),
                ty,
            },
        );
    }

    pub fn insert_global(&mut self, name: String, ty: &'ir Ty<'ir>, global_idx: ir::GlobalIdx) {
        self.scopes[*self.stack.last().unwrap()].variables.insert(
            name,
            Variable {
                kind: VariableKind::Global(global_idx),
                ty,
            },
        );
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable<'ir>> {
        self.find(|scope| scope.variables.get(name))
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut Variable<'ir>> {
        self.find_mut(|scope| scope.variables.get_mut(name))
    }

    pub fn insert_fn(
        &mut self,
        name: String,
        params: Vec<&'ir Ty<'ir>>,
        ret_ty: &'ir Ty<'ir>,
        idx: FunctionIdx,
    ) {
        self.scopes[*self.stack.last().unwrap()].functions.insert(
            name,
            Function {
                idx,
                signature: FnSig { params, ret_ty },
            },
        );
    }

    pub fn get_fn(&self, name: &str) -> Option<&Function<'ir>> {
        self.find(|scope| scope.functions.get(name))
    }

    pub fn get_symbol_ty(&self, name: &str, ctx: &mut Context<'ir>) -> Option<&'ir Ty<'ir>> {
        self.find(|scope| {
            scope
                .variables
                .get(name)
                .map(|variable| variable.ty)
                .or_else(|| {
                    scope.functions.get(name).map(|func| {
                        &*ctx.allocator.alloc(Ty::Fn(
                            ctx.allocator.alloc_slice_copy(&func.signature.params),
                            func.signature.ret_ty,
                        ))
                    })
                })
        })
    }
}
