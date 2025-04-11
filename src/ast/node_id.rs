use super::{
    Expr, Item, Stmt,
    visitor::{Visitor, walk_expr, walk_item, walk_stmt},
};

pub type NodeId = usize;
pub const DUMMY_NODE_ID: NodeId = 0;

struct Assigner(NodeId);

impl Assigner {
    fn new() -> Self {
        Assigner(0)
    }

    fn next_node_id(&mut self) -> NodeId {
        self.0 += 1;

        self.0
    }
}

impl<'ast> Visitor<'ast> for Assigner {
    fn visit_expr(&mut self, expr: &'ast mut Expr) {
        expr.id = self.next_node_id();

        walk_expr(self, expr)
    }

    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) {
        stmt.id = self.next_node_id();

        walk_stmt(self, stmt);
    }

    fn visit_item(&mut self, item: &'ast mut Item) {
        item.id = self.next_node_id();

        walk_item(self, item);
    }
}

pub fn assign_node_ids(ast: &mut Vec<Item>) {
    let mut visitor = Assigner::new();

    for item in ast {
        visitor.visit_item(item);
    }
}
