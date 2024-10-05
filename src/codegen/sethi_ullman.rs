use crate::parser::Expr;

pub trait SethiUllman {
    fn r_num(&self) -> usize;
    fn is_leaf(&self) -> bool;
}

impl SethiUllman for Expr {
    fn r_num(&self) -> usize {
        match self {
            Self::Binary(expr) => {
                let lhs = expr.left.r_num();
                let rhs = if expr.right.is_leaf() {
                    0
                } else {
                    expr.right.r_num()
                };

                if lhs == rhs {
                    lhs + 1
                } else {
                    std::cmp::max(lhs, rhs)
                }
            }
            Self::Unary(expr) => expr.expr.r_num(),
            Self::Lit(_) => 0,
            Self::Ident(_) => 1,
            Self::Cast(expr) => expr.expr.r_num(),
            Self::StructAccess(expr) => expr.expr.r_num(),
            Self::Struct(_)
            | Self::Array(_)
            | Self::StructMethod(_)
            | Self::ArrayAccess(_)
            | Self::FunctionCall(_) => {
                unreachable!("Not sure what to do here")
            }
        }
    }

    fn is_leaf(&self) -> bool {
        match self {
            Self::Binary(_) | Self::Unary(_) | Self::Cast(_) => false,
            Self::Lit(_) | Self::Ident(_) => true,
            Self::Struct(_)
            | Self::Array(_)
            | Self::StructAccess(_)
            | Self::StructMethod(_)
            | Self::ArrayAccess(_)
            | Self::FunctionCall(_) => unreachable!("Not sure what to do here"),
        }
    }
}
