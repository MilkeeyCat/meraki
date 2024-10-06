use crate::parser::Expr;

pub trait SethiUllman {
    fn r_num(&self) -> usize;
    fn is_leaf(&self) -> bool;
    fn eval(lhs: usize, rhs: usize) -> usize {
        if lhs == rhs {
            lhs + 1
        } else {
            std::cmp::max(lhs, rhs)
        }
    }
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

                Self::eval(lhs, rhs)
            }
            Self::Unary(expr) => expr.expr.r_num(),
            Self::Lit(_) => 0,
            Self::Ident(_) => 1,
            Self::Cast(expr) => expr.expr.r_num(),
            Self::StructAccess(expr) => expr.expr.r_num(),
            Self::StructMethod(expr) => {
                // NOTE: + 1 part here is because in codegen it allocates 1 more register to generate code for method call
                let method = expr.expr.r_num() + 1;
                let args = expr
                    .arguments
                    .iter()
                    .map(|expr| expr.r_num())
                    .max()
                    .unwrap_or(0);

                Self::eval(method, args)
            }
            Self::ArrayAccess(expr) => Self::eval(expr.expr.r_num(), expr.index.r_num()),
            //NOTE: I don't really know how to calculate correct value yet, so calculate functions first
            Self::FunctionCall(_) => usize::MAX,
            Self::Struct(_) | Self::Array(_) => unreachable!(),
        }
    }

    fn is_leaf(&self) -> bool {
        match self {
            Self::Binary(_)
            | Self::Unary(_)
            | Self::Cast(_)
            | Self::StructAccess(_)
            | Self::StructMethod(_)
            | Self::ArrayAccess(_)
            | Self::FunctionCall(_) => false,
            Self::Lit(_) | Self::Ident(_) => true,
            Self::Struct(_) | Self::Array(_) => unreachable!(),
        }
    }
}
