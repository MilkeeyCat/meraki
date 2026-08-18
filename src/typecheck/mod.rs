mod infer;
pub(crate) mod ty_problem;
mod typecheck;

pub(crate) use typecheck::typecheck;
