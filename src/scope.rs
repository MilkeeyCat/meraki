use crate::parser::Type;

#[derive(Default)]
pub enum Scope {
    #[default]
    Global,
    Local(String, Type),
}
