use crate::type_::Type;

#[derive(Default)]
pub enum Scope {
    #[default]
    Global,
    Local(String, Type),
}
