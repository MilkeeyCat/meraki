#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Char,
    Bool,
    Ptr(Box<Type>),
}
