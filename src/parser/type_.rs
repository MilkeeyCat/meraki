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
    Void,
    Ptr(Box<Type>),
    Arr(TypeArr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeArr {
    len: usize,
    type_: Box<Type>,
}

impl TypeArr {
    pub fn new(type_: Type, len: usize) -> Self {
        Self {
            type_: Box::new(type_),
            len,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::I8 => "i8".to_string(),
                Type::U8 => "u8".to_string(),
                Type::I16 => "i16".to_string(),
                Type::U16 => "u16".to_string(),
                Type::I32 => "i32".to_string(),
                Type::U32 => "u32".to_string(),
                Type::I64 => "i64".to_string(),
                Type::U64 => "u64".to_string(),
                Type::Char => "char".to_string(),
                Type::Bool => "bool".to_string(),
                Type::Void => "void".to_string(),
                Type::Ptr(type_) => {
                    type_.to_string() + "*"
                }
                Type::Arr(arr) => {
                    format!("{}[{}]", arr.type_.to_string(), arr.len)
                }
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::{Type, TypeArr};

    #[test]
    fn types_formatting() {
        let cases = vec![
            (Type::I8, "i8"),
            (Type::U8, "u8"),
            (Type::I16, "i16"),
            (Type::U16, "u16"),
            (Type::I32, "i32"),
            (Type::U32, "u32"),
            (Type::I64, "i64"),
            (Type::U64, "u64"),
            (Type::Char, "char"),
            (Type::Void, "void"),
            (Type::Bool, "bool"),
            (Type::Ptr(Box::new(Type::Char)), "char*"),
            (
                Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Char)))),
                "char**",
            ),
            (
                Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Ptr(Box::new(
                    Type::Ptr(Box::new(Type::Bool)),
                )))))),
                "bool****",
            ),
            (Type::Arr(TypeArr::new(Type::Char, 69)), "char[69]"),
            (
                Type::Arr(TypeArr::new(
                    Type::Ptr(Box::new(Type::Ptr(Box::new(Type::I8)))),
                    420,
                )),
                "i8**[420]",
            ),
        ];

        for (type_, expected) in cases {
            assert_eq!(
                type_.to_string(),
                expected,
                "Wrong type string, expected: {}, got: {}",
                expected,
                type_
            );
        }
    }
}
