use derive_more::derive::Display;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum IntTy {
    #[display("i8")]
    I8,
    #[display("i16")]
    I16,
    #[display("i32")]
    I32,
    #[display("i64")]
    I64,
    #[display("isize")]
    Isize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum UintTy {
    #[display("u8")]
    U8,
    #[display("u16")]
    U16,
    #[display("u32")]
    U32,
    #[display("u64")]
    U64,
    #[display("usize")]
    Usize,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum Ty {
    #[display("null")]
    Null,
    #[display("void")]
    Void,
    #[display("bool")]
    Bool,
    Int(IntTy),
    UInt(UintTy),
    Ident(String),
    #[display("*{_0}")]
    Ptr(Box<Ty>),
    #[display("{ty}[{len}]")]
    Array {
        ty: Box<Ty>,
        len: usize,
    },
    #[display("fn ({}) -> {_1}",
        _0
            .iter()
            .map(|type_| type_.to_string())
            .collect::<String>()
    )]
    Fn(Vec<Ty>, Box<Ty>),
    #[display("infer")]
    Infer,
}
