use crate::types::{IntType, Type, UintType};
use std::num::ParseIntError;
use thiserror::Error;

const I8_MIN: i64 = i8::MIN as i64;
const I8_MAX: i64 = i8::MAX as i64;
const I16_MIN: i64 = i16::MIN as i64;
const I16_MAX: i64 = i16::MAX as i64;
const I32_MIN: i64 = i32::MIN as i64;
const I32_MAX: i64 = i32::MAX as i64;
const I64_MIN: i64 = i64::MIN as i64;
const I64_MAX: i64 = i64::MAX as i64;

const U8_MIN: u64 = u8::MIN as u64;
const U8_MAX: u64 = u8::MAX as u64;
const U16_MIN: u64 = u16::MIN as u64;
const U16_MAX: u64 = u16::MAX as u64;
const U32_MIN: u64 = u32::MIN as u64;
const U32_MAX: u64 = u32::MAX as u64;
const U64_MIN: u64 = u64::MIN as u64;
const U64_MAX: u64 = u64::MAX as u64;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct IntLitRepr {
    pub inner: i64,
}

impl IntLitRepr {
    pub fn new(value: i64) -> Self {
        Self { inner: value }
    }

    pub fn type_(&self) -> Type {
        match self.inner {
            I8_MIN..=I8_MAX => Type::Int(IntType::I8),
            I16_MIN..=I16_MAX => Type::Int(IntType::I16),
            I32_MIN..=I32_MAX => Type::Int(IntType::I32),
            I64_MIN..=I64_MAX => Type::Int(IntType::I64),
        }
    }

    pub fn zero_except_n_bytes(&mut self, n: usize) {
        self.inner &= 0 << n * 8;
    }

    pub fn negate(&mut self) {
        self.inner = -self.inner;
    }
}

impl ToString for IntLitRepr {
    fn to_string(&self) -> String {
        self.inner.to_string()
    }
}

impl TryFrom<UIntLitRepr> for IntLitRepr {
    type Error = IntLitReprError;

    fn try_from(value: UIntLitRepr) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: value.inner.try_into().unwrap(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct UIntLitRepr {
    pub inner: u64,
}

impl UIntLitRepr {
    pub fn new(value: u64) -> Self {
        Self { inner: value }
    }

    pub fn type_(&self) -> Type {
        match self.inner {
            U8_MIN..=U8_MAX => Type::UInt(UintType::U8),
            U16_MIN..=U16_MAX => Type::UInt(UintType::U16),
            U32_MIN..=U32_MAX => Type::UInt(UintType::U32),
            U64_MIN..=U64_MAX => Type::UInt(UintType::U64),
        }
    }

    pub fn zero_except_n_bytes(&mut self, n: usize) {
        self.inner &= 0 << n * 8;
    }
}

impl ToString for UIntLitRepr {
    fn to_string(&self) -> String {
        self.inner.to_string()
    }
}

impl TryFrom<&str> for UIntLitRepr {
    type Error = IntLitReprError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: value.parse::<u64>()?,
        })
    }
}

#[derive(Error, Debug)]
pub enum IntLitReprError {
    #[error("{0} bits integers are not supported")]
    TooLarge(usize),
    #[error(transparent)]
    ParseInt(#[from] ParseIntError),
}
