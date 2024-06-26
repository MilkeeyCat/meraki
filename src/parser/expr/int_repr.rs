use std::fmt::Display;

use crate::type_::Type;

const MAX_BITS_NUM_SUPPORTED: usize = 16;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct IntLitRepr {
    bytes: Vec<u8>,
    negative: bool,
}

impl IntLitRepr {
    fn mul2(num: &str) -> String {
        let mut result = Vec::new();
        let mut carry = 0;

        for digit_char in num.chars().rev() {
            let digit = digit_char as u8 - b'0';
            let product = digit * 2 + carry;

            result.push((product % 10).to_string());
            carry = product / 10;
        }

        if carry > 0 {
            result.push(carry.to_string());
        }

        result.into_iter().rev().collect()
    }

    fn inc(num: &mut String) {
        let len = num.len();
        unsafe {
            num.as_bytes_mut()[len - 1] += 1;
        }
    }

    fn div2(num: &str) -> Option<(String, bool)> {
        let mut remainder = false;
        let result = num
            .chars()
            .map(|ch| {
                let mut new = ch as u8 - b'0';
                if remainder {
                    new += 10;
                }

                if new & 1 != 0 {
                    remainder = true;
                } else {
                    remainder = false;
                }

                new >>= 1;

                (new + b'0') as char
            })
            .collect::<String>();

        if result.len() > 0 {
            Some((result, remainder))
        } else {
            None
        }
    }

    fn set_bit(&mut self, n: usize) {
        if let None = self.bytes.get(n / 8) {
            self.bytes.push(0);
            self.set_bit(n);
        } else {
            self.bytes[n / 8] |= 1 << (n % 8);
        }
    }

    fn bits(&self) -> usize {
        self.bytes.len() * 8
    }

    pub fn type_(&self) -> Type {
        match (self.bytes.len(), self.negative) {
            (1, false) => Type::U8,
            (1, true) => Type::I8,
            (2, false) => Type::U16,
            (2, true) => Type::I16,
            _ => unreachable!(),
        }
    }

    pub fn first_bit_set(&self) -> bool {
        self.bytes.last().unwrap() & 0b1000_0000 != 0
    }

    pub fn widen_type(&self) -> Result<Type, IntLitReprError> {
        match self.type_() {
            Type::U8 => Ok(Type::I16),
            _ => Err(IntLitReprError::TooLarge(self.bits() * 2)),
        }
    }

    pub fn resize(&mut self, size: usize) {
        self.bytes.resize(size, 0);
    }

    pub fn negate(&mut self) {
        if self.bytes.iter().all(|byte| byte == &0) {
            return;
        }

        if !self.negative {
            if self.bytes.iter().last().unwrap() > &0x80
                || (self.bytes.len() > 0 && !self.bytes.iter().skip(1).all(|byte| byte > &0))
            {
                self.bytes.push(0);
            }
        }

        self.negative = !self.negative;

        *self.bytes.first_mut().unwrap() = self.bytes.first_mut().unwrap().wrapping_sub(1);
        self.bytes.iter_mut().for_each(|byte| *byte ^= 0xff);

        if self.bytes.iter().last().unwrap() == &0 {
            self.bytes.remove(self.bytes.len() - 1);
        }
    }
}

impl ToString for IntLitRepr {
    fn to_string(&self) -> String {
        let mut clone = self.clone();
        let mut res = String::from("0");

        if self.negative {
            clone.negate();
        }

        for byte in clone.bytes.iter().rev() {
            for j in 0..8 {
                let bit = (byte << j) & 0b1000_0000;
                res = Self::mul2(&res);

                if bit != 0 {
                    Self::inc(&mut res);
                }
            }
        }

        if self.negative {
            res.insert(0, '-');
        }

        res
    }
}

#[derive(Debug)]
pub enum IntLitReprError {
    TooLarge(usize),
}

impl Display for IntLitReprError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooLarge(bits) => write!(f, "{} bits integers are not supported", bits),
        }
    }
}

impl TryFrom<&str> for IntLitRepr {
    type Error = IntLitReprError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut int_repr = Self {
            negative: false,
            bytes: vec![0],
        };
        let mut result = value.to_owned();

        for i in 0.. {
            match Self::div2(&result) {
                Some((string, remainder)) => {
                    result = string;

                    if result.as_bytes()[0] == b'0' {
                        result.remove(0);
                    }

                    if remainder {
                        int_repr.set_bit(i);
                    }
                }
                None => break,
            }
        }

        if int_repr.bits() > MAX_BITS_NUM_SUPPORTED {
            return Err(IntLitReprError::TooLarge(int_repr.bits()));
        }

        Ok(int_repr)
    }
}

#[cfg(test)]
mod test {
    use super::{IntLitRepr, IntLitReprError};

    #[test]
    fn multiplication() {
        let tests = [
            (IntLitRepr::mul2("0"), "0"),
            (IntLitRepr::mul2("2"), "4"),
            (IntLitRepr::mul2("9"), "18"),
            (IntLitRepr::mul2("100"), "200"),
            (IntLitRepr::mul2("99"), "198"),
        ];

        for (actual, expected) in tests {
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn division() {
        let tests = [
            (IntLitRepr::div2("0"), "0"),
            (IntLitRepr::div2("2"), "1"),
            (IntLitRepr::div2("9"), "4"),
            (IntLitRepr::div2("100"), "050"),
            (IntLitRepr::div2("99"), "49"),
        ];

        for (actual, expected) in tests {
            assert_eq!(actual.unwrap().0, expected);
        }
    }

    #[test]
    fn addition() {
        let mut num = String::from("0");
        let tests = ["1", "2", "3", "4", "5", "6"];

        for actual in tests {
            IntLitRepr::inc(&mut num);
            assert_eq!(num, actual);
        }
    }

    #[test]
    fn int_repr() -> Result<(), IntLitReprError> {
        let tests = [
            (IntLitRepr::try_from("0")?.to_string(), "0"),
            (IntLitRepr::try_from("255")?.to_string(), "255"),
            (IntLitRepr::try_from("65535")?.to_string(), "65535"),
            //(IntLitRepr::from("4294967295").to_string(), "4294967295"),
            //(
            //    IntLitRepr::from("18446744073709551615").to_string(),
            //    "18446744073709551615",
            //),
        ];

        for (actual, expected) in tests {
            assert_eq!(actual, expected);
        }

        assert!(IntLitRepr::try_from("65536").is_err());

        Ok(())
    }

    #[test]
    fn negation() {
        for i in u16::MIN..=u16::MAX {
            let int_repr = IntLitRepr::try_from(i.to_string().as_str()).unwrap();

            assert_eq!(int_repr.to_string(), i.to_string());
        }

        for i in i16::MIN..=i16::MAX {
            let mut int_repr = if i < 0 {
                IntLitRepr::try_from(&i.to_string().as_str()[1..]).unwrap()
            } else {
                IntLitRepr::try_from(i.to_string().as_str()).unwrap()
            };

            if i <= 0 {
                int_repr.negate();
            }

            assert_eq!(int_repr.to_string(), i.to_string());
        }
    }
}
