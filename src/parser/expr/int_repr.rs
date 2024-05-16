#[derive(Debug, Clone, PartialEq, Default)]
pub struct IntLitRepr {
    bytes: Vec<u8>,
    signed: bool,
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

    pub fn signed(&mut self, signed: bool) {
        self.signed = signed;
    }
}

impl ToString for IntLitRepr {
    fn to_string(&self) -> String {
        let mut res = String::from("0");

        for (i, byte) in self.bytes.iter().rev().enumerate() {
            for j in 0..8 {
                if i == 0 && j == 0 && self.signed {
                    continue;
                }

                let bit = (byte << j) & 0b1000_0000;
                res = Self::mul2(&res);

                if bit != 0 {
                    Self::inc(&mut res);
                }
            }
        }

        if self.signed {
            res.insert(0, '-');
        }

        res
    }
}

impl From<&str> for IntLitRepr {
    fn from(value: &str) -> Self {
        let mut int_repr = Self::default();
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

        int_repr
    }
}

#[cfg(test)]
mod test {
    use super::IntLitRepr;

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
    fn int_repr() {
        let tests = [
            (IntLitRepr::from("0").to_string(), "0"),
            (IntLitRepr::from("65535").to_string(), "65535"),
            (IntLitRepr::from("4294967295").to_string(), "4294967295"),
            (
                IntLitRepr::from("18446744073709551615").to_string(),
                "18446744073709551615",
            ),
            (
                IntLitRepr::from("340282366920938463463374607431768211455").to_string(),
                "340282366920938463463374607431768211455",
            ),
            (
                IntLitRepr::from("340282366920938463463374607431768211456").to_string(),
                "340282366920938463463374607431768211456",
            ),
        ];

        for (actual, expected) in tests {
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn sign() {
        let mut lit = IntLitRepr::from("25");

        lit.signed(true);
        assert_eq!(lit.to_string(), "-25");

        lit.signed(false);
        assert_eq!(lit.to_string(), "25");
    }
}
