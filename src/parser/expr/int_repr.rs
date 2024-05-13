#[derive(Debug, Clone, PartialEq)]
pub struct IntLitRepr(Vec<u8>);

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
        if let None = self.0.get(n / 8) {
            self.0.push(0);
            self.set_bit(n);
        } else {
            self.0[n / 8] |= 1 << (n % 8);
        }
    }
}

impl ToString for IntLitRepr {
    fn to_string(&self) -> String {
        let mut res = String::from("0");

        for byte in self.0.iter().rev() {
            for i in 0..8 {
                let bit = (byte << i) & 0b10000000;
                res = Self::mul2(&res);

                if bit != 0 {
                    Self::inc(&mut res);
                }
            }
        }

        res
    }
}

impl From<&str> for IntLitRepr {
    fn from(value: &str) -> Self {
        let mut int_repr = Self(Vec::new());
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

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "1");

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "2");

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "3");

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "4");

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "5");

        IntLitRepr::inc(&mut num);
        assert_eq!(num, "6");
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
}
