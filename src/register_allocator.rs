#[derive(Clone, PartialEq)]
pub struct Register {
    byte: &'static str,
    word: &'static str,
    dword: &'static str,
    qword: &'static str,
}

impl Register {
    pub fn new(
        byte: &'static str,
        word: &'static str,
        dword: &'static str,
        qword: &'static str,
    ) -> Self {
        return Self {
            byte,
            word,
            dword,
            qword,
        };
    }

    pub fn byte(&self) -> &'static str {
        self.byte
    }

    pub fn word(&self) -> &'static str {
        self.word
    }

    pub fn dword(&self) -> &'static str {
        self.dword
    }

    pub fn qword(&self) -> &'static str {
        self.qword
    }

    pub fn from_size(&self, size: usize) -> &'static str {
        match size {
            1 => self.byte(),
            2 => self.word(),
            4 => self.dword(),
            8 => self.qword(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum AllocatorError {
    DoubleFree,
    RanOutOfRegisters,
}

impl std::fmt::Display for AllocatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DoubleFree => write!(f, "Register was double freed"),
            Self::RanOutOfRegisters => write!(f, "Ran out of registers, whoops!"),
        }
    }
}

pub struct RegisterAllocator {
    registers: Vec<Register>,
    used: Vec<u8>,
}

impl RegisterAllocator {
    pub fn new(registers: Vec<Register>) -> Self {
        return Self {
            used: Vec::with_capacity(registers.len()),
            registers,
        };
    }

    pub fn alloc(&mut self) -> Result<Register, AllocatorError> {
        for (i, reg) in self.registers.iter().enumerate() {
            if !self.used.contains(&i.try_into().unwrap()) {
                self.used.push(i.try_into().unwrap());

                return Ok(reg.clone());
            }
        }

        Err(AllocatorError::RanOutOfRegisters)
    }

    pub fn free(&mut self, r: Register) -> Result<(), AllocatorError> {
        for (i, register) in self.registers.iter().enumerate() {
            if r == *register {
                if self.used.contains(&i.try_into().unwrap()) {
                    self.used.remove(i);

                    return Ok(());
                } else {
                    return Err(AllocatorError::DoubleFree);
                }
            }
        }

        Ok(())
    }

    pub fn len(&self) -> usize {
        self.registers.len()
    }

    pub fn get(&self, n: usize) -> Option<Register> {
        self.registers.get(n).cloned()
    }
}
