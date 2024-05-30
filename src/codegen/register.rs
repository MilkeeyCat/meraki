#[derive(Clone, PartialEq)]
pub struct Register {
    byte: &'static str,
    dword: &'static str,
}

impl Register {
    pub fn new(byte: &'static str, dword: &'static str) -> Self {
        return Self { byte, dword };
    }

    pub fn byte(&self) -> &'static str {
        self.byte
    }

    pub fn dword(&self) -> &'static str {
        self.dword
    }
}

#[derive(Debug)]
pub enum AllocatorError {
    DoubleFree,
    RanOutOfRegisters,
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
}
