use super::AllocatorError;
use crate::register::Register;

#[derive(Clone)]
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
