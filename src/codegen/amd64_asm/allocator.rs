use super::{OperandSize, register::Register};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Register was double freed")]
    DoubleFree,
    #[error("Ran out of registers, whoops!")]
    RanOutOfRegisters,
    #[error("Register {0} is already in use")]
    AlreadyInUse(Register),
}

#[derive(Debug, Clone)]
pub struct RegisterAllocator {
    registers: Vec<Register>,
    used: Vec<usize>,
}

impl RegisterAllocator {
    pub fn new(registers: Vec<Register>) -> Self {
        return Self {
            used: Vec::with_capacity(registers.len()),
            registers,
        };
    }

    pub fn alloc(&mut self, size: OperandSize) -> Result<Register, Error> {
        for (i, reg) in self.registers.iter().enumerate() {
            if !self.used.contains(&i.try_into().unwrap()) {
                self.used.push(i.try_into().unwrap());

                return Ok(reg.resize(size));
            }
        }

        Err(Error::RanOutOfRegisters)
    }

    pub fn alloc_nth(&mut self, n: usize) -> Result<Register, Error> {
        if self.used.contains(&n) {
            Err(Error::AlreadyInUse(self.registers[n].clone()))
        } else {
            self.used.push(n);

            Ok(self.registers[n].clone())
        }
    }

    pub fn free(&mut self, r: Register) -> Result<(), Error> {
        for (i, register) in self.registers.iter().enumerate() {
            if &r == register {
                if let Some(i) = self.used.iter().position(|el| el == &i) {
                    self.used.remove(i);

                    break;
                } else {
                    return Err(Error::DoubleFree);
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

    pub fn is_used(&self, r: &Register) -> bool {
        match self
            .registers
            .iter()
            .enumerate()
            .find_map(|(i, reg)| if reg == r { Some(i) } else { None })
        {
            Some(i) => self.used.contains(&i),
            None => false,
        }
    }

    pub fn used(&self) -> &[usize] {
        &self.used
    }
}
