use crate::codegen::{
    operands::{self, Source},
    Destination,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Register {
    byte: &'static str,
    word: &'static str,
    dword: &'static str,
    qword: &'static str,
}

impl Register {
    pub const fn new(
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

    pub fn dest(&self, size: usize) -> Destination {
        Destination::Register(operands::Register {
            register: *self,
            size,
        })
    }

    pub fn source(&self, size: usize) -> Source {
        Source::Register(operands::Register {
            register: *self,
            size,
        })
    }
}
