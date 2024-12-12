use super::{
    operand::{Base, Destination, EffectiveAddress, Offset, Source},
    OperandSize,
};
use derive_more::derive::Display;

#[derive(Display, Debug, Copy, Clone, PartialEq)]
pub enum Register {
    #[display("al")]
    Al,
    #[display("ax")]
    Ax,
    #[display("eax")]
    Eax,
    #[display("rax")]
    Rax,

    #[display("bpl")]
    Bpl,
    #[display("bp")]
    Bp,
    #[display("ebp")]
    Ebp,
    #[display("rbp")]
    Rbp,

    #[display("cl")]
    Cl,
    #[display("cx")]
    Cx,
    #[display("ecx")]
    Ecx,
    #[display("rcx")]
    Rcx,

    #[display("dl")]
    Dl,
    #[display("dx")]
    Dx,
    #[display("edx")]
    Edx,
    #[display("rdx")]
    Rdx,

    #[display("sil")]
    Sil,
    #[display("si")]
    Si,
    #[display("esi")]
    Esi,
    #[display("rsi")]
    Rsi,

    #[display("dil")]
    Dil,
    #[display("di")]
    Di,
    #[display("edi")]
    Edi,
    #[display("rdi")]
    Rdi,

    #[display("rsp")]
    Rsp,
    #[display("esp")]
    Esp,
    #[display("sp")]
    Sp,
    #[display("spl")]
    Spl,

    #[display("r15b")]
    R15b,
    #[display("r15w")]
    R15w,
    #[display("r15d")]
    R15d,
    #[display("r15")]
    R15,

    #[display("r14b")]
    R14b,
    #[display("r14w")]
    R14w,
    #[display("r14d")]
    R14d,
    #[display("r14")]
    R14,

    #[display("r13b")]
    R13b,
    #[display("r13w")]
    R13w,
    #[display("r13d")]
    R13d,
    #[display("r13")]
    R13,

    #[display("r12b")]
    R12b,
    #[display("r12w")]
    R12w,
    #[display("r12d")]
    R12d,
    #[display("r12")]
    R12,

    #[display("r11b")]
    R11b,
    #[display("r11w")]
    R11w,
    #[display("r11d")]
    R11d,
    #[display("r11")]
    R11,

    #[display("r10b")]
    R10b,
    #[display("r10w")]
    R10w,
    #[display("r10d")]
    R10d,
    #[display("r10")]
    R10,

    #[display("r9b")]
    R9b,
    #[display("r9w")]
    R9w,
    #[display("r9d")]
    R9d,
    #[display("r9")]
    R9,

    #[display("r8b")]
    R8b,
    #[display("r8w")]
    R8w,
    #[display("r8d")]
    R8d,
    #[display("r8")]
    R8,
}

impl Register {
    pub fn resize(self, size: OperandSize) -> Self {
        match (self, size) {
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Byte) => Self::Al,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Word) => Self::Ax,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Dword) => Self::Eax,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Qword) => Self::Rax,

            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Byte) => Self::Bpl,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Word) => Self::Bp,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Dword) => Self::Ebp,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Qword) => Self::Rbp,

            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Byte) => Self::Cl,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Word) => Self::Cx,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Dword) => Self::Ecx,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Qword) => Self::Rcx,

            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Byte) => Self::Dl,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Word) => Self::Dx,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Dword) => Self::Edx,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Qword) => Self::Rdx,

            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Byte) => Self::Sil,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Word) => Self::Si,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Dword) => Self::Esi,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Qword) => Self::Rsi,

            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Byte) => Self::Dil,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Word) => Self::Di,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Dword) => Self::Edi,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Qword) => Self::Rdi,

            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Byte) => Self::Spl,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Word) => Self::Sp,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Dword) => Self::Esp,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Qword) => Self::Rsp,

            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Byte) => Self::R15b,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Word) => Self::R15w,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Dword) => Self::R15d,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Qword) => Self::R15,

            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Byte) => Self::R14b,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Word) => Self::R14w,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Dword) => Self::R14d,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Qword) => Self::R14,

            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Byte) => Self::R13b,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Word) => Self::R13w,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Dword) => Self::R13d,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Qword) => Self::R13,

            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Byte) => Self::R12b,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Word) => Self::R12w,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Dword) => Self::R12d,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Qword) => Self::R12,

            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Byte) => Self::R11b,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Word) => Self::R11w,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Dword) => Self::R11d,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Qword) => Self::R11,

            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Byte) => Self::R10b,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Word) => Self::R10w,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Dword) => Self::R10d,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Qword) => Self::R10,

            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Byte) => Self::R9b,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Word) => Self::R9w,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Dword) => Self::R9d,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Qword) => Self::R9,

            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Byte) => Self::R8b,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Word) => Self::R8w,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Dword) => Self::R8d,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Qword) => Self::R8,
        }
    }

    pub fn size(&self) -> OperandSize {
        match self {
            Self::Al => OperandSize::Byte,
            Self::Ax => OperandSize::Word,
            Self::Eax => OperandSize::Dword,
            Self::Rax => OperandSize::Qword,

            Self::Bpl => OperandSize::Byte,
            Self::Bp => OperandSize::Word,
            Self::Ebp => OperandSize::Dword,
            Self::Rbp => OperandSize::Qword,

            Self::Cl => OperandSize::Byte,
            Self::Cx => OperandSize::Word,
            Self::Ecx => OperandSize::Dword,
            Self::Rcx => OperandSize::Qword,

            Self::Dl => OperandSize::Byte,
            Self::Dx => OperandSize::Word,
            Self::Edx => OperandSize::Dword,
            Self::Rdx => OperandSize::Qword,

            Self::Sil => OperandSize::Byte,
            Self::Si => OperandSize::Word,
            Self::Esi => OperandSize::Dword,
            Self::Rsi => OperandSize::Qword,

            Self::Dil => OperandSize::Byte,
            Self::Di => OperandSize::Word,
            Self::Edi => OperandSize::Dword,
            Self::Rdi => OperandSize::Qword,

            Self::Spl => OperandSize::Byte,
            Self::Sp => OperandSize::Word,
            Self::Esp => OperandSize::Dword,
            Self::Rsp => OperandSize::Qword,

            Self::R15b => OperandSize::Byte,
            Self::R15w => OperandSize::Word,
            Self::R15d => OperandSize::Dword,
            Self::R15 => OperandSize::Qword,

            Self::R14b => OperandSize::Byte,
            Self::R14w => OperandSize::Word,
            Self::R14d => OperandSize::Dword,
            Self::R14 => OperandSize::Qword,

            Self::R13b => OperandSize::Byte,
            Self::R13w => OperandSize::Word,
            Self::R13d => OperandSize::Dword,
            Self::R13 => OperandSize::Qword,

            Self::R12b => OperandSize::Byte,
            Self::R12w => OperandSize::Word,
            Self::R12d => OperandSize::Dword,
            Self::R12 => OperandSize::Qword,

            Self::R11b => OperandSize::Byte,
            Self::R11w => OperandSize::Word,
            Self::R11d => OperandSize::Dword,
            Self::R11 => OperandSize::Qword,

            Self::R10b => OperandSize::Byte,
            Self::R10w => OperandSize::Word,
            Self::R10d => OperandSize::Dword,
            Self::R10 => OperandSize::Qword,

            Self::R9b => OperandSize::Byte,
            Self::R9w => OperandSize::Word,
            Self::R9d => OperandSize::Dword,
            Self::R9 => OperandSize::Qword,

            Self::R8b => OperandSize::Byte,
            Self::R8w => OperandSize::Word,
            Self::R8d => OperandSize::Dword,
            Self::R8 => OperandSize::Qword,
        }
    }

    pub fn into_effective_addr(self, displacement: isize) -> EffectiveAddress {
        EffectiveAddress {
            base: Base::Register(self),
            index: None,
            scale: None,
            displacement: Some(Offset(displacement)),
        }
    }
}

impl Into<Destination> for Register {
    fn into(self) -> Destination {
        Destination::Register(self)
    }
}

impl Into<Source> for Register {
    fn into(self) -> Source {
        Source::Register(self)
    }
}
