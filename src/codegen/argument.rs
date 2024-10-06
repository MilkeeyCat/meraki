use crate::register::Register;

pub enum Argument {
    Register(Register),
    Stack(usize),
}
