use super::Type;

#[derive(Debug, PartialEq)]
pub enum TypeError {
    Promotion(Type, Type),
    IdentNotFound(String),
    Assignment(Type, Type),
    Cast(Type, Type),
    Return(Type, Type),
    VoidVariable,
    Nonexistent(String),
    Deref(Type),
    Mismatched(Type, Type),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentNotFound(ident) => write!(f, "Ident {ident} not found"),
            Self::Promotion(lhs, rhs) => {
                write!(f, "Operation between {lhs} and {rhs} are not allowed")
            }
            Self::Assignment(lhs, rhs) => write!(f, "Can't assign {lhs} to {rhs}"),
            Self::Cast(from, to) => write!(f, "Can't cast {from} into {to}"),
            Self::Return(left, right) => write!(
                f,
                "Expected return value of type {right},  got {left} instead",
            ),
            Self::VoidVariable => write!(f, "Variable can't be of type void"),
            Self::Nonexistent(name) => write!(f, "Type '{name}' doens't exits"),
            Self::Deref(type_) => write!(f, "Type {type_} is not pointer"),
            Self::Mismatched(expected, actual) => {
                write!(f, "Mismatched types expected {expected}, found {actual}")
            }
        }
    }
}
