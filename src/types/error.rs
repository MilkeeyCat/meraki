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
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentNotFound(ident) => write!(f, "Ident {} not found", ident),
            Self::Promotion(lhs, rhs) => {
                write!(f, "Operation between {} and {} are not allowed", lhs, rhs)
            }
            Self::Assignment(lhs, rhs) => write!(f, "Can't assign {} to {}", lhs, rhs),
            Self::Cast(from, to) => write!(f, "Can't cast {} into {}", from, to),
            Self::Return(left, right) => write!(
                f,
                "Expected return value of type {},  got {} instead",
                right, left
            ),
            Self::VoidVariable => write!(f, "Variable can't be of type void"),
            Self::Nonexistent(name) => write!(f, "Type '{name}' doens't exits"),
            Self::Deref(type_) => write!(f, "Type {type_} is not pointer"),
        }
    }
}
