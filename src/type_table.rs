#[derive(Debug)]
pub enum Type {
    Struct(TypeStruct),
}

type Field = (String, crate::type_::Type);

#[derive(Debug)]
pub struct TypeStruct {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub struct TypeTable(Vec<Type>);

impl TypeTable {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn define(&mut self, type_: Type) {
        self.0.push(type_);
    }

    pub fn find(&self, type_name: &str) -> Option<&Type> {
        self.0.iter().find(|type_| match type_ {
            Type::Struct(type_struct) => type_struct.name == type_name,
        })
    }
}
