use crate::{
    archs::Arch,
    codegen::Offset,
    scope::Scope,
    types::{self, TypeError},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Struct(TypeStruct),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStruct {
    pub name: String,
    pub fields: Vec<(String, types::Type)>,
    pub methods: Vec<TypeStructMethod>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStructMethod {
    pub return_type: types::Type,
    pub name: String,
    pub params: Vec<(String, types::Type)>,
}

impl TypeStruct {
    pub fn size(&self, arch: &Arch, scope: &Scope) -> Result<usize, TypeError> {
        let mut offset: usize = 0;
        let mut largest = 0;

        for (_, type_) in &self.fields {
            let size = type_.size(arch, scope)?;

            offset = offset.next_multiple_of(size);
            offset += size;

            if size > largest {
                largest = size;
            }
        }

        // Align to the largest element in the struct
        Ok(offset.next_multiple_of(largest))
    }

    pub fn offset(&self, arch: &Arch, name: &str, scope: &Scope) -> Result<Offset, TypeError> {
        let mut offset: usize = 0;

        for (field_name, type_) in &self.fields {
            let size = type_.size(arch, scope)?;
            offset = offset.next_multiple_of(size);
            if name == field_name {
                break;
            }

            offset += size;
        }

        Ok(Offset(offset as isize))
    }

    pub fn get_field_type(&self, field: &str) -> Option<&types::Type> {
        self.fields
            .iter()
            .find(|(name, _)| name == field)
            .map(|(_, type_)| type_)
    }

    pub fn find_method(&self, name: &str) -> Option<&TypeStructMethod> {
        self.methods.iter().find(|method| method.name == name)
    }

    pub fn contains(&self, field: &str) -> bool {
        self.fields.iter().any(|(name, _)| name == field)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeTable(pub Vec<Type>);

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
