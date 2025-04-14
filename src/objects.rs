use std::collections::HashMap;

use crate::{ast, environment::Environment};

#[derive(Clone, derive_more::Display, derive_more::Debug)]
pub enum Object {
    #[display("null")]
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    #[display("'{_0}'")]
    String(String),
    Char(char),
    #[display(
        "[{}]",
        values.iter().map(|stmt| format!("{stmt}")).collect::<Vec<_>>().join(", ")
    )]
    Array {
        values: Vec<Object>,
    },
    #[display(
        "{{ {} }}",
        values.iter().map(|(key,value)| format!("{key}: {value}")).collect::<Vec<_>>().join(", ")
    )]
    Dict {
        values: HashMap<HashKey, Object>,
    },
    #[display("ERROR: {_0}")]
    Error(String),
    #[display("<function>")]
    Function {
        parameters: Vec<String>,
        body: ast::Block,
        scoped_env: Environment,
    },
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Bool(b) => *b,
            Object::Integer(n) => *n != 0,
            Object::Float(n) => *n != 0.0,
            Object::String(s) => s.chars().all(char::is_whitespace),
            Object::Char(c) => *c != '\0' && *c != ' ',
            Object::Error(_) => false,
            Object::Function { .. } => true,
            Object::Array { values } => !values.is_empty(),
            Object::Dict { values } => !values.is_empty(),
        }
    }
}

impl AsRef<Object> for Object {
    fn as_ref(&self) -> &Object {
        self
    }
}

impl TryFrom<&Object> for HashKey {
    type Error = String;

    fn try_from(value: &Object) -> Result<Self, Self::Error> {
        match value {
            Object::Bool(b) => Ok(Self::Bool(*b)),
            Object::Integer(n) => Ok(Self::Integer(*n)),
            Object::String(s) => Ok(Self::String(s.clone())),
            Object::Char(c) => Ok(Self::Char(*c)),
            _ => Err(format!(
                "{:?} can't be used as dict key",
                std::mem::discriminant(value)
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::Display)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Bool(bool),
    Char(char),
}
