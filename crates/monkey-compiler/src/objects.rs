use crate::{builtin::BuiltinFunction, code::Instructions};
use std::{collections::HashMap, sync::Arc};

#[derive(Clone, derive_more::Display, derive_more::Debug)]
pub enum Object {
    #[display("null")]
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    #[display("{_0}")]
    String(String),
    Char(char),
    #[display(
        "[{}]",
        values.iter().map(|value| format!("{value}")).collect::<Vec<_>>().join(", ")
    )]
    Array {
        values: Vec<Object>,
    },
    #[display(
        "{{ {} }}",
        values.iter().map(|(key, value)| format!("{key}: {value}")).collect::<Vec<_>>().join(", ")
    )]
    Dict {
        values: HashMap<HashKey, Object>,
    },
    #[display("ERROR: {_0}")]
    Error(String),
    #[display("<builtin>")]
    #[debug("<builtin>")]
    BuiltinFunction {
        handler: Arc<BuiltinFunction>,
    },
    #[display("<compiled fn>")]
    #[debug("<compiled fn>")]
    CompiledFunction(CompiledFunc),
    #[display("<closure>")]
    #[debug("<closure>")]
    Closure(Closure),
}

#[derive(Debug, Default, Clone)]
pub struct CompiledFunc {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: usize,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: CompiledFunc,
    pub free: Vec<Object>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Array { values: l_values }, Self::Array { values: r_values }) => {
                l_values == r_values
            }
            (Self::Dict { values: l_values }, Self::Dict { values: r_values }) => {
                l_values == r_values
            }
            (Self::Error(l0), Self::Error(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Object {
    #[must_use]
    pub fn null() -> Self {
        Self::Null
    }

    #[must_use]
    pub fn bool(value: bool) -> Self {
        Self::Bool(value)
    }

    #[must_use]
    pub fn integer(value: i64) -> Self {
        Self::Integer(value)
    }

    #[must_use]
    pub fn float(value: f64) -> Self {
        Self::Float(value)
    }

    #[must_use]
    pub fn string(value: impl Into<String>) -> Self {
        Self::String(value.into())
    }

    #[must_use]
    pub fn char(value: char) -> Self {
        Self::Char(value)
    }

    #[must_use]
    pub fn array(values: Vec<Object>) -> Self {
        Self::Array { values }
    }

    #[must_use]
    pub fn dict(values: HashMap<HashKey, Object>) -> Self {
        Self::Dict { values }
    }

    #[must_use]
    pub fn error(message: impl Into<String>) -> Self {
        Self::Error(message.into())
    }

    #[must_use]
    pub fn builtin_function(handler: Arc<BuiltinFunction>) -> Self {
        Self::BuiltinFunction { handler }
    }

    #[must_use]
    pub fn compiled_function(
        instructions: Instructions,
        num_locals: usize,
        num_parameters: usize,
    ) -> Self {
        Self::CompiledFunction(CompiledFunc {
            instructions,
            num_locals,
            num_parameters,
        })
    }

    #[must_use]
    pub fn closure(function: CompiledFunc, free: Vec<Object>) -> Self {
        Self::Closure(Closure { function, free })
    }

    #[must_use]
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null => false,
            Self::Bool(value) => *value,
            Self::Integer(value) => *value != 0,
            Self::Float(value) => *value != 0.0,
            Self::String(value) => !value.is_empty(),
            Self::Char(value) => *value != '\0' && *value != ' ',
            Self::Error(_) => false,
            Self::Array { values } => !values.is_empty(),
            Self::Dict { values } => !values.is_empty(),
            Self::BuiltinFunction { .. } => true,
            Self::CompiledFunction { .. } => true,
            Self::Closure(_) => true,
        }
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

impl TryFrom<&mut Object> for HashKey {
    type Error = String;

    fn try_from(value: &mut Object) -> Result<Self, Self::Error> {
        (value as &Object).try_into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::Display)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Bool(bool),
    Char(char),
}
