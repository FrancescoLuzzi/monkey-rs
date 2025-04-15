use std::{cmp::Ordering, collections::HashMap, sync::Arc};

use crate::{ast, builtin::BuiltinFunction, environment::Environment};

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
    #[display("<builtin>")]
    #[debug("<builtin>")]
    BuiltinFunction {
        handler: Arc<BuiltinFunction>,
    },
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Object::Null, _) | (_, Object::Null) => None,
            (Object::Bool(b1), Object::Bool(b2)) => b1.partial_cmp(b2),
            (Object::Bool(b), Object::Integer(n)) => (*b as i64).partial_cmp(n),
            (Object::Integer(n), Object::Bool(b)) => n.partial_cmp(&(*b as i64)),
            (Object::Bool(b), Object::Float(n)) => (*b as i64 as f64).partial_cmp(n),
            (Object::Integer(n1), Object::Integer(n2)) => n1.partial_cmp(n2),
            (Object::Integer(i), Object::Float(f)) => (*i as f64).partial_cmp(f),
            (Object::Integer(n), Object::Char(c)) => n.partial_cmp(&(*c as i64)),
            (Object::Float(n), Object::Bool(b)) => n.partial_cmp(&(*b as i64 as f64)),
            (Object::Float(f), Object::Integer(i)) => f.partial_cmp(&(*i as f64)),
            (Object::Float(f0), Object::Float(f1)) => f0.partial_cmp(f1),
            (Object::String(s0), Object::String(s1)) => s0.partial_cmp(s1),
            (Object::Char(c), Object::Integer(n)) => (*c as i64).partial_cmp(n),
            (Object::Char(c0), Object::Char(c1)) => c0.partial_cmp(c1),
            (Object::Array { values: arr0 }, Object::Array { values: arr1 }) => {
                let cmp = arr0
                    .iter()
                    .zip(arr1.iter())
                    .map(|(x0, x1)| x0.partial_cmp(x1))
                    .find(|x| x != &Some(Ordering::Equal));
                if let Some(cmp) = cmp {
                    cmp
                } else {
                    arr0.len().partial_cmp(&arr1.len())
                }
            }
            _ => None,
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
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
            (
                Self::Function {
                    parameters: l_parameters,
                    body: l_body,
                    ..
                },
                Self::Function {
                    parameters: r_parameters,
                    body: r_body,
                    ..
                },
            ) => l_parameters == r_parameters && l_body == r_body,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
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
            Object::Function { .. } | Object::BuiltinFunction { .. } => true,
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
