use derive_more::Display;

use crate::objects::{HashKey, Object};
use std::{
    collections::BTreeMap,
    error::Error,
    io::{Write, stdout},
    sync::Arc,
};

pub type BuiltinFunction = dyn Fn(&mut [Object]) -> Object;

#[derive(Debug, Display)]
pub enum BuiltinError {
    NotFound,
}

impl Error for BuiltinError {}

#[derive(Clone)]
pub struct Builtin {
    inner: Arc<BuiltinInner>,
}

struct BuiltinInner {
    builtins: BTreeMap<String, Arc<BuiltinFunction>>,
    names: Vec<String>,
}

pub struct BuiltinBuilder {
    builtins: BTreeMap<String, Arc<BuiltinFunction>>,
    names: Vec<String>,
}

impl BuiltinBuilder {
    pub fn new() -> Self {
        Self {
            builtins: BTreeMap::new(),
            names: Vec::new(),
        }
    }

    pub fn add(&mut self, name: &str, builtin: Arc<BuiltinFunction>) {
        if !self.builtins.contains_key(name) {
            self.names.push(name.into());
        }

        self.builtins.insert(name.into(), builtin);
    }

    #[must_use]
    pub fn build(self) -> Builtin {
        Builtin {
            inner: Arc::new(BuiltinInner {
                builtins: self.builtins,
                names: self.names,
            }),
        }
    }
}

impl Default for BuiltinBuilder {
    fn default() -> Self {
        let mut builtins = Self::new();
        builtins.add(
            "first",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::error("first accepts only one argument");
                }
                if let Object::Array { values } = &args[0] {
                    values
                        .first()
                        .cloned()
                        .unwrap_or_else(|| Object::error("the array is empty"))
                } else {
                    Object::error("first accepts arrays only")
                }
            }),
        );
        builtins.add(
            "last",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::error("last accepts only one argument");
                }
                if let Object::Array { values } = &args[0] {
                    values
                        .last()
                        .cloned()
                        .unwrap_or_else(|| Object::error("the array is empty"))
                } else {
                    Object::error("last accepts arrays only")
                }
            }),
        );
        builtins.add(
            "len",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::error("len accepts only one argument");
                }
                match &args[0] {
                    Object::String(s) => Object::integer(s.len() as i64),
                    Object::Array { values } => Object::integer(values.len() as i64),
                    Object::Dict { values } => Object::integer(values.len() as i64),
                    _ => Object::error("type not supported by len"),
                }
            }),
        );
        builtins.add(
            "push",
            Arc::new(|args: &mut [Object]| {
                if args.len() < 2 {
                    return Object::error("push accepts at least two arguments");
                }
                let mut args = args.iter_mut();
                let arr = args.next().unwrap();
                if let Object::Array { values } = arr {
                    for x in args {
                        values.push(x.clone());
                    }
                    Object::array(values.to_vec())
                } else {
                    Object::error("push first argument must be an array")
                }
            }),
        );
        builtins.add(
            "insert",
            Arc::new(|args: &mut [Object]| {
                if args.len() < 2 || args.len() > 3 {
                    return Object::error("insert accepts at least two argument, max three");
                }
                let mut args = args.iter_mut();
                let object = args.next().unwrap();
                match object {
                    Object::Array { values } => match (args.next(), args.next()) {
                        (Some(o), None) => {
                            values.insert(0, o.clone());
                            Object::array(values.to_vec())
                        }
                        (Some(o), Some(index_object)) => {
                            if let Object::Integer(index) = index_object {
                                let mut index = *index;
                                if index < 0 {
                                    index = values.len() as i64 - index;
                                }
                                if index < 0 {
                                    Object::error(format!("index '{}' out of range", index))
                                } else {
                                    values.insert(index as usize, o.clone());
                                    Object::array(values.to_vec())
                                }
                            } else {
                                Object::error(format!(
                                    "can't index array with object '{}'",
                                    index_object
                                ))
                            }
                        }
                        (None, _) => panic!("first argument was None, this is impossible"),
                    },
                    Object::Dict { values } => match (args.next(), args.next()) {
                        (Some(o), Some(index_object)) => {
                            let key_result = HashKey::try_from(index_object).map_err(Object::error);
                            if let Ok(key) = key_result {
                                values.insert(key, o.clone());
                                Object::dict(values.to_owned())
                            } else {
                                key_result.unwrap_err()
                            }
                        }
                        (_, None) => Object::error("Missing argument"),
                        (None, _) => panic!("first argument was None, this is impossible"),
                    },
                    _ => Object::error(format!(
                        "insert's first argument type {} not supported",
                        object
                    )),
                }
            }),
        );
        builtins.add(
            "puts",
            Arc::new(|args: &mut [Object]| {
                let mut out = stdout().lock();
                for x in args.iter() {
                    let _ = out.write_fmt(format_args!("{}", x));
                }
                Object::null()
            }),
        );
        builtins
    }
}

impl Builtin {
    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.inner.names.iter().map(String::as_str)
    }

    pub fn call(&self, name: &str, args: &mut [Object]) -> Result<Object, BuiltinError> {
        let builtin = self
            .inner
            .builtins
            .get(name)
            .ok_or(BuiltinError::NotFound)?;
        Ok(builtin(args))
    }

    pub fn get_handler(&self, name: &str) -> Result<Arc<BuiltinFunction>, BuiltinError> {
        let builtin = self
            .inner
            .builtins
            .get(name)
            .ok_or(BuiltinError::NotFound)?;
        Ok(builtin.clone())
    }

    pub fn get_handler_by_index(&self, index: usize) -> Result<Arc<BuiltinFunction>, BuiltinError> {
        let name = self.inner.names.get(index).ok_or(BuiltinError::NotFound)?;
        self.get_handler(name)
    }

    pub fn get_index(&self, name: &str) -> Result<usize, BuiltinError> {
        self.inner
            .names
            .iter()
            .position(|registered_name| registered_name == name)
            .ok_or(BuiltinError::NotFound)
    }
}
