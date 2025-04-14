use derive_more::Display;

use crate::objects::Object;
use std::{
    collections::BTreeMap,
    error::Error,
    io::{Write, stdout},
    marker::PhantomData,
    sync::Arc,
};

pub struct Builder;
pub struct Built;

pub type BuiltinFunction = dyn Fn(&mut [Object]) -> Object;

#[derive(Debug, Display)]
pub enum BuiltinError {
    NotFound,
}

impl Error for BuiltinError {}

pub struct Builtin<T> {
    builtins: BTreeMap<String, Arc<BuiltinFunction>>,
    status: PhantomData<T>,
}

impl Builtin<Builder> {
    pub fn new() -> Self {
        Self {
            builtins: BTreeMap::new(),
            status: PhantomData,
        }
    }

    pub fn add(&mut self, name: &str, builtin: Arc<BuiltinFunction>) {
        self.builtins.insert(name.into(), builtin);
    }

    #[must_use]
    pub fn build(self) -> Builtin<Built> {
        Builtin {
            builtins: self.builtins,
            status: PhantomData,
        }
    }
}

impl Default for Builtin<Builder> {
    fn default() -> Self {
        let mut builtins = Self::new();
        builtins.add(
            "first",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::Error("first accepts only one argument".into());
                }
                if let Object::Array { values } = &args[0] {
                    let first = values.first();
                    if let Some(first) = first {
                        first.clone()
                    } else {
                        Object::Error("the array is empty".into())
                    }
                } else {
                    Object::Error("first accepts arrays only".into())
                }
            }),
        );
        builtins.add(
            "last",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::Error("last accepts only one argument".into());
                }
                if let Object::Array { values } = &args[0] {
                    let last = values.last();
                    if let Some(first) = last {
                        first.clone()
                    } else {
                        Object::Error("the array is empty".into())
                    }
                } else {
                    Object::Error("first accepts arrays only".into())
                }
            }),
        );
        builtins.add(
            "len",
            Arc::new(|args: &mut [Object]| {
                if args.len() != 1 {
                    return Object::Error("len accepts only one argument".into());
                }
                match &args[0] {
                    Object::String(s) => Object::Integer(s.len() as i64),
                    Object::Array { values } => Object::Integer(values.len() as i64),
                    Object::Dict { values } => Object::Integer(values.len() as i64),
                    _ => Object::Error("type not supported by len".into()),
                }
            }),
        );
        builtins.add(
            "push",
            Arc::new(|args: &mut [Object]| {
                if args.len() < 2 {
                    return Object::Error("push accepts only one argument".into());
                }
                let mut args = args.iter_mut();
                let arr = args.next().unwrap();
                if let Object::Array { values } = arr {
                    for x in args {
                        values.push(x.clone());
                    }
                    Object::Array {
                        values: values.to_vec(),
                    }
                } else {
                    Object::Error("push first argument must be an array".into())
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
                Object::Null
            }),
        );
        builtins
    }
}

impl Builtin<Built> {
    pub fn call(&self, name: &str, args: &mut [Object]) -> Result<Object, BuiltinError> {
        let handler = self.builtins.get(name).ok_or(BuiltinError::NotFound)?;
        Ok(handler(args))
    }

    pub fn get(&self, name: &str) -> Result<Arc<BuiltinFunction>, BuiltinError> {
        let builtin = self.builtins.get(name).ok_or(BuiltinError::NotFound)?;
        Ok((*builtin).clone())
    }
}
