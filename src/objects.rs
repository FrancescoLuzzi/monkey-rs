use crate::{ast, environment::Environment};

#[derive(Clone, derive_more::Display, derive_more::Debug)]
pub enum Object {
    #[display("null")]
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Error(String),
    #[display("<function>")]
    Function {
        parameters: Vec<String>,
        body: ast::Block,
        scoped_env: Environment,
    },
}
