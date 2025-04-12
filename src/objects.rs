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
        values.iter().map(|stmt| format!("{stmt}")).collect::<Vec<_>>().join(",\n")
    )]
    Array {
        values: Vec<Object>,
    },
    Error(String),
    #[display("<function>")]
    Function {
        parameters: Vec<String>,
        body: ast::Block,
        scoped_env: Environment,
    },
}
