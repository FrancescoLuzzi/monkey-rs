#[derive(Debug, Clone, derive_more::Display)]
pub enum Object {
    #[display("null")]
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Error(String),
}
