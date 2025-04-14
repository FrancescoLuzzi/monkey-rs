use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression(Expression),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Let { name, value } => write!(f, "let {} = {};", name, value),
            Node::Return { value } => write!(f, "return {};", value),
            Node::Expression(expression) => expression.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Null,
    Identifier {
        name: String,
    },
    Integer {
        value: i64,
    },
    Float {
        value: f64,
    },
    String {
        value: String,
    },
    Bool {
        value: bool,
    },
    Char {
        value: char,
    },
    Negated {
        value: Box<Expression>,
    },
    Minus {
        value: Box<Expression>,
    },
    Block {
        value: Block,
    },
    Array {
        values: Vec<Expression>,
    },
    Dict {
        values: Vec<(Expression, Expression)>,
    },
    Function {
        parameters: Vec<String>,
        body: Block,
    },
    Index {
        value: Box<Expression>,
        index: Box<Expression>,
    },
    Call {
        function: String,
        parameters: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Block,
        alternative: Option<Block>,
    },
    Infix {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Token,
    },
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Null => f.write_str("null"),
            Expression::Identifier { name } => name.fmt(f),
            Expression::Integer { value } => value.fmt(f),
            Expression::Float { value } => value.fmt(f),
            Expression::String { value } => value.fmt(f),
            Expression::Bool { value } => value.fmt(f),
            Expression::Char { value } => value.fmt(f),
            Expression::Block { value } => value.fmt(f),
            Expression::Array { values } => {
                write!(
                    f,
                    "[{}]",
                    values
                        .iter()
                        .map(|stmt| format!("{stmt}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::Dict { values } => {
                write!(
                    f,
                    "{{\n{}}}",
                    values
                        .iter()
                        .map(|(key, value)| format!("{key}: {value}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::Negated { value } => f.write_fmt(format_args!("!{}", value)),
            Expression::Minus { value } => f.write_fmt(format_args!("-{}", value)),
            Expression::Function { parameters, body } => {
                write!(f, "fn({}){}", parameters.join(", "), body)
            }
            Expression::Index { value, index } => {
                write!(f, "{}[{}]", value, index)
            }
            Expression::Call {
                function,
                parameters,
            } => write!(
                f,
                "{}({})",
                function,
                parameters
                    .iter()
                    .map(|stmt| format!("{stmt}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                if let Some(alternative) = alternative {
                    write!(
                        f,
                        "if({}){{{}}}else{{{}}}",
                        condition, consequence, alternative
                    )
                } else {
                    write!(f, "if({}){{{}}}", condition, consequence)
                }
            }
            Expression::Infix { left, op, right } => write!(f, "{} {} {}", left, op, right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Node>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\n{}}}",
            self.statements
                .iter()
                .map(|stmt| format!("{stmt}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Default, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|stmt| format!("{stmt}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}
