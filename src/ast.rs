#[derive(Debug, PartialEq)]
pub enum Node {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Let(let_statement) => let_statement.fmt(f),
            Node::Return(return_statement) => return_statement.fmt(f),
            Node::Expression(expression) => expression.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Integer(IntegerExpression),
    Float(FloatExpression),
    String(StringExpression),
    Prefix(),
    Postfix(),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Prefix() => f.write_str("prefix"),
            Expression::Postfix() => f.write_str("postfix"),
            Expression::Identifier(identifier) => identifier.fmt(f),
            Expression::Integer(integer_expression) => integer_expression.fmt(f),
            Expression::Float(float_expression) => float_expression.fmt(f),
            Expression::String(string_expression) => string_expression.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression {
    pub name: String,
}

impl std::fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerExpression {
    pub number: i64,
}

impl std::fmt::Display for IntegerExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number)
    }
}

#[derive(Debug, PartialEq)]
pub struct FloatExpression {
    pub number: f64,
}

impl std::fmt::Display for FloatExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.number)
    }
}

#[derive(Debug, PartialEq)]
pub struct StringExpression {
    pub value: String,
}

impl std::fmt::Display for StringExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let statements: Vec<String> = (&self.statements)
            .into_iter()
            .map(|stmt| format!("{stmt}"))
            .collect();
        write!(f, "{}", statements.join(""))
    }
}
