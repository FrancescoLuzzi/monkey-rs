use std::error::Error;

use crate::token::Token;

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
    Char(CharExpression),
    Negated(NegatedExpression),
    Minus(MinusExpression),
    Block(BlockExpression),
    Function(FunctionExpression),
    Call(CallExpression),
    If(IfExpression),
    Infix(InfixExpression),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(identifier) => identifier.fmt(f),
            Expression::Integer(integer_expression) => integer_expression.fmt(f),
            Expression::Float(float_expression) => float_expression.fmt(f),
            Expression::String(string_expression) => string_expression.fmt(f),
            Expression::Char(char_expression) => char_expression.fmt(f),
            Expression::Negated(negated_expression) => negated_expression.fmt(f),
            Expression::Minus(minus_expression) => minus_expression.fmt(f),
            Expression::Block(block_expression) => block_expression.fmt(f),
            Expression::Function(function_expression) => function_expression.fmt(f),
            Expression::Call(call_expression) => call_expression.fmt(f),
            Expression::If(if_expression) => if_expression.fmt(f),
            Expression::Infix(infix) => infix.fmt(f),
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

#[derive(Debug, PartialEq)]
pub struct CharExpression {
    pub value: char,
}

impl std::fmt::Display for CharExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub op: Token,
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct NegatedExpression {
    pub expr: Box<Expression>,
}

impl std::fmt::Display for NegatedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{}", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct MinusExpression {
    pub expr: Box<Expression>,
}

impl std::fmt::Display for MinusExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "-{}", self.expr)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockExpression {
    pub statements: Vec<Node>,
}

impl std::fmt::Display for BlockExpression {
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

#[derive(Debug, PartialEq)]
pub struct FunctionExpression {
    // TODO: maybe add a bit to distinguish between
    // let test = fn(){}
    // fn test(){}
    // mostly for formatting purposes
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockExpression,
}

impl std::fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}){}",
            self.parameters
                .iter()
                .map(|stmt| format!("{stmt}"))
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub function: String,
    pub parameters: Vec<Expression>,
}

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.parameters
                .iter()
                .map(|stmt| format!("{stmt}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: BlockExpression,
    pub alternative: Option<BlockExpression>,
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(alternative) = &self.alternative {
            write!(
                f,
                "if({}){{{}}}else{{{}}}",
                self.condition, self.consequence, alternative
            )
        } else {
            write!(f, "if({}){{{}}}", self.condition, self.consequence)
        }
    }
}

#[derive(Debug, Default)]
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
