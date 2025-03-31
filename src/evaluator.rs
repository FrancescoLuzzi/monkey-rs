use crate::{ast, environment::Environment, objects::Object};

pub fn eval_program(env: &mut Environment, program: &ast::Program) -> Option<Object> {
    let mut result = Object::Null;
    for node in program.statements.iter() {
        result = eval_node(env, node)?;
        if matches!(result, Object::Error(_)) {
            return Some(result);
        }
    }
    Some(result)
}

pub fn eval_node(env: &mut Environment, node: &ast::Node) -> Option<Object> {
    match node {
        ast::Node::Let(let_statement) => {
            let value = eval_expr(env, &let_statement.value)?;
            env.set(&let_statement.name, value);
            Some(Object::Null)
        }
        ast::Node::Return(return_statement) => eval_expr(env, &return_statement.value),
        ast::Node::Expression(expression) => eval_expr(env, expression),
    }
}

pub fn eval_expr(env: &mut Environment, expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Identifier(identifier_expression) => {
            env.get(&identifier_expression.name).cloned()
        }
        ast::Expression::Integer(integer_expression) => {
            Some(Object::Integer(integer_expression.number))
        }
        ast::Expression::Float(float_expression) => Some(Object::Float(float_expression.number)),
        ast::Expression::String(string_expression) => {
            Some(Object::String(string_expression.value.clone()))
        }
        ast::Expression::Char(char_expression) => Some(Object::Char(char_expression.value)),
        ast::Expression::Negated(negated_expression) => {
            eval_negation(env, &negated_expression.expr)
        }
        ast::Expression::Minus(minus_expression) => todo!(),
        ast::Expression::Block(block_expression) => todo!(),
        ast::Expression::Function(function_expression) => todo!(),
        ast::Expression::Call(call_expression) => todo!(),
        ast::Expression::If(if_expression) => todo!(),
        ast::Expression::Infix(infix_expression) => todo!(),
    }
}

fn eval_negation(env: &mut Environment, expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Identifier(identifier_expression) => {
            env.get(&identifier_expression.name).cloned()
        }
        ast::Expression::Integer(integer_expression) => {
            Some(Object::Bool(integer_expression.number != 0))
        }
        ast::Expression::Float(float_expression) => {
            Some(Object::Bool(float_expression.number != 0.0))
        }
        ast::Expression::String(string_expression) => Some(Object::Bool(
            string_expression.value.chars().all(char::is_whitespace),
        )),
        ast::Expression::Char(char_expression) => Some(Object::Bool(char_expression.value != '\0')),
        ast::Expression::Negated(negated_expression) => {
            if let Object::Bool(value) = eval_negation(env, &negated_expression.expr)? {
                Some(Object::Bool(!value))
            } else {
                panic!("negate_expr returned something that is not an Object::Bool")
            }
        }
        ast::Expression::Minus(minus_expression) => todo!(),
        ast::Expression::Block(block_expression) => todo!(),
        ast::Expression::Function(function_expression) => todo!(),
        ast::Expression::Call(call_expression) => todo!(),
        ast::Expression::If(if_expression) => todo!(),
        ast::Expression::Infix(infix_expression) => todo!(),
    }
}

fn eval_infix(env: &mut Environment, infix: ast::InfixExpression) -> Option<Object> {
    match infix.op {
        crate::token::Token::Slash => todo!(),
        crate::token::Token::Plus => todo!(),
        crate::token::Token::Minus => todo!(),
        crate::token::Token::And => todo!(),
        crate::token::Token::BitAnd => todo!(),
        crate::token::Token::Or => todo!(),
        crate::token::Token::BitOr => todo!(),
        crate::token::Token::Eq => todo!(),
        crate::token::Token::Neq => todo!(),
        crate::token::Token::Gt => todo!(),
        crate::token::Token::Lt => todo!(),
        crate::token::Token::Ge => todo!(),
        crate::token::Token::Le => todo!(),
        _ => None,
    }
}
