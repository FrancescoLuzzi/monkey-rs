use crate::{ast, environment::Environment, objects::Object, token};

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
        ast::Node::Let { name, value } => {
            let value = eval_expr(env, value)?;
            env.set(name, value);
            Some(Object::Null)
        }
        ast::Node::Return { value } => eval_expr(env, &value),
        ast::Node::Expression(expression) => eval_expr(env, expression),
    }
}

pub fn eval_expr(env: &mut Environment, expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Identifier { name } => env.get(name),
        ast::Expression::Integer { value } => Some(Object::Integer(*value)),
        ast::Expression::Float { value } => Some(Object::Float(*value)),
        ast::Expression::String { value } => Some(Object::String(value.clone())),
        ast::Expression::Char { value } => Some(Object::Char(*value)),
        ast::Expression::Negated { value } => eval_negation(env, value),
        ast::Expression::Minus { value } => eval_minus(env, value),
        ast::Expression::Block { value } => todo!(),
        ast::Expression::Function { parameters, body } => todo!(),
        ast::Expression::Call {
            function,
            parameters,
        } => todo!(),
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => todo!(),
        ast::Expression::Infix { left, right, op } => eval_infix(env, left, op, right),
    }
}

fn eval_negation(env: &mut Environment, expr: &ast::Expression) -> Option<Object> {
    match eval_expr(env, expr)? {
        Object::Null => Some(Object::Bool(true)),
        Object::Bool(b) => Some(Object::Bool(!b)),
        Object::Integer(n) => Some(Object::Bool(n != 0)),
        Object::Float(n) => Some(Object::Bool(n != 0.0)),
        Object::String(s) => Some(Object::Bool(s.chars().all(char::is_whitespace))),
        Object::Char(c) => Some(Object::Bool(c != '\0' && c != ' ')),
        Object::Error(_) => Some(Object::Bool(true)),
    }
}

fn eval_minus(env: &mut Environment, expr: &ast::Expression) -> Option<Object> {
    match eval_expr(env, expr)? {
        Object::Bool(b) => Some(Object::Integer(-(b as i64))),
        Object::Integer(n) => Some(Object::Integer(-n)),
        Object::Float(n) => Some(Object::Float(-n)),
        _ => None,
    }
}

fn eval_infix(
    env: &mut Environment,
    left: &ast::Expression,
    op: &token::Token,
    right: &ast::Expression,
) -> Option<Object> {
    match op {
        crate::token::Token::Plus => eval_infix_sum(env, left, right),
        crate::token::Token::Minus => todo!(),
        crate::token::Token::Slash => todo!(),
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

#[inline]
fn eval_infix_sum(
    env: &mut Environment,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, left)?;
    let right = eval_expr(env, right)?;
    match (&left, &right) {
        (Object::Integer(n), Object::Integer(m)) => Some(Object::Integer(n + m)),
        (Object::Integer(i), Object::Float(f)) | (Object::Float(f), Object::Integer(i)) => {
            Some(Object::Float(*f + *i as f64))
        }
        (Object::Float(n), Object::Float(m)) => Some(Object::Float(n + m)),
        (Object::String(_), _) | (_, Object::String(_)) => {
            Some(Object::String(format!("{left}{right}")))
        }
        _ => None,
    }
}
