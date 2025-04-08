use crate::{ast, environment::Environment, objects::Object, token};

pub fn eval_program(env: &Environment, program: &ast::Program) -> Option<Object> {
    eval_statements(env, &program.statements)
}

fn eval_statements(env: &Environment, statements: &[ast::Node]) -> Option<Object> {
    let mut result = Object::Null;
    for node in statements.iter() {
        result = eval_node(env, node)?;
        if matches!(result, Object::Error(_)) {
            return Some(result);
        }
    }
    Some(result)
}

pub fn eval_node(env: &Environment, node: &ast::Node) -> Option<Object> {
    match node {
        ast::Node::Let { name, value } => {
            let value = eval_expr(env, value)?;
            env.set(name, value);
            Some(Object::Null)
        }
        ast::Node::Return { value } => eval_expr(env, value),
        ast::Node::Expression(expression) => eval_expr(env, expression),
    }
}

pub fn eval_expr(env: &Environment, expr: &ast::Expression) -> Option<Object> {
    match expr {
        ast::Expression::Identifier { name } => env.get(name),
        ast::Expression::Integer { value } => Some(Object::Integer(*value)),
        ast::Expression::Float { value } => Some(Object::Float(*value)),
        ast::Expression::String { value } => Some(Object::String(value.clone())),
        ast::Expression::Bool { value } => Some(Object::Bool(*value)),
        ast::Expression::Char { value } => Some(Object::Char(*value)),
        ast::Expression::Negated { value } => eval_negation(env, value),
        ast::Expression::Minus { value } => eval_minus(env, value),
        ast::Expression::Block { value } => eval_statements(env, &value.statements),
        ast::Expression::Function { parameters, body } => Some(Object::Function {
            parameters: parameters.clone(),
            body: body.clone(),
            scoped_env: env.new_derived_env(),
        }),
        ast::Expression::Call {
            function,
            parameters,
        } => eval_call(env, function, parameters),
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if(env, condition, consequence, alternative.as_ref()),
        ast::Expression::Infix { left, right, op } => eval_infix(env, left, op, right),
    }
}

fn eval_if(
    env: &Environment,
    condition: &ast::Expression,
    consequence: &ast::Block,
    alternative: Option<&ast::Block>,
) -> Option<Object> {
    if let Some(Object::Bool(condition)) = eval_expr(env, condition) {
        if condition {
            eval_statements(env, &consequence.statements)
        } else {
            eval_statements(env, &alternative?.statements)
        }
    } else {
        None
    }
}

fn eval_call(
    env: &Environment,
    function: &str,
    call_parameters: &[ast::Expression],
) -> Option<Object> {
    if let Some(Object::Function {
        parameters,
        body,
        scoped_env,
    }) = env.get(function)
    {
        if parameters.len() != call_parameters.len() {
            return None;
        }
        for (param_name, param_value) in parameters
            .iter()
            .zip(call_parameters.iter().map(|expr| eval_expr(env, expr)))
        {
            scoped_env.set(param_name, param_value?);
        }
        eval_statements(&scoped_env, &body.statements)
    } else {
        None
    }
}

fn eval_negation(env: &Environment, expr: &ast::Expression) -> Option<Object> {
    match eval_expr(env, expr)? {
        Object::Null => Some(Object::Bool(true)),
        Object::Bool(b) => Some(Object::Bool(!b)),
        Object::Integer(n) => Some(Object::Bool(n != 0)),
        Object::Float(n) => Some(Object::Bool(n != 0.0)),
        Object::String(s) => Some(Object::Bool(s.chars().all(char::is_whitespace))),
        Object::Char(c) => Some(Object::Bool(c != '\0' && c != ' ')),
        Object::Error(_) => Some(Object::Bool(true)),
        _ => None,
    }
}

fn eval_minus(env: &Environment, expr: &ast::Expression) -> Option<Object> {
    match eval_expr(env, expr)? {
        Object::Bool(b) => Some(Object::Integer(-(b as i64))),
        Object::Integer(n) => Some(Object::Integer(-n)),
        Object::Float(n) => Some(Object::Float(-n)),
        _ => None,
    }
}

fn eval_infix(
    env: &Environment,
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

fn eval_infix_sum(
    env: &Environment,
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
