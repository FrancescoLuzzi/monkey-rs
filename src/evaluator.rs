use crate::{
    ast::{self, Expression},
    environment::Environment,
    objects::Object,
    token,
};

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

fn eval_array(env: &Environment, expressions: &[ast::Expression]) -> Option<Object> {
    let mut values = Vec::new();
    for expr in expressions.iter() {
        let tmp_val = eval_expr(env, expr)?;
        if matches!(tmp_val, Object::Error(_)) {
            return Some(tmp_val);
        }
        values.push(tmp_val);
    }
    Some(Object::Array { values })
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
        ast::Expression::Array { values } => eval_array(env, values),
        ast::Expression::Function { parameters, body } => Some(Object::Function {
            parameters: parameters.clone(),
            body: body.clone(),
            scoped_env: env.new_derived_env(),
        }),
        ast::Expression::Index { value, index } => eval_index(env, value, index),
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
    let object = eval_expr(env, condition)?;
    if is_object_truthy(&object) {
        eval_statements(env, &consequence.statements)
    } else {
        eval_statements(env, &alternative?.statements)
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

fn eval_index(env: &Environment, value: &Expression, index: &Expression) -> Option<Object> {
    let value = eval_expr(env, value)?;
    let index = eval_expr(env, index)?;
    match (value, index) {
        (Object::String(s), Object::Integer(n)) => Some(Object::Char(s.chars().nth(n as usize)?)),
        (Object::Array { values }, Object::Integer(n)) => values.get(n as usize).cloned(),
        // TODO: add handling of maps here
        _ => None,
    }
}

fn is_object_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Bool(b) => *b,
        Object::Integer(n) => *n != 0,
        Object::Float(n) => *n != 0.0,
        Object::String(s) => s.chars().all(char::is_whitespace),
        Object::Char(c) => *c != '\0' && *c != ' ',
        Object::Error(_) => false,
        Object::Function { .. } => true,
        Object::Array { values } => !values.is_empty(),
    }
}

fn eval_negation(env: &Environment, expr: &ast::Expression) -> Option<Object> {
    let object = eval_expr(env, expr)?;
    Some(Object::Bool(!is_object_truthy(&object)))
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
        token::Token::Plus => eval_infix_sum(env, left, right),
        token::Token::Minus => eval_infix_minus(env, left, right),
        token::Token::Slash => eval_infix_slash(env, left, right),
        token::Token::Asterisk => eval_infix_mul(env, left, right),
        token::Token::And => todo!(),
        token::Token::BitAnd => todo!(),
        token::Token::Or => todo!(),
        token::Token::BitOr => todo!(),
        token::Token::Eq => todo!(),
        token::Token::Neq => todo!(),
        token::Token::Gt => todo!(),
        token::Token::Lt => todo!(),
        token::Token::Ge => todo!(),
        token::Token::Le => todo!(),
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
        (Object::Array { values: v1 }, Object::Array { values: v2 }) => {
            let v_out = v1.iter().chain(v2.iter()).cloned().collect();
            Some(Object::Array { values: v_out })
        }
        _ => None,
    }
}

fn eval_infix_minus(
    env: &Environment,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, left)?;
    let right = eval_expr(env, right)?;
    match (&left, &right) {
        (Object::Integer(n), Object::Integer(m)) => Some(Object::Integer(n - m)),
        (Object::Integer(i), Object::Float(f)) | (Object::Float(f), Object::Integer(i)) => {
            Some(Object::Float(*f - *i as f64))
        }
        (Object::Float(n), Object::Float(m)) => Some(Object::Float(n - m)),
        _ => None,
    }
}

fn eval_infix_slash(
    env: &Environment,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, left)?;
    let right = eval_expr(env, right)?;
    match (&left, &right) {
        (Object::Integer(n), Object::Integer(m)) => Some(Object::Integer(n / m)),
        (Object::Integer(i), Object::Float(f)) | (Object::Float(f), Object::Integer(i)) => {
            Some(Object::Float(*f / *i as f64))
        }
        (Object::Float(n), Object::Float(m)) => Some(Object::Float(n / m)),
        _ => None,
    }
}

fn eval_infix_mul(
    env: &Environment,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, left)?;
    let right = eval_expr(env, right)?;
    match (&left, &right) {
        (Object::Integer(n), Object::Integer(m)) => Some(Object::Integer(n * m)),
        (Object::Integer(i), Object::Float(f)) | (Object::Float(f), Object::Integer(i)) => {
            Some(Object::Float(*f * *i as f64))
        }
        (Object::Float(n), Object::Float(m)) => Some(Object::Float(n * m)),
        (Object::String(s), Object::Integer(n)) | (Object::Integer(n), Object::String(s)) => {
            Some(Object::String(s.repeat(*n as usize)))
        }
        (Object::Array { values }, Object::Integer(n))
        | (Object::Integer(n), Object::Array { values }) => {
            let v_out = values
                .iter()
                .cycle()
                .take(values.len().saturating_mul(*n as usize))
                .map(|obj| obj.to_owned())
                .collect();
            Some(Object::Array { values: v_out })
        }
        _ => None,
    }
}
