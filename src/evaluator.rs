use std::collections::HashMap;

use crate::{
    ast::{self, Expression},
    builtin::{Built, Builtin},
    environment::Environment,
    objects::{HashKey, Object},
    token,
};

pub fn eval_program(
    env: &Environment,
    builtin: &Builtin<Built>,
    program: &ast::Program,
) -> Option<Object> {
    eval_statements(env, builtin, &program.statements)
}

fn eval_statements(
    env: &Environment,
    builtin: &Builtin<Built>,
    statements: &[ast::Node],
) -> Option<Object> {
    let mut result = Object::Null;
    for node in statements.iter() {
        result = eval_node(env, builtin, node)?;
        if matches!(result, Object::Error(_)) {
            return Some(result);
        }
    }
    Some(result)
}

fn eval_array(
    env: &Environment,
    builtin: &Builtin<Built>,
    expressions: &[ast::Expression],
) -> Option<Object> {
    let mut values = Vec::new();
    for expr in expressions.iter() {
        let tmp_val = eval_expr(env, builtin, expr)?;
        if matches!(tmp_val, Object::Error(_)) {
            return Some(tmp_val);
        }
        values.push(tmp_val);
    }
    Some(Object::Array { values })
}

fn eval_dict(
    env: &Environment,
    builtin: &Builtin<Built>,
    key_values: &[(ast::Expression, ast::Expression)],
) -> Option<Object> {
    let mut values: HashMap<HashKey, Object> = HashMap::new();
    for (key, value) in key_values.iter() {
        let key = eval_expr(env, builtin, key)?;
        if matches!(key, Object::Error(_)) {
            return Some(key);
        }
        let value = eval_expr(env, builtin, value)?;
        if matches!(value, Object::Error(_)) {
            return Some(value);
        }
        values.insert(key.as_ref().try_into().ok()?, value);
    }
    Some(Object::Dict { values })
}

pub fn eval_node(env: &Environment, builtin: &Builtin<Built>, node: &ast::Node) -> Option<Object> {
    match node {
        ast::Node::Let { name, value } => {
            let value = eval_expr(env, builtin, value)?;
            env.set(name, value);
            Some(Object::Null)
        }
        ast::Node::Return { value } => eval_expr(env, builtin, value),
        ast::Node::Expression(expression) => eval_expr(env, builtin, expression),
    }
}

pub fn eval_expr(
    env: &Environment,
    builtin: &Builtin<Built>,
    expr: &ast::Expression,
) -> Option<Object> {
    match expr {
        ast::Expression::Null => Some(Object::Null),
        ast::Expression::Identifier { name } => Some(
            env.get(name)
                .unwrap_or(Object::Error(format!("{name} not yet defined"))),
        ),
        ast::Expression::Integer { value } => Some(Object::Integer(*value)),
        ast::Expression::Float { value } => Some(Object::Float(*value)),
        ast::Expression::String { value } => Some(Object::String(value.clone())),
        ast::Expression::Bool { value } => Some(Object::Bool(*value)),
        ast::Expression::Char { value } => Some(Object::Char(*value)),
        ast::Expression::Negated { value } => eval_negation(env, builtin, value),
        ast::Expression::Minus { value } => eval_minus(env, builtin, value),
        ast::Expression::Block { value } => eval_statements(env, builtin, &value.statements),
        ast::Expression::Array { values } => eval_array(env, builtin, values),
        ast::Expression::Dict { values } => eval_dict(env, builtin, values),
        ast::Expression::Function { parameters, body } => Some(Object::Function {
            parameters: parameters.clone(),
            body: body.clone(),
            scoped_env: env.new_derived_env(),
        }),
        ast::Expression::Index { value, index } => eval_index(env, builtin, value, index),
        ast::Expression::Call {
            function,
            parameters,
        } => eval_call(env, builtin, function, parameters),
        ast::Expression::If {
            condition,
            consequence,
            alternative,
        } => eval_if(env, builtin, condition, consequence, alternative.as_ref()),
        ast::Expression::Infix { left, right, op } => eval_infix(env, builtin, left, op, right),
    }
}

fn eval_if(
    env: &Environment,
    builtin: &Builtin<Built>,
    condition: &ast::Expression,
    consequence: &ast::Block,
    alternative: Option<&ast::Block>,
) -> Option<Object> {
    let object = eval_expr(env, builtin, condition)?;
    if object.is_truthy() {
        eval_statements(env, builtin, &consequence.statements)
    } else {
        eval_statements(env, builtin, &alternative?.statements)
    }
}

fn eval_call(
    env: &Environment,
    builtin: &Builtin<Built>,
    function: &str,
    call_parameters: &[ast::Expression],
) -> Option<Object> {
    if let Ok(handler) = builtin.get(function) {
        let mut params = Vec::new();
        for param in call_parameters.iter() {
            params.push(eval_expr(env, builtin, param)?);
        }
        Some(handler(&mut params))
    } else if let Some(Object::Function {
        parameters,
        body,
        scoped_env,
    }) = env.get(function)
    {
        if parameters.len() != call_parameters.len() {
            return None;
        }
        for (param_name, param_value) in parameters.iter().zip(
            call_parameters
                .iter()
                .map(|expr| eval_expr(env, builtin, expr)),
        ) {
            let param_value = param_value?;
            if matches!(param_value, Object::Error(_)) {
                return Some(param_value);
            }
            scoped_env.set(param_name, param_value);
        }
        eval_statements(&scoped_env, builtin, &body.statements)
    } else {
        None
    }
}

fn eval_index(
    env: &Environment,
    builtin: &Builtin<Built>,
    value: &Expression,
    index: &Expression,
) -> Option<Object> {
    let value = eval_expr(env, builtin, value)?;
    match index {
        Expression::Identifier { name } => {
            if let Object::Dict { values } = value {
                values.get(&HashKey::String(name.clone())).cloned()
            } else {
                Some(Object::Error(format!(
                    "can't index '{}' in {}",
                    name, value
                )))
            }
        }
        Expression::Integer { value: n } => {
            if let Object::Dict { values } = value {
                values.get(&HashKey::Integer(*n)).cloned()
            } else if let Object::Array { values } = value {
                values.get(*n as usize).cloned()
            } else {
                Some(Object::Error(format!("can't index '{}' in {}", n, value)))
            }
        }
        index => {
            let index = eval_expr(env, builtin, index)?;
            match (value, index) {
                (Object::String(s), Object::Integer(n)) => {
                    Some(Object::Char(s.chars().nth(n as usize)?))
                }
                (Object::Array { values }, Object::Integer(n)) => values.get(n as usize).cloned(),
                (Object::Dict { values }, obj) => {
                    values.get(&obj.as_ref().try_into().ok()?).cloned()
                }
                _ => None,
            }
        }
    }
}

fn eval_negation(
    env: &Environment,
    builtin: &Builtin<Built>,
    expr: &ast::Expression,
) -> Option<Object> {
    let object = eval_expr(env, builtin, expr)?;
    Some(Object::Bool(object.is_truthy()))
}

fn eval_minus(
    env: &Environment,
    builtin: &Builtin<Built>,
    expr: &ast::Expression,
) -> Option<Object> {
    match eval_expr(env, builtin, expr)? {
        Object::Bool(b) => Some(Object::Integer(-(b as i64))),
        Object::Integer(n) => Some(Object::Integer(-n)),
        Object::Float(n) => Some(Object::Float(-n)),
        _ => None,
    }
}

fn eval_infix(
    env: &Environment,
    builtin: &Builtin<Built>,
    left: &ast::Expression,
    op: &token::Token,
    right: &ast::Expression,
) -> Option<Object> {
    match op {
        token::Token::Plus => eval_infix_sum(env, builtin, left, right),
        token::Token::Minus => eval_infix_minus(env, builtin, left, right),
        token::Token::Slash => eval_infix_slash(env, builtin, left, right),
        token::Token::Asterisk => eval_infix_mul(env, builtin, left, right),
        token::Token::And => eval_infix_and(env, builtin, left, right),
        token::Token::Or => eval_infix_or(env, builtin, left, right),
        token::Token::Eq => eval_infix_eq(env, builtin, left, right),
        token::Token::Neq => eval_infix_neq(env, builtin, left, right),
        token::Token::Gt => eval_infix_gt(env, builtin, left, right),
        token::Token::Lt => eval_infix_lt(env, builtin, left, right),
        token::Token::Ge => eval_infix_ge(env, builtin, left, right),
        token::Token::Le => eval_infix_le(env, builtin, left, right),
        token::Token::BitAnd => eval_infix_bit_and(env, builtin, left, right),
        token::Token::BitOr => eval_infix_bit_or(env, builtin, left, right),
        _ => None,
    }
}

fn eval_infix_sum(
    env: &Environment,
    builtin: &Builtin<Built>,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, builtin, left)?;
    let right = eval_expr(env, builtin, right)?;
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
    builtin: &Builtin<Built>,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, builtin, left)?;
    let right = eval_expr(env, builtin, right)?;
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
    builtin: &Builtin<Built>,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, builtin, left)?;
    let right = eval_expr(env, builtin, right)?;
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
    builtin: &Builtin<Built>,
    left: &ast::Expression,
    right: &ast::Expression,
) -> Option<Object> {
    let left = eval_expr(env, builtin, left)?;
    let right = eval_expr(env, builtin, right)?;
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

macro_rules! define_infix_bool_evaluator {
    ($func_name:ident, $operator:tt) => {
        fn $func_name(
            env: &Environment,
    builtin: &Builtin<Built>,
            left: &ast::Expression,
            right: &ast::Expression,
        ) -> Option<Object> {
            let left = eval_expr(env,builtin, left)?;
            let right = eval_expr(env,builtin, right)?;
            Some(Object::Bool(left.is_truthy() $operator right.is_truthy()))
        }
    };
}

define_infix_bool_evaluator!(eval_infix_and,&&);
define_infix_bool_evaluator!(eval_infix_or,||);

macro_rules! define_infix_obj_eq {
    ($func_name:ident, $operator:tt) => {
        fn $func_name(
            env: &Environment,
    builtin: &Builtin<Built>,
            left: &ast::Expression,
            right: &ast::Expression,
        ) -> Option<Object> {
            let left = eval_expr(env,builtin, left)?;
            let right = eval_expr(env,builtin, right)?;
            Some(Object::Bool(left $operator right))
        }
    };
}
define_infix_obj_eq!(eval_infix_eq, == );
define_infix_obj_eq!(eval_infix_neq, != );

macro_rules! define_infix_bool_comparison {
    ($func_name:ident, $operator:tt) => {
        fn $func_name(
            env: &Environment,
            builtin: &Builtin<Built>,
            left: &ast::Expression,
            right: &ast::Expression,
        ) -> Option<Object> {
            let left = eval_expr(env, builtin, left)?;
            let right = eval_expr(env, builtin, right)?;
            if left.partial_cmp(&right).is_some() {
                Some(Object::Bool(left $operator right))
            }else{
                Some(Object::Error(format!("can't compare {} and {}",left,right)))
            }
        }
    };
}
define_infix_bool_comparison!(eval_infix_gt,>);
define_infix_bool_comparison!(eval_infix_ge,>=);
define_infix_bool_comparison!(eval_infix_lt,<);
define_infix_bool_comparison!(eval_infix_le,<=);

macro_rules! define_infix_bit_operator {
    ($func_name:ident, $operator:tt) => {
        fn $func_name(
            env: &Environment,
            builtin: &Builtin<Built>,
            left: &ast::Expression,
            right: &ast::Expression,
        ) -> Option<Object> {
            let left = eval_expr(env, builtin, left)?;
            let right = eval_expr(env, builtin, right)?;
            match (&left,&right){
                (Object::Integer(n1), Object::Integer(n2)) => Some(Object::Integer(*n1 $operator *n2)),
                _ => Some(Object::Error(format!("can't use bit operator `{}` on {} and {}", stringify!($operator), left, right)))
            }
        }
    };
}
define_infix_bit_operator!(eval_infix_bit_and,&);
define_infix_bit_operator!(eval_infix_bit_or,|);
