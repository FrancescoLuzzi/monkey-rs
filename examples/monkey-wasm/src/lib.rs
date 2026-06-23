use std::{collections::HashMap, sync::Arc};

use js_sys::{Array, Function, Map, Object as JsObject, Reflect, try_iter};
use miette::Report;
use monkey_core::{lexer::Lexer, parser::Parser};
use monkey_eval::{
    builtin::{Builtin, BuiltinBuilder, BuiltinFunction},
    environment::Environment,
    evaluator::eval_program,
    objects::{HashKey, Object},
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MonkeyStateBuilder {
    #[wasm_bindgen(skip)]
    builder: BuiltinBuilder,
}

#[wasm_bindgen]
impl MonkeyStateBuilder {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            builder: BuiltinBuilder::default(),
        }
    }

    #[wasm_bindgen(js_name = registerFunction)]
    pub fn register_function(&mut self, name: &str, function: Function) {
        self.builder.add(name, js_function_handler(function));
    }

    pub fn build(self) -> MonkeyState {
        MonkeyState {
            env: Environment::new(),
            builtins: self.builder.build(),
        }
    }
}

#[wasm_bindgen]
pub struct MonkeyState {
    #[wasm_bindgen(skip)]
    env: Environment,
    #[wasm_bindgen(skip)]
    builtins: Builtin,
}

#[wasm_bindgen]
impl MonkeyState {
    #[wasm_bindgen]
    pub fn eval(&mut self, input: &str) -> Result<String, String> {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser
            .parse_program()
            .map_err(|error| format!("{:?}", Report::new(error)))?;
        eval_program(&self.env, &self.builtins, &program)
            .ok_or("Error evaluating".to_string())
            .map(|x| x.to_string())
    }
}

fn js_function_handler(function: Function) -> Arc<BuiltinFunction> {
    let function = function.clone();
    Arc::new(move |args: &mut [Object]| {
        let js_args = Array::new();
        for arg in args.iter() {
            let value = match object_to_js_value(arg) {
                Ok(value) => value,
                Err(error) => return Object::error(error),
            };
            js_args.push(&value);
        }

        match function.apply(&JsValue::NULL, js_args.as_ref()) {
            Ok(result) => js_value_to_object(result).unwrap_or_else(Object::error),
            Err(error) => Object::error(js_error_to_string(error)),
        }
    })
}

fn object_to_js_value(value: &Object) -> Result<JsValue, String> {
    match value {
        Object::Null => Ok(JsValue::NULL),
        Object::Bool(boolean) => Ok(JsValue::from_bool(*boolean)),
        Object::Integer(integer) => Ok(JsValue::from_f64(*integer as f64)),
        Object::Float(float) => Ok(JsValue::from_f64(*float)),
        Object::String(string) => Ok(JsValue::from_str(string)),
        Object::Char(character) => Ok(JsValue::from_str(&character.to_string())),
        Object::Array { values } => {
            let array = Array::new();
            for value in values {
                array.push(&object_to_js_value(value)?);
            }
            Ok(array.into())
        }
        Object::Dict { values } => {
            let map = Map::new();
            for (key, value) in values {
                map.set(&hash_key_to_js_value(key), &object_to_js_value(value)?);
            }
            Ok(map.into())
        }
        Object::Error(error) => Ok(JsValue::from_str(error)),
        Object::Function { .. } | Object::BuiltinFunction { .. } => {
            Err("can't convert Monkey functions to JavaScript".into())
        }
    }
}

fn hash_key_to_js_value(key: &HashKey) -> JsValue {
    match key {
        HashKey::Integer(value) => JsValue::from_f64(*value as f64),
        HashKey::String(value) => JsValue::from_str(value),
        HashKey::Bool(value) => JsValue::from_bool(*value),
        HashKey::Char(value) => JsValue::from_str(&value.to_string()),
    }
}

fn js_value_to_object(value: JsValue) -> Result<Object, String> {
    if value.is_null() || value.is_undefined() {
        Ok(Object::null())
    } else if let Some(boolean) = value.as_bool() {
        Ok(Object::bool(boolean))
    } else if let Some(number) = value.as_f64() {
        if number.is_finite() && number.fract() == 0.0 {
            let integer = number as i64;
            if integer as f64 == number {
                return Ok(Object::integer(integer));
            }
        }
        Ok(Object::float(number))
    } else if let Some(string) = value.as_string() {
        Ok(Object::string(string))
    } else if Array::is_array(&value) {
        let array = Array::from(&value);
        let mut values = Vec::with_capacity(array.length() as usize);
        for value in array.iter() {
            values.push(js_value_to_object(value)?);
        }
        Ok(Object::array(values))
    } else if value.is_instance_of::<Map>() {
        iterable_to_dict(value)
    } else if value.is_object() && !value.is_instance_of::<Function>() {
        object_to_dict(value)
    } else {
        Err("unsupported JavaScript value returned to Monkey".into())
    }
}

fn iterable_to_dict(value: JsValue) -> Result<Object, String> {
    let mut values = HashMap::new();
    let Some(entries) = try_iter(&value).map_err(js_error_to_string)? else {
        return Err("JavaScript map is not iterable".into());
    };

    for entry in entries {
        let entry = entry.map_err(js_error_to_string)?;
        let pair = Array::from(&entry);
        let key = js_value_to_hash_key(pair.get(0))?;
        let value = js_value_to_object(pair.get(1))?;
        values.insert(key, value);
    }

    Ok(Object::dict(values))
}

fn object_to_dict(value: JsValue) -> Result<Object, String> {
    let object = JsObject::from(value);
    let keys = JsObject::keys(&object);
    let mut values = HashMap::new();

    for key in keys.iter() {
        let key_string = key
            .as_string()
            .ok_or("JavaScript object key is not a string")?;
        let value = Reflect::get(&object, &key).map_err(js_error_to_string)?;
        values.insert(HashKey::String(key_string), js_value_to_object(value)?);
    }

    Ok(Object::dict(values))
}

fn js_value_to_hash_key(value: JsValue) -> Result<HashKey, String> {
    if let Some(boolean) = value.as_bool() {
        Ok(HashKey::Bool(boolean))
    } else if let Some(number) = value.as_f64() {
        if number.is_finite() && number.fract() == 0.0 {
            let integer = number as i64;
            if integer as f64 == number {
                return Ok(HashKey::Integer(integer));
            }
        }
        Err("JavaScript number keys must be safe integers".into())
    } else if let Some(string) = value.as_string() {
        Ok(HashKey::String(string))
    } else {
        Err("JavaScript object keys must be booleans, integers, or strings".into())
    }
}

fn js_error_to_string(error: JsValue) -> String {
    error
        .as_string()
        .or_else(|| {
            Reflect::get(&error, &JsValue::from_str("message"))
                .ok()?
                .as_string()
        })
        .unwrap_or_else(|| "JavaScript error".into())
}
