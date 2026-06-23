use crate::{
    builtin::Builtin,
    code::{CodeError, Instructions, Opcode, read_u16},
    compiler::Bytecode,
    objects::{Closure, CompiledFunc, HashKey, Object},
};
use std::{cmp::Ordering, collections::HashMap};
use thiserror::Error;

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;

#[derive(Debug, Error, PartialEq)]
pub enum VmError {
    #[error(transparent)]
    InvalidOpcode(#[from] CodeError),
    #[error("stack underflow")]
    StackUnderflow,
    #[error("stack overflow")]
    StackOverflow,
    #[error("object can't be used as a dict key: {0}")]
    InvalidDictKey(String),
    #[error("constant index {0} is out of bounds")]
    InvalidConstantIndex(usize),
    #[error("unsupported binary operation `{op:?}` on {left} and {right}")]
    UnsupportedBinaryOperation {
        op: Opcode,
        left: Object,
        right: Object,
    },
    #[error("unsupported unary operation `{op:?}` on {value}")]
    UnsupportedUnaryOperation { op: Opcode, value: Object },
    #[error("object is not callable: {0}")]
    NotCallable(Object),
    #[error("wrong argument count: expected {expected}, got {got}")]
    WrongArgumentCount { expected: usize, got: usize },
    #[error("constant at index {0} is not a compiled function")]
    InvalidFunctionConstant(usize),
    #[error("builtin index {0} is out of bounds")]
    InvalidBuiltinIndex(usize),
}

type Result<T> = std::result::Result<T, VmError>;

#[derive(Debug, Clone)]
struct Frame {
    closure: Closure,
    pub ip: usize,
    pub base_sp: usize,
}

impl Frame {
    pub fn new(closure: Closure) -> Self {
        Self {
            closure,
            ip: 0,
            base_sp: 0,
        }
    }

    #[must_use]
    pub fn with_stack_pointer(mut self, new_stack_pointer: usize) -> Self {
        self.base_sp = new_stack_pointer;
        self
    }
}

#[derive(Clone)]
pub struct Vm {
    frames: Vec<Frame>,
    constants: Vec<Object>,
    stack: Vec<Object>,
    globals: Vec<Object>,
    builtins: Builtin,
    last_popped: Option<Object>,
}

impl Vm {
    pub fn new(bytecode: Bytecode, builtins: Builtin) -> Self {
        let function = CompiledFunc {
            instructions: bytecode.instructions,
            ..Default::default()
        };
        let closure = Closure {
            function,
            free: Vec::new(),
        };
        Self {
            constants: bytecode.constants,
            frames: vec![Frame::new(closure)],
            stack: Vec::new(),
            globals: Vec::new(),
            builtins,
            last_popped: None,
        }
    }

    fn current_frame(&self) -> &Frame {
        self.frames
            .last()
            .expect("something went wrong when popping frame")
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("something went wrong when popping mut frame")
    }

    fn pop_frame(&mut self) -> Result<Frame> {
        self.frames.pop().ok_or(VmError::StackUnderflow)
    }

    fn push_frame(&mut self, frame: Frame) -> Result<()> {
        if self.frames.len() >= MAX_FRAMES {
            return Err(VmError::StackOverflow);
        }
        self.frames.push(frame);
        Ok(())
    }

    fn current_instructions(&self) -> &Instructions {
        &self.current_frame().closure.function.instructions
    }

    fn advance_ip(&mut self, amount: usize) {
        self.current_frame_mut().ip += amount;
    }

    fn read_u8_operand(&mut self) -> usize {
        let operand = {
            let ip = self.current_frame().ip;
            usize::from(self.current_instructions()[ip])
        };
        self.advance_ip(1);
        operand
    }

    fn read_u16_operand(&mut self) -> usize {
        let operand = {
            let ip = self.current_frame().ip;
            usize::from(read_u16(&self.current_instructions()[ip..]))
        };
        self.advance_ip(2);
        operand
    }

    pub fn run(&mut self) -> Result<()> {
        while self.current_frame().ip < self.current_instructions().len() {
            let opcode = {
                let ip = self.current_frame().ip;
                Opcode::try_from(self.current_instructions()[ip])?
            };
            self.advance_ip(1);

            match opcode {
                Opcode::Constant => {
                    let index = self.read_u16_operand();
                    let constant = self
                        .constants
                        .get(index)
                        .cloned()
                        .ok_or(VmError::InvalidConstantIndex(index))?;
                    self.push(constant)?;
                }
                Opcode::Null => self.push(Object::null())?,
                Opcode::True => self.push(Object::bool(true))?,
                Opcode::False => self.push(Object::bool(false))?,
                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::Equal
                | Opcode::NotEqual
                | Opcode::GreaterThan
                | Opcode::GreaterEqual
                | Opcode::And
                | Opcode::Or
                | Opcode::BitAnd
                | Opcode::BitOr
                | Opcode::LessThan
                | Opcode::LessEqual => self.execute_binary_operation(opcode)?,
                Opcode::Pop => {
                    self.last_popped = Some(self.pop()?);
                }
                Opcode::Minus | Opcode::Bang => self.execute_unary_operation(opcode)?,
                Opcode::JumpNotTruthy => {
                    let position = self.read_u16_operand();
                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        self.current_frame_mut().ip = position;
                    }
                }
                Opcode::Jump => {
                    let position = self.read_u16_operand();
                    self.current_frame_mut().ip = position;
                }

                Opcode::ReturnValue => {
                    let return_value = self.pop()?;
                    let frame = self.pop_frame()?;
                    self.stack.truncate(frame.base_sp - 1);
                    self.push(return_value)?;
                }

                Opcode::Return => {
                    let frame = self.pop_frame()?;
                    self.stack.truncate(frame.base_sp - 1);
                    self.push(Object::Null)?;
                }
                Opcode::SetGlobal => {
                    let index = self.read_u16_operand();
                    let value = self.pop()?;
                    if self.globals.len() <= index {
                        self.globals.resize(index + 1, Object::Null);
                    }
                    self.globals[index] = value;
                }
                Opcode::GetGlobal => {
                    let index = self.read_u16_operand();
                    let value = self.globals.get(index).cloned().unwrap_or(Object::Null);
                    self.push(value)?;
                }
                Opcode::Array => {
                    let elements_count = self.read_u16_operand();
                    let mut values = Vec::with_capacity(elements_count);
                    for _ in 0..elements_count {
                        values.push(self.pop()?);
                    }
                    values.reverse();
                    self.push(Object::array(values))?;
                }
                Opcode::Dictionary => {
                    let elements_count = self.read_u16_operand();
                    let mut values = HashMap::with_capacity(elements_count / 2);
                    for _ in 0..(elements_count / 2) {
                        let value = self.pop()?;
                        let key_object = self.pop()?;
                        let key: HashKey = (&key_object)
                            .try_into()
                            .map_err(|e| VmError::InvalidDictKey(e))?;
                        values.insert(key, value);
                    }
                    self.push(Object::dict(values))?;
                }
                Opcode::Index => {
                    let index = TryInto::<HashKey>::try_into(&self.pop()?)
                        .map_err(|e| VmError::InvalidDictKey(e))?;
                    let value = self.pop()?;

                    let indexed = match index {
                        HashKey::Integer(indx) => {
                            if let Object::Dict { values } = value {
                                values.get(&HashKey::Integer(indx)).cloned()
                            } else if let Object::Array { values } = value {
                                values.get(indx as usize).cloned()
                            } else if let Object::String(val) = value {
                                val.chars().nth(indx as usize).map(Object::char)
                            } else {
                                return Err(VmError::InvalidDictKey(format!(
                                    "can't index '{}' in {}",
                                    indx, value
                                )));
                            }
                        }
                        HashKey::String(indx) => {
                            if let Object::Dict { values } = value {
                                values.get(&HashKey::String(indx)).cloned()
                            } else {
                                return Err(VmError::InvalidDictKey(format!(
                                    "can't index '{}' in {}",
                                    indx, value
                                )));
                            }
                        }
                        HashKey::Bool(indx) => {
                            if let Object::Dict { values } = value {
                                values.get(&HashKey::Bool(indx)).cloned()
                            } else {
                                return Err(VmError::InvalidDictKey(format!(
                                    "can't index '{}' in {}",
                                    indx, value
                                )));
                            }
                        }
                        HashKey::Char(indx) => {
                            if let Object::Dict { values } = value {
                                values.get(&HashKey::Char(indx)).cloned()
                            } else {
                                return Err(VmError::InvalidDictKey(format!(
                                    "can't index '{}' in {}",
                                    indx, value
                                )));
                            }
                        }
                    };
                    self.push(indexed.unwrap_or(Object::Null))?;
                }
                Opcode::Call => {
                    let num_args = self.read_u8_operand();
                    self.execute_call(num_args)?;
                }
                Opcode::Closure => {
                    let constant_index = self.read_u16_operand();
                    let num_free = self.read_u8_operand();
                    self.push_closure(constant_index, num_free)?;
                }
                Opcode::CurrentClosure => {
                    let curr_closure = self.current_frame().closure.clone();
                    self.push(Object::Closure(curr_closure))?;
                }
                Opcode::GetFree => {
                    let index = self.read_u8_operand();
                    let value = self.current_frame().closure.free[index].clone();
                    self.push(value)?;
                }
                Opcode::GetBuiltin => {
                    let index = self.read_u8_operand();
                    let handler = self
                        .builtins
                        .get_handler_by_index(index)
                        .map_err(|_| VmError::InvalidBuiltinIndex(index))?;
                    self.push(Object::builtin_function(handler))?;
                }
                Opcode::SetLocal => {
                    let index = self.read_u8_operand();
                    let base_sp = self.current_frame().base_sp;
                    self.stack[base_sp + index] = self.pop()?;
                }
                Opcode::GetLocal => {
                    let index = self.read_u8_operand();
                    let base_sp = self.current_frame().base_sp;
                    self.push(self.stack[base_sp + index].clone())?;
                }
            }
        }

        Ok(())
    }

    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.last()
    }

    pub fn last_popped_stack_elem(&self) -> Option<&Object> {
        self.last_popped.as_ref()
    }

    #[must_use]
    fn push(&mut self, object: Object) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.stack.push(object);
        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn execute_unary_operation(&mut self, opcode: Opcode) -> Result<()> {
        let value = self.pop()?;
        let result = match opcode {
            Opcode::Minus => match value {
                Object::Integer(integer) => Object::integer(-integer),
                Object::Float(float) => Object::float(-float),
                value => return Err(VmError::UnsupportedUnaryOperation { op: opcode, value }),
            },
            Opcode::Bang => Object::bool(!value.is_truthy()),
            _ => return Err(VmError::UnsupportedUnaryOperation { op: opcode, value }),
        };
        self.push(result)
    }

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        let result = match opcode {
            Opcode::Add => add_objects(&left, &right),
            Opcode::Sub => numeric_binary(&left, &right, |l, r| l - r, |l, r| l - r),
            Opcode::Mul => numeric_binary(&left, &right, |l, r| l * r, |l, r| l * r),
            Opcode::Div => numeric_binary(&left, &right, |l, r| l / r, |l, r| l / r),
            Opcode::Equal => Ok(Object::bool(left == right)),
            Opcode::NotEqual => Ok(Object::bool(left != right)),
            Opcode::GreaterThan => compare_objects(&left, &right, |ordering| ordering.is_gt()),
            Opcode::GreaterEqual => compare_objects(&left, &right, |ordering| ordering.is_ge()),
            Opcode::LessThan => compare_objects(&left, &right, |ordering| ordering.is_lt()),
            Opcode::LessEqual => compare_objects(&left, &right, |ordering| ordering.is_le()),
            Opcode::And => Ok(Object::bool(left.is_truthy() && right.is_truthy())),
            Opcode::Or => Ok(Object::bool(left.is_truthy() || right.is_truthy())),
            Opcode::BitAnd => bitwise_binary(&left, &right, |l, r| l & r),
            Opcode::BitOr => bitwise_binary(&left, &right, |l, r| l | r),
            _ => unreachable!("binary opcode dispatched through wrong path"),
        }
        .map_err(|_| VmError::UnsupportedBinaryOperation {
            op: opcode,
            left,
            right,
        })?;

        self.push(result)?;
        Ok(())
    }

    fn execute_call(&mut self, num_args: usize) -> Result<()> {
        let callee_index = self
            .stack
            .len()
            .checked_sub(1 + num_args)
            .ok_or(VmError::StackUnderflow)?;
        match self.stack[callee_index].clone() {
            Object::Closure(closure) => self.call_closure(closure, num_args),
            Object::BuiltinFunction { handler } => {
                let result = handler(&mut self.stack[callee_index + 1..]);
                self.stack.truncate(callee_index);
                self.push(result)
            }
            object => Err(VmError::NotCallable(object)),
        }
    }

    fn call_closure(&mut self, closure: Closure, num_args: usize) -> Result<()> {
        if num_args != closure.function.num_parameters {
            return Err(VmError::WrongArgumentCount {
                expected: closure.function.num_parameters,
                got: num_args,
            });
        }

        let base_sp = self.stack.len() - num_args;
        let num_locals = closure.function.num_locals;
        let new_sp = base_sp + num_locals;
        if new_sp > STACK_SIZE {
            return Err(VmError::StackOverflow);
        }
        self.push_frame(Frame::new(closure).with_stack_pointer(base_sp))?;
        self.stack.resize(new_sp, Object::Null);
        Ok(())
    }

    fn push_closure(&mut self, constant_index: usize, num_free: usize) -> Result<()> {
        let function = match self.constants.get(constant_index).cloned() {
            Some(Object::CompiledFunction(function)) => function,
            Some(_) => return Err(VmError::InvalidFunctionConstant(constant_index)),
            None => return Err(VmError::InvalidConstantIndex(constant_index)),
        };

        let free_start = self.stack.len() - num_free;
        let free = self.stack[free_start..].to_vec();
        self.stack.truncate(free_start);
        self.push(Object::closure(function, free))
    }
}

fn add_objects(left: &Object, right: &Object) -> std::result::Result<Object, ()> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Ok(Object::integer(left + right)),
        (Object::Integer(left), Object::Float(right)) => Ok(Object::float(*left as f64 + right)),
        (Object::Float(left), Object::Integer(right)) => Ok(Object::float(left + *right as f64)),
        (Object::Float(left), Object::Float(right)) => Ok(Object::float(left + right)),
        (Object::String(left), Object::String(right)) => {
            Ok(Object::string(format!("{left}{right}")))
        }
        _ => Err(()),
    }
}

fn numeric_binary(
    left: &Object,
    right: &Object,
    int_op: impl FnOnce(i64, i64) -> i64,
    float_op: impl FnOnce(f64, f64) -> f64 + Copy,
) -> std::result::Result<Object, ()> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => {
            Ok(Object::integer(int_op(*left, *right)))
        }
        (Object::Integer(left), Object::Float(right)) => {
            Ok(Object::float(float_op(*left as f64, *right)))
        }
        (Object::Float(left), Object::Integer(right)) => {
            Ok(Object::float(float_op(*left, *right as f64)))
        }
        (Object::Float(left), Object::Float(right)) => Ok(Object::float(float_op(*left, *right))),
        _ => Err(()),
    }
}

fn bitwise_binary(
    left: &Object,
    right: &Object,
    op: impl FnOnce(i64, i64) -> i64,
) -> std::result::Result<Object, ()> {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => Ok(Object::integer(op(*left, *right))),
        _ => Err(()),
    }
}

fn compare_objects(
    left: &Object,
    right: &Object,
    predicate: impl FnOnce(Ordering) -> bool,
) -> std::result::Result<Object, ()> {
    let ordering = match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => left.partial_cmp(right),
        (Object::Integer(left), Object::Float(right)) => (*left as f64).partial_cmp(right),
        (Object::Float(left), Object::Integer(right)) => left.partial_cmp(&(*right as f64)),
        (Object::Float(left), Object::Float(right)) => left.partial_cmp(right),
        (Object::Bool(left), Object::Bool(right)) => left.partial_cmp(right),
        (Object::Char(left), Object::Char(right)) => left.partial_cmp(right),
        (Object::String(left), Object::String(right)) => left.partial_cmp(right),
        _ => None,
    }
    .ok_or(())?;

    Ok(Object::bool(predicate(ordering)))
}

#[cfg(test)]
mod test {
    use super::Vm;
    use crate::{builtin::BuiltinBuilder, compiler::compile_program, objects::Object};
    use monkey_core::{lexer::Lexer, parser::Parser};

    fn run_vm(input: &str) -> Object {
        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .unwrap_or_else(|error| panic!("failed to parse compiler test input\n{error:?}"));
        let builtins = BuiltinBuilder::default().build();
        let bytecode = compile_program(&program, &builtins)
            .unwrap_or_else(|error| panic!("failed to compile compiler test input\n{error:?}"));
        let mut vm = Vm::new(bytecode, builtins);
        vm.run()
            .unwrap_or_else(|error| panic!("failed to run compiler test input\n{error:?}"));
        vm.last_popped_stack_elem()
            .cloned()
            .expect("vm should leave a last popped value")
    }

    #[test]
    fn executes_arithmetic_programs() {
        let tests = [
            ("1 + 2", Object::integer(3)),
            ("(1 + 2) * 3", Object::integer(9)),
            ("1 | 2", Object::integer(3)),
            ("5 & 2", Object::integer(0)),
            ("1.5 + 2", Object::float(3.5)),
            ("-3", Object::integer(-3)),
            ("\"mon\" + \"key\"", Object::string("monkey")),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_boolean_and_conditional_programs() {
        let tests = [
            ("true == false", Object::bool(false)),
            ("true && false", Object::bool(false)),
            ("true || false", Object::bool(true)),
            ("1 && \"x\"", Object::bool(true)),
            ("0 || null", Object::bool(false)),
            ("1 < 2", Object::bool(true)),
            ("1 >= 2", Object::bool(false)),
            ("if (true) { 10 } else { 20 }", Object::integer(10)),
            ("if (false) { 10 }", Object::null()),
            ("!true", Object::bool(false)),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_global_bindings() {
        let tests = [
            ("let one = 1; one", Object::integer(1)),
            ("let one = 1; let two = one + 2; two", Object::integer(3)),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_index_expressions_with_brackets_and_dot_syntax() {
        let tests = [
            ("[1, 2][0]", Object::integer(1)),
            (r#"{"answer": 42}["answer"]"#, Object::integer(42)),
            (r#"{"answer": 42}.answer"#, Object::integer(42)),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_function_calls() {
        let tests = [
            ("let five = fn() { 5; }; five();", Object::integer(5)),
            (
                "let add = fn(a, b) { a + b; }; add(1, 2);",
                Object::integer(3),
            ),
            (
                "let add = fn(a, b) { let sum = a + b; sum; }; add(1, 2);",
                Object::integer(3),
            ),
            ("let noReturn = fn() { }; noReturn();", Object::null()),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_local_recursive_let_defined_function() {
        let input = r#"
            let wrapper = fn() {
                let countDown = fn(x) {
                    if (x == 0) { 0 } else { countDown(x - 1); }
                };
                countDown(1);
            };
            wrapper();
        "#;

        assert_eq!(run_vm(input), Object::integer(0));
    }

    #[test]
    fn executes_closures() {
        let tests = [
            (
                "let newAdder = fn(x) { fn(y) { x + y; }; }; let addTwo = newAdder(2); addTwo(3);",
                Object::integer(5),
            ),
            (
                "let global = 10; let newAdder = fn(a) { fn(b) { fn(c) { global + a + b + c; }; }; }; let add = newAdder(1)(2); add(3);",
                Object::integer(16),
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }

    #[test]
    fn executes_builtin_calls() {
        let tests = [
            ("len(\"four\");", Object::integer(4)),
            ("len([1, 2, 3]);", Object::integer(3)),
            ("first([1, 2, 3]);", Object::integer(1)),
            ("last([1, 2, 3]);", Object::integer(3)),
            (
                "push([1, 2], 3);",
                Object::array(vec![
                    Object::integer(1),
                    Object::integer(2),
                    Object::integer(3),
                ]),
            ),
            ("let f = fn(x) { len(x); }; f(\"abc\");", Object::integer(3)),
        ];

        for (input, expected) in tests {
            assert_eq!(run_vm(input), expected, "input: {input}");
        }
    }
}
