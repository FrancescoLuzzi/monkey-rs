use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MonkeyState {
    #[wasm_bindgen(skip)]
    pub env: monkey_core::environment::Environment,
    #[wasm_bindgen(skip)]
    pub builtins: monkey_core::builtin::Builtin,
}

#[wasm_bindgen]
impl MonkeyState {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            env: monkey_core::environment::Environment::new(),
            builtins: monkey_core::builtin::BuiltinBuilder::default().build(),
        }
    }

    #[wasm_bindgen]
    pub fn eval(&mut self, input: &str) -> Result<String, String> {
        let mut parser = monkey_core::parser::Parser::new(monkey_core::lexer::Lexer::new(input));
        let program = parser.parse_program().ok_or("Error parsing".to_string())?;
        monkey_core::evaluator::eval_program(&self.env, &self.builtins, &program)
            .ok_or("Error parsing".to_string())
            .map(|x| x.to_string())
    }
}
