use crate::{
    builtin::Builtin,
    code::{CodeError, Instructions, Opcode, disassemble, make},
    objects::Object,
};
use monkey_core::{
    ast::{self, Expression, Program},
    token::TokenType::{self},
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum CompilerError {
    #[error("unsupported statement in compiler: {0}")]
    UnsupportedStatement(&'static str),
    #[error("unsupported expression in compiler: {0}")]
    UnsupportedExpression(&'static str),
    #[error("undefined symbol `{0}`")]
    UndefinedSymbol(String),
    #[error("too many constants for current bytecode format")]
    TooManyConstants,
    #[error("too many globals for current bytecode format")]
    TooManyGlobals,
}

type Result<T> = std::result::Result<T, CompilerError>;
type SymbolIndex = usize;

#[derive(Debug, Clone, Copy)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

#[derive(Debug, Clone, Copy)]
enum SymbolScope {
    Global,
    Local,
    Free,
    Builtin,
    Function,
}

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    scope: SymbolScope,
    index: SymbolIndex,
}

#[derive(Debug, Clone, Default)]
struct SymbolTable {
    func_name: Option<String>,
    outer: Option<Box<SymbolTable>>,
    symbols: HashMap<String, Symbol>,
    free_symbols: Vec<Symbol>,
    num_defined: usize,
}

impl SymbolTable {
    fn define(&mut self, name: &str) -> Result<Symbol> {
        let scope = if self.outer.is_some() {
            SymbolScope::Local
        } else {
            SymbolScope::Global
        };

        let index = self.num_defined;
        let new_symbol = Symbol {
            name: name.into(),
            scope,
            index,
        };
        _ = self.symbols.insert(name.into(), new_symbol.clone());
        self.num_defined += 1;
        Ok(new_symbol)
    }

    fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let Some(func_name) = &self.func_name
            && name == func_name
        {
            return Some(Symbol {
                name: name.into(),
                scope: SymbolScope::Function,
                index: 0,
            });
        }
        if let Some(local_symbol) = self.symbols.get(name) {
            return Some(local_symbol.clone());
        }

        let symbol = self.outer.as_mut()?.resolve(name)?;
        if matches!(symbol.scope, SymbolScope::Global | SymbolScope::Builtin) {
            return Some(symbol);
        }

        Some(self.define_free(symbol))
    }

    fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());
        let symbol = Symbol {
            name: original.name,
            scope: SymbolScope::Free,
            index: self.free_symbols.len() - 1,
        };
        _ = self.symbols.insert(symbol.name.clone(), symbol.clone());
        symbol
    }

    fn define_builtin(&mut self, index: usize, name: &str) {
        let symbol = Symbol {
            name: name.into(),
            scope: SymbolScope::Builtin,
            index,
        };
        _ = self.symbols.insert(name.into(), symbol);
    }
}

#[derive(Debug, Default)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        self.previous_instruction = self.last_instruction;
        self.last_instruction = Some(EmittedInstruction { opcode, position });
    }

    fn remove_last_pop(&mut self) {
        if let Some(last_instruction) = self.last_instruction {
            self.instructions.truncate(last_instruction.position);
            self.last_instruction = self.previous_instruction;
            self.previous_instruction = None;
        }
    }

    #[must_use]
    fn change_operand(&mut self, instruction_position: usize, operand: usize) -> Result<()> {
        let opcode = Opcode::try_from(self.instructions[instruction_position])
            .expect("compiler should only patch known opcodes");
        let mut replacement = Instructions::new();
        _ = make(&mut replacement, opcode, &[operand]);
        self.instructions[instruction_position + 1..instruction_position + replacement.len()]
            .copy_from_slice(&replacement[1..]);
        Ok(())
    }

    fn leave(&mut self) {
        if let Some(mut last_instruction) = self.last_instruction.take() {
            match last_instruction.opcode {
                Opcode::Pop => {
                    self.instructions.truncate(last_instruction.position);
                    last_instruction.opcode = Opcode::ReturnValue;
                    self.last_instruction = Some(last_instruction);
                    make(&mut self.instructions, Opcode::ReturnValue, &[]);
                }
                _ => {
                    self.last_instruction = Some(last_instruction);
                    self.add_instruction(Opcode::Return, &[]);
                }
            }
        } else {
            self.add_instruction(Opcode::Return, &[]);
        }
    }

    fn add_instruction(&mut self, opcode: Opcode, operands: &[usize]) -> usize {
        let position = self.instructions.len();
        _ = make(&mut self.instructions, opcode, operands);
        self.set_last_instruction(opcode, position);
        position
    }
}

#[derive(Debug, Default)]
pub struct Compiler {
    scopes: Vec<CompilationScope>,
    constants: Vec<Object>,
    symbols: SymbolTable,
}

impl Bytecode {
    pub fn disassemble(&self) -> std::result::Result<String, CodeError> {
        disassemble(&self.instructions)
    }
}

impl Compiler {
    pub fn new(builtins: &Builtin) -> Self {
        let mut res = Self::default();
        res.scopes.push(Default::default());
        for (index, name) in builtins.names().enumerate() {
            res.symbols.define_builtin(index, name);
        }
        res
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<Bytecode> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }

        Ok(Bytecode {
            instructions: self.current_scope_mut().instructions.clone(),
            constants: self.constants.clone(),
        })
    }

    fn enter_scope(&mut self, name: Option<&str>) {
        self.scopes.push(Default::default());
        let symbols = std::mem::take(&mut self.symbols);
        self.symbols = SymbolTable {
            outer: Some(Box::new(symbols)),
            func_name: name.map(Into::into),
            ..Default::default()
        }
    }

    fn leave_scope(&mut self) -> (Instructions, usize, Vec<Symbol>) {
        let mut scope = self
            .scopes
            .pop()
            .expect("something went wrong popping the scope");

        let num_defined = self.symbols.num_defined;
        let free_symbols = std::mem::take(&mut self.symbols.free_symbols);
        let symbols = std::mem::take(&mut self.symbols);
        self.symbols = *symbols.outer.expect("something went very bad");

        scope.leave();
        (scope.instructions, num_defined, free_symbols)
    }

    fn current_scope_mut(&mut self) -> &mut CompilationScope {
        self.scopes.last_mut().expect("soemthing went very bad")
    }

    fn current_scope(&self) -> &CompilationScope {
        self.scopes.last().expect("soemthing went very bad")
    }

    #[must_use]
    fn compile_statement(&mut self, statement: &ast::Node) -> Result<()> {
        match statement {
            ast::Node::Let { name, value } => {
                // first define the symbol the compile the expression
                // this enables recursion
                let symbol = self.symbols.define(name)?;
                self.compile_expression(value)?;
                let opcode = match symbol.scope {
                    SymbolScope::Global => Opcode::SetGlobal,
                    SymbolScope::Local => Opcode::SetLocal,
                    // those are defined when accessing a symbol defines in a parent symboltable
                    SymbolScope::Free => unreachable!("free symbols are not assignment targets"),
                    // those are defined manually at the start
                    SymbolScope::Builtin => unreachable!("builtins are not assignment targets"),
                    // this is the self referencial scope when having a recursive call
                    // this can't be set other then when creating a symbol table
                    SymbolScope::Function => unreachable!("function are not assignment targets"),
                };
                self.emit(opcode, &[symbol.index]);
                Ok(())
            }
            ast::Node::Return { value } => {
                self.compile_expression(value)?;
                self.emit(Opcode::ReturnValue, &[]);
                Ok(())
            }
            ast::Node::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(Opcode::Pop, &[]);
                Ok(())
            }
        }
    }

    #[must_use]
    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<()> {
        match expression {
            ast::Expression::Null => {
                self.emit(Opcode::Null, &[]);
                Ok(())
            }
            ast::Expression::Identifier { name } => {
                let symbol = self
                    .symbols
                    .resolve(name)
                    .ok_or_else(|| CompilerError::UndefinedSymbol(name.clone()))?;
                self.load_symbol(&symbol);
                Ok(())
            }
            ast::Expression::Integer { value } => {
                let index = self.add_constant(Object::integer(*value))?;
                self.emit(Opcode::Constant, &[index]);
                Ok(())
            }
            ast::Expression::Float { value } => {
                let index = self.add_constant(Object::float(*value))?;
                self.emit(Opcode::Constant, &[index]);
                Ok(())
            }
            ast::Expression::String { value } => {
                let index = self.add_constant(Object::string(value.clone()))?;
                self.emit(Opcode::Constant, &[index]);
                Ok(())
            }
            ast::Expression::Bool { value } => {
                self.emit(if *value { Opcode::True } else { Opcode::False }, &[]);
                Ok(())
            }
            ast::Expression::Char { value } => {
                let index = self.add_constant(Object::char(*value))?;
                self.emit(Opcode::Constant, &[index]);
                Ok(())
            }
            ast::Expression::Negated { value } => {
                self.compile_expression(value)?;
                self.emit(Opcode::Bang, &[]);
                Ok(())
            }
            ast::Expression::Minus { value } => {
                self.compile_expression(value)?;
                self.emit(Opcode::Minus, &[]);
                Ok(())
            }
            ast::Expression::Block { value } => self.compile_block(value),
            ast::Expression::If {
                condition,
                consequence,
                alternative,
            } => self.compile_if(condition, consequence, alternative.as_ref()),
            ast::Expression::Infix { left, right, op } => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;

                let opcode = match op {
                    TokenType::Plus => Opcode::Add,
                    TokenType::Minus => Opcode::Sub,
                    TokenType::Asterisk => Opcode::Mul,
                    TokenType::Slash => Opcode::Div,
                    TokenType::Eq => Opcode::Equal,
                    TokenType::Neq => Opcode::NotEqual,
                    TokenType::Gt => Opcode::GreaterThan,
                    TokenType::Ge => Opcode::GreaterEqual,
                    TokenType::Lt => Opcode::LessThan,
                    TokenType::Le => Opcode::LessEqual,
                    TokenType::And => Opcode::And,
                    TokenType::Or => Opcode::Or,
                    TokenType::BitAnd => Opcode::BitAnd,
                    TokenType::BitOr => Opcode::BitOr,
                    _ => {
                        return Err(CompilerError::UnsupportedExpression(
                            "this infix operator is not implemented in the compiler yet",
                        ));
                    }
                };

                self.emit(opcode, &[]);
                Ok(())
            }
            ast::Expression::Function {
                name,
                parameters,
                body,
            } => self.compile_function(name, parameters, body),

            ast::Expression::Array { values } => self.compile_array(&values),
            ast::Expression::Dict { values } => self.compile_dictionary(values),
            ast::Expression::Index { value, index } => self.compile_index(&value, &index),
            ast::Expression::Call {
                function,
                parameters,
            } => self.compile_call(function, parameters),
        }
    }

    #[must_use]
    fn compile_if(
        &mut self,
        condition: &ast::Expression,
        consequence: &ast::Block,
        alternative: Option<&ast::Block>,
    ) -> Result<()> {
        self.compile_expression(condition)?;
        let jump_not_truthy = self.emit(Opcode::JumpNotTruthy, &[u16::MAX as usize]);

        self.compile_block(consequence)?;
        self.ensure_branch_value(consequence);

        let jump = self.emit(Opcode::Jump, &[u16::MAX as usize]);
        let alternative_start = self.current_scope_mut().instructions.len();
        self.change_operand(jump_not_truthy, alternative_start)?;

        if let Some(alternative) = alternative {
            self.compile_block(alternative)?;
            self.ensure_branch_value(alternative);
        } else {
            self.emit(Opcode::Null, &[]);
        }

        let after_alternative = self.current_scope_mut().instructions.len();
        self.change_operand(jump, after_alternative)
    }

    #[must_use]
    fn compile_block(&mut self, block: &ast::Block) -> Result<()> {
        for statement in &block.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    #[must_use]
    fn compile_function(
        &mut self,
        name: &Option<String>,
        parameters: &[String],
        block: &ast::Block,
    ) -> Result<()> {
        self.enter_scope(name.as_deref());
        for parameter in parameters {
            self.symbols.define(parameter)?;
        }
        self.compile_block(block)?;

        let (instructions, num_locals, free_symbols) = self.leave_scope();

        for symbol in &free_symbols {
            self.load_symbol(symbol);
        }

        let index = self.add_constant(Object::compiled_function(
            instructions,
            num_locals,
            parameters.len(),
        ))?;
        self.emit(Opcode::Closure, &[index, free_symbols.len()]);
        Ok(())
    }

    fn load_symbol(&mut self, symbol: &Symbol) {
        match symbol.scope {
            SymbolScope::Global => self.emit(Opcode::GetGlobal, &[symbol.index]),
            SymbolScope::Local => self.emit(Opcode::GetLocal, &[symbol.index]),
            SymbolScope::Free => self.emit(Opcode::GetFree, &[symbol.index]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, &[symbol.index]),
            SymbolScope::Function => self.emit(Opcode::CurrentClosure, &[]),
        };
    }

    fn ensure_branch_value(&mut self, block: &ast::Block) {
        if self.last_instruction_is(Opcode::Pop) {
            self.remove_last_pop();
            return;
        }

        let leaves_value = matches!(block.statements.last(), Some(ast::Node::Expression(_)));
        if !leaves_value {
            self.emit(Opcode::Null, &[]);
        }
    }

    #[must_use]
    fn add_constant(&mut self, object: Object) -> Result<SymbolIndex> {
        let index = SymbolIndex::try_from(self.constants.len())
            .map_err(|_| CompilerError::TooManyConstants)?;
        self.constants.push(object);
        Ok(index)
    }

    fn emit(&mut self, opcode: Opcode, operands: &[usize]) -> usize {
        self.current_scope_mut().add_instruction(opcode, operands)
    }

    fn last_instruction_is(&self, opcode: Opcode) -> bool {
        self.current_scope()
            .last_instruction
            .is_some_and(|instruction| instruction.opcode == opcode)
    }

    fn remove_last_pop(&mut self) {
        self.current_scope_mut().remove_last_pop();
    }

    #[must_use]
    fn change_operand(&mut self, instruction_position: usize, operand: usize) -> Result<()> {
        self.current_scope_mut()
            .change_operand(instruction_position, operand)
    }

    fn compile_array(&mut self, values: &[Expression]) -> Result<()> {
        for value in values {
            self.compile_expression(value)?;
        }
        self.emit(Opcode::Array, &[values.len()]);
        Ok(())
    }

    fn compile_dictionary(&mut self, values: &[(Expression, Expression)]) -> Result<()> {
        for (key, value) in values {
            self.compile_expression(key)?;
            self.compile_expression(value)?;
        }
        self.emit(Opcode::Dictionary, &[values.len() * 2]);
        Ok(())
    }

    fn compile_index(&mut self, value: &Expression, index: &Expression) -> Result<()> {
        self.compile_expression(value)?;
        if let Expression::Identifier { name } = index {
            let index = self.add_constant(Object::string(name.clone()))?;
            self.emit(Opcode::Constant, &[index]);
        } else {
            self.compile_expression(index)?;
        }
        self.emit(Opcode::Index, &[]);
        Ok(())
    }

    fn compile_call(&mut self, function: &Expression, parameters: &[Expression]) -> Result<()> {
        self.compile_expression(function)?;
        for parameter in parameters {
            self.compile_expression(parameter)?;
        }
        self.emit(Opcode::Call, &[parameters.len()]);
        Ok(())
    }
}

pub fn compile_program(program: &Program, builtins: &Builtin) -> Result<Bytecode> {
    Compiler::new(builtins).compile_program(program)
}

#[cfg(test)]
mod test {
    use super::compile_program;
    use crate::{builtin::BuiltinBuilder, code::disassemble, objects::Object};
    use monkey_core::{lexer::Lexer, parser::Parser};

    fn compile(input: &str) -> super::Bytecode {
        let program = Parser::new(Lexer::new(input))
            .parse_program()
            .unwrap_or_else(|error| panic!("failed to parse compiler test input\n{error:?}"));

        let builtins = BuiltinBuilder::default().build();
        compile_program(&program, &builtins)
            .unwrap_or_else(|error| panic!("failed to compile compiler test input\n{error:?}"))
    }

    fn compiled_function_instructions(bytecode: &super::Bytecode, index: usize) -> String {
        match &bytecode.constants[index] {
            Object::CompiledFunction(function) => disassemble(&function.instructions).unwrap(),
            object => panic!("constant {index} should be a compiled function, got {object:?}"),
        }
    }

    #[test]
    fn emits_function_call_opcodes() {
        let bytecode = compile("let add = fn(a, b) { a + b; }; add(1, 2);");

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 Closure 0 0\n0004 SetGlobal 0\n0007 GetGlobal 0\n0010 Constant 1\n0013 Constant 2\n0016 Call 2\n0018 Pop\n"
        );
        assert_eq!(
            compiled_function_instructions(&bytecode, 0),
            "0000 GetLocal 0\n0002 GetLocal 1\n0004 Add\n0005 ReturnValue\n"
        );
    }

    #[test]
    fn emits_recursive_function_call_opcodes() {
        let bytecode = compile("let count_down = fn(x) { count_down(x - 1); }; count_down(1);");

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 Closure 1 0\n0004 SetGlobal 0\n0007 GetGlobal 0\n0010 Constant 2\n0013 Call 1\n0015 Pop\n"
        );
        assert_eq!(
            compiled_function_instructions(&bytecode, 1),
            "0000 CurrentClosure\n0001 GetLocal 0\n0003 Constant 0\n0006 Sub\n0007 Call 1\n0009 ReturnValue\n"
        );
    }

    #[test]
    fn emits_closure_opcodes_with_free_variables() {
        let bytecode = compile("fn(x) { fn(y) { x + y; }; };");

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 Closure 1 0\n0004 Pop\n"
        );
        assert_eq!(
            compiled_function_instructions(&bytecode, 0),
            "0000 GetFree 0\n0002 GetLocal 0\n0004 Add\n0005 ReturnValue\n"
        );
        assert_eq!(
            compiled_function_instructions(&bytecode, 1),
            "0000 GetLocal 0\n0002 Closure 0 1\n0006 ReturnValue\n"
        );
    }

    #[test]
    fn emits_builtin_call_opcodes() {
        let bytecode = compile("len([1, 2, 3]);");

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 GetBuiltin 2\n0002 Constant 0\n0005 Constant 1\n0008 Constant 2\n0011 Array 3\n0014 Call 1\n0016 Pop\n"
        );
    }

    #[test]
    fn emits_logical_and_bitwise_opcodes() {
        let bytecode = compile("true && false; true || false; 1 | 2; 5 & 2;");

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 True\n0001 False\n0002 And\n0003 Pop\n0004 True\n0005 False\n0006 Or\n0007 Pop\n0008 Constant 0\n0011 Constant 1\n0014 BitOr\n0015 Pop\n0016 Constant 2\n0019 Constant 3\n0022 BitAnd\n0023 Pop\n"
        );
    }

    #[test]
    fn emits_index_opcodes_for_brackets_and_dot_syntax() {
        let bytecode = compile(r#"[1, 2][0]; {"answer": 42}["answer"]; {"answer": 42}.answer;"#);

        assert_eq!(
            bytecode.disassemble().unwrap(),
            "0000 Constant 0\n0003 Constant 1\n0006 Array 2\n0009 Constant 2\n0012 Index\n0013 Pop\n0014 Constant 3\n0017 Constant 4\n0020 Dictionary 2\n0023 Constant 5\n0026 Index\n0027 Pop\n0028 Constant 6\n0031 Constant 7\n0034 Dictionary 2\n0037 Constant 8\n0040 Index\n0041 Pop\n"
        );
    }
}
