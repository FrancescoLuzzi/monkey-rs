use miette::Report;
use monkey_compiler::{builtin::BuiltinBuilder, compiler::compile_program, vm::Vm};
use monkey_core::{lexer::Lexer, parser::Parser};
use std::io;

const PROMPT: &[u8; 3] = b">> ";

pub fn start(mut input: impl io::BufRead, mut output: impl io::Write) -> io::Result<()> {
    let mut source = String::new();
    let mut line = String::new();
    let builtins = BuiltinBuilder::default().build();

    output.write_all(PROMPT)?;
    output.flush()?;

    while input.read_line(&mut line)? != 0 {
        let candidate = format!("{source}{line}");
        let mut parser = Parser::new(Lexer::new(&candidate));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(error) => {
                output.write_fmt(format_args!("{:?}\n", Report::new(error)))?;
                output.write_all(PROMPT)?;
                output.flush()?;
                line.clear();
                continue;
            }
        };

        let bytecode = match compile_program(&program, &builtins) {
            Ok(bytecode) => bytecode,
            Err(error) => {
                output.write_fmt(format_args!("{error}\n"))?;
                output.write_all(PROMPT)?;
                output.flush()?;
                line.clear();
                continue;
            }
        };

        let mut vm = Vm::new(bytecode, builtins.clone());
        if let Err(error) = vm.run() {
            output.write_fmt(format_args!("{error}\n"))?;
            output.write_all(PROMPT)?;
            output.flush()?;
            line.clear();
            continue;
        }

        source = candidate;
        if let Some(value) = vm.last_popped_stack_elem() {
            output.write_fmt(format_args!("{value}\n"))?;
        }

        output.write_all(PROMPT)?;
        output.flush()?;
        line.clear();
    }

    Ok(())
}

fn main() {
    let stdin = io::BufReader::new(io::stdin());
    let stdout = io::stdout();

    start(stdin, stdout).unwrap();
}
