use miette::Report;
use monkey_compiler::{builtin::BuiltinBuilder, compiler::compile_program, vm::Vm};
use monkey_core::{lexer::Lexer, parser::Parser};
use std::io::{self, IsTerminal};

const PROMPT: &[u8; 3] = b">> ";

pub fn start(mut input: impl io::BufRead, mut output: impl io::Write) -> io::Result<()> {
    start_with_prompt(&mut input, &mut output, true)
}

fn write_prompt(output: &mut impl io::Write, interactive: bool) -> io::Result<()> {
    if interactive {
        output.write_all(PROMPT)?;
        output.flush()?;
    }
    Ok(())
}

fn start_with_prompt(
    mut input: impl io::BufRead,
    mut output: impl io::Write,
    interactive: bool,
) -> io::Result<()> {
    let mut source = String::new();
    let mut line = String::new();
    let builtins = BuiltinBuilder::default().build();

    if !interactive {
        input.read_to_string(&mut source)?;
        if source.trim().is_empty() {
            return Ok(());
        }

        let mut parser = Parser::new(Lexer::new(&source));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(error) => {
                output.write_fmt(format_args!("{:?}\n", Report::new(error)))?;
                return Ok(());
            }
        };

        let bytecode = match compile_program(&program, &builtins) {
            Ok(bytecode) => bytecode,
            Err(error) => {
                output.write_fmt(format_args!("{error}\n"))?;
                return Ok(());
            }
        };

        let mut vm = Vm::new(bytecode, builtins);
        if let Err(error) = vm.run() {
            output.write_fmt(format_args!("{error}\n"))?;
            return Ok(());
        }

        if let Some(value) = vm.last_popped_stack_elem() {
            output.write_fmt(format_args!("{value}\n"))?;
        }
        return Ok(());
    }

    write_prompt(&mut output, interactive)?;

    while input.read_line(&mut line)? != 0 {
        let candidate = format!("{source}{line}");
        let mut parser = Parser::new(Lexer::new(&candidate));
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(error) => {
                output.write_fmt(format_args!("{:?}\n", Report::new(error)))?;
                write_prompt(&mut output, interactive)?;
                line.clear();
                continue;
            }
        };

        let bytecode = match compile_program(&program, &builtins) {
            Ok(bytecode) => bytecode,
            Err(error) => {
                output.write_fmt(format_args!("{error}\n"))?;
                write_prompt(&mut output, interactive)?;
                line.clear();
                continue;
            }
        };

        let mut vm = Vm::new(bytecode, builtins.clone());
        if let Err(error) = vm.run() {
            output.write_fmt(format_args!("{error}\n"))?;
            write_prompt(&mut output, interactive)?;
            line.clear();
            continue;
        }

        source = candidate;
        if let Some(value) = vm.last_popped_stack_elem() {
            output.write_fmt(format_args!("{value}\n"))?;
        }

        write_prompt(&mut output, interactive)?;
        line.clear();
    }

    Ok(())
}

fn main() {
    let stdin = io::stdin();
    let interactive = stdin.is_terminal();
    let stdin = io::BufReader::new(stdin);
    let stdout = io::stdout();

    start_with_prompt(stdin, stdout, interactive).unwrap();
}
