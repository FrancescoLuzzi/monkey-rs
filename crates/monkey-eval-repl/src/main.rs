use miette::Report;
use monkey_core::{lexer::Lexer, parser::Parser};
use monkey_eval::{builtin, environment::Environment, evaluator::eval_program};
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
    let env = Environment::new();
    let builtins = builtin::BuiltinBuilder::default().build();

    if !interactive {
        let mut source = String::new();
        input.read_to_string(&mut source)?;
        if !source.trim().is_empty() {
            eval_source(&source, &env, &builtins, &mut output)?;
        }
        return Ok(());
    }

    let mut buff = String::new();
    write_prompt(&mut output, interactive)?;

    while input.read_line(&mut buff)? != 0 {
        eval_source(&buff, &env, &builtins, &mut output)?;
        write_prompt(&mut output, interactive)?;
        buff.clear();
    }
    Ok(())
}

fn eval_source(
    source: &str,
    env: &Environment,
    builtins: &builtin::Builtin,
    output: &mut impl io::Write,
) -> io::Result<()> {
    let mut parser = Parser::new(Lexer::new(source));
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(error) => {
            output.write_fmt(format_args!("{:?}\n", Report::new(error)))?;
            return Ok(());
        }
    };

    output.write_fmt(format_args!(
        "{}\n",
        eval_program(env, builtins, &program).ok_or(io::Error::other("error eval"))?
    ))
}

fn main() {
    let stdin = io::stdin();
    let interactive = stdin.is_terminal();
    let stdin = io::BufReader::new(stdin);
    let stdout = io::stdout();

    start_with_prompt(stdin, stdout, interactive).unwrap();
}
