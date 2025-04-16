use monkey_core::{
    builtin, environment::Environment, evaluator::eval_program, lexer::Lexer, parser::Parser,
};
use std::io;

const PROMPT: &[u8; 3] = b">> ";

pub fn start(mut input: impl io::BufRead, mut output: impl io::Write) -> io::Result<()> {
    let mut buff = String::new();
    output.write_all(PROMPT)?;
    output.flush()?;
    let env = Environment::new();
    let builtins = builtin::BuiltinBuilder::default().build();
    while input.read_line(&mut buff).is_ok() {
        let mut parser = Parser::new(Lexer::new(&buff));
        output.write_fmt(format_args!(
            "{}\n",
            eval_program(
                &env,
                &builtins,
                &parser
                    .parse_program()
                    .ok_or(io::Error::new(io::ErrorKind::Other, "error parsing"))?
            )
            .ok_or(io::Error::new(io::ErrorKind::Other, "error eval"))?
        ))?;
        output.write_all(PROMPT)?;
        output.flush()?;
        buff.clear();
    }
    Ok(())
}

fn main() {
    let stdin = io::BufReader::new(io::stdin());
    let stdout = io::stdout();

    start(stdin, stdout).unwrap();
}
