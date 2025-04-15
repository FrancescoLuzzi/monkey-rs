use crate::{
    builtin, environment::Environment, evaluator::eval_program, lexer::Lexer, parser::Parser,
};
use std::io::{self, BufRead, Write};

pub fn start(mut input: impl BufRead, mut output: impl Write) -> io::Result<()> {
    let mut buff = String::new();
    output.write_all(b">>")?;
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
        output.write_all(b">>")?;
        output.flush()?;
        buff.clear();
    }
    Ok(())
}
