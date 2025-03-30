use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, parser::Parser};

pub fn start(mut input: impl BufRead, mut output: impl Write) -> io::Result<()> {
    let mut buff = String::new();
    output.write_all(b">>")?;
    output.flush()?;
    while input.read_line(&mut buff).is_ok() {
        let mut parser = Parser::new(Lexer::new(&buff));
        output.write_fmt(format_args!(
            "{}\n",
            parser
                .parse_program()
                .ok_or(io::Error::new(io::ErrorKind::Other, "error parsing"))?
        ))?;
        output.write_all(b">>")?;
        output.flush()?;
    }
    Ok(())
}
