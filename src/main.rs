use monkey::repl::start;
use std::io::{BufReader, stdin, stdout};
fn main() {
    let stdin = BufReader::new(stdin());
    let stdout = stdout();

    start(stdin, stdout).unwrap();
}
