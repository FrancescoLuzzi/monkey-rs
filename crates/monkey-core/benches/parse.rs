use criterion::{Criterion, criterion_group, criterion_main};
use monkey_core::{lexer::Lexer, parser::Parser};
use std::hint::black_box;

const FIBONACCI_PROGRAM: &str = r#"
let fibonacci = fn(n) {
    if (n <= 1) {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
};

fibonacci(10);
"#;

fn benchmark_parse(c: &mut Criterion) {
    c.bench_function("parse_fibonacci", |b| {
        b.iter(|| {
            let program = Parser::new(Lexer::new(FIBONACCI_PROGRAM))
                .parse_program()
                .unwrap_or_else(|error| panic!("fibonacci program should parse\n{error:?}"));

            black_box(program);
        })
    });
}

criterion_group!(benches, benchmark_parse);
criterion_main!(benches);
