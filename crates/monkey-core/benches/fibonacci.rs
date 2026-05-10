use criterion::{Criterion, criterion_group, criterion_main};
use monkey_core::{
    builtin::BuiltinBuilder, environment::Environment, evaluator::eval_program, lexer::Lexer,
    objects::Object, parser::Parser,
};
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

fn benchmark_fibonacci(c: &mut Criterion) {
    c.bench_function("parse_fibonacci", |b| {
        b.iter(|| {
            let program = Parser::new(Lexer::new(FIBONACCI_PROGRAM))
                .parse_program()
                .unwrap_or_else(|error| panic!("fibonacci program should parse\n{error:?}"));

            black_box(program);
        })
    });

    let program = Parser::new(Lexer::new(FIBONACCI_PROGRAM))
        .parse_program()
        .unwrap_or_else(|error| panic!("fibonacci program should parse\n{error:?}"));
    let builtin = BuiltinBuilder::default().build();

    c.bench_function("eval_fibonacci", |b| {
        b.iter(|| {
            let env = Environment::new();
            let result = eval_program(&env, &builtin, black_box(&program))
                .expect("fibonacci program should evaluate");

            debug_assert_eq!(result, Object::Integer(55));
            black_box(result);
        })
    });
}

criterion_group!(benches, benchmark_fibonacci);
criterion_main!(benches);
