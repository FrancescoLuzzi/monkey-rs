use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use monkey_compiler::{
    builtin::BuiltinBuilder, compiler::compile_program, objects::Object, vm::Vm,
};
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

fn benchmark_compiler(c: &mut Criterion) {
    let program = Parser::new(Lexer::new(FIBONACCI_PROGRAM))
        .parse_program()
        .unwrap_or_else(|error| panic!("fibonacci program should parse\n{error:?}"));
    let builtins = BuiltinBuilder::default().build();
    let bytecode = compile_program(&program, &builtins)
        .unwrap_or_else(|error| panic!("fibonacci program should compile\n{error:?}"));

    c.bench_function("compiler_fibonacci", |b| {
        b.iter_batched(
            || (bytecode.clone(), builtins.clone()),
            |(bytecode, builtins)| {
                let mut vm = Vm::new(bytecode, builtins);
                vm.run()
                    .unwrap_or_else(|error| panic!("fibonacci program should execute\n{error:?}"));
                let result = vm
                    .last_popped_stack_elem()
                    .expect("vm should leave a last popped value");

                debug_assert_eq!(result, &Object::Integer(55));
                black_box(result);
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, benchmark_compiler);
criterion_main!(benches);
