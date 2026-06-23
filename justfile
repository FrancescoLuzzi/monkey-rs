set windows-powershell := true
set dotenv-load

default:
    @just --list

test-all:
    @cargo test --workspace

repl:
    @cargo run -p monkey-eval-repl

compiler-repl:
    @cargo run -p monkey-compiler-repl

build-repl:
    @cargo build --release -p monkey-eval-repl

build-compiler-repl:
    @cargo build --release -p monkey-compiler-repl

bench-parse:
    @cargo bench -p monkey-parser --bench parse

bench-eval:
    @cargo bench -p monkey-eval --bench eval

bench-compiler:
    @cargo bench -p monkey-compiler --bench compiler

setup-github-pages:
    @cargo install wasm-pack

build-github-pages:
    @wasm-pack build --target web examples/monkey-wasm
    @mkdir pages
    @cp -r examples/monkey-wasm/pkg pages/
    @cp -r examples/monkey-wasm/index.html pages/
