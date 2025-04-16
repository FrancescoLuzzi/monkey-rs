set windows-powershell := true
set dotenv-load

default:
    @just --list

test-all:
    @cargo test --workspace

repl:
    @cargo run -p cargo-repl

setup-dev:
    @cargo install wasm-pack

build-repl:
    @cargo build --release -p monkey-repl

build-pages:
    @wasm-pack build --target web examples/monkey-wasm
    @mkdir pages
    @cp -r examples/monkey-wasm/pkg pages/
    @cp -r examples/monkey-wasm/index.html pages/
