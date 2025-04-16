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

wasm-example:
    @wasm-pack build --target web examples/monkey-wasm
    @python -m http.server --directory examples/monkey-wasm