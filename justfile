set windows-powershell := true
set dotenv-load

default:
    @just --list

test-all:
    @cargo test --workspace

repl:
    @cargo run -p monkey-repl

build-repl:
    @cargo build --release -p monkey-repl

setup-github-pages:
    @cargo install wasm-pack

build-github-pages:
    @wasm-pack build --target web examples/monkey-wasm
    @mkdir pages
    @cp -r examples/monkey-wasm/pkg pages/
    @cp -r examples/monkey-wasm/index.html pages/
