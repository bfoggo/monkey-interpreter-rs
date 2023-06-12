mod errors;
mod eval;
mod lexer;
mod parser;
mod repl;

use repl::repl;

fn main() {
    repl(">> ").unwrap();
}
