mod errors;
mod lexer;
mod parser;
mod repl;
mod virtual_machine;

use repl::repl;

fn main() {
    repl(">> ").unwrap();
}
