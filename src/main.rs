mod errors;
mod lexer;
mod repl;
mod virtual_machine;

use repl::repl;

fn main() {
    repl(">> ").unwrap();
}
