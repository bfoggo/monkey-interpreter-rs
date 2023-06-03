mod errors;
mod lexer;
mod virtual_machine;

use lexer::lex;

fn main() {
    let source = "funky fresh m n = m * n;\0";
    let tokens = lex(source).unwrap();
    println!("{:?}", tokens);
}
