mod errors;
mod lexer;

use lexer::lex;

fn main() {
    let source = "funky fresh m n = m * n;";
    let tokens = lex(source).unwrap();
    println!("{:?}", tokens);
}
