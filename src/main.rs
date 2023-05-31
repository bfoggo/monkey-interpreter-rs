mod errors;
mod lexer;

use errors::LexerError;
use lexer::Lexer;

fn main() {
    let source = "let x = 5 + 5 * 2";
    let mut lexer = Lexer::new(source.chars().peekable());

    while lexer.get_character().clone() != '\0' {
        println!(
            "{:?} - {:?}",
            lexer.get_character(),
            lexer.get_token().unwrap()
        );
        lexer.next_char();
    }
}
