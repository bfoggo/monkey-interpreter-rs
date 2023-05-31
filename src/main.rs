mod errors;
mod lexer;

use lexer::{CharacterBuffer, Lexer};

fn main() {
    let source = "funky fresh m n = m * n";
    let mut lexer = Lexer::new(source.chars().peekable());
    let mut buffer = CharacterBuffer::new();

    while *lexer.get_character() != '\0' {
        println!(
            "{:?} - {:?}",
            lexer.get_character(),
            lexer.get_token(&mut buffer).unwrap().clone()
        );
        lexer.next_char();
    }
}
