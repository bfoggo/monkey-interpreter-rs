mod errors;
mod lexer;

use lexer::{CharacterBuffer, Lexer};

fn main() {
    let source = "funky fresh m n = m * n;";
    let mut lexer = Lexer::new(source.chars().peekable());
    let mut buffer = CharacterBuffer::new();

    while *lexer.get_character() != '\0' {
        println!("{:?}", buffer);
        println!(
            "{:?} - {:?}",
            lexer.get_character(),
            lexer.get_token(&mut buffer).unwrap().clone()
        );
        lexer.next_char();
    }
    let final_token = lexer.finish(&mut buffer).unwrap();
    println!("{:?}", final_token);
}
