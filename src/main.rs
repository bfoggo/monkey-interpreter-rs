mod errors;
mod lexer;
mod virtual_machine;

use lexer::lex;

fn main() {
    let source = "let five = 5;\nlet ten = 10;\nlet add = funky(x, y) {\n  x + y;\n};\nlet result = add(five, ten);\n";
    println!("{}", source);
    let tokens = lex(source).unwrap();
    println!("{:?}", tokens);
}
