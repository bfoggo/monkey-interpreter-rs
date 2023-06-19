use crate::eval::Environment;
use crate::lexer::lex;
use crate::parser::{parse, AST};
use std::io::{self, Write};

pub fn repl(prompt: &'static str) -> io::Result<()> {
    let mut environment = Environment::new();
    loop {
        io::stdout().write(prompt.as_bytes())?;
        io::stdout().flush()?;
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let tokens = lex(&buffer).unwrap();
        let program = parse(tokens);
        if program.is_err() {
            continue;
        }
        let evaluation = environment.eval(AST::Program(program.unwrap()));
        println!("  : {}", evaluation);
    }
}
