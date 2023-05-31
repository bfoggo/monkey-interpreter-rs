use crate::errors::LexerError;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    NEWLINE,
    NUMBER,
    IDENT,
    STRING,
    WHITESPACE,

    // Keywords
    PRINT,
    INPUT,
    LET,
    IF,
    ELSE,
    ELIF,
    WHILE,
    FOR,
    IN,
    CONTINUE,
    BREAK,
    RETURN,
    FUNKY,
    TRUE,
    FALSE,
    NULL,
    AND,
    OR,
    NOT,
    IS,

    // Operators
    EQ,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    EQEQ,
    NOTEQ,
    LT,
    LTEQ,
    GT,
    GTEQ,
}

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    character: char,
}

impl<'a> Lexer<'a> {
    pub fn new(source: Peekable<Chars<'a>>) -> Lexer<'a> {
        let mut lexer = Lexer {
            source,
            character: '\0',
        };
        lexer.next_char();
        lexer
    }

    pub fn get_character(&self) -> &char {
        &self.character
    }

    pub fn next_char(&mut self) -> char {
        self.character = self.source.next().unwrap_or('\0');
        self.character
    }

    pub fn peek(&mut self) -> &char {
        self.source.peek().unwrap_or(&'\0')
    }
    pub fn abort(&self, message: &str) -> ! {
        panic!("Lexing error: {}", message);
    }
    pub fn get_token(&self) -> Result<Token, LexerError> {
        match self.character {
            '\0' => Ok(Token::EOF),
            '\n' => Ok(Token::NEWLINE),
            '0'..='9' => Ok(Token::NUMBER),
            'a'..='z' | 'A'..='Z' | '_' => Ok(Token::IDENT),
            '"' => Ok(Token::STRING),
            '+' => Ok(Token::PLUS),
            '-' => Ok(Token::MINUS),
            '*' => Ok(Token::ASTERISK),
            '/' => Ok(Token::SLASH),
            '=' => Ok(Token::EQ),
            '<' => Ok(Token::LT),
            '>' => Ok(Token::GT),
            ' ' | '\t' => Ok(Token::WHITESPACE),
            _ => Err(LexerError::InvalidToken(self.character)),
        }
    }
}
