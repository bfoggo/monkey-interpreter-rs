use crate::errors::LexerError;
use std::{
    fmt::{Debug, Formatter},
    iter::Peekable,
    str::Chars,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    NEWLINE,
    WHITESPACE,
    NUMBER(String),
    IDENT(String),
    SEMICOLON,

    // Keywords
    PRINT,
    LET,
    IF,
    ELSE,
    ELIF,
    WHILE,
    FOR,
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

    // Delimiters
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    DOT,
}

struct CharacterBuffer<'a> {
    origin: &'a str,
    position: usize,
    size: usize,
}

impl<'a> Debug for CharacterBuffer<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CharacterBuffer")
            .field("as_str", &self.as_ref())
            .finish()
    }
}

impl<'a> AsRef<str> for CharacterBuffer<'a> {
    fn as_ref(&self) -> &'a str {
        &self.origin[self.position..self.position + self.size]
    }
}

impl<'a> CharacterBuffer<'a> {
    fn new(origin: &'a str) -> CharacterBuffer<'a> {
        CharacterBuffer {
            origin,
            position: 0,
            size: 0,
        }
    }

    fn push(&mut self) -> Result<(), LexerError> {
        self.size += 1;
        Ok(())
    }

    fn skip(&mut self) -> Result<(), LexerError> {
        self.position += 1;
        Ok(())
    }

    fn clear(&mut self) {
        self.position += self.size;
        self.size = 0;
    }

    fn is_empty(&self) -> bool {
        self.size == 0
    }

    fn is_single(&self) -> bool {
        self.size == 1
    }

    fn parse(&mut self) -> Result<Option<Token>, LexerError> {
        if self.is_empty() {
            return Ok(None);
        }
        if self.is_single() {
            let rtoken = Lexer::parse_single(
                &self.origin[self.position..self.position + 1]
                    .chars()
                    .next()
                    .unwrap(),
            );
            self.clear();
            return rtoken;
        }
        let as_str = self.as_ref();
        let rtoken = match as_str {
            "print" => Ok(Some(Token::PRINT)),
            "let" => Ok(Some(Token::LET)),
            "if" => Ok(Some(Token::IF)),
            "else" => Ok(Some(Token::ELSE)),
            "elif" => Ok(Some(Token::ELIF)),
            "while" => Ok(Some(Token::WHILE)),
            "for" => Ok(Some(Token::FOR)),
            "continue" => Ok(Some(Token::CONTINUE)),
            "break" => Ok(Some(Token::BREAK)),
            "return" => Ok(Some(Token::RETURN)),
            "funky" => Ok(Some(Token::FUNKY)),
            "true" => Ok(Some(Token::TRUE)),
            "false" => Ok(Some(Token::FALSE)),
            "null" => Ok(Some(Token::NULL)),
            "and" => Ok(Some(Token::AND)),
            "or" => Ok(Some(Token::OR)),
            "not" => Ok(Some(Token::NOT)),
            "is" => Ok(Some(Token::IS)),
            "==" => Ok(Some(Token::EQEQ)),
            "!=" => Ok(Some(Token::NOTEQ)),
            "<=" => Ok(Some(Token::LTEQ)),
            ">=" => Ok(Some(Token::GTEQ)),
            "" => Ok(None),
            digits if digits.chars().all(|c| c.is_digit(10)) => {
                Ok(Some(Token::NUMBER(as_str.to_string())))
            }
            letters if letters.chars().all(|c| c.is_alphabetic()) => {
                Ok(Some(Token::IDENT(as_str.to_string())))
            }
            _ => Err(LexerError::InvalidBufferToken(as_str.to_string())),
        };
        self.clear();
        rtoken
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    buffer: CharacterBuffer<'a>,
    char_iter: Peekable<Chars<'a>>,
    curr_character: char,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            buffer: CharacterBuffer::new(source),
            char_iter: source.chars().peekable(),
            curr_character: '\0',
        };
        lexer.curr_character = lexer.get_character().unwrap();
        lexer
    }

    fn advance(&mut self) {
        self.curr_character = self.get_character().unwrap_or('\0');
    }

    fn get_character(&mut self) -> Option<char> {
        self.char_iter.next()
    }

    fn is_end(&self) -> bool {
        self.curr_character == '\0'
    }

    fn finish(&mut self) -> Result<Option<Token>, LexerError> {
        self.buffer.parse()
    }

    fn needs_buffer(character: &char) -> bool {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
            '=' => true,
            '<' => true,
            '>' => true,
            _ => false,
        }
    }
    fn is_buffer_terminating(character: &char) -> bool {
        match character {
            '\0' | '\n' | ' ' | '\t' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | ';' | '('
            | ')' | '{' | '}' | ',' | '.' => true,
            _ => false,
        }
    }
    fn parse_single(character: &char) -> Result<Option<Token>, LexerError> {
        match character {
            '\0' => Ok(Some(Token::EOF)),
            '\n' => Ok(Some(Token::NEWLINE)),
            '+' => Ok(Some(Token::PLUS)),
            '-' => Ok(Some(Token::MINUS)),
            '*' => Ok(Some(Token::ASTERISK)),
            '/' => Ok(Some(Token::SLASH)),
            '=' => Ok(Some(Token::EQ)),
            '<' => Ok(Some(Token::LT)),
            '>' => Ok(Some(Token::GT)),
            '0'..='9' => Ok(Some(Token::NUMBER(character.to_string()))),
            'a'..='z' | 'A'..='Z' | '_' => Ok(Some(Token::IDENT(character.to_string()))),
            ';' => Ok(Some(Token::SEMICOLON)),
            '(' => Ok(Some(Token::LPAREN)),
            ')' => Ok(Some(Token::RPAREN)),
            '{' => Ok(Some(Token::LBRACE)),
            '}' => Ok(Some(Token::RBRACE)),
            ',' => Ok(Some(Token::COMMA)),
            '.' => Ok(Some(Token::DOT)),
            ' ' | '\t' => Ok(Some(Token::WHITESPACE)),
            _ => Err(LexerError::InvalidToken(character.clone())),
        }
    }

    fn get_token(&mut self) -> Result<Option<Token>, LexerError> {
        match self.curr_character {
            character if Lexer::needs_buffer(&character) => {
                while !self.is_end()
                    && !Lexer::is_buffer_terminating(&self.char_iter.peek().unwrap_or(&'\0'))
                {
                    self.buffer.push()?;
                    self.advance();
                }
                self.buffer.push()?;
                let token = self.buffer.parse();
                token
            }
            _ => {
                let token = Lexer::parse_single(&self.curr_character);
                self.buffer.skip()?;
                token
            }
        }
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source);
    let mut tokens = vec![];
    while !lexer.is_end() {
        let token = lexer.get_token()?;
        if let Some(token) = token {
            tokens.push(token);
        }
        lexer.advance();
    }
    let final_token = lexer.finish()?;
    if let Some(final_token) = final_token {
        tokens.push(final_token);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_buffer() {
        let mut buffer = CharacterBuffer::new("hello world");
        for i in 1..3 {
            buffer.push().unwrap();
            assert_eq!(buffer.size, i);
        }
        assert_eq!(buffer.as_ref(), "he");
        let he_token = buffer.parse().unwrap().unwrap();
        assert_eq!(he_token, Token::IDENT("he".to_string()));
        assert_eq!(buffer.position, 2);
        assert_eq!(buffer.size, 0);
        for i in 1..6 {
            buffer.push().unwrap();
            assert_eq!(buffer.size, i);
        }
        assert_eq!(buffer.as_ref(), "llo w");
        buffer.clear();
        assert_eq!(buffer.position, 7);
        for i in 1..4 {
            buffer.push().unwrap();
            assert_eq!(buffer.size, i);
        }
        assert!(buffer.push().is_err());
    }
}
