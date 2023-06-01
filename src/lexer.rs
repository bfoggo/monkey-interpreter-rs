use crate::errors::LexerError;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    NEWLINE,
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
}

#[derive(Debug)]
struct CharacterBuffer<'a> {
    origin: &'a str,
    position: usize,
    size: usize,
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

    fn push(&mut self) {
        self.size += 1;
    }

    fn popleft(&mut self) -> Result<(), LexerError> {
        if self.size == 0 {
            return Err(LexerError::EmptyBuffer);
        }
        self.position += 1;
        self.size -= 1;
        Ok(())
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
            // I don't actually know if I need this case
            return Lexer::parse_single(
                &self.origin[self.position..self.position + 1]
                    .chars()
                    .next()
                    .unwrap(),
            );
        }
        let as_str = self.as_ref();
        match as_str {
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
        }
    }
}

impl Iterator for &mut CharacterBuffer {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.popleft()
    }
}

struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    character: char,
}

impl<'a> Lexer<'a> {
    fn new(source: Peekable<Chars<'a>>) -> Lexer<'a> {
        let mut lexer = Lexer {
            source,
            character: '\0',
        };
        lexer.next_char();
        lexer
    }

    fn get_character(&self) -> &char {
        &self.character
    }

    fn next_char(&mut self) -> char {
        self.character = self.source.next().unwrap_or('\0');
        self.character
    }
    fn finish(&mut self, buffer: &mut CharacterBuffer) -> Result<Option<Token>, LexerError> {
        self.parse_buffer(buffer)
    }
    fn needs_buffer(character: char) -> bool {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
            '=' => true,
            '<' => true,
            '>' => true,
            _ => false,
        }
    }
    fn is_buffer_terminating(character: char) -> bool {
        match character {
            '\0' | '\n' | ' ' | '\t' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | ';' => true,
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
            _ => Err(LexerError::InvalidToken(character.clone())),
        }
    }

    fn get_token(&self, buffer: &mut CharacterBuffer) -> Result<Option<Token>, LexerError> {
        match self.character {
            character if Lexer::needs_buffer(character) => {
                buffer.push(character);
                Ok(None)
            }
            character if Lexer::is_buffer_terminating(character) => {
                let token = self.parse_buffer(buffer)?;
                if character != '\0' && character != ' ' && character != '\t' && character != '\n' {
                    buffer.push(character);
                }
                Ok(token)
            }
            _ => Lexer::parse_single(&self.character),
        }
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source.chars());
    let mut buffer = CharacterBuffer::new();
    let mut tokens = vec![];
    while *lexer.get_character() != '\0' {
        let token = lexer.get_token(&mut buffer)?;
        if let Some(token) = token {
            tokens.push(token);
        }
        lexer.next_char();
    }
    let final_token = lexer.finish(&mut buffer)?;
    if let Some(final_token) = final_token {
        tokens.push(final_token);
    }
    Ok(tokens)
}
