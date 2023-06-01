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

#[derive(Debug)]
struct CharacterBuffer(Vec<char>);

impl CharacterBuffer {
    fn new() -> CharacterBuffer {
        CharacterBuffer(vec![])
    }

    fn push(&mut self, item: char) {
        self.0.push(item);
    }

    fn popleft(&mut self) -> Option<char> {
        if self.0.is_empty() {
            return None;
        }
        Some(self.0.remove(0))
    }
}

impl Iterator for &mut CharacterBuffer {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.popleft()
    }
}

struct Lexer<'a> {
    source: Chars<'a>,
    character: char,
}

impl<'a> Lexer<'a> {
    fn new(source: Chars<'a>) -> Lexer<'a> {
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
    fn parse_buffer(&self, buffer: &mut CharacterBuffer) -> Result<Option<Token>, LexerError> {
        if buffer.0.is_empty() {
            return Ok(None);
        }
        if buffer.0.len() == 1 {
            return Lexer::parse_single(&buffer.popleft().unwrap());
        }
        let buffer_as_string: String = buffer.into_iter().collect();
        match buffer_as_string.as_str() {
            "print" => Ok(Some(Token::PRINT)),
            "input" => Ok(Some(Token::INPUT)),
            "let" => Ok(Some(Token::LET)),
            "if" => Ok(Some(Token::IF)),
            "else" => Ok(Some(Token::ELSE)),
            "elif" => Ok(Some(Token::ELIF)),
            "while" => Ok(Some(Token::WHILE)),
            "for" => Ok(Some(Token::FOR)),
            "in" => Ok(Some(Token::IN)),
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
                Ok(Some(Token::NUMBER(buffer_as_string)))
            }
            letters if letters.chars().all(|c| c.is_alphabetic()) => {
                Ok(Some(Token::IDENT(buffer_as_string)))
            }
            _ => Err(LexerError::InvalidBufferToken(buffer_as_string)),
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
