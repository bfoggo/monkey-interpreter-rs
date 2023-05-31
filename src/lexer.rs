use crate::errors::LexerError;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    EOF,
    NEWLINE,
    NUMBER(String),
    IDENT(String),
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

pub struct CharacterBuffer(Vec<char>);

impl CharacterBuffer {
    pub fn new() -> CharacterBuffer {
        CharacterBuffer(vec![])
    }

    pub fn push(&mut self, item: char) {
        self.0.push(item);
    }

    pub fn popleft(&mut self) -> Option<char> {
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
    fn needs_buffer(character: char) -> bool {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
            '=' => true,
            '<' => true,
            '>' => true,
            _ => false,
        }
    }
    fn is_ending_character(character: char) -> bool {
        match character {
            '\0' | '\n' | ' ' | '\t' | '+' | '-' | '*' | '/' | '=' | '<' | '>' => true,
            _ => false,
        }
    }
    pub fn get_token(&self, buffer: &mut CharacterBuffer) -> Result<Option<Token>, LexerError> {
        match self.character {
            '\0' => Ok(Some(Token::EOF)),
            '\n' => Ok(Some(Token::NEWLINE)),
            character if Lexer::needs_buffer(character) => {
                buffer.push(character);
                Ok(None)
            }
            '+' => Ok(Some(Token::PLUS)),
            '-' => Ok(Some(Token::MINUS)),
            '*' => Ok(Some(Token::ASTERISK)),
            '/' => Ok(Some(Token::SLASH)),
            '=' => Ok(Some(Token::EQ)),
            '<' => Ok(Some(Token::LT)),
            '>' => Ok(Some(Token::GT)),
            character if Lexer::is_ending_character(character) => {
                let token = self.parse_buffer(buffer)?;
                Ok(token)
            }
            _ => Err(LexerError::InvalidToken(self.character)),
        }
    }
    fn parse_buffer(&self, buffer: &mut CharacterBuffer) -> Result<Option<Token>, LexerError> {
        let buffer_as_string: String = buffer.into_iter().collect();
        match buffer_as_string.as_str() {
            single_item if buffer_as_string.len() == 1 => match single_item {
                "=" => Ok(Some(Token::EQ)),
                ">" => Ok(Some(Token::GT)),
                "<" => Ok(Some(Token::LT)),
                _ => Ok(Some(Token::IDENT(String::from(single_item)))),
            },
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
            "" => Ok(None),
            _ => Ok(Some(Token::IDENT(buffer_as_string))),
        }
    }
}
