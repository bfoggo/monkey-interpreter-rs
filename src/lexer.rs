use crate::errors::LexerError;
use std::{fmt::Debug, iter::Peekable, str::Chars};

#[derive(Debug, PartialEq, Clone)]
pub enum SingleToken {
    EOF,
    NEWLINE,
    SEMICOLON,
    PLUS,
    MINUS,
    ASTERISK,
    DOT,
    COMMA,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    INVALID,
}

#[derive(Debug, PartialEq, Clone)]
enum ComposableToken {
    NUMBER(String),
    CHARS(String),
    EQ,
    NOT,
    LT,
    GT,
}

#[derive(Debug, PartialEq, Clone)]
enum SingleOrComposable {
    Single(SingleToken),
    Composable(ComposableToken),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompositeToken {
    NUMBER(String),
    IDENT(String),
    EQ,
    NOT,
    LT,
    GT,
    EQEQ,
    NOTEQ,
    LTEQ,
    GTEQ,

    // keywords
    IF,
    ELSE,
    WHILE,
    FOR,
    RETURN,
    BREAK,
    CONTINUE,
    PRINT,
    INPUT,
    TRUE,
    FALSE,
    FN,
    LET,
    CONST,
    NULL,
}

#[derive(Debug, PartialEq, Clone)]
enum IntermediateToken {
    Continue(ComposableToken),
    FinishInclusive(CompositeToken),
    FinishExclusive(CompositeToken),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Single(SingleToken),
    Composite(CompositeToken),
}

impl Token {
    fn check_single(character: &char) -> Option<SingleOrComposable> {
        match character {
            '\0' => Some(SingleOrComposable::Single(SingleToken::EOF)),
            '\n' => Some(SingleOrComposable::Single(SingleToken::NEWLINE)),
            ';' => Some(SingleOrComposable::Single(SingleToken::SEMICOLON)),
            '+' => Some(SingleOrComposable::Single(SingleToken::PLUS)),
            '-' => Some(SingleOrComposable::Single(SingleToken::MINUS)),
            '*' => Some(SingleOrComposable::Single(SingleToken::ASTERISK)),
            '.' => Some(SingleOrComposable::Single(SingleToken::DOT)),
            ',' => Some(SingleOrComposable::Single(SingleToken::COMMA)),
            '(' => Some(SingleOrComposable::Single(SingleToken::LPAREN)),
            ')' => Some(SingleOrComposable::Single(SingleToken::RPAREN)),
            '{' => Some(SingleOrComposable::Single(SingleToken::LBRACE)),
            '}' => Some(SingleOrComposable::Single(SingleToken::RBRACE)),
            '=' => Some(SingleOrComposable::Composable(ComposableToken::EQ)),
            '!' => Some(SingleOrComposable::Composable(ComposableToken::NOT)),
            '<' => Some(SingleOrComposable::Composable(ComposableToken::LT)),
            '>' => Some(SingleOrComposable::Composable(ComposableToken::GT)),
            '0'..='9' => Some(SingleOrComposable::Composable(ComposableToken::NUMBER(
                character.to_string(),
            ))),
            'a'..='z' | 'A'..='Z' | '_' => Some(SingleOrComposable::Composable(
                ComposableToken::CHARS(character.to_string()),
            )),
            ' ' | '\t' => None,
            _ => Some(SingleOrComposable::Single(SingleToken::INVALID)),
        }
    }
    fn check_termination(
        composable: IntermediateToken,
        terminating_character: &char,
    ) -> IntermediateToken {
        match composable {
            IntermediateToken::FinishInclusive(composite_token) => {
                IntermediateToken::FinishInclusive(composite_token)
            }
            IntermediateToken::FinishExclusive(composite_token) => {
                IntermediateToken::FinishExclusive(composite_token)
            }
            IntermediateToken::Continue(ComposableToken::NUMBER(curr_number)) => {
                match terminating_character {
                    '0'..='9' => IntermediateToken::Continue(ComposableToken::NUMBER(
                        curr_number + &terminating_character.to_string(),
                    )),
                    _ => IntermediateToken::FinishExclusive(CompositeToken::NUMBER(curr_number)),
                }
            }
            IntermediateToken::Continue(ComposableToken::CHARS(curr_chars)) => {
                match terminating_character {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => IntermediateToken::Continue(
                        ComposableToken::CHARS(curr_chars + &terminating_character.to_string()),
                    ),
                    _ => {
                        let final_token = match curr_chars.as_str() {
                            "if" => CompositeToken::IF,
                            "else" => CompositeToken::ELSE,
                            "while" => CompositeToken::WHILE,
                            "for" => CompositeToken::FOR,
                            "return" => CompositeToken::RETURN,
                            "break" => CompositeToken::BREAK,
                            "continue" => CompositeToken::CONTINUE,
                            "print" => CompositeToken::PRINT,
                            "input" => CompositeToken::INPUT,
                            "true" => CompositeToken::TRUE,
                            "false" => CompositeToken::FALSE,
                            "fn" => CompositeToken::FN,
                            "let" => CompositeToken::LET,
                            "const" => CompositeToken::CONST,
                            "null" => CompositeToken::NULL,
                            _ => CompositeToken::IDENT(curr_chars),
                        };
                        IntermediateToken::FinishExclusive(final_token)
                    }
                }
            }
            IntermediateToken::Continue(ComposableToken::EQ) => match terminating_character {
                '=' => IntermediateToken::FinishInclusive(CompositeToken::EQEQ),
                _ => IntermediateToken::FinishExclusive(CompositeToken::EQ),
            },
            IntermediateToken::Continue(ComposableToken::NOT) => match terminating_character {
                '=' => IntermediateToken::FinishInclusive(CompositeToken::NOTEQ),
                _ => IntermediateToken::FinishExclusive(CompositeToken::NOT),
            },
            IntermediateToken::Continue(ComposableToken::LT) => match terminating_character {
                '=' => IntermediateToken::FinishInclusive(CompositeToken::LTEQ),
                _ => IntermediateToken::FinishExclusive(CompositeToken::LT),
            },
            IntermediateToken::Continue(ComposableToken::GT) => match terminating_character {
                '=' => IntermediateToken::FinishInclusive(CompositeToken::GTEQ),
                _ => IntermediateToken::FinishExclusive(CompositeToken::GT),
            },
        }
    }
}

impl From<SingleToken> for Token {
    fn from(single_token: SingleToken) -> Self {
        Token::Single(single_token)
    }
}

impl From<CompositeToken> for Token {
    fn from(final_composite_token: CompositeToken) -> Self {
        Token::Composite(final_composite_token)
    }
}

#[derive(Debug)]
struct Lexer<'a> {
    char_iter: Peekable<Chars<'a>>,
    curr_character: char,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            char_iter: source.chars().peekable(),
            curr_character: '\0',
        };
        lexer.curr_character = lexer.get_character().unwrap();
        lexer
    }

    fn peek(&mut self) -> &char {
        self.char_iter.peek().unwrap()
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

    fn build_composite_token(
        &mut self,
        intermediate_token: IntermediateToken,
    ) -> Result<Token, LexerError> {
        let new_token = Token::check_termination(intermediate_token, &self.peek());
        match new_token {
            IntermediateToken::FinishExclusive(composite_token) => Ok(Token::from(composite_token)),
            IntermediateToken::FinishInclusive(composite_token) => {
                self.advance();
                Ok(Token::from(composite_token))
            }
            IntermediateToken::Continue(_) => {
                self.advance();
                let final_token = self.build_composite_token(new_token)?;
                Ok(Token::from(final_token))
            }
        }
    }

    fn get_token(&mut self) -> Result<Option<Token>, LexerError> {
        match Token::check_single(&self.curr_character) {
            Some(SingleOrComposable::Single(single_token)) => Ok(Some(Token::from(single_token))),
            Some(SingleOrComposable::Composable(composable_token)) => Ok(Some(
                self.build_composite_token(IntermediateToken::Continue(composable_token))?,
            )),
            None => Ok(None),
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
    Ok(tokens)
}
