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
    IDENT(String),
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
}

#[derive(Debug, PartialEq, Clone)]
enum IntermediateToken {
    Continue(ComposableToken),
    Finish(CompositeToken),
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
                ComposableToken::IDENT(character.to_string()),
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
            IntermediateToken::Finish(composite_token) => {
                IntermediateToken::Finish(composite_token)
            }
            IntermediateToken::Continue(ComposableToken::NUMBER(curr_number)) => {
                match terminating_character {
                    '0'..='9' => IntermediateToken::Continue(ComposableToken::NUMBER(
                        curr_number + &terminating_character.to_string(),
                    )),
                    _ => IntermediateToken::Finish(CompositeToken::NUMBER(curr_number)),
                }
            }
            IntermediateToken::Continue(ComposableToken::IDENT(curr_ident)) => {
                match terminating_character {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => IntermediateToken::Continue(
                        ComposableToken::IDENT(curr_ident + &terminating_character.to_string()),
                    ),
                    _ => IntermediateToken::Finish(CompositeToken::IDENT(curr_ident)),
                }
            }
            IntermediateToken::Continue(ComposableToken::EQ) => match terminating_character {
                '=' => IntermediateToken::Finish(CompositeToken::EQEQ),
                _ => IntermediateToken::Finish(CompositeToken::EQ),
            },
            IntermediateToken::Continue(ComposableToken::NOT) => match terminating_character {
                '=' => IntermediateToken::Finish(CompositeToken::NOTEQ),
                _ => IntermediateToken::Finish(CompositeToken::NOT),
            },
            IntermediateToken::Continue(ComposableToken::LT) => match terminating_character {
                '=' => IntermediateToken::Finish(CompositeToken::LTEQ),
                _ => IntermediateToken::Finish(CompositeToken::LT),
            },
            IntermediateToken::Continue(ComposableToken::GT) => match terminating_character {
                '=' => IntermediateToken::Finish(CompositeToken::GTEQ),
                _ => IntermediateToken::Finish(CompositeToken::GT),
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
        let new_token = Token::check_termination(intermediate_token, &self.curr_character);
        match new_token {
            IntermediateToken::Finish(composite_token) => {
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
            Some(SingleOrComposable::Single(single_token)) => {
                self.advance();
                Ok(Some(Token::from(single_token)))
            }
            Some(SingleOrComposable::Composable(composable_token)) => Ok(Some(
                self.build_composite_token(IntermediateToken::Continue(composable_token))?,
            )),
            None => {
                self.advance();
                Ok(None)
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
    Ok(tokens)
}
