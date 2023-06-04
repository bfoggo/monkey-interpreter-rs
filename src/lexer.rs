use crate::errors::LexerError;
use std::{fmt::Debug, iter::Peekable, str::Chars};

mod tagged_token {

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
    pub enum ComposableToken {
        NUMBER(String),
        CHARS(String),
        EQ,
        NOT,
        LT,
        GT,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum SingleOrComposable {
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
    pub enum IntermediateToken {
        Continue(ComposableToken),
        FinishInclusive(CompositeToken),
        FinishExclusive(CompositeToken),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum TaggedToken {
        Single(SingleToken),
        Composite(CompositeToken),
    }

    impl TaggedToken {
        pub fn check_single(character: &char) -> Option<SingleOrComposable> {
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
        pub fn check_termination(
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
                        _ => {
                            IntermediateToken::FinishExclusive(CompositeToken::NUMBER(curr_number))
                        }
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

    impl From<SingleToken> for TaggedToken {
        fn from(single_token: SingleToken) -> Self {
            TaggedToken::Single(single_token)
        }
    }

    impl From<CompositeToken> for TaggedToken {
        fn from(final_composite_token: CompositeToken) -> Self {
            TaggedToken::Composite(final_composite_token)
        }
    }
}

use tagged_token::{IntermediateToken, SingleOrComposable, TaggedToken};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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
    EQ,
    NOT,
    LT,
    GT,
    EQEQ,
    NOTEQ,
    LTEQ,
    GTEQ,
    NUMBER(String),
    IDENT(String),
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
    INVALID,
}

impl From<TaggedToken> for Token {
    fn from(tagged_token: TaggedToken) -> Self {
        match tagged_token {
            TaggedToken::Single(single_token) => match single_token {
                tagged_token::SingleToken::EOF => Token::EOF,
                tagged_token::SingleToken::NEWLINE => Token::NEWLINE,
                tagged_token::SingleToken::SEMICOLON => Token::SEMICOLON,
                tagged_token::SingleToken::PLUS => Token::PLUS,
                tagged_token::SingleToken::MINUS => Token::MINUS,
                tagged_token::SingleToken::ASTERISK => Token::ASTERISK,
                tagged_token::SingleToken::DOT => Token::DOT,
                tagged_token::SingleToken::COMMA => Token::COMMA,
                tagged_token::SingleToken::LPAREN => Token::LPAREN,
                tagged_token::SingleToken::RPAREN => Token::RPAREN,
                tagged_token::SingleToken::LBRACE => Token::LBRACE,
                tagged_token::SingleToken::RBRACE => Token::RBRACE,
                tagged_token::SingleToken::INVALID => Token::INVALID,
            },
            TaggedToken::Composite(composite_token) => match composite_token {
                tagged_token::CompositeToken::NUMBER(number) => Token::NUMBER(number),
                tagged_token::CompositeToken::IDENT(identifier) => Token::IDENT(identifier),
                tagged_token::CompositeToken::EQ => Token::EQ,
                tagged_token::CompositeToken::NOT => Token::NOT,
                tagged_token::CompositeToken::LT => Token::LT,
                tagged_token::CompositeToken::GT => Token::GT,
                tagged_token::CompositeToken::EQEQ => Token::EQEQ,
                tagged_token::CompositeToken::NOTEQ => Token::NOTEQ,
                tagged_token::CompositeToken::LTEQ => Token::LTEQ,
                tagged_token::CompositeToken::GTEQ => Token::GTEQ,
                tagged_token::CompositeToken::IF => Token::IF,
                tagged_token::CompositeToken::ELSE => Token::ELSE,
                tagged_token::CompositeToken::WHILE => Token::WHILE,
                tagged_token::CompositeToken::FOR => Token::FOR,
                tagged_token::CompositeToken::RETURN => Token::RETURN,
                tagged_token::CompositeToken::BREAK => Token::BREAK,
                tagged_token::CompositeToken::CONTINUE => Token::CONTINUE,
                tagged_token::CompositeToken::PRINT => Token::PRINT,
                tagged_token::CompositeToken::INPUT => Token::INPUT,
                tagged_token::CompositeToken::TRUE => Token::TRUE,
                tagged_token::CompositeToken::FALSE => Token::FALSE,
                tagged_token::CompositeToken::FN => Token::FN,
                tagged_token::CompositeToken::LET => Token::LET,
                tagged_token::CompositeToken::CONST => Token::CONST,
                tagged_token::CompositeToken::NULL => Token::NULL,
            },
        }
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
    ) -> Result<TaggedToken, LexerError> {
        let new_token = TaggedToken::check_termination(intermediate_token, &self.peek());
        match new_token {
            IntermediateToken::FinishExclusive(composite_token) => {
                Ok(TaggedToken::from(composite_token))
            }
            IntermediateToken::FinishInclusive(composite_token) => {
                self.advance();
                Ok(TaggedToken::from(composite_token))
            }
            IntermediateToken::Continue(_) => {
                self.advance();
                let final_token = self.build_composite_token(new_token)?;
                Ok(TaggedToken::from(final_token))
            }
        }
    }

    fn get_token(&mut self) -> Result<Option<TaggedToken>, LexerError> {
        match TaggedToken::check_single(&self.curr_character) {
            Some(SingleOrComposable::Single(single_token)) => {
                Ok(Some(TaggedToken::from(single_token)))
            }
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
        let token = lexer.get_token()?.map(Token::from);
        if let Some(token) = token {
            tokens.push(token);
        }
        lexer.advance();
    }
    Ok(tokens)
}
