use crate::lexer::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {}

#[derive(Debug, Error)]
pub enum UndefinedBehaviorError {
    #[error("tried to parse {0} as {1}")]
    InvalidParse(Token, String),
}

#[derive(Debug, Error)]
pub enum ExpressionError {
    #[error("Invalid expression")]
    InvalidExpression,
}

#[derive(Debug, Error)]
pub enum LetStatementError {
    #[error("Let statements need an equal sign")]
    NoEqualSign,
    #[error("Let statements need an identifier")]
    NoIdentifier,
    #[error("Let statements need a semicolon")]
    NoSemicolon,
    #[error("Let statements need a value")]
    NoValue,
}

impl From<UndefinedBehaviorError> for LetStatementError {
    fn from(error: UndefinedBehaviorError) -> Self {
        match error {
            UndefinedBehaviorError::InvalidParse(token, expected) => match token {
                Token::IDENT(_) => LetStatementError::NoIdentifier,
                Token::EQ => LetStatementError::NoEqualSign,
                Token::SEMICOLON => LetStatementError::NoSemicolon,
                _ => LetStatementError::NoValue,
            },
        }
    }
}

impl From<ExpressionError> for LetStatementError {
    fn from(error: ExpressionError) -> Self {
        match error {
            ExpressionError::InvalidExpression => LetStatementError::NoValue,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Invalid token")]
    InvalidToken,
    #[error("Let statement error: {0}")]
    LetStatementError(LetStatementError),
    #[error("Undefined behavior: {0}")]
    UndefinedBehaviorError(UndefinedBehaviorError),
    #[error("Expression error: {0}")]
    ExpressionError(ExpressionError),
}

impl From<LetStatementError> for ParserError {
    fn from(error: LetStatementError) -> Self {
        ParserError::LetStatementError(error)
    }
}

impl From<ExpressionError> for ParserError {
    fn from(error: ExpressionError) -> Self {
        ParserError::ExpressionError(error)
    }
}

impl From<UndefinedBehaviorError> for ParserError {
    fn from(error: UndefinedBehaviorError) -> Self {
        ParserError::UndefinedBehaviorError(error)
    }
}
