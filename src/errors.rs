use crate::lexer::Token;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {}

#[derive(Debug, Error)]
pub enum ExpressionError {
    #[error("Invalid expression at token: {0:?}")]
    InvalidExpression(Option<Token>),
}

#[derive(Debug, Error)]
pub enum LetStatementError {
    #[error("Let statements need an equal sign")]
    NoEqualSign,
    #[error("Let statements need an identifier")]
    NoIdentifier,
    #[error("Let statements need a value")]
    NoValue,
}

impl From<ExpressionError> for LetStatementError {
    fn from(error: ExpressionError) -> Self {
        match error {
            ExpressionError::InvalidExpression(_) => LetStatementError::NoValue,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Let statement error: {0}")]
    LetStatementError(LetStatementError),
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
