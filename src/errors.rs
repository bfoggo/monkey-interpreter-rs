use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {}

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

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Invalid token")]
    InvalidToken,
    #[error("Let statement error: {0}")]
    LetStatementError(LetStatementError),
}
