use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Invalid token: {0}")]
    InvalidToken(char),
}
