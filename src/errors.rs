use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Invalid token: {0}")]
    InvalidToken(String),
    #[error("Invalid sequence token: {0}")]
    InvalidBufferToken(String),
    #[error("Tried to manipulate an empty buffer")]
    EmptyBuffer,
    #[error("Tried to extend the buffer past its source")]
    BufferOverflow,
}
