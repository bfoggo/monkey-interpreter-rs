use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {}

#[derive(Debug, Error)]
pub enum ExpressionError {
    #[error("Invalid expression at token: {0:?}")]
    InvalidExpression(String),
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

#[derive(Debug, Error)]
pub enum IfStatementError {
    #[error("If statements need a condition")]
    NoCondition,
    #[error("If statements need a consequence")]
    NoConsequence,
    #[error("If statements need an alternative if ELSE is used")]
    NoAlternative,
    #[error("Invalid Expression: {0}")]
    InvalidExpression(ExpressionError),
}

#[derive(Debug, Error)]
pub enum ReturnStatementError {
    #[error("Return statements need a value")]
    NoValue,
    #[error("Invalid Expression: {0}")]
    InvalidExpression(ExpressionError),
}

impl From<ExpressionError> for ReturnStatementError {
    fn from(error: ExpressionError) -> Self {
        ReturnStatementError::InvalidExpression(error)
    }
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
    #[error("If statement error: {0}")]
    IfStatementError(IfStatementError),
    #[error("Return statement error: {0}")]
    ReturnStatementError(ReturnStatementError),
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

impl From<IfStatementError> for ParserError {
    fn from(error: IfStatementError) -> Self {
        ParserError::IfStatementError(error)
    }
}

impl From<ExpressionError> for IfStatementError {
    fn from(error: ExpressionError) -> Self {
        IfStatementError::InvalidExpression(error)
    }
}

impl From<ReturnStatementError> for ParserError {
    fn from(error: ReturnStatementError) -> Self {
        ParserError::ReturnStatementError(error)
    }
}
