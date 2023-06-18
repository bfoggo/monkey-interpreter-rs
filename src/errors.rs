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
    #[error("If statements need a 'then' keyword")]
    NoThen,
    #[error("If statements need a consequence")]
    NoConsequence,
    #[error("If statements need an else")]
    NoElse,
    #[error("If statements need an alternative")]
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

#[derive(Debug, Error)]
pub enum FnStatementError {
    #[error("Fn statements need a name")]
    NoName,
    #[error("Fn statements need parameters")]
    NoParameters,
    #[error("Fn statements need a body")]
    NoBody,
    #[error("Syntax error: {0}")]
    SyntaxError(String),
    #[error("Invalid Expression: {0}")]
    InvalidExpression(ExpressionError),
}

impl From<ExpressionError> for FnStatementError {
    fn from(error: ExpressionError) -> Self {
        FnStatementError::InvalidExpression(error)
    }
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
    #[error("Expected a return statement")]
    ExpectedReturnStatementError,
    #[error("Return statement error: {0}")]
    ReturnStatementError(ReturnStatementError),
    #[error("Fn statement error: {0}")]
    FnStatementError(FnStatementError),
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

impl From<FnStatementError> for ParserError {
    fn from(error: FnStatementError) -> Self {
        ParserError::FnStatementError(error)
    }
}
