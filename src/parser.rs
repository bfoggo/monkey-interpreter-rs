use crate::errors::{ExpressionError, LetStatementError, ParserError, UndefinedBehaviorError};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, PartialEq, Clone)]
struct PrefixExpression {
    operator: Token,
    right: Box<Option<Expression>>,
}

fn prefix_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::NUMBER(_) => Some(0),
        Token::IDENT(_) => Some(0),
        Token::NOT => Some(3),
        Token::MINUS => Some(3),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct InfixExpression {
    left: Box<Expression>,
    operator: Token,
    right: Box<Expression>,
}

fn infix_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::EQ => Some(0),
        Token::EQEQ => Some(0),
        Token::PLUS => Some(1),
        Token::MINUS => Some(1),
        Token::ASTERISK => Some(2),
        Token::SLASH => Some(2),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl From<PrefixExpression> for Expression {
    fn from(prefix: PrefixExpression) -> Self {
        Expression::PrefixExpression(prefix)
    }
}
impl From<InfixExpression> for Expression {
    fn from(infix: InfixExpression) -> Self {
        Expression::InfixExpression(infix)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct LetStatement {
    token: Token,
    value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
struct ReturnStatement {
    value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
struct ExpressionStatement {
    expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    statements: Vec<Statement>,
}
struct Parser<'a> {
    tokens: Peekable<IntoIter<Token>>,
    curr_token: Option<&'a Token>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token>) -> Parser<'a> {
        Parser {
            tokens: tokens.into_iter().peekable(),
            curr_token: None,
        }
    }

    fn advance(&mut self) {
        self.curr_token = self.tokens.next().as_ref();
    }

    fn parse(&mut self) -> Result<Program, ParserError> {
        let mut program = Program {
            statements: Vec::new(),
        };
        while let Some(token) = self.tokens.peek() {
            match token {
                Token::LET => {
                    let statement = self.parse_let_statement()?;
                    program.statements.push(Statement::Let(statement));
                }
                Token::RETURN => {
                    let statement = self.parse_return_statement()?;
                    program.statements.push(Statement::Return(statement));
                }
                Token::NEWLINE | Token::SEMICOLON => {
                    self.tokens.next().unwrap();
                }
                _ => {
                    let statement = self.parse_expression_statement()?;
                    program.statements.push(Statement::Expression(statement));
                }
            }
        }
        Ok(program)
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, LetStatementError> {
        self.advance();
        let next_token = self.tokens.peek().unwrap().clone();
        if !matches!(next_token, Token::IDENT(_)) {
            return Err(LetStatementError::NoIdentifier);
        } else {
            self.advance();
        }
        let check_eq_sign = self.tokens.peek().unwrap();
        if !matches!(check_eq_sign, Token::EQ) {
            return Err(LetStatementError::NoEqualSign);
        } else {
            self.advance();
        }
        let value = self.parse_expression()?;
        if value.is_none() {
            return Err(LetStatementError::NoValue);
        }
        Ok(LetStatement {
            token: next_token,
            value: value.unwrap(),
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ExpressionError> {
        self.advance();
        let value = self.parse_expression()?;
        if value.is_none() {
            return Err(ExpressionError::InvalidExpression);
        }
        Ok(ReturnStatement {
            value: value.unwrap(),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ExpressionError> {
        self.advance();
        let expression = self.parse_expression()?;
        if expression.is_none() {
            return Err(ExpressionError::InvalidExpression);
        }
        Ok(ExpressionStatement {
            expression: expression.unwrap(),
        })
    }

    fn parse_expression(&mut self) -> Result<Option<Expression>, ExpressionError> {
        self.advance();
        let left: Expression;
        if let Some(precedence) = prefix_precedence(self.curr_token.unwrap()) {
            let left = Expression::from(self.parse_prefix_expression()?);
        } else {
            return Err(ExpressionError::InvalidExpression);
        }
        while let Some(token) = self.tokens.peek() {
            if let Some(precedence) = infix_precedence(token) {
                let operator = self.tokens.next().unwrap();
                let right = self.parse_expression()?;
                left = Expression::from(InfixExpression {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                });
            } else {
                return Ok(None);
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, ExpressionError> {
        let operator = self.tokens.next().unwrap();
        let right = self.parse_expression()?;
        Ok(PrefixExpression {
            operator,
            right: Box::new(right),
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
