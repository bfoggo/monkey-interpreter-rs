pub mod expressions;

use expressions::{
    map_token_to_left_parsable_expression, map_token_to_parsable_expression, Expression,
};

use crate::errors::{ExpressionError, LetStatementError, ParserError};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

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
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    curr_token: Option<Token>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
            curr_token: None,
        }
    }

    fn advance(&mut self) {
        self.curr_token = self.tokens.next();
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
        let value = self.parse_expression(0)?;
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
        let value = self.parse_expression(0)?;
        if value.is_none() {
            return Err(ExpressionError::InvalidExpression(None));
        }
        Ok(ReturnStatement {
            value: value.unwrap(),
        })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ExpressionError> {
        let expression = self.parse_expression(0)?;
        if expression.is_none() {
            return Err(ExpressionError::InvalidExpression(None));
        }
        Ok(ExpressionStatement {
            expression: expression.unwrap(),
        })
    }

    fn parse_expression(&mut self, precedence: u8) -> Result<Option<Expression>, ExpressionError> {
        self.advance();
        let mut left: Expression;
        let token = self.curr_token.clone();
        if token.is_none() {
            return Ok(None);
        }
        if let Some(expr) = map_token_to_left_parsable_expression(&token.unwrap()) {
            left = expr.lparse(self)?;
        } else {
            return Ok(None);
        }
        loop {
            if matches!(
                self.tokens.peek(),
                Some(Token::SEMICOLON) | Some(Token::NEWLINE) | Some(Token::RPAREN) | None
            ) {
                self.advance();
                return Ok(Some(left));
            }
            let next_precedence: u8;
            let next_expr;
            if let Some(expr) = map_token_to_parsable_expression(&self.tokens.peek().unwrap()) {
                next_precedence = expr.precedence(&self.tokens.peek().unwrap()).unwrap();
                next_expr = expr;
            } else {
                return Ok(Some(left));
            }
            if precedence >= next_precedence {
                return Ok(Some(left));
            } else {
                self.advance();
                left = next_expr.parse(left, self)?;
            }
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
