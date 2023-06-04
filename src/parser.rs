use crate::errors::{ExpressionError, LetStatementError, ParserError, UndefinedBehaviorError};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, PartialEq, Clone)]
struct Identifier(String);

#[derive(Debug, PartialEq, Clone)]
struct LiteralExpression(String);

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Literal(LiteralExpression),
}

#[derive(Debug, PartialEq, Clone)]
struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
enum Statement {
    Let(LetStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    statements: Vec<Statement>,
}
struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
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
                _ => {}
            }
        }
        Ok(program)
    }

    fn parse_identifier(&mut self) -> Result<Identifier, UndefinedBehaviorError> {
        let token = self.tokens.next().unwrap();
        match token {
            Token::IDENT(identifier) => Ok(Identifier(identifier)),
            _ => {
                return Err(UndefinedBehaviorError::InvalidParse(
                    token,
                    "identifier".to_string(),
                ))
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, LetStatementError> {
        let token = self.tokens.next().unwrap();
        let next_token = self.tokens.peek().unwrap();
        let identifier;
        match next_token {
            Token::IDENT(_) => {
                identifier = self.parse_identifier()?;
            }
            _ => return Err(LetStatementError::NoIdentifier),
        }
        let check_eq_sign = self.tokens.peek().unwrap();
        if *check_eq_sign != Token::EQ {
            return Err(LetStatementError::NoEqualSign);
        } else {
            self.tokens.next().unwrap();
        }
        let value = self.parse_expression()?;
        Ok(LetStatement {
            token,
            name: identifier,
            value,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ExpressionError> {
        let token = self.tokens.next().unwrap();
        match token {
            Token::NUMBER(num) => Ok(Expression::Literal(LiteralExpression(num))),
            _ => Err(ExpressionError::InvalidExpression),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
