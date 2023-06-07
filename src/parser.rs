use crate::errors::{ExpressionError, LetStatementError, ParserError};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, PartialEq, Clone)]
struct LiteralExpression {
    token: Token,
}

fn literal_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::TRUE => Some(0),
        Token::FALSE => Some(0),
        Token::NUMBER(_) => Some(0),
        Token::IDENT(_) => Some(0),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct GroupedExpression {
    expression: Box<Option<Expression>>,
}

fn grouped_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::LPAREN => Some(0),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct PrefixExpression {
    token: Token,
    right: Box<Option<Expression>>,
}

impl From<LiteralExpression> for PrefixExpression {
    fn from(literal: LiteralExpression) -> Self {
        PrefixExpression {
            token: literal.token,
            right: Box::new(None),
        }
    }
}

fn prefix_precedence(token: &Token) -> Option<u8> {
    match token {
        _ if literal_precedence(token).is_some() => Some(0),
        Token::NOT => Some(3),
        Token::MINUS => Some(3),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Clone)]
struct InfixExpression {
    left: Box<Expression>,
    token: Token,
    right: Box<Option<Expression>>,
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
    GroupedExpression(GroupedExpression),
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
impl From<GroupedExpression> for Expression {
    fn from(grouped: GroupedExpression) -> Self {
        Expression::GroupedExpression(grouped)
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
struct Parser {
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
        if let Some(_) = prefix_precedence(self.curr_token.as_ref().unwrap()) {
            left = Expression::from(self.parse_prefix_expression()?);
        } else if let Some(_) = grouped_precedence(self.curr_token.as_ref().unwrap()) {
            left = Expression::from(self.parse_grouped_expression()?);
        } else {
            return Err(ExpressionError::InvalidExpression(self.curr_token.clone()));
        }
        loop {
            if matches!(
                self.tokens.peek(),
                Some(Token::SEMICOLON) | Some(Token::NEWLINE) | Some(Token::RPAREN) | None
            ) || infix_precedence(self.tokens.peek().unwrap()).is_none()
            {
                self.advance();
                return Ok(Some(left));
            }
            let next_precedance = infix_precedence(self.tokens.peek().unwrap()).unwrap();
            if next_precedance <= precedence {
                return Ok(Some(left));
            } else {
                left = Expression::from(self.parse_infix_expression(left, next_precedance)?);
            }
        }
    }

    fn parse_literal_expression(&mut self) -> Result<LiteralExpression, ExpressionError> {
        Ok(LiteralExpression {
            token: self.curr_token.clone().unwrap(),
        })
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, ExpressionError> {
        if let Some(_) = literal_precedence(self.curr_token.as_ref().unwrap()) {
            return Ok(PrefixExpression::from(self.parse_literal_expression()?));
        }
        let token = self.curr_token.clone().unwrap();
        let right = self.parse_expression(0)?;
        Ok(PrefixExpression {
            token,
            right: Box::new(right),
        })
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression,
        precedence: u8,
    ) -> Result<InfixExpression, ExpressionError> {
        self.advance();
        let token = self.curr_token.clone().unwrap();
        let right = self.parse_expression(precedence)?;
        Ok(InfixExpression {
            left: Box::new(left),
            token,
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<GroupedExpression, ExpressionError> {
        let expression = self.parse_expression(0)?;
        Ok(GroupedExpression {
            expression: Box::new(expression),
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
