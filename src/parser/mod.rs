pub mod expressions;

use expressions::{
    map_token_to_left_parsable_expression, map_token_to_parsable_expression, Expression,
};

use crate::errors::{
    ExpressionError, FnStatementError, IfStatementError, LetStatementError, ParserError,
    ReturnStatementError,
};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    token: Token,
    value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    condition: Expression,
    consequence: Expression,
    alternative: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnStatement {
    name: Token,
    parameters: Vec<Token>,
    body: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    Fn(FnStatement),
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
                Token::IF => {
                    let statement = self.parse_if_statement()?;
                    program.statements.push(Statement::If(statement));
                }
                Token::FN => {
                    let statement = self.parse_fn_statement()?;
                    program.statements.push(Statement::Fn(statement));
                }
                _ => {
                    let statement = self.parse_expression_statement()?;
                    if statement.is_none() {
                        continue;
                    }
                    program
                        .statements
                        .push(Statement::Expression(statement.unwrap()));
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

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ReturnStatementError> {
        self.advance();
        let value = self.parse_expression(0)?;
        if value.is_none() {
            return Err(ReturnStatementError::NoValue);
        }
        Ok(ReturnStatement {
            value: value.unwrap(),
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, IfStatementError> {
        self.advance();
        let condition = self.parse_expression(0)?;
        if !matches!(condition, Some(Expression::BracketedExpression(_))) {
            return Err(IfStatementError::NoCondition);
        }
        let consequence = self.parse_expression(0)?;
        if consequence.is_none() {
            return Err(IfStatementError::NoConsequence);
        }
        let mut alternative = None;
        if matches!(self.tokens.peek(), Some(Token::ELSE)) {
            self.advance();
            alternative = self.parse_expression(0)?;
            if alternative.is_none() {
                return Err(IfStatementError::NoAlternative);
            }
        }
        Ok(IfStatement {
            condition: condition.unwrap(),
            consequence: consequence.unwrap(),
            alternative,
        })
    }

    fn parse_fn_statement(&mut self) -> Result<FnStatement, FnStatementError> {
        self.advance();
        if !matches!(self.tokens.peek(), Some(Token::IDENT(_))) {
            return Err(FnStatementError::NoName);
        }
        self.advance();
        let name = self.curr_token.clone().unwrap();
        if !matches!(self.tokens.peek(), Some(Token::LPAREN)) {
            return Err(FnStatementError::NoParameters);
        }
        self.advance();
        let mut parameters: Vec<Token> = Vec::new();
        loop {
            if matches!(self.tokens.peek(), Some(Token::IDENT(_))) {
                self.advance();
                parameters.push(self.curr_token.clone().unwrap());
            }
            if matches!(self.tokens.peek(), Some(Token::RPAREN)) {
                self.advance();
                break;
            }
            if !matches!(self.tokens.peek(), Some(Token::COMMA)) {
                return Err(FnStatementError::SyntaxError(
                    "Function arguments must be separated by a comma".to_string(),
                ));
            }
            self.advance();
        }
        if parameters.is_empty() {
            return Err(FnStatementError::NoParameters);
        }
        let body = self.parse_expression(0)?;
        if body.is_none() || !matches!(&body, Some(Expression::BracketedExpression(_))) {
            return Err(FnStatementError::NoBody);
        }
        Ok(FnStatement {
            name,
            parameters,
            body: body.unwrap(),
        })
    }

    fn parse_expression_statement(
        &mut self,
    ) -> Result<Option<ExpressionStatement>, ExpressionError> {
        let expression = self.parse_expression(0)?;
        if expression.is_none() {
            return Ok(None);
        }
        Ok(Some(ExpressionStatement {
            expression: expression.unwrap(),
        }))
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

pub enum AST {
    Statement(Statement),
    Expression(Expression),
}
