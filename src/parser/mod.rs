pub mod expressions;

use expressions::{
    map_token_to_left_parsable_expression, map_token_to_parsable_expression, Expression,
};

use crate::errors::{
    BlockStatementError, ExpressionError, FnStatementError, IfStatementError, LetStatementError,
    ParserError, ReturnStatementError, StatementError,
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
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub consequence: Expression,
    pub alternative: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnStatement {
    name: Token,
    parameters: Vec<Token>,
    body: BlockStatments,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatments {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    If(IfStatement),
    Fn(FnStatement),
    Block(BlockStatments),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
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
        while let Some(token) = self.tokens.peek().cloned() {
            match token {
                Token::NEWLINE | Token::SEMICOLON => {
                    self.tokens.next().unwrap();
                    self.advance();
                }
                _ => {
                    let new_statment = self.parse_statement(&token)?;
                    program.statements.push(new_statment);
                }
            }
        }
        Ok(program)
    }

    fn parse_statement(&mut self, token: &Token) -> Result<Statement, StatementError> {
        match token {
            Token::LET => {
                let statement = self.parse_let_statement()?;
                Ok(Statement::Let(statement))
            }
            Token::RETURN => {
                let statement = self.parse_return_statement()?;
                Ok(Statement::Return(statement))
            }
            Token::IF => {
                let statement = self.parse_if_statement()?;
                Ok(Statement::If(statement))
            }
            Token::FN => {
                let statement = self.parse_fn_statement()?;
                Ok(Statement::Fn(statement))
            }
            Token::LBRACE => {
                let statements = self.parse_block_statements()?;
                Ok(Statement::Block(statements))
            }
            _ => {
                let statement = self.parse_expression_statement()?;
                if statement.is_none() {
                    return Err(StatementError::ExpressionError(
                        ExpressionError::InvalidExpression("No expression".to_string()),
                    ));
                }
                Ok(Statement::Expression(statement.unwrap()))
            }
        }
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
        if !matches!(condition, Some(Expression::Grouped(_))) {
            return Err(IfStatementError::NoCondition);
        }
        self.advance();
        if !matches!(self.curr_token.clone().unwrap(), Token::THEN) {
            return Err(IfStatementError::NoThen);
        }
        let consequence = self.parse_expression(0)?;
        if !matches!(consequence, Some(_)) {
            return Err(IfStatementError::NoConsequence);
        }
        self.advance();
        if !matches!(self.curr_token.clone().unwrap(), Token::ELSE) {
            return Err(IfStatementError::NoElse);
        }
        let alternative = self.parse_expression(0)?;
        if !matches!(alternative, Some(_)) {
            return Err(IfStatementError::NoAlternative);
        }
        Ok(IfStatement {
            condition: condition.unwrap(),
            consequence: consequence.unwrap(),
            alternative: alternative.unwrap(),
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
        if !matches!(self.tokens.peek(), Some(Token::LBRACE)) {
            return Err(FnStatementError::NoBody);
        }
        let body = self.parse_block_statements()?;
        Ok(FnStatement {
            name,
            parameters,
            body,
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

    fn parse_block_statements(&mut self) -> Result<BlockStatments, BlockStatementError> {
        let mut statements: Vec<Statement> = Vec::new();
        self.advance();
        loop {
            let next_token = self.tokens.peek().cloned();
            if matches!(next_token, Some(Token::RBRACE)) {
                self.advance();
                break;
            }
            let statement = self.parse_statement(&next_token.unwrap())?;
            statements.push(statement);
        }
        Ok(BlockStatments { statements })
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
            let next_precedence: u8;
            let next_expr;
            if let Some(expr) = map_token_to_parsable_expression(&self.tokens.peek().unwrap()) {
                next_precedence = expr.precedence(&self.tokens.peek().unwrap()).unwrap();
                next_expr = expr;
            } else {
                return Ok(Some(left));
            }
            if precedence > next_precedence {
                return Ok(Some(left));
            } else if precedence == next_precedence {
                self.advance();
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
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}
