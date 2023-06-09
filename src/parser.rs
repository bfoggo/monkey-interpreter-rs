use crate::errors::{ExpressionError, LetStatementError, ParserError};
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

trait SelfParsableExpressionTrait {
    fn parse(parser: &mut Parser) -> Result<Expression, ExpressionError>;
}

trait PrerequisiteParsableExpressionTrait {
    fn precedence(token: &Token) -> Option<u8>;
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError>;
}

enum SelfParsableExpression {
    Literal(LiteralParser),
    Prefix(PrefixParser),
    Grouped(GroupedParser),
}

struct LiteralParser;
impl SelfParsableExpressionTrait for LiteralParser {
    fn parse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let literal_expression = LiteralExpression { token };
        Ok(Expression::Literal(literal_expression))
    }
}

struct PrefixParser;
impl SelfParsableExpressionTrait for PrefixParser {
    fn parse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let right = parser.parse_expression(0)?;
        let prefix_expression = PrefixExpression {
            token,
            right: Box::new(right),
        };
        Ok(Expression::Prefix(prefix_expression))
    }
}

struct GroupedParser;
impl SelfParsableExpressionTrait for GroupedParser {
    fn parse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let expression = parser.parse_expression(0)?;
        let grouped_expression = GroupedExpression {
            expression: Box::new(expression),
        };
        Ok(Expression::Grouped(grouped_expression))
    }
}

impl PrerequisiteParsableExpressionTrait for GroupedParser {
    fn precedence(token: &Token) -> Option<u8> {
        match token {
            Token::LPAREN => Some(0),
            _ => None,
        }
    }
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let right = parser.parse_expression(0)?;
        let infix_expression = InfixExpression {
            left: Box::new(left),
            token,
            right: Box::new(right),
        };
        Ok(Expression::InfixExpression(infix_expression))
    }
}

fn map_token_to_self_parsable_expression(token: &Token) -> Option<SelfParsableExpression> {
    match token {
        Token::TRUE | Token::FALSE | Token::NUMBER(_) | Token::IDENT(_) => {
            Some(SelfParsableExpression::Literal(LiteralParser))
        }
        Token::NOT | Token::MINUS => Some(SelfParsableExpression::Prefix(PrefixParser)),
        Token::LPAREN => Some(SelfParsableExpression::Grouped(GroupedParser)),
        _ => None,
    }
}

impl SelfParsableExpression {
    fn parse(&self, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        match self {
            SelfParsableExpression::Literal(_) => LiteralParser::parse(parser),
            SelfParsableExpression::Prefix(expr) => PrefixParser::parse(parser),
            SelfParsableExpression::Grouped(expr) => GroupedParser::parse(parser),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum PrerequisiteParsableExpression {
    Infix(InfixExpression),
}

#[derive(Debug, PartialEq, Clone)]
enum Expression {
    Literal(LiteralExpression),
    Prefix(PrefixExpression),
    Grouped(GroupedExpression),
    InfixExpression(InfixExpression),
}

#[derive(Debug, PartialEq, Clone)]
struct LiteralExpression {
    token: Token,
}

#[derive(Debug, PartialEq, Clone)]
struct PrefixExpression {
    token: Token,
    right: Box<Option<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
struct GroupedExpression {
    expression: Box<Option<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
struct InfixExpression {
    left: Box<Expression>,
    token: Token,
    right: Box<Option<Expression>>,
}

impl PrerequisiteParsableExpressionTrait for InfixExpression {
    fn precedence(token: &Token) -> Option<u8> {
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
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let right = parser.parse_expression(0)?;
        let infix_expression = InfixExpression {
            left: Box::new(left),
            token,
            right: Box::new(right),
        };
        Ok(Expression::InfixExpression(infix_expression))
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
        if let Some(expr) = map_token_to_self_parsable_expression(&self.curr_token.unwrap()) {
            left = expr.parse(self)?;
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
                if matches!(self.curr_token, Some(Token::RPAREN)) {
                    return Ok(Some(left));
                }
            }
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}
