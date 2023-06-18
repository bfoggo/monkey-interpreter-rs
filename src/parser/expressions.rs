use crate::errors::ExpressionError;
use crate::lexer::Token;
use crate::parser::{Parser, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Prefix(PrefixExpression),
    Grouped(GroupedExpression),
    InfixExpression(InfixExpression),
    CallExpression(CallExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LiteralExpression {
    pub token: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: Box<Option<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupedExpression {
    pub expression: Box<Option<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub token: Token,
    pub right: Box<Option<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Box<Option<Expression>>,
}

pub trait LeftParsable {
    fn lparse(parser: &mut Parser) -> Result<Expression, ExpressionError>;
}

pub trait Parsable {
    fn precedence(token: &Token) -> Option<u8>;
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError>;
}

pub enum LeftParsableImpl {
    Literal(LiteralParser),
    Prefix(PrefixParser),
    Grouped(GroupedParser),
}

pub enum ParsableImpl {
    Infix(InfixParser),
    Call(CallParser),
    Terminating(TerminatingParser),
}

pub struct LiteralParser;
impl LeftParsable for LiteralParser {
    fn lparse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let literal_expression = LiteralExpression { token };
        Ok(Expression::Literal(literal_expression))
    }
}

pub struct PrefixParser;
impl LeftParsable for PrefixParser {
    fn lparse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let right = parser.parse_expression(0)?;
        let prefix_expression = PrefixExpression {
            token,
            right: Box::new(right),
        };
        Ok(Expression::Prefix(prefix_expression))
    }
}

#[derive(Default)]
pub struct GroupedParser;
impl LeftParsable for GroupedParser {
    fn lparse(parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let expression = parser.parse_expression(0)?;
        let grouped_expression = GroupedExpression {
            expression: Box::new(expression),
        };
        Ok(Expression::Grouped(grouped_expression))
    }
}

#[derive(Default)]
pub struct InfixParser;
impl Parsable for InfixParser {
    fn precedence(token: &Token) -> Option<u8> {
        match token {
            Token::EQ => Some(2),
            Token::EQEQ => Some(2),
            Token::NOTEQ => Some(2),
            Token::GTEQ => Some(2),
            Token::GT => Some(2),
            Token::LTEQ => Some(2),
            Token::LT => Some(2),
            Token::PLUS => Some(3),
            Token::MINUS => Some(3),
            Token::ASTERISK => Some(4),
            Token::SLASH => Some(4),
            _ => None,
        }
    }
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let token = parser.curr_token.clone().unwrap();
        let right = parser.parse_expression(InfixParser::precedence(&token).unwrap())?;
        let infix_expression = InfixExpression {
            left: Box::new(left),
            token,
            right: Box::new(right),
        };
        Ok(Expression::InfixExpression(infix_expression))
    }
}

#[derive(Default)]
pub struct TerminatingParser;
impl Parsable for TerminatingParser {
    fn precedence(token: &Token) -> Option<u8> {
        match token {
            Token::SEMICOLON => Some(0),
            Token::RPAREN => Some(0),
            _ => None,
        }
    }

    fn parse(left: Expression, _parser: &mut Parser) -> Result<Expression, ExpressionError> {
        Ok(left)
    }
}

pub struct CallParser;

impl Parsable for CallParser {
    fn precedence(token: &Token) -> Option<u8> {
        match token {
            Token::LPAREN => Some(5),
            _ => None,
        }
    }
    fn parse(left: Expression, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        let function = left;
        let arguments = parser.parse_expression(0)?;
        let call_expression = CallExpression {
            function: Box::new(function),
            arguments: Box::new(arguments),
        };
        Ok(Expression::CallExpression(call_expression))
    }
}

impl LeftParsableImpl {
    pub fn lparse(&self, parser: &mut Parser) -> Result<Expression, ExpressionError> {
        match self {
            LeftParsableImpl::Literal(_) => LiteralParser::lparse(parser),
            LeftParsableImpl::Prefix(_) => PrefixParser::lparse(parser),
            LeftParsableImpl::Grouped(_) => GroupedParser::lparse(parser),
        }
    }
}

impl ParsableImpl {
    pub fn precedence(&self, token: &Token) -> Option<u8> {
        match self {
            ParsableImpl::Infix(_) => InfixParser::precedence(token),
            ParsableImpl::Call(_) => CallParser::precedence(token),
            ParsableImpl::Terminating(_) => TerminatingParser::precedence(token),
        }
    }
    pub fn parse(
        &self,
        left: Expression,
        parser: &mut Parser,
    ) -> Result<Expression, ExpressionError> {
        match self {
            ParsableImpl::Infix(_) => InfixParser::parse(left, parser),
            ParsableImpl::Call(_) => CallParser::parse(left, parser),
            ParsableImpl::Terminating(_) => TerminatingParser::parse(left, parser),
        }
    }
}

pub fn map_token_to_left_parsable_expression(token: &Token) -> Option<LeftParsableImpl> {
    match token {
        Token::TRUE | Token::FALSE | Token::NUMBER(_) | Token::IDENT(_) => {
            Some(LeftParsableImpl::Literal(LiteralParser))
        }
        Token::NOT | Token::MINUS => Some(LeftParsableImpl::Prefix(PrefixParser)),
        Token::LPAREN => Some(LeftParsableImpl::Grouped(GroupedParser)),
        _ => None,
    }
}

pub fn map_token_to_parsable_expression(token: &Token) -> Option<ParsableImpl> {
    match token {
        _ if CallParser::precedence(token).is_some() => Some(ParsableImpl::Call(CallParser)),
        _ if InfixParser::precedence(token).is_some() => Some(ParsableImpl::Infix(InfixParser)),
        _ if TerminatingParser::precedence(token).is_some() => {
            Some(ParsableImpl::Terminating(TerminatingParser))
        }
        _ => None,
    }
}
