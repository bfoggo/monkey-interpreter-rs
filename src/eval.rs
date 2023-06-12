use crate::lexer::Token;
use crate::parser::{
    expressions::{Expression, LiteralExpression},
    AST,
};
use std::fmt::Display;

type ObjectType = &'static str;
trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectImpl {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

impl Display for ObjectImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectImpl::Integer(integer) => write!(f, "{}", integer.inspect()),
            ObjectImpl::Boolean(boolean) => write!(f, "{}", boolean.inspect()),
            ObjectImpl::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        "INTEGER"
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        "BOOLEAN"
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        "NULL"
    }
    fn inspect(&self) -> String {
        format!("null")
    }
}

pub fn eval(ast_node: AST) -> ObjectImpl {
    match ast_node {
        AST::Expression(expr) => match expr {
            Expression::Literal(LiteralExpression { token }) => match token {
                Token::NUMBER(value) => ObjectImpl::Integer(Integer {
                    value: value.parse::<i64>().unwrap(),
                }),
                Token::TRUE => ObjectImpl::Boolean(Boolean { value: true }),
                Token::FALSE => ObjectImpl::Boolean(Boolean { value: false }),
                _ => ObjectImpl::Null,
            },
            _ => ObjectImpl::Null,
        },
        _ => ObjectImpl::Null,
    }
}
