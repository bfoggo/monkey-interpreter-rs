use crate::lexer::Token;
use crate::parser::{
    expressions::{Expression, LiteralExpression},
    AST,
};

type ObjectType = &'static str;
trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}
pub enum ObjectImpl {
    Integer(Integer),
    Boolean(Boolean),
    Null,
}

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
