use crate::lexer::Token;
use crate::parser::{
    expressions::{Expression, InfixExpression, LiteralExpression, PrefixExpression},
    ExpressionStatement, Program, Statement, AST,
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
    Null(Null),
}

impl Display for ObjectImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectImpl::Integer(integer) => write!(f, "{}", integer.inspect()),
            ObjectImpl::Boolean(boolean) => write!(f, "{}", boolean.inspect()),
            ObjectImpl::Null(_) => write!(f, "null"),
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

#[derive(Clone, PartialEq, Debug)]
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
        AST::Program(Program { statements }) => {
            let mut values = Vec::new();
            for statement in statements {
                values.push(eval(AST::Statement(statement)));
            }
            values.pop().unwrap()
        }
        AST::Statement(statement) => match statement {
            Statement::Expression(ExpressionStatement { expression }) => {
                eval(AST::Expression(expression))
            }
            _ => ObjectImpl::Null(Null),
        },
        AST::Expression(expr) => match expr {
            Expression::Literal(LiteralExpression { token }) => match token {
                Token::NUMBER(value) => ObjectImpl::Integer(Integer {
                    value: value.parse::<i64>().unwrap(),
                }),
                Token::TRUE => ObjectImpl::Boolean(Boolean { value: true }),
                Token::FALSE => ObjectImpl::Boolean(Boolean { value: false }),
                _ => ObjectImpl::Null(Null),
            },
            Expression::Prefix(PrefixExpression { token, right }) => {
                let right_object = eval(AST::Expression(right.unwrap()));
                eval_prefix_expression(&token, right_object)
            }
            Expression::InfixExpression(InfixExpression { token, left, right }) => {
                let left_obj = eval(AST::Expression(*left));
                let mut right_obj: ObjectImpl;
                if right.is_none() {
                    right_obj = ObjectImpl::Null(Null);
                } else {
                    right_obj = eval(AST::Expression(right.unwrap()));
                }
                eval_infix_expression(left_obj, &token, right_obj)
            }
            _ => ObjectImpl::Null(Null),
        },
        _ => ObjectImpl::Null(Null),
    }
}

fn eval_prefix_expression(operator: &Token, right: ObjectImpl) -> ObjectImpl {
    match operator {
        Token::NOT => match right {
            ObjectImpl::Boolean(boolean) => ObjectImpl::Boolean(Boolean {
                value: !boolean.value,
            }),
            ObjectImpl::Integer(_) => ObjectImpl::Boolean(Boolean { value: false }),
            _ => ObjectImpl::Null(Null),
        },
        Token::MINUS => match right {
            ObjectImpl::Integer(integer) => ObjectImpl::Integer(Integer {
                value: -integer.value,
            }),
            ObjectImpl::Boolean(_) => ObjectImpl::Boolean(Boolean { value: false }),
            _ => ObjectImpl::Null(Null),
        },
        _ => ObjectImpl::Null(Null),
    }
}

fn eval_infix_expression(left: ObjectImpl, operator: &Token, right: ObjectImpl) -> ObjectImpl {
    match operator {
        Token::PLUS => add(left, right),
        Token::ASTERISK => multiply(left, right),
        Token::MINUS => subtract(left, right),
        Token::SLASH => divide(left, right),
        _ => ObjectImpl::Null(Null),
    }
}

fn add(left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
    match (left, right) {
        (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => ObjectImpl::Integer(Integer {
            value: left.value + right.value,
        }),
        _ => ObjectImpl::Null(Null),
    }
}

fn multiply(left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
    match (left, right) {
        (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => ObjectImpl::Integer(Integer {
            value: left.value * right.value,
        }),
        _ => ObjectImpl::Null(Null),
    }
}

fn subtract(left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
    match (left, right) {
        (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => ObjectImpl::Integer(Integer {
            value: left.value - right.value,
        }),
        _ => ObjectImpl::Null(Null),
    }
}

fn divide(left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
    match (left, right) {
        (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => ObjectImpl::Integer(Integer {
            value: left.value / right.value,
        }),
        _ => ObjectImpl::Null(Null),
    }
}
