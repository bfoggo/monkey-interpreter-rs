use crate::lexer::Token;
use crate::parser::{
    expressions::{Expression, InfixExpression, LiteralExpression, PrefixExpression},
    ExpressionStatement, Program, Statement, AST,
};
use crate::parser::{BlockStatments, FnStatement, IfStatement, LetStatement};
use std::collections::HashMap;
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
    Return(ReturnedObj),
    Null(Null),
}

impl Display for ObjectImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectImpl::Integer(integer) => write!(f, "{}", integer.inspect()),
            ObjectImpl::Boolean(boolean) => write!(f, "{}", boolean.inspect()),
            ObjectImpl::Return(rval) => write!(f, "{}", rval.inspect()),
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
pub struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        "NULL"
    }
    fn inspect(&self) -> String {
        format!("null")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnedObj {
    pub value: Box<ObjectImpl>,
}

impl Object for ReturnedObj {
    fn object_type(&self) -> ObjectType {
        "RETURN_VALUE"
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnObj {
    pub parameters: Vec<LiteralExpression>,
    pub body: BlockStatments,
}

impl Object for FnObj {
    fn object_type(&self) -> ObjectType {
        "FUNCTION"
    }
    fn inspect(&self) -> String {
        let mut params = Vec::new();
        for param in &self.parameters {
            params.push(param.clone());
        }
        format!(
            "fn({})",
            params
                .into_iter()
                .map(|p| p.literal().unwrap())
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, ObjectImpl>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn eval(&mut self, ast_node: AST) -> ObjectImpl {
        match ast_node {
            AST::Program(Program { statements }) => {
                let mut values = Vec::new();
                for statement in statements {
                    values.push(self.eval(AST::Statement(statement)));
                }
                values.pop().unwrap()
            }
            AST::Statement(Statement::Expression(ExpressionStatement { expression })) => {
                self.eval(AST::Expression(expression))
            }
            AST::Statement(Statement::Return(return_statement)) => {
                let return_value = self.eval(AST::Expression(return_statement.value));
                ObjectImpl::Return(ReturnedObj {
                    value: Box::new(return_value),
                })
            }
            AST::Statement(Statement::Block(BlockStatments { statements })) => {
                let mut values: Vec<ObjectImpl> = Vec::new();
                for statement in statements {
                    let blocked_obj = self.eval(AST::Statement(statement));
                    if matches!(blocked_obj, ObjectImpl::Return(_)) {
                        return blocked_obj;
                    }
                    values.push(blocked_obj);
                }
                return ObjectImpl::Null(Null);
            }
            AST::Statement(Statement::Let(let_statement)) => self.eval_let_statement(let_statement),
            AST::Statement(Statement::If(if_statement)) => self.eval_if_statement(if_statement),
            AST::Statement(Statement::FnStatement(fn_statement)) => {
                self.eval_fn_statement(fn_statement)
            }
            AST::Expression(expr) => match expr {
                Expression::Literal(LiteralExpression { token }) => match token {
                    Token::NUMBER(value) => ObjectImpl::Integer(Integer {
                        value: value.parse::<i64>().unwrap(),
                    }),
                    Token::TRUE => ObjectImpl::Boolean(Boolean { value: true }),
                    Token::FALSE => ObjectImpl::Boolean(Boolean { value: false }),
                    Token::IDENT(ident) => match self.store.get(&ident) {
                        Some(obj) => obj.clone(),
                        None => ObjectImpl::Null(Null),
                    },
                    _ => ObjectImpl::Null(Null),
                },
                Expression::Grouped(grouped_expression) => {
                    let expr = *grouped_expression.expression;
                    match expr {
                        Some(somexpr) => self.eval(AST::Expression(somexpr)),
                        None => ObjectImpl::Null(Null),
                    }
                }
                Expression::Prefix(PrefixExpression { token, right }) => {
                    let right_object = self.eval(AST::Expression(right.unwrap()));
                    self.eval_prefix_expression(&token, right_object)
                }
                Expression::InfixExpression(InfixExpression { token, left, right }) => {
                    let left_obj = self.eval(AST::Expression(*left));
                    let right_obj: ObjectImpl;
                    if right.is_none() {
                        right_obj = ObjectImpl::Null(Null);
                    } else {
                        right_obj = self.eval(AST::Expression(right.unwrap()));
                    }
                    self.eval_infix_expression(left_obj, &token, right_obj)
                }
                _ => ObjectImpl::Null(Null),
            },
            _ => ObjectImpl::Null(Null),
        }
    }

    fn eval_prefix_expression(&self, operator: &Token, right: ObjectImpl) -> ObjectImpl {
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

    fn eval_infix_expression(
        &self,
        left: ObjectImpl,
        operator: &Token,
        right: ObjectImpl,
    ) -> ObjectImpl {
        match operator {
            Token::PLUS => self.add(left, right),
            Token::ASTERISK => self.multiply(left, right),
            Token::MINUS => self.subtract(left, right),
            Token::SLASH => self.divide(left, right),
            Token::GT => self.gt(left, right),
            Token::LT => self.lt(left, right),
            Token::GTEQ => self.gte(left, right),
            Token::LTEQ => self.lte(left, right),
            Token::EQEQ => self.eqeq(left, right),
            Token::NOTEQ => self.neq(left, right),
            _ => ObjectImpl::Null(Null),
        }
    }

    fn add(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Integer(Integer {
                    value: left.value + right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn multiply(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Integer(Integer {
                    value: left.value * right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn subtract(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Integer(Integer {
                    value: left.value - right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn divide(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Integer(Integer {
                    value: left.value / right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn gt(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value > right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn gte(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value >= right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn lt(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value < right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn lte(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value <= right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn eqeq(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value == right.value,
                })
            }
            (ObjectImpl::Boolean(left), ObjectImpl::Boolean(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value == right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn neq(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (left, right) {
            (ObjectImpl::Integer(left), ObjectImpl::Integer(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value != right.value,
                })
            }
            (ObjectImpl::Boolean(left), ObjectImpl::Boolean(right)) => {
                ObjectImpl::Boolean(Boolean {
                    value: left.value != right.value,
                })
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn eval_if_statement(&mut self, if_statement: IfStatement) -> ObjectImpl {
        let condition_eval = self.eval(AST::Expression(if_statement.condition));
        println!("{:?}", condition_eval);
        match condition_eval {
            ObjectImpl::Boolean(Boolean { value: true }) => {
                self.eval(AST::Expression(if_statement.consequence))
            }
            ObjectImpl::Boolean(Boolean { value: false }) => {
                self.eval(AST::Expression(if_statement.alternative))
            }
            _ => ObjectImpl::Null(Null),
        }
    }

    fn eval_let_statement(&mut self, let_statement: LetStatement) -> ObjectImpl {
        let key;
        match let_statement.token {
            Token::IDENT(identifier) => key = identifier,
            _ => return ObjectImpl::Null(Null),
        }
        let value = self.eval(AST::Expression(let_statement.value));
        self.store.insert(key, value.clone());
        value
    }

    fn eval_fn_statment(&mut self, fn_statement: FnStatement) -> ObjectImpl {
        return FnObj {
            parameters: fn_statement.parameters,
            body: fn_statement.body,
        };
    }
}
