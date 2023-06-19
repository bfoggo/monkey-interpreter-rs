use crate::lexer::Token;
use crate::parser::expressions::CallExpression;
use crate::parser::{
    expressions::{Expression, InfixExpression, LiteralExpression, PrefixExpression},
    ExpressionStatement, Program, Statement, AST,
};
use crate::parser::{BlockStatments, FnStatement, IfStatement, LetStatement};
use std::collections::HashMap;
use std::fmt::Display;

type ObjectType = &'static str;
pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectImpl {
    Integer(Integer),
    Boolean(Boolean),
    Tuple(Tuple),
    Return(ReturnedObj),
    Fn(FnObj),
    Null(Null),
}

impl Display for ObjectImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectImpl::Integer(integer) => write!(f, "{}", integer.inspect()),
            ObjectImpl::Boolean(boolean) => write!(f, "{}", boolean.inspect()),
            ObjectImpl::Return(rval) => write!(f, "{}", rval.inspect()),
            ObjectImpl::Tuple(tuple) => write!(f, "{}", tuple.inspect()),
            ObjectImpl::Fn(func) => write!(f, "{}", func.inspect()),
            ObjectImpl::Null(_) => write!(f, "null"),
        }
    }
}

impl ObjectImpl {
    fn inspect(&self) -> String {
        match self {
            ObjectImpl::Integer(integer) => integer.inspect(),
            ObjectImpl::Boolean(boolean) => boolean.inspect(),
            ObjectImpl::Return(rval) => rval.inspect(),
            ObjectImpl::Tuple(tuple) => tuple.inspect(),
            ObjectImpl::Fn(func) => func.inspect(),
            ObjectImpl::Null(_) => "null".to_string(),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Tuple {
    pub elements: Vec<ObjectImpl>,
}

impl Object for Tuple {
    fn object_type(&self) -> ObjectType {
        "TUPLE"
    }
    fn inspect(&self) -> String {
        format!(
            "({})",
            self.elements
                .iter()
                .map(|e| e.inspect())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, ObjectImpl>,
    local_store: HashMap<String, ObjectImpl>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            local_store: HashMap::new(),
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
            AST::Statement(Statement::Fn(fn_statement)) => self.eval_fn_statement(fn_statement),
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
                Expression::CallExpression(call_expression) => {
                    self.eval_call_expression(call_expression)
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
            Token::COMMA => self.comma(left, right),
            _ => ObjectImpl::Null(Null),
        }
    }

    fn comma(&self, left: ObjectImpl, right: ObjectImpl) -> ObjectImpl {
        match (&left, &right) {
            (
                ObjectImpl::Integer(_) | ObjectImpl::Boolean(_),
                ObjectImpl::Integer(_) | ObjectImpl::Boolean(_),
            ) => ObjectImpl::Tuple(Tuple {
                elements: vec![left, right],
            }),
            (ObjectImpl::Tuple(left_tuple), ObjectImpl::Integer(_) | ObjectImpl::Boolean(_)) => {
                let mut tuple_ele_clone = left_tuple.elements.clone();
                tuple_ele_clone.push(right.clone());
                ObjectImpl::Tuple(Tuple {
                    elements: tuple_ele_clone,
                })
            }
            (ObjectImpl::Integer(_) | ObjectImpl::Boolean(_), ObjectImpl::Tuple(right_tuple)) => {
                let mut tuple_ele_clone = right_tuple.elements.clone();
                tuple_ele_clone.insert(0, left.clone());
                ObjectImpl::Tuple(Tuple {
                    elements: tuple_ele_clone,
                })
            }
            (ObjectImpl::Tuple(left_tuple), ObjectImpl::Tuple(right_tuple)) => {
                let mut tuple_ele_clone = left_tuple.elements.clone();
                tuple_ele_clone.extend(right_tuple.elements.clone());
                ObjectImpl::Tuple(Tuple {
                    elements: tuple_ele_clone,
                })
            }
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

    fn eval_fn_statement(&mut self, fn_statement: FnStatement) -> ObjectImpl {
        let fn_name = match fn_statement.name {
            Token::IDENT(identifier) => identifier,
            _ => return ObjectImpl::Null(Null),
        };
        let mut literal_expressions: Vec<LiteralExpression> = vec![];
        for token in fn_statement.parameters {
            match token {
                Token::IDENT(identifier) => literal_expressions.push(LiteralExpression {
                    token: Token::IDENT(identifier),
                }),
                _ => return ObjectImpl::Null(Null),
            }
        }
        self.store.insert(
            fn_name.clone(),
            ObjectImpl::Fn(FnObj {
                parameters: literal_expressions.clone(),
                body: fn_statement.body.clone(),
            }),
        );

        ObjectImpl::Fn(FnObj {
            parameters: literal_expressions,
            body: fn_statement.body.clone(),
        })
    }

    fn eval_call_expression(&mut self, call_expression: CallExpression) -> ObjectImpl {
        let fn_name = match *call_expression.function {
            Expression::Literal(litexpr) => litexpr.literal(),
            _ => return ObjectImpl::Null(Null),
        };
        let fn_obj;
        match self.store.get(&fn_name.unwrap()) {
            Some(ObjectImpl::Fn(f)) => fn_obj = f,
            _ => return ObjectImpl::Null(Null),
        }
        self.local_store.clear();
        let param_names = fn_obj.parameters.clone();
        let param_expr = *call_expression.arguments.clone();
        if param_expr.is_none() {
            return ObjectImpl::Null(Null);
        }
        let param_vals = self.eval(AST::Expression(param_expr.unwrap()));
        return param_vals;
    }
}
