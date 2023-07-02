use crate::context::Context;
use crate::error;

#[derive(Debug, Clone)]
pub enum BinOp {
    Addition,
    Subtraction,
    Product,
    Division,
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    Constant(f32),
    Variable(String),
}

use std::str::FromStr;
impl FromStr for Expression {
    type Err = error::ParserError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use crate::parser::parse_expression;
        parse_expression(s)
    }
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        match self {
            Expression::Constant(val) => val.to_string(),
            Expression::Variable(var) => var.to_string(),
            Expression::BinOp(op, e1, e2) => {
                let s1 = e1.to_string();
                let s2 = e2.to_string();
                match op {
                    BinOp::Addition => format!("({} + {})", s1, s2),
                    BinOp::Subtraction => format!("({} - {})", s1, s2),
                    BinOp::Product => format!("({} * {})", s1, s2),
                    BinOp::Division => format!("({} / {})", s1, s2),
                }
            }
        }
    }
}

impl Expression {
    /// Creates a new constant from a floating point value.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::constant(2.0);
    /// let eval = expr.eval().unwrap();
    ///
    /// assert_eq!(eval, 2.0);
    /// ```
    pub fn constant(val: f32) -> Self {
        Expression::Constant(val)
    }

    /// Creates a variable.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::context::Context;
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::variable("x");
    /// let ctx = Context::new().with_variable("x", 32.0);
    /// let eval = expr.eval_with_context(&ctx).unwrap();
    ///
    /// assert_eq!(eval, 32.0);
    /// ```
    pub fn variable(name: &str) -> Self {
        Expression::Variable(name.to_string())
    }

    /// Creates a new binary operation which sums two sub-expressions
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::addition(
    ///     Expression::constant(2.0), 
    ///     Expression::Constant(3.0)
    /// );
    /// let eval = expr.eval().unwrap();
    ///
    /// assert_eq!(eval, 5.0);
    /// ```
    pub fn addition(e1: Expression, e2: Expression) -> Self {
        Expression::BinOp(BinOp::Addition, Box::new(e1), Box::new(e2))
    }

    /// Creates a new binary operation which subtracts two sub-expressions
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::subtraction(
    ///     Expression::constant(2.0), 
    ///     Expression::Constant(3.0)
    /// );
    /// let eval = expr.eval().unwrap();
    ///
    /// assert_eq!(eval, -1.0);
    /// ```
    pub fn subtraction(e1: Expression, e2: Expression) -> Self {
        Expression::BinOp(BinOp::Subtraction, Box::new(e1), Box::new(e2))
    }

    /// Creates a new binary operation which multiplies two sub-expressions
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::product(
    ///     Expression::constant(2.0), 
    ///     Expression::Constant(3.0)
    /// );
    /// let eval = expr.eval().unwrap();
    ///
    /// assert_eq!(eval, 6.0);
    /// ```
    pub fn product(e1: Expression, e2: Expression) -> Self {
        Expression::BinOp(BinOp::Product, Box::new(e1), Box::new(e2))
    }

    /// Creates a new binary operation which divides two sub-expressions
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    ///
    /// let expr = Expression::division(
    ///     Expression::constant(3.0), 
    ///     Expression::Constant(2.0)
    /// );
    /// let eval = expr.eval().unwrap();
    ///
    /// assert_eq!(eval, 1.5);
    /// ```
    pub fn division(e1: Expression, e2: Expression) -> Self {
        Expression::BinOp(BinOp::Division, Box::new(e1), Box::new(e2))
    }

    fn eval_core(&self, ctx: Option<&Context>) -> Result<f32, error::EvalError> {
        match self {
            Expression::Constant(val) => Ok(*val),
            Expression::BinOp(op, e1, e2) => {
                let r1 = e1.eval_core(ctx)?;
                let r2 = e2.eval_core(ctx)?;
                let r = match op {
                    BinOp::Addition => r1 + r2,
                    BinOp::Subtraction => r1 - r2,
                    BinOp::Product => r1 * r2,
                    BinOp::Division => r1 / r2,
                };
                if r.is_nan() {
                    Err(error::EvalError::NotANumber)
                } else if r.is_infinite() {
                    Err(error::EvalError::IsInfinite)
                } else {
                    Ok(r)
                }
            },
            Expression::Variable(name) => {
                match ctx {
                    Some(ctx) => match ctx.get_variable(name) {
                        Ok(r) => Ok(r),
                        Err(_) => Err(error::EvalError::VariableNotFound(name.clone())),
                    },
                    None => Err(error::EvalError::NoContextGiven),
                }
            },
        }
    }

    /// Evaluates the expression into a floating point value without a context.
    ///
    /// As of now, floating point value is the only supported evaluation. Please
    /// note that it is therefore subject to approximations due to some values
    /// not being representable.
    ///
    /// # Examples
    ///
    /// ```
    /// use math_engine::context::Context;
    /// use math_engine::expression::Expression;
    ///
    /// // Expression is (1 - 5) + (2 * (4 + 6))
    /// let expr = Expression::addition(
    ///     Expression::subtraction(
    ///         Expression::constant(1.0), 
    ///         Expression::constant(5.0)
    ///     ),
    ///     Expression::product(
    ///         Expression::constant(2.0),
    ///         Expression::addition(
    ///             Expression::constant(4.0),
    ///             Expression::constant(6.0)
    ///         )
    ///     )
    /// );
    /// let eval = expr.eval().unwrap();
    /// 
    /// assert_eq!(eval, 16.0);
    /// ```
    ///
    /// # Errors
    ///
    /// If any intermediary result is not a number of is infinity, an error is
    /// returned.
    /// If the expression contains a variable, an error is returned
    pub fn eval(&self) -> Result<f32, error::EvalError> {
        self.eval_core(None)
    }

    /// Evaluates the expression into a floating point value with a given context.
    ///
    /// As of now, floating point value is the only supported evaluation. Please
    /// note that it is therefore subject to approximations due to some values
    /// not being representable.
    ///
    /// # Examples
    ///
    /// ```
    /// use math_engine::context::Context;
    /// use math_engine::expression::Expression;
    ///
    /// // Expression is (1 / (1 + x))
    /// let expr = Expression::division(
    ///     Expression::constant(1.0),
    ///     Expression::addition(
    ///         Expression::constant(1.0),
    ///         Expression::variable("x"),
    ///     )
    /// );
    /// let ctx = Context::new().with_variable("x", 2.0);
    /// let eval = expr.eval_with_context(&ctx).unwrap();
    /// 
    /// assert_eq!(eval, 1.0/3.0);
    /// ```
    ///
    /// # Errors
    ///
    /// If any intermediary result is not a number of is infinity, an error is
    /// returned.
    /// If the expression contains a variable but the context does not define all
    /// the variables, an error is returned.
    pub fn eval_with_context(&self, ctx: &Context) -> Result<f32, error::EvalError> {
        self.eval_core(Some(ctx))
    }

    /// Calculates the derivative of an expression.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::expression::Expression;
    /// use std::str::FromStr;
    ///
    /// //Represents y + 2x
    /// let expr = Expression::from_str("1.0 * y + 2.0 * x");
    ///
    /// //Represents y + 2
    /// let deri = expr.derivative("x");
    /// ```
    pub fn derivative(&self, deriv_var: &str) -> Self {
        match self {
            Expression::Constant(_) => Expression::constant(0.0),
            Expression::Variable(var) =>
                if var.as_str() == deriv_var {
                    Expression::constant(1.0)
                } else {
                    Expression::variable(var.as_str())
                },
            Expression::BinOp(op, e1, e2) => {
                let deriv_e1 = e1.derivative(deriv_var);
                let deriv_e2 = e2.derivative(deriv_var);
                match op {
                    BinOp::Addition => Expression::addition(deriv_e1, deriv_e2),
                    BinOp::Subtraction => Expression::subtraction(deriv_e1, deriv_e2),
                    BinOp::Product => Expression::addition(
                        Expression::product(*e1.clone(), deriv_e2),
                        Expression::product(deriv_e1, *e2.clone()),
                    ),
                    BinOp::Division => Expression::division(
                        Expression::subtraction(
                            Expression::product(*e2.clone(), deriv_e1),
                            Expression::product(deriv_e2, *e1.clone()),
                        ),
                        Expression::product(
                            *e2.clone(), *e2.clone()
                        ),
                    ),
                }
            },
        }
    }
}

