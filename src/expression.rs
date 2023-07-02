use crate::context::Context;
use crate::error;

pub enum BinOp {
    Addition,
    Subtraction,
    Product,
    Division,
}

pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    Constant(f32),
    Variable(String),
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
    /// let eval = expr.eval(None).unwrap();
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
    /// let eval = expr.eval(Some(&ctx)).unwrap();
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
    /// let eval = expr.eval(None).unwrap();
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
    /// let eval = expr.eval(None).unwrap();
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
    /// let eval = expr.eval(None).unwrap();
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
    /// let eval = expr.eval(None).unwrap();
    ///
    /// assert_eq!(eval, 1.5);
    /// ```
    pub fn division(e1: Expression, e2: Expression) -> Self {
        Expression::BinOp(BinOp::Division, Box::new(e1), Box::new(e2))
    }

    /// Evaluates the expression into a floating point value.
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
    /// let eval = expr.eval(None).unwrap();
    /// 
    /// assert_eq!(eval, 16.0);
    /// ```
    ///
    /// # Errors
    ///
    /// If any intermediary result is not a number of is infinity, an error is
    /// returned.
    /// If a the expression contains a variable, a context must be given and
    /// must contain the definition for this variable. An error is returned if
    /// one of these conditions is not filled.
    pub fn eval(&self, ctx: Option<&Context>) -> Result<f32, error::EvalError> {
        match self {
            Expression::Constant(val) => Ok(*val),
            Expression::BinOp(op, e1, e2) => {
                let r1 = e1.eval(ctx)?;
                let r2 = e2.eval(ctx)?;
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
}

