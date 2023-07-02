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
}

impl Expression {
    /// Creates a new constant from a floating point value.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// let cons = Expression::constant(2);
    /// let eval = cons.eval().unwrap();
    ///
    /// assert_eq!(eval, 2);
    /// ```
    pub fn constant(val: f32) -> Self {
        Expression::Constant(val)
    }

    /// Creates a new binary operation which sums two sub-expressions
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// let sum = Expression::addition(Expression::constant(2), Expression::Constant(3));
    /// let eval = cons.eval().unwrap();
    ///
    /// assert_eq!(eval, 5);
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
    /// let sum = Expression::subtraction(Expression::constant(2), Expression::Constant(3));
    /// let eval = cons.eval().unwrap();
    ///
    /// assert_eq!(eval, -1);
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
    /// let sum = Expression::product(Expression::constant(2), Expression::Constant(3));
    /// let eval = cons.eval().unwrap();
    ///
    /// assert_eq!(eval, 6);
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
    /// let sum = Expression::addition(Expression::constant(3), Expression::Constant(2));
    /// let eval = cons.eval().unwrap();
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
    /// // Expression is (1 - 5) + (2 * (4 + 6))
    /// let expr = Expression::addition(
    ///     Expression::subtraction(
    ///         Expression::constant(1), 
    ///         Expression::constant(5)
    ///     ),
    ///     Expression::product(
    ///         Expression::constant(2),
    ///         Expression::addition(
    ///             Expression::constant(4),
    ///             Expression::constant(6)
    ///         )
    ///     )
    /// );
    /// let eval = expr.eval().unwrap();
    /// 
    /// assert_eq!(eval, 16);
    /// ```
    pub fn eval(&self) -> Result<f32, error::Error> {
        match self {
            Expression::Constant(val) => Ok(*val),
            Expression::BinOp(op, e1, e2) => {
                let r1 = e1.eval()?;
                let r2 = e2.eval()?;
                match op {
                    BinOp::Addition => Ok(r1 + r2),
                    BinOp::Subtraction => Ok(r1 - r2),
                    BinOp::Product => Ok(r1 * r2),
                    BinOp::Division => Ok(r1 / r2),
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
}
