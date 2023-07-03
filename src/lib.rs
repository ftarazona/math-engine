//! Tools for parsing and manipulating mathematical expressions
//!
//! Math-engine is a set of tools for manipulating mathematical expressions.
//! These expressions may contain various operations and variables. The crate
//! defines tools for building, evaluating, and operating on such expressions.
//!
//! ## Example
//!
//! Math-engine allows you to build variable expressions and evaluate them
//! within a context, i.e. a set of values for each variable.
//! ```
//! let expr = Expression::parse("1.0 + 3.0 * (4.0 - x)").unwrap();
//! let ctx = Context:new().with_variable("x", 3.0);
//!
//! let eval = expr.eval_with_context(&ctx).unwrap();
//!
//! assert!(eval, 4.0);
//! ```
//!
//! Basic operators are implemented for allowing easy to use building:
//! ```
//! let e1 = Expression::constant(3.0);
//! let e2 = Expression::variable("x");
//! let e = e1 + e2;
//!
//! println!("{}", e);
//! ```
//!
//! Also, some useful operations are available for manipulating expressions,
//! such as derivation and simplification by constant propagation:
//! ```
//! let e1 = Expression("1.0 * y + 2.0 * x").unwrap();
//! let deriv = e1.derivate("x");
//! println!("{}", deriv);
//!
//! let simp = deriv.constant_propagation().unwrap();
//! println!("{}", simp);
//! ```
pub mod context;
pub mod error;
pub mod expression;
pub mod parser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parser() {
        let expr = parser::parse_expression("1.0 + 3.0 * (4.0 - 2.0)").unwrap();
        let eval = expr.eval().unwrap();

        assert_eq!(eval, 7.0);
    }

    #[test]
    fn parser_variable() {
        let expr = parser::parse_expression("1.0 + x").unwrap();
        let ctx = context::Context::new().with_variable("x", 4.0);
        let eval = expr.eval_with_context(&ctx).unwrap();

        assert_eq!(eval, 5.0);
    }
}
