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
        let eval = expr.eval(None).unwrap();

        assert_eq!(eval, 7.0);
    }

    #[test]
    fn parser_variable() {
        let expr = parser::parse_expression("1.0 + x").unwrap();
        let ctx = context::Context::new().with_variable("x", 4.0);
        let eval = expr.eval(Some(&ctx)).unwrap();

        assert_eq!(eval, 5.0);
    }
}
