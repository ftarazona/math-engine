use math_engine::expression::Expression;
use std::str::FromStr;

fn main() {
    let expr = Expression::from_str("1.0 * y + 2.0 * x").unwrap();
    println!("{}", expr.to_string());

    let deriv = expr.derivative("x");
    println!("{}", deriv.to_string());

    let simp = deriv.constant_propagation().unwrap();
    println!("{}", simp.to_string());

    let expr2 = Expression::parse("z / 4.0").unwrap();
    let expr = expr + expr2;
    println!("{}", expr);
}
