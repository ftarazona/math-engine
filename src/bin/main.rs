use math_engine::expression::Expression;
use std::str::FromStr;

fn main() {
    let expr = Expression::from_str("2.0 * (x + x * x) * x").unwrap();
    println!("initial: {}", expr);

    let deriv = expr.derivative("x");
    println!("derivative: {}", deriv);

    let fact = expr.factorize().unwrap();
    println!("factorized initial: {}", fact);

    let simp = deriv.factorize().unwrap();
    println!("factorized derivative: {}", simp);

    let deriv = fact.derivative("x");
    println!("derived factorized: {}", deriv);

    let fact = deriv.factorize().unwrap();
    println!("factorized again: {}", fact);

    let expr2 = Expression::parse("z / 4.0").unwrap();
    let expr = expr * expr2;
    println!("{}", expr);
}
