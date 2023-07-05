use math_engine::expression::Expression;
use std::str::FromStr;

fn main() {
    let expr = Expression::from_str("(x + y) * x - (x * x + x * y)").unwrap();
    println!("initial: {}", expr);

    let deriv = expr.derivative("x");
    println!("derivative: {}", deriv);

    let fact = expr.expansion().unwrap();
    println!("expanded initial: {}", fact);

    let simp = deriv.expansion().unwrap();
    println!("expanded derivative: {}", simp);

    let deriv = fact.derivative("x");
    println!("derived expansion: {}", deriv);

    let fact = deriv.expansion().unwrap();
    println!("expanded again: {}", fact);

    

    let expr2 = Expression::parse("z / 4.0").unwrap();
    let expr = expr * expr2;
    println!("{}", expr);
}
