use crate::error::ParserError;
use crate::expression::Expression;
use lexgen::lexer;
use pomelo::pomelo;

pomelo! {
    %include {
        use crate::expression::*;
    }

    %type input Option<Expression>;
    %type expr Expression;
    %type Constant f32;
    %type Literal String;

    %left Plus;
    %left Minus;
    %left Prod;
    %left Div;

    input ::= expr?(E) { E };
    expr ::= Constant(N) { Expression::constant(N) }
    expr ::= Literal(V) { Expression::variable(V.as_str()) }
    expr ::= LPar expr(E) RPar { E }
    expr ::= expr(E1) Plus expr(E2) { Expression::addition(E1, E2) }
    expr ::= expr(E1) Minus expr(E2) { Expression::subtraction(E1, E2) }
    expr ::= expr(E1) Prod expr(E2) { Expression::product(E1, E2) }
    expr ::= expr(E1) Div expr(E2) { Expression::division(E1, E2) }
}

use parser::Token;

lexer! {
    ExprLexer -> Token;

    let number = ['0'-'9']+ '.' ['0'-'9']*;
    let var = ['a'-'z''A'-'Z''_']*;

    rule Init {
        [' ''\t' '\n' '\r']+,
        '+' => |lex| lex.return_(Token::Plus),
        '-' => |lex| lex.return_(Token::Minus),
        '*' => |lex| lex.return_(Token::Prod),
        '/' => |lex| lex.return_(Token::Div),
        '(' => |lex| lex.return_(Token::LPar),
        ')' => |lex| lex.return_(Token::RPar),
        $number => |lex| lex.return_(Token::Constant(
                lex.match_()[..].to_owned().parse::<f32>().unwrap()
        )),
        $var => |lex| lex.return_(Token::Literal(
                lex.match_()[..].to_string()
        )),
    }
}

/// Parses a mathematical expression from a string.
/// As of now, the user must use full notation for floating point values.
///
/// # Examples
/// Basid usage:
///
/// ```
/// use math_engine::context::Context;
/// use math_engine::expression::Expression;
/// use math_engine::parser::parse_expression;
///
/// let expr = parse_expression("1.0 + x").unwrap();
/// let ctx = Context::new().with_variable("x", 4.0);
/// let eval = expr.eval_with_context(&ctx).unwrap();
///
/// assert_eq!(eval, 5.0);
/// ```
///
/// # Errors
/// An error is returned if the string could not be parsed.
pub fn parse_expression(input: &str) -> Result<Expression, ParserError> {
    use parser::Parser;

    let mut p = Parser::new();
    let lexer = ExprLexer::new(input);

    for tok in lexer {
        match tok {
            Ok((_, tok, _)) => {
                if p.parse(tok).is_err() {
                    return Err(ParserError::ParserError);
                }
            }
            Err(_) => return Err(ParserError::LexerError),
        }
    }

    match p.end_of_input() {
        Ok(Some(e)) => Ok(e),
        Ok(None) => Err(ParserError::NoExpressionFound),
        Err(_) => Err(ParserError::ParserError),
    }
}
