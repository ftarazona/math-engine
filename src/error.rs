#[derive(Debug)]
pub enum ContextError {
    VariableAlreadyDefined(String, f32),
    VariableNotFound,
}

#[derive(Debug)]
pub enum EvalError {
    DivisionByZero,
    IsInfinite,
    NotANumber,
    NoContextGiven,
    VariableNotFound(String),
}

#[derive(Debug)]
pub enum ParserError {
    LexerError,
    ParserError,
    NoExpressionFound,
}
