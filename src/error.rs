pub enum ContextError {
    VariableAlreadyDefined(String, f32),
    VariableNotFound,
}

pub enum EvalError {
    IsInfinite,
    NotANumber,
    NoContextGiven,
    VariableNotFound(String),
}
