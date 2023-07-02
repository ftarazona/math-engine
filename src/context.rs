use crate::error;
use std::collections::HashMap;

pub struct Context {
    vars: HashMap<String, f32>,
}

impl Context {
    /// Creates a new empty context.
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    /// Adds a variable to the context, overwriting it if already existing.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```
    /// use math_engine::context::Context;
    ///
    /// let ctx = Context::new().with_variable("x", 32.0);
    /// ```
    pub fn with_variable(mut self, name: &str, val: f32) -> Self {
        let _ = self.vars.insert(name.to_string(), val);
        self
    }

    /// Adds a variable to the context, checking whether it has already been defined.
    ///
    /// # Examples
    /// Basic usage:
    ///
    /// ```should_panic
    /// use math_engine::context::Context;
    ///
    /// let mut ctx = Context::new();
    /// ctx.add_variable("x", 32.0).unwrap();
    /// ctx.add_variable("x", 31.0).unwrap(); // Panics
    /// ```
    pub fn add_variable(&mut self, name: &str, val: f32) -> Result<(), error::ContextError> {
        let name = name.to_string();
        let old_val = self.vars.insert(name.clone(), val);

        match old_val {
            Some(val) => Err(error::ContextError::VariableAlreadyDefined(name, val)),
            None => Ok(()),
        }
    }

    pub fn get_variable(&self, name: &String) -> Result<f32, error::ContextError> {
        match self.vars.get(name) {
            Some(r) => Ok(*r),
            None => Err(error::ContextError::VariableNotFound),
        }
    }
}
