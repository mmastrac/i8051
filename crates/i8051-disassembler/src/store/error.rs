use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DslError {
    pub message: String,
    /// The byte offset where a positional parse error occurred. `None` for
    /// structural/validation errors (e.g. a missing field) that aren't tied to
    /// a spot in the input.
    pub offset: Option<usize>,
}

impl DslError {
    /// A positional error: something went wrong at a specific byte of input.
    pub fn at(offset: usize, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            offset: Some(offset),
        }
    }

    /// A non-positional error: a structural or validation failure with no
    /// meaningful location in the input (e.g. a missing field).
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            offset: None,
        }
    }
}

impl fmt::Display for DslError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.offset {
            Some(offset) => write!(f, "dsl parse error at byte {offset}: {}", self.message),
            None => write!(f, "dsl error: {}", self.message),
        }
    }
}

impl std::error::Error for DslError {}

impl serde::ser::Error for DslError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::new(msg.to_string())
    }
}

impl serde::de::Error for DslError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::new(msg.to_string())
    }
}
