use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DslError {
    pub message: String,
    pub offset: usize,
}

impl DslError {
    pub fn new(offset: usize, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            offset,
        }
    }
}

impl fmt::Display for DslError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dsl parse error at byte {}: {}", self.offset, self.message)
    }
}

impl std::error::Error for DslError {}

impl serde::ser::Error for DslError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::new(0, msg.to_string())
    }
}

impl serde::de::Error for DslError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::new(0, msg.to_string())
    }
}
