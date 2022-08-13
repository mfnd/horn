use std::fmt::{Debug, Display};


#[derive(Debug)]
pub enum RuntimeErrorType {
    ValueError,
    ArithmeticError,
    ParseError,
    QueryNotSetError
}

pub trait Error : Display {
    fn source(&self) -> Option<&dyn Error>;
    fn message(&self) -> Option<&str>;
}

pub struct GenericError<T : Debug> {
    pub error_type: T,
    pub cause: Option<Box<dyn Error>>,
    pub message: String
}

impl<T: Debug> Display for GenericError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}: {}", self.error_type, self.message))
    }
}

impl<T : Debug> Error for GenericError<T> {
    fn source(&self) -> Option<&dyn Error> {
        self.cause.as_deref()
    }

    fn message(&self) -> Option<&str> {
        Some(&self.message)
    }
}

pub type RuntimeError = GenericError<RuntimeErrorType>;

impl RuntimeError {

    pub fn query_not_set() -> Box<Self> {
        Box::from(
            RuntimeError {
                error_type: RuntimeErrorType::QueryNotSetError,
                cause: None,
                message: String::from("No query is set for execution"),
            }
        )
    }
}

