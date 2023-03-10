use std::{fmt, error::Error};


#[derive(Debug, Clone)]
pub struct MyError {
    message: String,
}

impl MyError {
    pub fn new<S>(message: S) -> MyError where S: Into<String> {
        MyError { message: message.into() }
    }

    pub fn wrap<E>(error: E) -> MyError where E: Error {
        MyError { message: error.to_string() }
    }
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl Error for MyError {
    // empty
}
