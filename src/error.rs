pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Lexer error: `{msg}` at {module}:{line}:{col}")]
    Lexer {
        module: String,
        line: usize,
        col: usize,
        msg: String,
    },
    #[error("Syntax error: `{msg}` at {module}:{line}:{col}")]
    Syntax {
        module: String,
        line: usize,
        col: usize,
        msg: String,
    },
    #[error("Semanteme error: `{msg}` at {module}:{line}:{col}")]
    Semanteme {
        module: String,
        line: usize,
        col: usize,
        msg: String,
    },
    #[error("Internal: `{0}`")]
    Internal(String),
}

macro_rules! lexer {
    ($module:expr, $line:expr, $col:expr, $($arg:tt)*) => {
        $crate::error::Error::Lexer {
            module: $module.to_string(),
            line: $line,
            col: $col,
            msg: format!($($arg)*),
        }
    };
}

macro_rules! syntax {
    ($module:expr, $line:expr, $col:expr, $($arg:tt)*) => {
        $crate::error::Error::Syntax {
            module: $module.to_string(),
            line: $line,
            col: $col,
            msg: format!($($arg)*),
        }
    };
}

macro_rules! semanteme {
    ($module:expr, $line:expr, $col:expr, $($arg:tt)*) => {
        $crate::error::Error::Semanteme {
            module: $module.to_string(),
            line: $line,
            col: $col,
            msg: format!($($arg)*),
        }
    };
}

macro_rules! internal {
    ($($arg:tt)*) => { $crate::error::Error::Internal(format!($($arg)*)) };
}

pub(crate) use {internal, lexer, semanteme, syntax};
