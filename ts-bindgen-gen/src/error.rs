use std::fmt;
use swc_common::source_map::Span;

#[derive(Debug)]
pub enum Error {
    LogicError {
        // TODO: convert spans into a useful snippet
        msg: &'static str,
        span: Span,
    },
    NamespaceError {
        msg: &'static str,
        ns: Vec<String>,
    },
    CompositeError {
        errors: Vec<Error>,
    },
    ParseError {
        error: swc_ecma_parser::error::Error,
    },
    IoError {
        error: std::io::Error,
    },
}

impl From<swc_ecma_parser::error::Error> for Error {
    fn from(error: swc_ecma_parser::error::Error) -> Error {
        Error::ParseError { error }
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Error {
        Error::IoError { error }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::LogicError { .. } => "LogicError",
            Error::NamespaceError { .. } => "NamespaceError",
            Error::CompositeError { .. } => "CompositeError",
            Error::ParseError { .. } => "ParseError",
            Error::IoError { .. } => "IoError",
        }
    }
}

impl Error {
    pub fn with_msg_and_span(msg: &'static str, span: Span) -> Error {
        Error::LogicError { msg, span }
    }

    pub fn with_msg_and_namespace(msg: &'static str, ns: Vec<String>) -> Error {
        Error::NamespaceError { msg, ns }
    }

    pub fn with_errors(errors: Vec<Error>) -> Error {
        Error::CompositeError { errors }
    }
}
