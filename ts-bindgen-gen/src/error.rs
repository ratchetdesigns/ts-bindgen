use std::fmt;
use swc_common::{
    source_map::{SourceMap, Span},
    Spanned,
};

#[derive(Debug)]
pub struct Error {
    msg: String,
}

fn span_error_msg(span: Span, source_map: &SourceMap) -> String {
    format!(
        "{}: {}",
        source_map.span_to_string(span),
        source_map
            .span_to_snippet(span)
            .unwrap_or_else(|_| "<failed to retrieve snippet>".to_owned()),
    )
}

impl Error {
    pub(crate) fn with_errors(errors: Vec<InternalError>, source_map: &SourceMap) -> Error {
        let msg: Vec<String> = errors
            .into_iter()
            .map(|error| match error {
                InternalError::LogicError { msg, span } => format!(
                    "binding generation error: {}\n{}",
                    msg,
                    span_error_msg(span, source_map)
                ),
                InternalError::NamespaceError { msg, ns } => format!(
                    "binding generation error: {} for namespace {}",
                    msg,
                    ns.join("::")
                ),
                InternalError::ParseError { error } => format!(
                    "typescript parsing error: {}\n{}",
                    error.kind().msg(),
                    span_error_msg(error.span(), source_map)
                ),
                InternalError::IoError { error } => format!("io error: {}", error),
            })
            .collect();

        Error {
            msg: msg.join("\n\n"),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Error {
        Self {
            msg: format!("io error: {}", error),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "ts-bindgen error"
    }
}

#[derive(Debug)]
pub(crate) enum InternalError {
    LogicError {
        // TODO: convert spans into a useful snippet
        msg: &'static str,
        span: Span,
    },
    NamespaceError {
        msg: &'static str,
        ns: Vec<String>,
    },
    ParseError {
        error: swc_ecma_parser::error::Error,
    },
    IoError {
        error: std::io::Error,
    },
}

impl From<swc_ecma_parser::error::Error> for InternalError {
    fn from(error: swc_ecma_parser::error::Error) -> InternalError {
        InternalError::ParseError { error }
    }
}

impl From<std::io::Error> for InternalError {
    fn from(error: std::io::Error) -> InternalError {
        InternalError::IoError { error }
    }
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for InternalError {
    fn description(&self) -> &str {
        match self {
            InternalError::LogicError { .. } => "LogicError",
            InternalError::NamespaceError { .. } => "NamespaceError",
            InternalError::ParseError { .. } => "ParseError",
            InternalError::IoError { .. } => "IoError",
        }
    }
}

impl InternalError {
    pub fn with_msg_and_span(msg: &'static str, span: Span) -> InternalError {
        InternalError::LogicError { msg, span }
    }

    pub fn with_msg_and_namespace(msg: &'static str, ns: Vec<String>) -> InternalError {
        InternalError::NamespaceError { msg, ns }
    }
}
