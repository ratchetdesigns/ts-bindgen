use std::fmt;
use swc_common::{
    source_map::{SourceMap, Span},
    Spanned,
};

/// Public Error type for ts-bindgen-gen.
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
                InternalError::Logic { msg, span } => format!(
                    "binding generation error: {}\n  source: {}",
                    msg,
                    span_error_msg(span, source_map)
                ),
                InternalError::Parse { error } => format!(
                    "typescript parsing error: {}\n  source: {}",
                    error.kind().msg(),
                    span_error_msg(error.span(), source_map)
                ),
                InternalError::Io { error } => format!("io error: {}", error),
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
    Logic {
        // TODO: convert spans into a useful snippet
        msg: &'static str,
        span: Span,
    },
    Parse {
        error: swc_ecma_parser::error::Error,
    },
    Io {
        error: std::io::Error,
    },
}

impl From<swc_ecma_parser::error::Error> for InternalError {
    fn from(error: swc_ecma_parser::error::Error) -> InternalError {
        InternalError::Parse { error }
    }
}

impl From<std::io::Error> for InternalError {
    fn from(error: std::io::Error) -> InternalError {
        InternalError::Io { error }
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
            InternalError::Logic { .. } => "Logic",
            InternalError::Parse { .. } => "Parse",
            InternalError::Io { .. } => "Io",
        }
    }
}

impl InternalError {
    pub fn with_msg_and_span(msg: &'static str, span: Span) -> InternalError {
        InternalError::Logic { msg, span }
    }
}
