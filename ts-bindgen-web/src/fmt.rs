use ::rustfmt::{format, Config, Input};
use std::error::Error;

pub fn rust_fmt(s: String) -> Result<String, Box<dyn Error>> {
    let config = Config::default();
    let report = format(Input::Text(s), &config, Default::default())?;
    let result = report
        .format_result()
        .next()
        .map(|(_, res)| res.formatted_text())
        .unwrap_or_default();

    Ok(result.to_owned())
}
