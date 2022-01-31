use ::rustfmt::{Config, EmitMode, Input, Session, Verbosity};
use std::error::Error;

pub fn rust_fmt(s: String) -> Result<String, Box<dyn Error>> {
    let config = {
        let mut config = Config::default();
        config.set().emit_mode(EmitMode::Stdout);
        config.set().verbose(Verbosity::Quiet);
        config
    };
    let mut buf: Vec<u8> = Default::default();

    {
        let mut session = Session::new(config, Some(&mut buf));
        session.format(Input::Text(s))?;
    }

    Ok(String::from_utf8(buf)?)
}
