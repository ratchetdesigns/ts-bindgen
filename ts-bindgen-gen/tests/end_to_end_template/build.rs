use std::process::Command;

fn main() -> std::io::Result<()> {
    let status = Command::new("npm")
        .arg("ci")
        .env_clear()
        .env("HOME", env!("HOME"))
        .env("PATH", env!("PATH"))
        .env("USER", env!("USER"))
        .spawn()?
        .wait()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "npm install failed",
        ))
    }
}
