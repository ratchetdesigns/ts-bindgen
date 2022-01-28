use std::path::Path;
use std::process::Command;

fn main() {
    let cargo_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    npm_ci(cargo_dir).expect("npm ci failed");
    generate_binding(cargo_dir).expect("binding generation failed");
}

fn npm_ci<T: AsRef<Path>>(dir: T) -> std::io::Result<()> {
    let status = Command::new("npm")
        .arg("ci")
        .current_dir(dir.as_ref())
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

fn generate_binding<T: AsRef<Path>>(dir: T) -> std::io::Result<()> {
    let cargo = env!("CARGO");
    let status = Command::new(cargo)
        .arg("run")
        .arg("--manifest-path")
        .arg("../../Cargo.toml")
        .arg("--")
        .arg("--output")
        .arg("src/paper.rs")
        .arg("--rerun-if-changed")
        .arg("paper")
        .current_dir(dir.as_ref())
        .spawn()?
        .wait()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "ts->rust binding generation failed",
        ))
    }
}
