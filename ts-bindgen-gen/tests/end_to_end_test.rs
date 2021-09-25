use std::fs::{copy as copy_file, create_dir_all, read_dir, remove_file, write as write_file};
#[cfg(target_os = "linux")]
use std::os::unix::fs::symlink;
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;
use ts_bindgen_gen::generate_rust_for_typescript;

#[test]
fn end_to_end() -> std::io::Result<()> {
    let cargo_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let target_dir = Path::new("/tmp/test"); //tempdir()?;
    let test_template_path = cargo_dir.join("tests/end_to_end_template");

    copy_dir(&test_template_path, &target_dir)?;

    make_symlink(
        cargo_dir.join("../ts-bindgen-rt"),
        target_dir.join("ts-bindgen-rt"),
    )?;

    let ts_index = target_dir.join("ts").join("index");
    build_ts(&ts_index)?;
    generate_ts_bindgen_file(&ts_index, target_dir.join("src").join("js_lib.rs"))?;

    run_cargo_test(target_dir)?;

    Ok(())
}

#[cfg(target_os = "linux")]
fn make_symlink<Src: AsRef<Path>, Dest: AsRef<Path>>(src: Src, dest: Dest) -> std::io::Result<()> {
    let dest = dest.as_ref();
    if dest.exists() {
        if dest.read_link()? == dest {
            return Ok(());
        }

        remove_file(dest)?;
    }

    symlink(src, dest)
}

fn build_ts<T: AsRef<Path>>(ts_path: T) -> std::io::Result<()> {
    let status = Command::new("npx")
        .arg("--yes")
        .arg("--package=typescript")
        .arg("--")
        .arg("tsc")
        .arg("--declaration")
        .arg(ts_path.as_ref())
        .env_clear()
        .env("HOME", env!("HOME"))
        .env("PATH", env!("PATH"))
        .env("USER", env!("USER"))
        .env("HOSTNAME", env!("HOSTNAME"))
        .env("CARGO_HOME", env!("CARGO_HOME"))
        .env("RUSTUP_HOME", env!("RUSTUP_HOME"))
        .env("RUST_VERSION", env!("RUST_VERSION"))
        .spawn()?
        .wait()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "test failed",
        ))
    }
}

fn run_cargo_test<Dir: AsRef<Path>>(dir: Dir) -> std::io::Result<()> {
    let status = Command::new("wasm-pack")
        .current_dir(dir.as_ref())
        .arg("test")
        .arg("--node")
        .env_clear()
        .env("HOME", env!("HOME"))
        .env("PATH", env!("PATH"))
        .env("USER", env!("USER"))
        .env("HOSTNAME", env!("HOSTNAME"))
        .env("CARGO_HOME", env!("CARGO_HOME"))
        .env("RUSTUP_HOME", env!("RUSTUP_HOME"))
        .env("RUST_VERSION", env!("RUST_VERSION"))
        .spawn()?
        .wait()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "test failed",
        ))
    }
}

fn generate_ts_bindgen_files<Src: AsRef<Path>, Dest: AsRef<Path>>(
    src: Src,
    dest: Dest,
) -> std::io::Result<()> {
    for ts in read_dir(src.as_ref())? {
        let ts = ts?;
        generate_ts_bindgen_file(
            ts.path(),
            dest.as_ref().join(ts.file_name()).with_extension("rs"),
        )?;
    }

    Ok(())
}

fn generate_ts_bindgen_file<Src: AsRef<Path>, Dest: AsRef<Path>>(
    src: Src,
    dest: Dest,
) -> std::io::Result<()> {
    let rust = generate_rust_for_typescript(src.as_ref().to_string_lossy());
    write_file(dest.as_ref(), rust.to_string().as_bytes())?;

    Ok(())
}

fn copy_dir<Src: AsRef<Path>, Dest: AsRef<Path>>(src: Src, dest: Dest) -> std::io::Result<()> {
    create_dir_all(dest.as_ref())?;

    for entry in read_dir(src.as_ref())? {
        let entry = entry?;
        let entry_dest = dest.as_ref().join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_dir(entry.path(), entry_dest)?;
        } else {
            copy_file(entry.path(), entry_dest)?;
        }
    }

    Ok(())
}
