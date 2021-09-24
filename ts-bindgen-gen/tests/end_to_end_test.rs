use std::fs::{copy as copy_file, create_dir_all, read_dir, write as write_file};
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;
use ts_bindgen_gen::generate_rust_for_typescript;

#[test]
fn end_to_end() -> std::io::Result<()> {
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    let target_dir = Path::new("/tmp/test"); //tempdir()?;
    let test_template_path = Path::new(cargo_dir).join("tests/end_to_end_template");

    copy_dir(&test_template_path, &target_dir)?;

    generate_ts_bindgen_files(test_template_path.join("ts"), target_dir.join("src"))?;

    run_cargo_test(target_dir);

    Ok(())
}

fn run_cargo_test<Dir: AsRef<Path>>(dir: Dir) -> std::io::Result<()> {
    Command::new("cargo")
        .current_dir(dir.as_ref())
        .arg("test")
        .env_clear()
        .spawn()?
        .wait()?;

    Ok(())
}

fn generate_ts_bindgen_files<Src: AsRef<Path>, Dest: AsRef<Path>>(
    src: Src,
    dest: Dest,
) -> std::io::Result<()> {
    for ts in read_dir(src.as_ref())? {
        let ts = ts?;
        let rust = generate_rust_for_typescript(ts.path().to_string_lossy());
        write_file(
            dest.as_ref().join(ts.file_name()).with_extension("rs"),
            rust.to_string().as_bytes(),
        )?;
    }

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
