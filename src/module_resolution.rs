use std::path::{Path, PathBuf};
use serde_json::Value;
use std::fs::File;
use std::io::Result;
use std::ffi::OsStr;

pub fn typings_module_resolver(import_path: &Path, pkg: &Value) -> Result<PathBuf> {
    let types_rel_path = pkg
        .as_object()
        .ok_or(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "Bad package.json (expected top-level object) found in {}",
                import_path.display()
            ),
        ))?
        .get("types")
        .ok_or(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "Bad package.json (expected 'types' property) found in {}",
                import_path.display()
            ),
        ))?
        .as_str()
        .ok_or(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "Bad package.json (expected 'types' to be a string) found in {}",
                import_path.display()
            ),
        ))?;

    let types_path = import_path.join(types_rel_path);
    if types_path.is_file() {
        Ok(types_path)
    } else {
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "Package.json in {} specified non-existent file for types, {}",
                import_path.display(),
                types_path.display()
            ),
        ))
    }
}

fn path_with_ext_appended(path: &Path, ext: &str) -> PathBuf {
    path.with_file_name(format!(
        "{}.{}",
        path.file_name()
            .unwrap_or(OsStr::new(""))
            .to_str()
            .unwrap_or(""),
        ext
    ))
}

fn get_file_with_any_ext(path: &Path) -> Result<PathBuf> {
    let exts = vec!["d.ts", "ts", "tsx", "js", "jsx", "json"];
    exts.iter()
        .map(|ext| path_with_ext_appended(path, ext))
        .find(|path_with_ext| path_with_ext.is_file())
        .ok_or(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "Could not find module with any extension, {}",
                path.display()
            ),
        ))
}

pub fn get_ts_path(
    module_base: Option<PathBuf>,
    import: &str,
    module_resolver: &dyn Fn(&Path, &Value) -> Result<PathBuf>,
) -> Result<PathBuf> {
    let cwd = module_base.unwrap_or(std::env::current_dir()?);
    let mut path = cwd.clone();
    let abs_import_path = Path::new(import);

    if abs_import_path.is_absolute() {
        if abs_import_path.is_dir() {
            get_file_with_any_ext(&abs_import_path.join("index"))
        } else {
            get_file_with_any_ext(&abs_import_path)
        }
    } else if import.starts_with(".") {
        let import_path = path.join(import);
        if import_path.is_dir() {
            get_file_with_any_ext(&import_path.join("index"))
        } else {
            get_file_with_any_ext(&import_path)
        }
    } else {
        loop {
            let possible_node_modules = path.join("node_modules");
            if possible_node_modules.is_dir() {
                let import_path = possible_node_modules.join(import);
                if import_path.is_dir() {
                    // module path
                    // check package.json for typings
                    let pkg_json_path = import_path.join("package.json");
                    let file = File::open(&pkg_json_path)?;
                    let pkg: Value = serde_json::from_reader(file)?;
                    break module_resolver(&import_path, &pkg);
                } else if import_path.exists() {
                    // must be a module + file path
                    break Ok(import_path);
                } else {
                    // check with different file extensions
                    match get_file_with_any_ext(&import_path) {
                        Ok(import_path) => break Ok(import_path),
                        Err(_) => (), // fall through so that we iterate up the directory tree, looking for a higher-level node_modules folder
                    };
                }
            }

            if !path.pop() {
                break Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Could not find node_modules directory starting at {}",
                        cwd.display()
                    ),
                ));
            }
        }
    }
}
