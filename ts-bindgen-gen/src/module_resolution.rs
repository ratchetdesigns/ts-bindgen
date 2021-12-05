use crate::fs::{Fs, StdFs};
use serde_json::Value;
use std::fs::File;
use std::io::Result;
use std::iter;
use std::path::{Path, PathBuf};

pub fn typings_module_resolver(import_path: &Path, pkg: &Value) -> Result<PathBuf> {
    let types_rel_path = pkg
        .as_object()
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Bad package.json (expected top-level object) found in {}",
                    import_path.display()
                ),
            )
        })?
        .get("types")
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Bad package.json (expected 'types' property) found in {}",
                    import_path.display()
                ),
            )
        })?
        .as_str()
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Bad package.json (expected 'types' to be a string) found in {}",
                    import_path.display()
                ),
            )
        })?;

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
        path.file_name().unwrap_or_default().to_string_lossy(),
        ext
    ))
}

fn get_file_with_any_ext<FS: Fs>(fs: &FS, path: &Path) -> Result<PathBuf> {
    let exts = ["d.ts", "ts", "tsx", "js", "jsx", "json"];
    iter::once(path.to_path_buf())
        .chain(exts.iter().map(|ext| path_with_ext_appended(path, ext)))
        .find(|path_with_ext| fs.is_file(path_with_ext))
        .ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!(
                    "Could not find module with any extension, {}",
                    path.display()
                ),
            )
        })
}

pub fn get_ts_path(
    module_base: Option<PathBuf>,
    import: &str,
    module_resolver: &dyn Fn(&Path, &Value) -> Result<PathBuf>,
) -> Result<PathBuf> {
    let fs = StdFs; // TODO: pass this in
    let cwd = module_base.unwrap_or(std::env::current_dir()?);
    let mut path = cwd.clone();
    let abs_import_path = Path::new(import);

    if abs_import_path.is_absolute() {
        if abs_import_path.is_dir() {
            get_file_with_any_ext(&fs, &abs_import_path.join("index"))
        } else {
            get_file_with_any_ext(&fs, abs_import_path)
        }
    } else if import.starts_with('.') {
        let import_path = path.join(import);
        if import_path.is_dir() {
            get_file_with_any_ext(&fs, &import_path.join("index"))
        } else {
            get_file_with_any_ext(&fs, &import_path)
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
                    if let Ok(import_path) = get_file_with_any_ext(&fs, &import_path) {
                        break Ok(import_path);
                    }
                    // fall through so that we iterate up the directory tree, looking for a higher-level node_modules folder
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

#[cfg(test)]
mod test {
    use super::{get_file_with_any_ext, path_with_ext_appended};
    use crate::fs::test::{Fs, TestFs};
    use proptest::prelude::*;
    use std::path::Path;

    proptest! {
        #[test]
        fn test_path_with_ext_appended_prop(
            prefix in "(/[a-zA-Z0-9-]+)*",
            filename in "[a-zA-Z0-9-]+",
            next_ext in "[a-zA-Z0-9.-]+",
        ) {
            let path = format!("{}/{}", prefix, filename);
            let with_ext = path_with_ext_appended(Path::new(&path), &next_ext);
            prop_assert_eq!(with_ext.to_string_lossy(), format!("{}/{}.{}", prefix, filename, next_ext));
        }
    }

    proptest! {
        #[test]
        fn test_get_file_with_any_ext_success(
            prefix in "(/[a-zA-Z0-9-]+)*",
            path in "[a-zA-Z0-9-]+",
            ext in prop_oneof![
                Just("d.ts"),
                Just("ts"),
                Just("tsx"),
                Just("js"),
                Just("jsx"),
                Just("json"),
            ]
        ) {
            let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
            let path_with_ext = Path::new(&path_with_ext);
            let fs = {
                let mut fs: TestFs = Default::default();
                fs.add_file_at(&path_with_ext);
                fs
            };
            let path_without_ext = format!("{}/{}", &prefix, &path);
            let path_without_ext = Path::new(&path_without_ext);
            prop_assert_eq!(get_file_with_any_ext(&fs, &path_without_ext)?, path_with_ext.to_path_buf());
        }
    }

    proptest! {
        #[test]
        fn test_get_file_with_any_ext_vacuous_success(
            prefix in "(/[a-zA-Z0-9-]+)*",
            path in "[a-zA-Z0-9-]+",
            ext in prop_oneof![
                Just("d.ts"),
                Just("ts"),
                Just("tsx"),
                Just("js"),
                Just("jsx"),
                Just("json"),
            ]
        ) {
            let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
            let path_with_ext = Path::new(&path_with_ext);
            let fs = {
                let mut fs: TestFs = Default::default();
                fs.add_file_at(&path_with_ext);
                fs
            };
            prop_assert_eq!(get_file_with_any_ext(&fs, &path_with_ext)?, path_with_ext.to_path_buf());
        }
    }

    proptest! {
        #[test]
        fn test_get_file_with_any_ext_fail(
            prefix in "(/[a-zA-Z0-9-]+)*",
            path in "[a-zA-Z0-9-]+",
            ext in "[\\w].*"
        ) {
            prop_assume!(
                ["d.ts", "ts", "tsx", "js", "jsx", "json"]
                    .iter()
                    .all(|e| *e != ext)
            );
            let fs = {
                let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
                let path_with_ext = Path::new(&path_with_ext);
                let mut fs: TestFs = Default::default();
                fs.add_file_at(&path_with_ext);
                fs
            };
            let path_without_ext = format!("{}/{}", &prefix, &path);
            let path_without_ext = Path::new(&path_without_ext);
            prop_assert!(matches!(get_file_with_any_ext(&fs, &path_without_ext), Err(_)));
        }
    }
}
