use crate::fs::Fs;
use serde_json::Value;
use std::io::Result;
use std::iter;
use std::path::{Path, PathBuf};

pub fn typings_module_resolver<FS: Fs + ?Sized>(
    fs: &FS,
    import_path: &Path,
    pkg: &Value,
) -> Result<PathBuf> {
    let pkg_json = pkg.as_object().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "Bad package.json (expected top-level object) found in {}",
                import_path.display()
            ),
        )
    })?;
    let types_rel_path = pkg_json
        .get("types")
        .or_else(|| pkg_json.get("typings"))
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
    if fs.is_file(&types_path) {
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

fn get_file_with_any_ext<FS: Fs + ?Sized>(fs: &FS, path: &Path) -> Result<PathBuf> {
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

pub fn get_ts_path<FS: Fs + ?Sized>(
    fs: &FS,
    module_base: Option<PathBuf>,
    import: &str,
    module_resolver: &dyn Fn(&FS, &Path, &Value) -> Result<PathBuf>,
) -> Result<PathBuf> {
    let cwd = module_base.map_or_else(|| fs.cwd(), Ok)?;
    let mut path = cwd.clone();
    let abs_import_path = Path::new(import);

    if fs.is_absolute_path(abs_import_path) {
        if fs.is_dir(abs_import_path) {
            get_file_with_any_ext(fs, &abs_import_path.join("index"))
        } else {
            get_file_with_any_ext(fs, abs_import_path)
        }
    } else if import.starts_with('.') {
        let import_path = path.join(import);
        if fs.is_dir(&import_path) {
            get_file_with_any_ext(fs, &import_path.join("index"))
        } else {
            get_file_with_any_ext(fs, &import_path)
        }
    } else {
        loop {
            let possible_node_modules = path.join("node_modules");
            if fs.is_dir(&possible_node_modules) {
                let import_path = possible_node_modules.join(import);
                if fs.is_dir(&import_path) {
                    // module path
                    // check package.json for typings
                    let pkg_json_path = import_path.join("package.json");
                    let file = fs.open(&pkg_json_path)?;
                    let pkg: Value = serde_json::from_reader(file)?;
                    break module_resolver(fs, &import_path, &pkg);
                } else if fs.exists(&import_path) {
                    // must be a module + file path
                    break Ok(import_path);
                } else {
                    // check with different file extensions
                    if let Ok(import_path) = get_file_with_any_ext(fs, &import_path) {
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
    use super::{
        get_file_with_any_ext, get_ts_path, path_with_ext_appended, typings_module_resolver,
    };
    use crate::fs::MemFs;
    use crate::generators::*;
    use proptest::bool::ANY;
    use proptest::prelude::*;
    use std::io::{Error, ErrorKind};
    use std::path::Path;

    proptest! {
        #[test]
        fn test_path_with_ext_appended_prop(
            prefix in arb_path(),
            filename in arb_path_part(),
            next_ext in arb_path_ext()
        ) {
            let path = format!("{}/{}", prefix, filename);
            let with_ext = path_with_ext_appended(Path::new(&path), &next_ext);
            prop_assert_eq!(with_ext.to_string_lossy(), format!("{}/{}.{}", prefix, filename, next_ext));
        }
    }

    proptest! {
        #[test]
        fn test_get_file_with_any_ext_success(
            prefix in arb_path(),
            path in arb_path_part(),
            ext in arb_ts_ext()
        ) {
            let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
            let path_with_ext = Path::new(&path_with_ext);
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.add_file_at(&path_with_ext, "".to_string());
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
            prefix in arb_path(),
            path in arb_path_part(),
            ext in arb_ts_ext()
        ) {
            let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
            let path_with_ext = Path::new(&path_with_ext);
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.add_file_at(&path_with_ext, "".to_string());
                fs
            };
            prop_assert_eq!(get_file_with_any_ext(&fs, &path_with_ext)?, path_with_ext.to_path_buf());
        }
    }

    proptest! {
        #[test]
        fn test_get_file_with_any_ext_fail(
            prefix in arb_path(),
            path in arb_path_part(),
            ext in arb_ext()
        ) {
            prop_assume!(
                ["d.ts", "ts", "tsx", "js", "jsx", "json"]
                    .iter()
                    .all(|e| *e != ext)
            );
            let fs = {
                let path_with_ext = format!("{}/{}.{}", &prefix, &path, &ext);
                let path_with_ext = Path::new(&path_with_ext);
                let mut fs: MemFs = Default::default();
                fs.add_file_at(&path_with_ext, "".to_string());
                fs
            };
            let path_without_ext = format!("{}/{}", &prefix, &path);
            let path_without_ext = Path::new(&path_without_ext);
            prop_assert!(matches!(get_file_with_any_ext(&fs, &path_without_ext), Err(_)));
        }
    }

    proptest! {
        #[test]
        fn test_get_ts_path_for_non_node_mods(
            use_cwd in ANY,
            use_dir in ANY,
            use_abs in ANY,
            cwd in arb_abs_path(),
            import_str in arb_abs_path(),
            ext in arb_ts_ext()
        ) {
            let mut fs: MemFs = Default::default();
            let cwd = Path::new(&cwd);
            let module_base = if use_cwd {
                fs.set_cwd(cwd);
                None
            } else {
                Some(cwd.to_path_buf())
            };
            let (import_str, import_path) = if use_abs {
                // use import_str as an absolute path
                let import_path = Path::new(&import_str).to_path_buf();

                (import_str, import_path)
            } else {
                // use import_str as a relative path with cwd as base

                // import_str is abs (starts with /). interpret as relative to cwd
                let rel_import = import_str.get(1..).unwrap();
                let import_path = Path::new(&rel_import);
                (format!(".{}", &import_str), import_path.to_path_buf())
            };

            let (import_str, expected_resolved_path) = if use_dir {
                // we will import a directory and we need an index.<ext> file under it
                let file_path = cwd.join(path_with_ext_appended(&import_path.join("index"), &ext));

                fs.add_dir_at(&cwd.join(import_path));
                fs.add_file_at(&file_path, "".to_string());

                (import_str.clone(), file_path)
            } else {
                // we will import the ts file directly
                let import_path = path_with_ext_appended(&Path::new(&import_str), &ext);
                let import_str = import_path.to_string_lossy();

                let file_path = cwd.join(&import_path);
                fs.add_file_at(&file_path, "".to_string());

                (import_str.into_owned(), file_path)
            };

            // sanity test since this test is a bit complex
            if use_abs {
                prop_assert!(import_str.starts_with("/"));
            } else {
                prop_assert!(import_str.starts_with("./"));
            };

            // sanity test since this test is a bit complex
            let full_ext = format!(".{}", &ext);
            if use_dir {
                prop_assert!(!import_str.ends_with(&full_ext));
            } else {
                prop_assert!(import_str.ends_with(&full_ext));
            }

            let resolved_path = get_ts_path(
                &fs,
                module_base,
                &import_str,
                &|_fs, _path, _value| Err(Error::new(ErrorKind::InvalidData, "unexpected")),
            );

            prop_assert!(resolved_path.is_ok());

            let resolved_path = resolved_path.unwrap();
            prop_assert_eq!(resolved_path, expected_resolved_path);
        }

        #[test]
        fn test_get_ts_path_for_node_mods(
            mode in prop_oneof![
                Just("full_path"),
                Just("pkg_json"),
                Just("file_without_ext")
            ],
            use_cwd in ANY,
            node_modules_dir in arb_abs_path(),
            node_modules_to_cwd_dir in arb_rel_path(),
            import_str in arb_rel_path(),
            typings_file in arb_path_part(),
            ext in arb_ts_ext()
        ) {
            let mut fs: MemFs = Default::default();
            let cwd = Path::new(&node_modules_dir).join(&node_modules_to_cwd_dir);
            let module_base = if use_cwd {
                fs.set_cwd(&cwd);
                None
            } else {
                Some(cwd.to_path_buf())
            };

            let node_modules_dir = Path::new(&node_modules_dir).join("node_modules");
            let import_path = node_modules_dir.join(&import_str);

            {
                // make all of the directories up from our import path
                let mut dir_to_make = import_path.to_path_buf();
                loop {
                    fs.add_dir_at(&dir_to_make);
                    if !dir_to_make.pop() {
                        break;
                    }
                }
            }

            let (import_str, expected_resolved_path) = match mode {
                "full_path" => {
                    // import_str should point to a full path + extension relative to node_modules
                    let file_path = path_with_ext_appended(&import_path, &ext);
                    let import_str = path_with_ext_appended(Path::new(&import_str), &ext);
                    let import_str = import_str.to_string_lossy().into_owned();

                    (import_str, file_path.clone())
                },
                "pkg_json" => {
                    // import_str should point to a directory containing a package.json with a
                    // typings key pointing to a file relative to the package.json directory
                    let pkg_json_path = import_path.join("package.json");

                    fs.add_file_at(&pkg_json_path, format!("{{\"types\": \"{}.{}\"}}", &typings_file, &ext));

                    let file_path = path_with_ext_appended(&import_path.join(&typings_file), &ext);

                    (import_str, file_path)
                },
                "file_without_ext" => {
                    // import_str should point to a file such that file + valid ext exists
                    let file_path = path_with_ext_appended(&import_path, &ext);

                    // resolution prefers directories if they exist so make sure the directory doesn't
                    // exist so we can test the extension appending
                    fs.rm_at(&import_path);

                    (import_str, file_path.clone())
                },
                _ => unreachable!(),
            };

            fs.add_file_at(&expected_resolved_path, "".to_string());

            let resolved_path = get_ts_path(
                &fs,
                module_base,
                &import_str,
                &typings_module_resolver,
            );

            prop_assert!(resolved_path.is_ok());

            let resolved_path = resolved_path.unwrap();
            prop_assert_eq!(resolved_path, expected_resolved_path);
        }
    }
}
