use std::env::current_dir;
use std::fs;
use std::io::{Error, Read};
use std::path::{Path, PathBuf};

/// Filesystem abstraction
pub trait Fs {
    fn is_file(&self, path: &Path) -> bool;
    fn is_dir(&self, path: &Path) -> bool;
    fn exists(&self, path: &Path) -> bool;
    fn open<'a>(&'a self, path: &Path) -> Result<Box<dyn Read + 'a>, Error>;
    fn cwd(&self) -> Result<PathBuf, Error>;
}

/// Regular filesystem
pub struct StdFs;

impl Fs for StdFs {
    fn is_file(&self, path: &Path) -> bool {
        path.is_file()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn open<'a>(&'a self, path: &Path) -> Result<Box<dyn Read + 'a>, Error> {
        Ok(Box::new(fs::File::open(path)?) as Box<dyn Read>)
    }

    fn cwd(&self) -> Result<PathBuf, Error> {
        current_dir()
    }
}

#[cfg(test)]
pub mod test {
    pub use super::Fs;
    use std::collections::HashMap;
    use std::io::{Error, ErrorKind, Read};
    use std::path::{Path, PathBuf};

    /// Testing filesystem
    #[derive(Default)]
    pub struct TestFs {
        cwd: Option<PathBuf>,
        paths: HashMap<PathBuf, FileEntry>,
    }

    enum FileEntry {
        File(String),
        Dir(),
    }

    impl TestFs {
        pub fn set_cwd(&mut self, path: &Path) {
            self.cwd = Some(path.to_path_buf());
        }

        pub fn add_file_at(&mut self, path: &Path, contents: String) {
            self.paths
                .insert(path.to_path_buf(), FileEntry::File(contents));
        }

        pub fn add_dir_at(&mut self, path: &Path) {
            self.paths.insert(path.to_path_buf(), FileEntry::Dir());
        }

        pub fn rm_at(&mut self, path: &Path) {
            self.paths.remove(path);
        }
    }

    impl Fs for TestFs {
        fn is_file(&self, path: &Path) -> bool {
            matches!(self.paths.get(path), Some(FileEntry::File(_)))
        }

        fn is_dir(&self, path: &Path) -> bool {
            matches!(self.paths.get(path), Some(FileEntry::Dir()))
        }

        fn exists(&self, path: &Path) -> bool {
            self.paths.get(path).is_some()
        }

        fn open<'a>(&'a self, path: &Path) -> Result<Box<dyn Read + 'a>, Error> {
            match self.paths.get(path) {
                Some(FileEntry::File(contents)) => {
                    Ok(Box::new(contents.as_bytes()) as Box<dyn Read>)
                }
                _ => Err(Error::new(ErrorKind::NotFound, "not found")),
            }
        }

        fn cwd(&self) -> Result<PathBuf, Error> {
            Ok(self
                .cwd
                .as_ref()
                .ok_or_else(|| Error::new(ErrorKind::NotFound, "not found"))?
                .clone())
        }
    }
}
