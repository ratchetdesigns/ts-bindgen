use std::collections::HashMap;
use std::env::current_dir;
use std::fmt::Debug;
use std::fs;
use std::io::{Error, ErrorKind, Read};
use std::path::{Component, Path, PathBuf};

/// Filesystem abstraction
pub trait Fs: Debug {
    /// Does path refer to a regular file?
    fn is_file(&self, path: &Path) -> bool;

    /// Does path refer to a directory?
    fn is_dir(&self, path: &Path) -> bool;

    /// Does path exist?
    fn exists(&self, path: &Path) -> bool;

    /// Open the file at path, returning a reader (dyn [`Read`]) or an Error.
    fn open<'a>(&'a self, path: &Path) -> Result<Box<dyn Read + 'a>, Error>;

    /// Get the current working directory.
    fn cwd(&self) -> Result<PathBuf, Error>;

    /// Normalize the path. Normalization converts, for example, /a/b/../c -> /a/c.
    fn normalize(&self, path: &Path) -> PathBuf {
        path.components()
            .fold(PathBuf::new(), |cur, component| match component {
                Component::Prefix(p) => Path::new(p.as_os_str()).to_path_buf(),
                Component::RootDir => Path::new("/").to_path_buf(),
                Component::CurDir => self.cwd().unwrap_or_else(|_| Path::new(".").to_path_buf()),
                Component::ParentDir => {
                    cur.parent().unwrap_or_else(|| Path::new("/")).to_path_buf()
                }
                Component::Normal(c) => cur.join(c),
            })
    }

    /// Is the provided path absolute?
    fn is_absolute_path(&self, path: &Path) -> bool {
        if cfg!(any(target_arch = "wasm32", target_arch = "wasm64")) {
            // wasm is_absolute checks path.has_root && path.prefix().is_some()
            // because the prefix check is only skipped for unix.
            // we don't care about the prefix.
            path.has_root()
        } else {
            path.is_absolute()
        }
    }
}

/// Regular filesystem
#[derive(Debug)]
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

/// In-memory filesystem
#[derive(Default, Debug)]
pub struct MemFs {
    cwd: Option<PathBuf>,
    paths: HashMap<PathBuf, FileEntry>,
}

#[derive(Debug)]
enum FileEntry {
    File(String),
    Dir(),
}

impl MemFs {
    /// Set the current working directory to `path`.
    pub fn set_cwd(&mut self, path: &Path) {
        self.cwd = Some(path.to_path_buf());
    }

    /// Add a regular file at `path` with contents, `contents`.
    pub fn add_file_at(&mut self, path: &Path, contents: String) {
        self.paths
            .insert(path.to_path_buf(), FileEntry::File(contents));
    }

    /// Add a directory at `path`.
    pub fn add_dir_at(&mut self, path: &Path) {
        self.paths.insert(path.to_path_buf(), FileEntry::Dir());
    }

    /// Remove the file or directory at `path`. If `path` is a directory, we do not perform a
    /// recursive deletion of contents.
    pub fn rm_at(&mut self, path: &Path) {
        self.paths.remove(path);
    }
}

impl Fs for MemFs {
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
            Some(FileEntry::File(contents)) => Ok(Box::new(contents.as_bytes()) as Box<dyn Read>),
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
