use std::path::Path;

/// Filesystem abstraction
pub trait Fs {
    fn is_file(&self, path: &Path) -> bool;
}

/// Regular filesystem
pub struct StdFs;

impl Fs for StdFs {
    fn is_file(&self, path: &Path) -> bool {
        path.is_file()
    }
}

#[cfg(test)]
pub mod test {
    pub use super::Fs;
    use std::collections::HashMap;
    use std::path::{Path, PathBuf};

    /// Testing filesystem
    #[derive(Default)]
    pub struct TestFs {
        paths: HashMap<PathBuf, FileEntry>,
    }

    enum FileEntry {
        File(),
        Dir(),
    }

    impl TestFs {
        pub fn add_file_at(&mut self, path: &Path) {
            self.paths.insert(path.to_path_buf(), FileEntry::File());
        }
    }

    impl Fs for TestFs {
        fn is_file(&self, path: &Path) -> bool {
            matches!(self.paths.get(path), Some(FileEntry::File()))
        }
    }
}
