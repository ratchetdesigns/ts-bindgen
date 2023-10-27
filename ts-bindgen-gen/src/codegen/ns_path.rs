use crate::fs::Fs;
use crate::identifier::{make_identifier, Identifier};
use crate::mod_def::ToModPathIter;

pub trait ToNsPath<T: ?Sized> {
    fn to_ns_path<FS: Fs + ?Sized>(&self, fs: &FS, current_mod: &T) -> Vec<Identifier>;
}

impl<T, U> ToNsPath<T> for U
where
    T: ToModPathIter + ?Sized,
    U: ToModPathIter + ?Sized,
{
    fn to_ns_path<FS: Fs + ?Sized>(&self, fs: &FS, current_mod: &T) -> Vec<Identifier> {
        let mut cur_mod_path = current_mod.to_mod_path_iter(fs);
        let mut target_mod_path = self.to_mod_path_iter(fs);

        let mut ns: Vec<Identifier> = Default::default();

        // we skip all of the common elements of cur and target
        // then, we add in a "super" for each remaining element of cur
        // then, we add in all of the remaining elements of target

        loop {
            let cur = cur_mod_path.next();
            let tar = target_mod_path.next();

            if tar.is_none() {
                if cur.is_some() {
                    ns.push(make_identifier!(super));
                }
                for _ in cur_mod_path {
                    ns.push(make_identifier!(super));
                }
                break;
            }

            let tar = tar.unwrap();

            if cur.is_none() {
                ns.push(tar);
                ns.extend(&mut target_mod_path);
                break;
            }

            let cur = cur.unwrap();

            if cur == tar {
                // skip any shared prefix
                continue;
            }

            // from this point on, current and target vary.
            // add supers for all remaining currents to get us up to the fork
            // and then append target
            ns.push(make_identifier!(super));
            for _ in cur_mod_path {
                ns.push(make_identifier!(super));
            }

            ns.push(tar);
            ns.extend(&mut target_mod_path);
            break;
        }

        ns
    }
}

#[cfg(test)]
mod test {
    use super::ToNsPath;
    use crate::fs::MemFs;
    use crate::generators::*;
    use crate::identifier::{make_identifier, to_ns_name};
    use crate::ir::TypeIdent;
    use proptest::prelude::*;
    use std::path::Path;

    proptest! {
        #[test]
        fn test_to_ns_path_for_sub_mod(
            prefix in arb_abs_path(),
            ns_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let cur = TypeIdent::Name {
                file: prefix_path.join(&ns_rest).to_path_buf(),
                name: "Hi".to_string(),
            };

            let expected: Vec<_> = ns_rest.split('/')
                .map(to_ns_name)
                .collect();

            prop_assert_eq!(
                cur.to_ns_path(&fs, prefix_path),
                expected
            );
        }
    }

    proptest! {
        #[test]
        fn test_to_ns_path_for_super_mod(
            prefix in arb_abs_path(),
            ns_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let full_path = prefix_path.join(&ns_rest);
            let cur = TypeIdent::Name {
                file: prefix_path.to_path_buf(),
                name: "Hi".to_string(),
            };

            let expected: Vec<_> = ns_rest.split('/')
                .map(|_| make_identifier!(super))
                .collect();

            prop_assert_eq!(
                cur.to_ns_path(&fs, full_path.as_path()),
                expected
            );
        }
    }

    proptest! {
        #[test]
        fn test_to_ns_path_for_branch_mod(
            prefix in arb_abs_path(),
            cur_rest in arb_rel_path(),
            target_rest in arb_rel_path(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let cur = TypeIdent::Name {
                file: prefix_path.join(&cur_rest).to_path_buf(),
                name: "Hi".to_string(),
            };
            let full_path = prefix_path.join(&target_rest);

            let cur_path = Path::new(&cur_rest);
            let tar_path = Path::new(&target_rest);

            let expected = cur_path.to_ns_path(&fs, tar_path);

            prop_assert_eq!(
                cur.to_ns_path(&fs, full_path.as_path()),
                expected
            );
        }
    }

    proptest! {
        #[test]
        fn test_to_ns_path_for_same_mod(
            prefix in arb_abs_path(),
            cur_suffix in arb_path_part(),
            tar_suffix in arb_path_part(),
        ) {
            let fs = {
                let mut fs: MemFs = Default::default();
                fs.set_cwd(Path::new("/"));
                fs
            };
            let prefix_path = Path::new(&prefix);
            let cur = TypeIdent::Name {
                file: prefix_path.to_path_buf(),
                name: cur_suffix,
            };

            let tar = TypeIdent::Name {
                file: prefix_path.to_path_buf(),
                name: tar_suffix,
            };

            prop_assert!(cur.to_ns_path(&fs, &tar).is_empty());
        }
    }
}
