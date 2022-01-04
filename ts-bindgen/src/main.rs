use clap::Parser;
use std::fs::write as write_file;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{exit, Command, Stdio};
use std::str::from_utf8;
use ts_bindgen_gen::{generate_rust_for_typescript_with_file_processor, StdFs};
use which::which;

/// Generate wasm-bindgen Rust bindings for typescript definitions.
#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    /// Typescript input file path (e.g. my-library for my-library under $(pwd)/node_modules,
    /// ./ts/my-lib.d.ts for a local typescript declaration file).
    #[clap(name = "input-path")]
    ts_input_file_path: String,

    /// Rust wasm-bindgen output file. Writes to stdout if not specified.
    #[clap(short = 'o', long = "output", name = "rust-output-path")]
    rust_output_path: Option<String>,

    /// Skip running rustfmt on the generated bindings.
    #[clap(long = "skip-rustfmt", name = "skip-rustfmt")]
    skip_rustfmt: bool,

    /// Emit "cargo:rerun-if-changed=PATH" lines for each typescript file used as input.
    /// Ensures that, if used in a build script, rust bindings will be re-built if any typescript
    /// files change. See
    /// https://doc.rust-lang.org/cargo/reference/build-scripts.html#rerun-if-changed for more
    /// info.
    #[clap(long)]
    rerun_if_changed: bool,
}

fn main() {
    let args = Args::parse();

    if args.rust_output_path.is_none() && args.rerun_if_changed {
        eprintln!("--rust-output-path must be specified if --rerun-if-changed is set");
        exit(1);
    }

    let process_file = if args.rerun_if_changed {
        |f: &Path| {
            println!("cargo:rerun-if-changed={:?}", f.to_string_lossy());
        }
    } else {
        |_: &Path| {}
    };

    let rust = generate_rust_for_typescript_with_file_processor(
        StdFs,
        args.ts_input_file_path,
        process_file,
    )
    .to_string();

    let rust = if args.skip_rustfmt {
        rust
    } else {
        rustfmt_code(rust)
    };

    if let Some(out_path) = args.rust_output_path {
        if let Err(err) = write_file(&out_path, rust.to_string()) {
            eprintln!("Failed to write file {}: {}", out_path, err);
            exit(2);
        }
    } else {
        println!("{}", rust);
    }
}

fn rustfmt_code(code: String) -> String {
    match get_rustfmt_path() {
        None => code,
        Some(rustfmt) => {
            let mut proc = Command::new(rustfmt)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();

            let mut stdin = proc.stdin.take().expect("Failed to open rustfmt stdin");
            let stdin_code = code.clone();
            std::thread::spawn(move || {
                if let Err(err) = stdin.write_all(stdin_code.as_bytes()) {
                    eprintln!("Failed to write to rustfmt: {}", err);
                }
            });

            let result = proc
                .wait_with_output()
                .expect("Failed to read rustfmt stdout");
            if result.status.success() {
                from_utf8(&result.stdout).unwrap().to_owned()
            } else {
                eprintln!("Failed to run rustfmt: {:?}", result.status.code());
                code
            }
        }
    }
}

fn get_rustfmt_path() -> Option<PathBuf> {
    which("rustfmt").ok()
}
