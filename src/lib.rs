extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Token, LitStr, Result as ParseResult};
use std::path::{PathBuf, Path};
use std::fs::File;
use std::ffi::OsStr;
use serde_json::Value;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_ast::*;
use swc_common::{sync::Lrc, SourceMap};

#[proc_macro]
pub fn import_ts(input: TokenStream) -> TokenStream {
    println!("HERE: {:?}", input);
    let import_args = parse_macro_input!(input as ImportArgs);
    import_args.modules.into_iter().map(|module| {
        let tt = TsTypes::try_new(&module).expect("tt error");
        Ok("hi".to_string())
    }).collect::<Vec<Result<String, std::io::Error>>>();
    (quote! {
        struct Hello {}
    }).into()
}

struct ImportArgs {
    modules: Vec<String>,
}

impl Parse for ImportArgs {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let modules = Punctuated::<LitStr, Token![,]>::parse_terminated(input)?;
        Ok(ImportArgs {
            modules: modules.into_iter().map(|m| m.value()).collect(),
        })
    }
}

fn typings_module_resolver(import_path: &Path, pkg: &Value) -> std::io::Result<PathBuf> {
    let types_rel_path = pkg.as_object()
        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected top-level object) found in {}", import_path.display())))?
        .get("types")
        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' property) found in {}", import_path.display())))?
        .as_str()
        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' to be a string) found in {}", import_path.display())))?;

    let types_path = import_path.join(types_rel_path);
    if types_path.is_file() {
        Ok(types_path)
    } else {
        Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Package.json in {} specified non-existent file for types, {}", import_path.display(), types_path.display())))
    }
}

fn path_with_ext_appended(path: &Path, ext: &str) -> PathBuf {
    path.with_file_name(
        format!(
            "{}.{}",
            path.file_name().unwrap_or(OsStr::new("")).to_str().unwrap_or(""),
            ext
        )
    )
}

fn get_file_with_any_ext(path: &Path) -> std::io::Result<PathBuf> {
    let exts = vec!["d.ts", "ts", "tsx", "js", "jsx", "json"];
    exts.iter()
        .map(|ext| path_with_ext_appended(path, ext))
        .find(|path_with_ext| path_with_ext.is_file())
        .ok_or(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find module with any extension, {}", path.display())))
}

fn get_ts_path(module_base: Option<PathBuf>, import: &str, module_resolver: &dyn Fn(&Path, &Value) -> std::io::Result<PathBuf>) -> std::io::Result<PathBuf> {
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
                    break module_resolver(&import_path, &pkg)
                } else if import_path.exists() {
                    // must be a module + file path
                    break Ok(import_path)
                } else {
                    // check with different file extensions
                    break get_file_with_any_ext(&import_path)

                    //break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find module, {}, in node_modules directory, {}", import, possible_node_modules.display())))
                }
            } else {
                if !path.pop() {
                    break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find node_modules directory starting at {}", cwd.display())))
                }
            }
        }
    }
}

struct Typ {
    defined_in: PathBuf,
    mod_path: Vec<String>,
}

struct TsTypes {
    types: Vec<Typ>,
}

impl TsTypes {
    fn try_new(module_name: &str) -> Result<TsTypes, swc_ecma_parser::error::Error> {
        let mut tt = TsTypes {
            types: Default::default(),
        };
        tt.process_module(None, module_name)?;
        Ok(tt)
    }

    fn load_module(&mut self, ts_path: &Path) -> Result<Module, swc_ecma_parser::error::Error> {
        let cm: Lrc<SourceMap> = Default::default();
        let fm = cm.load_file(ts_path).expect("Can't load file");
        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig {
                tsx: true,
                decorators: true,
                dynamic_import: true,
                dts: true,
                no_early_errors: true,
                import_assertions: true,
            }),
            Default::default(),
            StringInput::from(&*fm),
            None,
        );

        let mut parser = Parser::new_from(lexer);
        let module = parser.parse_typescript_module()?;
        if ts_path.file_name() == Some(OsStr::new("aspect.d.ts")) {
            println!("MOD!, {:?}", module);
        }

        Ok(module)
    }

    fn process_module(&mut self, module_base: Option<PathBuf>, module_name: &str) -> Result<(), swc_ecma_parser::error::Error> {
        let ts_path = get_ts_path(module_base, &module_name, &typings_module_resolver).expect("TODO: Need to convert this exception type");
        let module = self.load_module(&ts_path)?;

        for item in &module.body {
            match item {
                ModuleItem::ModuleDecl(decl) => self.process_module_decl(&ts_path, &decl),
                ModuleItem::Stmt(stmt) => self.process_stmt(&stmt),
            }
        }
        Ok(())
    }

    fn process_import_decl(&mut self, import_decl: &ImportDecl) {
        let ImportDecl { specifiers, src, .. } = import_decl;

    }

    fn process_module_decl(&mut self, ts_path: &Path, module_decl: &ModuleDecl) {
        match module_decl {
            ModuleDecl::Import(decl) => self.process_import_decl(&decl),
            ModuleDecl::ExportDecl(decl) => (),
            ModuleDecl::ExportNamed(decl) => (),
            ModuleDecl::ExportDefaultDecl(decl) => (),
            ModuleDecl::ExportDefaultExpr(decl) => (),
            ModuleDecl::ExportAll(decl) => {
                let s = decl.src.value.to_string();
                let dir = ts_path.parent().expect("All files must have a parent");
                self.process_module(Some(dir.to_path_buf()), &s);
                ()
            },
            ModuleDecl::TsImportEquals(decl) => (),
            ModuleDecl::TsExportAssignment(decl) => (),
            ModuleDecl::TsNamespaceExport(decl) => (),
        }
    }

    fn process_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(BlockStmt) => (),
            Stmt::Empty(EmptyStmt) => (),
            Stmt::Debugger(DebuggerStmt) => (),
            Stmt::With(WithStmt) => (),
            Stmt::Return(ReturnStmt) => (),
            Stmt::Labeled(LabeledStmt) => (),
            Stmt::Break(BreakStmt) => (),
            Stmt::Continue(ContinueStmt) => (),
            Stmt::If(IfStmt) => (),
            Stmt::Switch(SwitchStmt) => (),
            Stmt::Throw(ThrowStmt) => (),
            Stmt::Try(TryStmt) => (),
            Stmt::While(WhileStmt) => (),
            Stmt::DoWhile(DoWhileStmt) => (),
            Stmt::For(ForStmt) => (),
            Stmt::ForIn(ForInStmt) => (),
            Stmt::ForOf(ForOfStmt) => (),
            Stmt::Decl(Decl) => (),
            Stmt::Expr(ExprStmt) => (),
        }
    }
}
