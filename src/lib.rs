extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Token, LitStr, Result as ParseResult};
use std::path::{PathBuf, Path};
use std::fs::File;
use serde_json::Value;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};
use swc_ecma_ast::*;
use swc_common::{sync::Lrc, SourceMap};

#[proc_macro]
pub fn import_ts(input: TokenStream) -> TokenStream {
    println!("HERE: {:?}", input);
    let import_args = parse_macro_input!(input as ImportArgs);
    import_args.modules.into_iter().map(|module| {
        let import = get_ts_path(&module)?;
        println!("IMPORT {:?}", import);
        load_ts_defs(import.as_path());
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

fn get_ts_path(import: &str) -> std::io::Result<PathBuf> {
    let cwd = std::env::current_dir()?;
    let mut path = cwd.clone();

    if import.starts_with(".") {
        Ok(path.join(import))
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
                    let types_rel_path = pkg.as_object()
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected top-level object) found in {}", pkg_json_path.display())))?
                        .get("types")
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' property) found in {}", pkg_json_path.display())))?
                        .as_str()
                        .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("Bad package.json (expected 'types' to be a string) found in {}", pkg_json_path.display())))?;

                    let types_path = import_path.join(types_rel_path);
                    if !types_path.is_file() {
                        break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Package.json, {}, specified non-existent file for types, {}", pkg_json_path.display(), types_path.display())))
                    }

                    break Ok(types_path)
                } else if import_path.exists() {
                    // must be a module + file path
                    break Ok(import_path)
                } else {
                    break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find module, {}, in node_modules directory, {}", import, possible_node_modules.display())))
                }
            } else {
                if !path.pop() {
                    break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find node_modules directory starting at {}", cwd.display())))
                }
            }
        }
    }
}

fn load_ts_defs(ts_path: &Path) -> Result<TsTypes, swc_ecma_parser::error::Error> {
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
    let tt = TsTypes::new(ts_path, &module);
    println!("MOD!, {:?}", module);

    Ok(tt)
}

struct Typ {
    defined_in: PathBuf,
}

struct TsTypes {
    import_path: PathBuf,
    types: Vec<Typ>,
}

impl TsTypes {
    fn new(import_path: &Path, module: &Module) -> TsTypes {
        let mut tt = TsTypes {
            import_path: PathBuf::from(import_path),
            types: Default::default(),
        };
        tt.process_module(module);
        tt
    }

    fn process_module(&mut self, module: &Module) {
        for item in &module.body {
            match item {
                ModuleItem::ModuleDecl(decl) => self.process_module_decl(&decl),
                ModuleItem::Stmt(stmt) => self.process_stmt(&stmt),
            }
        }
    }

    fn process_module_decl(&mut self, module_decl: &ModuleDecl) {
        match module_decl {
            ModuleDecl::Import(decl) => (),
            ModuleDecl::ExportDecl(decl) => (),
            ModuleDecl::ExportNamed(decl) => (),
            ModuleDecl::ExportDefaultDecl(decl) => (),
            ModuleDecl::ExportDefaultExpr(decl) => (),
            ModuleDecl::ExportAll(decl) => (),
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
