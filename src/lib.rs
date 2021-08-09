extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Token, LitStr, Result as ParseResult};
use std::path::{PathBuf, Path};
use std::fs::File;
use std::ffi::OsStr;
use std::collections::{HashMap};
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
                    match get_file_with_any_ext(&import_path) {
                        Ok(import_path) => break Ok(import_path),
                        Err(err) => (), // fall through so that we iterate up the directory tree, looking for a higher-level node_modules folder
                    };
                }
            }

            if !path.pop() {
                break Err(std::io::Error::new(std::io::ErrorKind::NotFound, format!("Could not find node_modules directory starting at {}", cwd.display())))
            }
        }
    }
}

#[derive(Debug, Clone)]
enum TypeIdent {
    Name(String),
    DefaultExport(),
    AllExports(),
    QualifiedName(Vec<String>),
}

#[derive(Debug, Clone)]
struct TypeName {
    file: PathBuf,
    name: TypeIdent,
}

impl TypeName {
    fn default_export_for(file: PathBuf) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::DefaultExport(),
        }
    }

    fn all_exports_for(file: PathBuf) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::AllExports(),
        }
    }

    fn for_name(file: PathBuf, name: &str) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::Name(name.to_string()),
        }
    }

    fn for_qualified_name(file: PathBuf, names: Vec<String>) -> TypeName {
        TypeName {
            file,
            name: TypeIdent::QualifiedName(names),
        }
    }
}

#[derive(Default, Debug)]
struct Context {
    local_names_to_type_names: HashMap<String, TypeName>,
}

#[derive(Debug)]
enum TsType {
    TypeRef {
        type_name: TypeName,
    }
}

#[derive(Debug)]
enum TypeInfo {
    Interface {
        fields: HashMap<String, TsType>,
    }
}

#[derive(Debug)]
struct Type {
    name: String,
    is_exported: bool,
    info: TypeInfo,
}

#[derive(Default, Debug)]
struct TsTypes {
    types_by_name_by_file: HashMap<PathBuf, HashMap<String, Type>>,
    context_stack: Vec<Context>,
}

impl TsTypes {
    fn try_new(module_name: &str) -> Result<TsTypes, swc_ecma_parser::error::Error> {
        let mut tt: TsTypes = Default::default();
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

    fn process_module(&mut self, module_base: Option<PathBuf>, module_name: &str) -> Result<PathBuf, swc_ecma_parser::error::Error> {
        let ts_path = get_ts_path(module_base, &module_name, &typings_module_resolver).expect("TODO: Need to convert this exception type").canonicalize().expect("TODO: Need to convert this exception type");

        if self.types_by_name_by_file.insert(ts_path.clone(), Default::default()).is_some() {
            // already processed
            return Ok(ts_path);
        }

        let module = self.load_module(&ts_path)?;

        // TODO: would be cool to have a guard to ensure we pop
        self.context_stack.push(Default::default());

        for item in &module.body {
            match item {
                ModuleItem::ModuleDecl(decl) => self.process_module_decl(&ts_path, &decl),
                ModuleItem::Stmt(stmt) => self.process_stmt(&stmt),
            }
        }

        self.context_stack.pop();

        Ok(ts_path)
    }

    fn cur_ctx(&mut self) -> &mut Context {
        self.context_stack.last_mut().unwrap()
    }

    fn process_import_decl(&mut self, ts_path: &Path, import_decl: &ImportDecl) {
        let ImportDecl { specifiers, src, .. } = import_decl;
        let base = ts_path.parent().expect("All files must have a parent");
        let import = src.value.to_string();

        let file = self.process_module(Some(base.to_path_buf()), &import).expect("failed to process module");

        specifiers.into_iter().for_each(|specifier| {
            match specifier {
                ImportSpecifier::Named(ImportNamedSpecifier {
                    local, imported, ..
                }) => {
                    self.cur_ctx().local_names_to_type_names.insert(
                        local.sym.to_string(),
                        TypeName::for_name(
                            file.clone(),
                            &imported.as_ref().unwrap_or(local).sym
                        )
                    );
                },
                ImportSpecifier::Default(ImportDefaultSpecifier {
                    local, ..
                }) => {
                    self.cur_ctx().local_names_to_type_names.insert(
                        local.sym.to_string(),
                        TypeName::default_export_for(file.clone())
                    );
                },
                ImportSpecifier::Namespace(ImportStarAsSpecifier {
                    local, ..
                }) => {
                    self.cur_ctx().local_names_to_type_names.insert(
                        local.sym.to_string(),
                        TypeName::all_exports_for(file.clone())
                    );
                }
            }
        })
    }

    fn process_export_all(&mut self, ts_path: &Path, export_all: &ExportAll) {
        let s = export_all.src.value.to_string();
        let dir = ts_path.parent().expect("All files must have a parent");

        let file = self.process_module(Some(dir.to_path_buf()), &s).expect("failed to process mdoule");

        let names_to_add: Vec<(String, TypeName)> = self.types_by_name_by_file.get(&file).unwrap_or(&HashMap::new())
            .into_iter()
            .filter(|(_, typ)| typ.is_exported)
            .map(|(name, typ)| {
                (name.to_string(), TypeName::for_name(file.clone(), &typ.name))
            }).collect();
        
        let ctx = self.cur_ctx();
        names_to_add.into_iter()
            .for_each(|(name, typ)| {
                ctx.local_names_to_type_names.insert(name, typ);
            });
    }

    fn qualified_name_to_str_vec(&mut self, ts_path: &Path, qn: &TsQualifiedName) -> Vec<String> {
        let mut en = TsEntityName::TsQualifiedName(Box::new(qn.clone()));
        let mut names = Vec::new();

        loop {
            match en {
                TsEntityName::TsQualifiedName(qn) => {
                    names.push(qn.right.sym.to_string());
                    en = qn.left;
                },
                TsEntityName::Ident(Ident { sym, .. }) => {
                    names.push(sym.to_string());
                    break;
                }
            }
        }

        names.reverse();
        names
    }

    // TODO: move ts_path to context
    fn qualified_name_to_type_name(&mut self, ts_path: &Path, qn: &TsQualifiedName) -> TypeName {
        let name_path = self.qualified_name_to_str_vec(&ts_path, qn);
        TypeName::for_qualified_name(ts_path.to_path_buf(), name_path)
    }

    fn process_type_ref(&mut self, ts_path: &Path, TsTypeRef { type_name, type_params, .. }: &TsTypeRef) -> TsType {
        match type_name {
            TsEntityName::Ident(Ident { sym, .. }) => {
                TsType::TypeRef {
                    type_name: self.cur_ctx().local_names_to_type_names.get(&sym.to_string()).unwrap().clone()
                }
            },
            TsEntityName::TsQualifiedName(qn) => {
                TsType::TypeRef {
                    type_name: self.qualified_name_to_type_name(ts_path, qn)
                }
            }
        }
    }

    fn process_type(&mut self, ts_path: &Path, ts_type: &swc_ecma_ast::TsType) -> TsType {
        match ts_type {
            swc_ecma_ast::TsType::TsTypeRef(type_ref) => self.process_type_ref(ts_path, type_ref),
            // TODO: more cases
            _ => TsType::TypeRef {
                type_name: TypeName::default_export_for(ts_path.to_path_buf())
            }
        }
    }

    fn prop_key_to_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Lit(lit) => match lit {
                Lit::Str(s) => Some(s.value.to_string()),
                _ => {
                    println!("We only handle string properties. Received {:?}", lit);
                    None
                }
            },
            Expr::Ident(Ident { sym, .. }) => Some(sym.to_string()),
            _ => {
                println!("We only handle literal and identifier properties. Received {:?}", expr);
                None
            }
        }
    }

    fn process_ts_interface(&mut self, ts_path: &Path, TsInterfaceDecl { id, type_params, extends, body, .. }: &TsInterfaceDecl) -> Type {
        Type {
            name: id.sym.to_string(),
            is_exported: false,
            info: TypeInfo::Interface {
                fields: body.body.iter().filter_map(|el| match el {
                    TsTypeElement::TsPropertySignature(TsPropertySignature {
                        key, type_ann, ..
                    }) => {
                        type_ann.as_ref().and_then(|t| {
                            self.prop_key_to_name(key)
                                .map(|n| (n, self.process_type(ts_path, &t.type_ann)))
                        })
                    },
                    // TODO: add other variants
                    _ => Some(("a".to_string(), TsType::TypeRef { type_name: TypeName::for_name(ts_path.to_path_buf(), "hello") }))
                }).collect()
            }
        }
    }

    fn process_export_decl(&mut self, ts_path: &Path, export_decl: &ExportDecl) {
        let ExportDecl { decl, .. } = export_decl;

        let mut typ = match decl {
            Decl::TsInterface(iface) =>  self.process_ts_interface(ts_path, iface),
            _ => 
                Type {
                    name: "hi".to_string(),
                    is_exported: false,
                    info: TypeInfo::Interface {
                        fields: HashMap::new()
                    }
                }
        };

        typ.is_exported = true;
        let type_name = typ.name.to_string();

        self.types_by_name_by_file
            .entry(ts_path.to_path_buf())
            .or_insert(HashMap::new())
            .insert(type_name.to_string(), typ);
        self.cur_ctx().local_names_to_type_names.insert(type_name.to_string(), TypeName::for_name(ts_path.to_path_buf(), &type_name));
    }

    fn process_module_decl(&mut self, ts_path: &Path, module_decl: &ModuleDecl) {
        match module_decl {
            ModuleDecl::Import(decl) => self.process_import_decl(&ts_path, &decl),
            ModuleDecl::ExportDecl(decl) => self.process_export_decl(&ts_path, &decl),
            ModuleDecl::ExportNamed(decl) => (),
            ModuleDecl::ExportDefaultDecl(decl) => (),
            ModuleDecl::ExportDefaultExpr(decl) => (),
            ModuleDecl::ExportAll(decl) => self.process_export_all(&ts_path, &decl),
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
