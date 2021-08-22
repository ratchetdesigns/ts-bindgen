use crate::ir::{
    BaseClass, Class, Ctor, EnumMember, Func, Indexer, Interface, Member, NamespaceImport, Param,
    Type, TypeIdent, TypeInfo, TypeName, TypeRef,
};
use crate::module_resolution::{get_ts_path, typings_module_resolver};
use std::collections::{hash_map::Entry, HashMap};
use std::path::{Path, PathBuf};
use swc_common::{sync::Lrc, SourceMap};
use swc_ecma_ast::*;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

#[derive(Default, Debug)]
pub struct TsTypes {
    pub types_by_name_by_file: HashMap<PathBuf, HashMap<TypeIdent, Type>>,
    pub namespace_stack: Vec<Vec<String>>,
}

trait TypeRefExt {
    fn entity_name(&self) -> &TsEntityName;
    fn type_args(&self) -> &Option<TsTypeParamInstantiation>;
}

impl TypeRefExt for TsTypeRef {
    fn entity_name(&self) -> &TsEntityName {
        &self.type_name
    }

    fn type_args(&self) -> &Option<TsTypeParamInstantiation> {
        &self.type_params
    }
}

impl TypeRefExt for TsExprWithTypeArgs {
    fn entity_name(&self) -> &TsEntityName {
        &self.expr
    }

    fn type_args(&self) -> &Option<TsTypeParamInstantiation> {
        &self.type_args
    }
}

impl<'a> TypeRefExt for ClassSuperTypeRef<'a> {
    fn entity_name(&self) -> &TsEntityName {
        &self.entity_name
    }

    fn type_args(&self) -> &Option<TsTypeParamInstantiation> {
        self.type_args
    }
}

struct ClassSuperTypeRef<'a> {
    entity_name: TsEntityName,
    type_args: &'a Option<TsTypeParamInstantiation>,
}

impl<'a> ClassSuperTypeRef<'a> {
    fn from(class: &'a swc_ecma_ast::Class) -> Option<Self> {
        class
            .super_class
            .as_ref()
            .and_then(|sc| {
                if let Expr::Ident(ident) = &**sc {
                    Some(ident)
                } else {
                    None
                }
            })
            .map(|sc| ClassSuperTypeRef {
                entity_name: sc.clone().into(),
                type_args: &class.super_type_params,
            })
    }
}

struct Source<'a, T> {
    ts_types: &'a mut TsTypes,
    ts_path: &'a Path,
    node: &'a T,
}

impl<'a, T> Source<'a, T> {
    fn from(ts_types: &'a mut TsTypes, ts_path: &'a Path, node: &'a T) -> Source<'a, T> {
        Source {
            ts_types,
            ts_path,
            node,
        }
    }
}

impl<'a, T: TypeRefExt> From<Source<'a, T>> for TypeRef {
    fn from(source: Source<'a, T>) -> TypeRef {
        let Source {
            ts_types,
            ts_path,
            node,
        } = source;

        match node.entity_name() {
            TsEntityName::Ident(Ident { sym, .. }) => TypeRef {
                referent: TypeName::for_name(ts_path.to_path_buf(), &sym.to_string()),
                type_params: node
                    .type_args()
                    .as_ref()
                    .map(|tps| {
                        tps.params
                            .iter()
                            .map(|tp| ts_types.process_type(ts_path, tp))
                            .collect()
                    })
                    .unwrap_or_default(),
            },
            TsEntityName::TsQualifiedName(qn) => TypeRef {
                referent: ts_types.qualified_name_to_type_name(ts_path, qn),
                type_params: node
                    .type_args()
                    .as_ref()
                    .map(|tps| {
                        tps.params
                            .iter()
                            .map(|tp| ts_types.process_type(ts_path, tp))
                            .collect()
                    })
                    .unwrap_or_default(),
            },
        }
    }
}

trait KeyedExt {
    fn key(&self) -> Option<String>;
}

trait ExprKeyed {
    fn expr_key(&self) -> &Expr;
}

impl<T: ExprKeyed> KeyedExt for T {
    fn key(&self) -> Option<String> {
        let expr = self.expr_key();
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
                println!(
                    "We only handle literal and identifier properties. Received {:?}",
                    expr
                );
                None
            }
        }
    }
}

impl ExprKeyed for TsPropertySignature {
    fn expr_key(&self) -> &Expr {
        &*self.key
    }
}

impl ExprKeyed for TsMethodSignature {
    fn expr_key(&self) -> &Expr {
        &*self.key
    }
}

impl ExprKeyed for ClassProp {
    fn expr_key(&self) -> &Expr {
        &*self.key
    }
}

impl KeyedExt for PropName {
    fn key(&self) -> Option<String> {
        match self {
            PropName::Ident(ident) => Some(ident.sym.to_string()),
            PropName::Str(s) => Some(s.value.to_string()),
            PropName::Num(n) => Some(n.value.to_string()),
            _ => None,
        }
    }
}

impl KeyedExt for ClassMethod {
    fn key(&self) -> Option<String> {
        self.key.key()
    }
}

impl KeyedExt for Constructor {
    fn key(&self) -> Option<String> {
        self.key.key()
    }
}

trait PropExt {
    fn item_type(&self) -> Option<&TsType>;

    fn is_optional(&self) -> bool;

    fn to_type_info(&self, ts_path: &Path, ts_types: &mut TsTypes) -> TypeInfo {
        self.item_type()
            .map(|t| {
                let item_type = ts_types.process_type(ts_path, t);
                if self.is_optional() {
                    TypeInfo::Optional {
                        item_type: Box::new(item_type),
                    }
                } else {
                    item_type
                }
            })
            .unwrap_or(TypeInfo::PrimitiveAny {})
    }
}

impl PropExt for TsPropertySignature {
    fn item_type(&self) -> Option<&TsType> {
        self.type_ann.as_ref().map(|t| &*t.type_ann)
    }

    fn is_optional(&self) -> bool {
        self.optional
    }
}

impl PropExt for ClassProp {
    fn item_type(&self) -> Option<&TsType> {
        self.type_ann.as_ref().map(|t| &*t.type_ann)
    }

    fn is_optional(&self) -> bool {
        self.is_optional
    }
}

trait FnParamExt {
    fn to_param(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Param;
}

impl FnParamExt for TsFnParam {
    fn to_param(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Param {
        match self {
            TsFnParam::Ident(ident) => Param {
                name: ident.id.sym.to_string(),
                is_variadic: false,
                type_info: ident
                    .type_ann
                    .as_ref()
                    .map(|t| ts_types.process_type(ts_path, &t.type_ann))
                    .unwrap_or(TypeInfo::PrimitiveAny {}),
            },
            _ => panic!("we only support ident params for methods"),
        }
    }
}

impl FnParamExt for swc_ecma_ast::Param {
    fn to_param(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Param {
        match &self.pat {
            Pat::Ident(BindingIdent { id, type_ann }) => Param {
                name: id.sym.to_string(),
                is_variadic: false,
                type_info: type_ann
                    .as_ref()
                    .map(|t| ts_types.process_type(ts_path, &*t.type_ann))
                    .unwrap_or(TypeInfo::PrimitiveAny {}),
            },
            Pat::Rest(RestPat { arg, type_ann, .. }) => match &**arg {
                Pat::Ident(id_param) => Param {
                    name: id_param.id.sym.to_string(),
                    is_variadic: true,
                    type_info: type_ann
                        .as_ref()
                        .map(|t| ts_types.process_type(ts_path, &t.type_ann))
                        .unwrap_or(TypeInfo::PrimitiveAny {}),
                },
                _ => {
                    println!("found rest param arg {:?}", &arg);
                    panic!("we only handle idents in rest patterns");
                }
            },
            _ => panic!("we only support ident params for methods"),
        }
    }
}

impl FnParamExt for BindingIdent {
    fn to_param(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Param {
        Param {
            name: self.id.sym.to_string(),
            is_variadic: false,
            type_info: self
                .type_ann
                .as_ref()
                .map(|t| ts_types.process_type(ts_path, &*t.type_ann))
                .unwrap_or(TypeInfo::PrimitiveAny {}),
        }
    }
}

trait CtorExt {
    fn to_ctor(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Ctor;
}

impl CtorExt for Constructor {
    fn to_ctor(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Ctor {
        Ctor {
            params: self
                .params
                .iter()
                .map(|p| match p {
                    ParamOrTsParamProp::Param(p) => p.to_param(ts_path, ts_types),
                    ParamOrTsParamProp::TsParamProp(tsp) => match &tsp.param {
                        TsParamPropParam::Ident(id) => id.to_param(ts_path, ts_types),
                        TsParamPropParam::Assign(_a) => {
                            panic!("we don't handle assignment params yet")
                        }
                    },
                })
                .collect(),
        }
    }
}

trait FuncExt {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Vec<Param>;

    fn type_params(&self) -> &Option<TsTypeParamDecl>;

    fn return_type(&self) -> Option<&TsType>;

    fn to_func(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Func {
        Func {
            params: self.params(ts_path, ts_types),
            type_params: ts_types.process_fn_type_params(ts_path, self.type_params()),
            return_type: Box::new(
                self.return_type()
                    .map(|t| ts_types.process_type(ts_path, t))
                    .unwrap_or(TypeInfo::PrimitiveAny {}),
            ),
        }
    }

    fn to_type_info(&self, ts_path: &Path, ts_types: &mut TsTypes) -> TypeInfo {
        TypeInfo::Func(self.to_func(ts_path, ts_types))
    }
}

impl FuncExt for TsMethodSignature {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Vec<Param> {
        self.params
            .iter()
            .map(|param| param.to_param(ts_path, ts_types))
            .collect()
    }

    fn type_params(&self) -> &Option<TsTypeParamDecl> {
        &self.type_params
    }

    fn return_type(&self) -> Option<&TsType> {
        self.type_ann.as_ref().map(|t| &*t.type_ann)
    }
}

impl FuncExt for TsFnType {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Vec<Param> {
        self.params
            .iter()
            .map(|param| param.to_param(ts_path, ts_types))
            .collect()
    }

    fn type_params(&self) -> &Option<TsTypeParamDecl> {
        &self.type_params
    }

    fn return_type(&self) -> Option<&TsType> {
        Some(&self.type_ann.type_ann)
    }
}

impl FuncExt for Function {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Vec<Param> {
        self.params
            .iter()
            .map(|param| param.to_param(ts_path, ts_types))
            .collect()
    }

    fn type_params(&self) -> &Option<TsTypeParamDecl> {
        &self.type_params
    }

    fn return_type(&self) -> Option<&TsType> {
        self.return_type.as_ref().map(|t| &*t.type_ann)
    }
}

impl FuncExt for ClassMethod {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Vec<Param> {
        self.function
            .params
            .iter()
            .map(|param| param.to_param(ts_path, ts_types))
            .collect()
    }

    fn type_params(&self) -> &Option<TsTypeParamDecl> {
        &self.function.type_params
    }

    fn return_type(&self) -> Option<&TsType> {
        self.function.return_type.as_ref().map(|t| &*t.type_ann)
    }
}

trait IndexerExt {
    fn is_readonly(&self) -> bool;

    fn value_type(&self) -> Option<&TsType>;

    fn to_indexer(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Indexer {
        Indexer {
            readonly: self.is_readonly(),
            type_info: Box::new(
                self.value_type()
                    .map(|t| ts_types.process_type(ts_path, t))
                    .unwrap_or(TypeInfo::PrimitiveAny {}),
            ),
        }
    }
}

impl IndexerExt for TsIndexSignature {
    fn is_readonly(&self) -> bool {
        self.readonly
    }

    fn value_type(&self) -> Option<&TsType> {
        if self.params.len() != 1 {
            panic!("indexing signatures should only have 1 param");
        }

        match self.params.first().unwrap() {
            TsFnParam::Ident(ident) => ident.type_ann.as_ref().map(|t| &*t.type_ann),
            _ => panic!("we only support ident indexers"),
        }
    }
}

impl TsTypes {
    pub fn try_new(module_name: &str) -> Result<TsTypes, swc_ecma_parser::error::Error> {
        let mut tt: TsTypes = Default::default();
        tt.process_module(None, module_name)?;

        let mut resolved_types_by_name_by_file: HashMap<PathBuf, HashMap<TypeIdent, Type>> =
            HashMap::new();
        for (file, types_by_name) in &tt.types_by_name_by_file {
            let resolved = types_by_name
                .iter()
                .map(|(n, typ)| (n.clone(), typ.resolve_names(&tt.types_by_name_by_file)))
                .collect();
            resolved_types_by_name_by_file.insert(file.clone(), resolved);
        }

        tt.types_by_name_by_file = resolved_types_by_name_by_file;

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
        if ts_path.to_string_lossy().contains("hello.d.ts") {
            println!("MOD!, {:?}", module);
        }

        Ok(module)
    }

    fn process_module_item(&mut self, ts_path: &Path, item: &ModuleItem) {
        match item {
            ModuleItem::ModuleDecl(decl) => self.process_module_decl(ts_path, decl),
            ModuleItem::Stmt(stmt) => self.process_stmt(ts_path, stmt),
        }
    }

    fn process_module_items(&mut self, ts_path: &Path, items: &[ModuleItem]) {
        for item in items {
            self.process_module_item(ts_path, item);
        }
    }

    fn process_module(
        &mut self,
        module_base: Option<PathBuf>,
        module_name: &str,
    ) -> Result<PathBuf, swc_ecma_parser::error::Error> {
        let ts_path = get_ts_path(module_base, module_name, &typings_module_resolver)
            .expect("TODO: Need to convert this exception type")
            .canonicalize()
            .expect("TODO: Need to convert this exception type");

        match self.types_by_name_by_file.entry(ts_path.clone()) {
            Entry::Occupied(_) => return Ok(ts_path),
            Entry::Vacant(v) => {
                v.insert(Default::default());
            }
        }

        let module = self.load_module(&ts_path)?;
        self.process_module_items(&ts_path, &module.body);

        Ok(ts_path)
    }

    fn set_type_for_name_for_file(&mut self, file: &Path, name: TypeIdent, typ: Type) {
        match self.namespace_stack.last() {
            Some(ns) => {
                let mut ns = ns.clone();
                match name {
                    TypeIdent::Name(s) => {
                        ns.push(s);
                    }
                    TypeIdent::DefaultExport() => panic!("default export within namespace"),
                    TypeIdent::QualifiedName(mut name) => {
                        ns.append(&mut name);
                    }
                }

                self.types_by_name_by_file
                    .entry(file.to_path_buf())
                    .and_modify(|names_to_types: &mut HashMap<TypeIdent, Type>| {
                        names_to_types.insert(TypeIdent::QualifiedName(ns), typ);
                    });
            }
            None => {
                self.types_by_name_by_file
                    .entry(file.to_path_buf())
                    .and_modify(|names_to_types: &mut HashMap<TypeIdent, Type>| {
                        names_to_types.insert(name.clone(), typ);
                    });
            }
        }
    }

    fn process_import_decl(
        &mut self,
        ts_path: &Path,
        ImportDecl {
            specifiers, src, ..
        }: &ImportDecl,
    ) {
        let base = ts_path.parent().expect("All files must have a parent");
        let import = src.value.to_string();

        let file = self
            .process_module(Some(base.to_path_buf()), &import)
            .expect("failed to process module");

        specifiers.iter().for_each(|specifier| match specifier {
            ImportSpecifier::Named(ImportNamedSpecifier {
                local, imported, ..
            }) => {
                self.set_type_for_name_for_file(
                    ts_path,
                    TypeIdent::Name(local.sym.to_string()),
                    Type {
                        name: TypeName::for_name(ts_path, &local.sym.to_string()),
                        is_exported: false,
                        info: TypeInfo::NamespaceImport(NamespaceImport::Named {
                            src: file.to_path_buf(),
                            name: imported.as_ref().unwrap_or(local).sym.to_string(),
                        }),
                    },
                );
            }
            ImportSpecifier::Default(ImportDefaultSpecifier { local, .. }) => {
                self.set_type_for_name_for_file(
                    ts_path,
                    TypeIdent::Name(local.sym.to_string()),
                    Type {
                        name: TypeName::for_name(ts_path, &local.sym.to_string()),
                        is_exported: false,
                        info: TypeInfo::NamespaceImport(NamespaceImport::Default {
                            src: file.to_path_buf(),
                        }),
                    },
                );
            }
            ImportSpecifier::Namespace(ImportStarAsSpecifier { local, .. }) => {
                self.set_type_for_name_for_file(
                    ts_path,
                    TypeIdent::Name(local.sym.to_string()),
                    Type {
                        name: TypeName::for_name(ts_path, &local.sym.to_string()),
                        is_exported: false,
                        info: TypeInfo::NamespaceImport(NamespaceImport::All {
                            src: file.to_path_buf(),
                        }),
                    },
                );
            }
        })
    }

    fn process_export_all(&mut self, ts_path: &Path, export_all: &ExportAll) {
        let s = export_all.src.value.to_string();
        let dir = ts_path.parent().expect("All files must have a parent");

        let file = self
            .process_module(Some(dir.to_path_buf()), &s)
            .expect("failed to process module");

        let to_export = self
            .types_by_name_by_file
            .get(&file)
            .expect("should have processed file already")
            .iter()
            .filter(|(_, t)| t.is_exported)
            .filter_map(|(n, t)| match n {
                n @ TypeIdent::Name(_) => Some((n.clone(), t.clone())),
                _ => None,
            })
            .collect::<HashMap<TypeIdent, Type>>();

        to_export.into_iter().for_each(|(name, typ)| {
            self.set_type_for_name_for_file(ts_path, name, typ);
        });
    }

    fn qualified_name_to_str_vec(&mut self, _ts_path: &Path, qn: &TsQualifiedName) -> Vec<String> {
        let mut en = TsEntityName::TsQualifiedName(Box::new(qn.clone()));
        let mut names = Vec::new();

        loop {
            match en {
                TsEntityName::TsQualifiedName(qn) => {
                    names.push(qn.right.sym.to_string());
                    en = qn.left;
                }
                TsEntityName::Ident(Ident { sym, .. }) => {
                    names.push(sym.to_string());
                    break;
                }
            }
        }

        names.reverse();
        names
    }

    fn qualified_name_to_type_name(&mut self, ts_path: &Path, qn: &TsQualifiedName) -> TypeName {
        let name_path = self.qualified_name_to_str_vec(ts_path, qn);
        TypeName::for_qualified_name(ts_path.to_path_buf(), name_path)
    }

    fn process_type_ref(&mut self, ts_path: &Path, ts_type_ref: &TsTypeRef) -> TypeInfo {
        TypeInfo::Ref(Source::from(self, ts_path, ts_type_ref).into())
    }

    fn process_keyword_type(
        &mut self,
        _ts_path: &Path,
        TsKeywordType { kind, .. }: &TsKeywordType,
    ) -> TypeInfo {
        match kind {
            TsKeywordTypeKind::TsAnyKeyword => TypeInfo::PrimitiveAny {},
            TsKeywordTypeKind::TsUnknownKeyword => panic!("unknown keyword"),
            TsKeywordTypeKind::TsNumberKeyword => TypeInfo::PrimitiveNumber {},
            TsKeywordTypeKind::TsObjectKeyword => TypeInfo::PrimitiveObject {},
            TsKeywordTypeKind::TsBooleanKeyword => TypeInfo::PrimitiveBoolean {},
            TsKeywordTypeKind::TsBigIntKeyword => TypeInfo::PrimitiveBigInt {},
            TsKeywordTypeKind::TsStringKeyword => TypeInfo::PrimitiveString {},
            TsKeywordTypeKind::TsSymbolKeyword => TypeInfo::PrimitiveSymbol {},
            TsKeywordTypeKind::TsVoidKeyword => TypeInfo::PrimitiveVoid {},
            TsKeywordTypeKind::TsUndefinedKeyword => TypeInfo::PrimitiveUndefined {},
            TsKeywordTypeKind::TsNullKeyword => TypeInfo::PrimitiveNull {},
            TsKeywordTypeKind::TsNeverKeyword => panic!("never keyword"),
            TsKeywordTypeKind::TsIntrinsicKeyword => panic!("intrinsic keyword"),
        }
    }

    fn process_array_type(
        &mut self,
        ts_path: &Path,
        TsArrayType { elem_type, .. }: &TsArrayType,
    ) -> TypeInfo {
        TypeInfo::Array {
            item_type: Box::new(self.process_type(ts_path, elem_type)),
        }
    }

    fn process_optional_type(
        &mut self,
        ts_path: &Path,
        TsOptionalType { type_ann, .. }: &TsOptionalType,
    ) -> TypeInfo {
        TypeInfo::Optional {
            item_type: Box::new(self.process_type(ts_path, type_ann)),
        }
    }

    fn process_union_type(
        &mut self,
        ts_path: &Path,
        TsUnionType { types, .. }: &TsUnionType,
    ) -> TypeInfo {
        TypeInfo::Union {
            types: types
                .iter()
                .map(|t| self.process_type(ts_path, t))
                .collect(),
        }
    }

    fn process_intersection_type(
        &mut self,
        ts_path: &Path,
        TsIntersectionType { types, .. }: &TsIntersectionType,
    ) -> TypeInfo {
        TypeInfo::Intersection {
            types: types
                .iter()
                .map(|t| self.process_type(ts_path, t))
                .collect(),
        }
    }

    fn process_type_lit(
        &mut self,
        ts_path: &Path,
        TsTypeLit { members, .. }: &TsTypeLit,
    ) -> TypeInfo {
        if members.len() != 1 || !members.first().expect("no members").is_ts_index_signature() {
            panic!("Bad type lit, {:?}, in {:?}", members, ts_path);
        }

        let mem = members.first().expect("no members for mapped type");
        if let TsTypeElement::TsIndexSignature(index_sig) = mem {
            TypeInfo::Mapped {
                value_type: Box::new(
                    self.process_type(
                        ts_path,
                        &index_sig
                            .type_ann
                            .as_ref()
                            .expect("Need a type for a mapped type")
                            .type_ann,
                    ),
                ),
            }
        } else {
            panic!("bad members for mapped type, {:?}", members);
        }
    }

    fn process_literal_type(
        &mut self,
        _ts_path: &Path,
        TsLitType { lit, .. }: &TsLitType,
    ) -> TypeInfo {
        match lit {
            TsLit::Number(n) => TypeInfo::LitNumber { n: n.value },
            TsLit::Str(s) => TypeInfo::LitString {
                s: s.value.to_string(),
            },
            TsLit::Bool(b) => TypeInfo::LitBoolean { b: b.value },
            TsLit::BigInt(_) => panic!("we don't support literal bigints yet"),
            TsLit::Tpl(_) => panic!("we don't support template literals yet"),
        }
    }

    fn process_params(&mut self, ts_path: &Path, params: &[TsFnParam]) -> Vec<Param> {
        params.iter().map(|p| p.to_param(ts_path, self)).collect()
    }

    fn process_fn_type_params(
        &mut self,
        ts_path: &Path,
        type_params: &Option<TsTypeParamDecl>,
    ) -> HashMap<String, TypeInfo> {
        type_params
            .as_ref()
            .map(|params| {
                params
                    .params
                    .iter()
                    .map(|p| {
                        (
                            p.name.sym.to_string(),
                            p.constraint
                                .as_ref()
                                .map(|c| self.process_type(ts_path, c))
                                .unwrap_or(TypeInfo::PrimitiveAny {}),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn process_fn_type(
        &mut self,
        ts_path: &Path,
        TsFnType {
            type_ann,
            params,
            type_params,
            ..
        }: &TsFnType,
    ) -> TypeInfo {
        TypeInfo::Func(Func {
            type_params: self.process_fn_type_params(ts_path, type_params),
            params: self.process_params(ts_path, params),
            return_type: Box::new(self.process_type(ts_path, &type_ann.type_ann)),
        })
    }

    fn process_ctor_type(
        &mut self,
        ts_path: &Path,
        TsConstructorType {
            params,
            type_params: _,
            ..
        }: &TsConstructorType,
    ) -> TypeInfo {
        TypeInfo::Constructor(Ctor {
            params: self.process_params(ts_path, params),
        })
    }

    fn process_type_predicate(
        &mut self,
        ts_path: &Path,
        TsTypePredicate {
            param_name,
            type_ann,
            ..
        }: &TsTypePredicate,
    ) -> TypeInfo {
        TypeInfo::Func(Func {
            type_params: Default::default(),
            params: vec![Param {
                name: match param_name {
                    TsThisTypeOrIdent::Ident(ident) => ident.sym.to_string(),
                    TsThisTypeOrIdent::TsThisType(_) => "this".to_string(),
                },
                is_variadic: false,
                type_info: type_ann
                    .as_ref()
                    .map(|t| self.process_type(ts_path, &t.type_ann))
                    .unwrap_or(TypeInfo::PrimitiveAny {}),
            }],
            return_type: Box::new(TypeInfo::PrimitiveBoolean {}),
        })
    }

    fn process_type(&mut self, ts_path: &Path, ts_type: &TsType) -> TypeInfo {
        match ts_type {
            TsType::TsTypeRef(type_ref) => self.process_type_ref(ts_path, type_ref),
            TsType::TsKeywordType(keyword_type) => self.process_keyword_type(ts_path, keyword_type),
            TsType::TsArrayType(array_type) => self.process_array_type(ts_path, array_type),
            TsType::TsOptionalType(opt_type) => self.process_optional_type(ts_path, opt_type),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
                union_type,
            )) => self.process_union_type(ts_path, union_type),
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
                isect_type,
            )) => self.process_intersection_type(ts_path, isect_type),
            TsType::TsTypeLit(type_lit) => self.process_type_lit(ts_path, type_lit),
            TsType::TsLitType(lit_type) => self.process_literal_type(ts_path, lit_type),
            TsType::TsParenthesizedType(TsParenthesizedType { type_ann, .. }) => {
                self.process_type(ts_path, type_ann)
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(f)) => {
                self.process_fn_type(ts_path, f)
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(ctor)) => {
                self.process_ctor_type(ts_path, ctor)
            }
            TsType::TsTypePredicate(pred) => self.process_type_predicate(ts_path, pred),
            // TODO: more cases
            _ => {
                println!("MISSING {:?} {:?}", ts_path, ts_type);
                TypeInfo::Ref(TypeRef {
                    referent: TypeName::default_export_for(ts_path.to_path_buf()),
                    type_params: Default::default(),
                })
            }
        }
    }

    fn process_ts_interface(
        &mut self,
        ts_path: &Path,
        TsInterfaceDecl {
            id,
            type_params: _,
            extends,
            body,
            ..
        }: &TsInterfaceDecl,
    ) -> Type {
        Type {
            name: TypeName::for_name(ts_path, &id.sym.to_string()),
            is_exported: false,
            info: TypeInfo::Interface(Interface {
                indexer: body.body.iter().find_map(|el| match el {
                    TsTypeElement::TsIndexSignature(indexer) => {
                        Some(indexer.to_indexer(ts_path, self))
                    }
                    _ => None,
                }),
                extends: extends
                    .iter()
                    .map(|e| BaseClass::Unresolved(Source::from(self, ts_path, e).into()))
                    .collect(),
                fields: body
                    .body
                    .iter()
                    .filter_map(|el| match el {
                        TsTypeElement::TsPropertySignature(prop) => Some((
                            prop.key().expect("bad prop key"),
                            prop.to_type_info(ts_path, self),
                        )),
                        TsTypeElement::TsMethodSignature(method) => Some((
                            method.key().expect("bad method key"),
                            method.to_type_info(ts_path, self),
                        )),
                        TsTypeElement::TsIndexSignature(TsIndexSignature { .. }) => None,
                        // TODO: add other variants
                        _ => {
                            println!("unknown_variant: {:?}", el);
                            None
                        }
                    })
                    .collect(),
            }),
        }
    }

    fn process_ts_enum(
        &mut self,
        ts_path: &Path,
        TsEnumDecl { id, members, .. }: &TsEnumDecl,
    ) -> Type {
        Type {
            name: TypeName::for_name(ts_path, &id.sym.to_string()),
            is_exported: false,
            info: TypeInfo::Enum {
                members: members
                    .iter()
                    .map(|TsEnumMember { id, init, .. }| {
                        EnumMember {
                            id: match id {
                                TsEnumMemberId::Ident(ident) => ident.sym.to_string(),
                                TsEnumMemberId::Str(s) => s.value.to_string(),
                            },
                            value: init.as_ref().and_then(|v| {
                                match &**v {
                                    Expr::Lit(l) => match l {
                                        Lit::Str(s) => Some(s.value.to_string()),
                                        // TODO: might need to capture numbers too
                                        _ => None,
                                    },
                                    _ => panic!("enums may only be initialized with lits"),
                                }
                            }),
                        }
                    })
                    .collect(),
            },
        }
    }

    fn process_ts_alias(
        &mut self,
        ts_path: &Path,
        TsTypeAliasDecl {
            id,
            type_params: _,
            type_ann,
            ..
        }: &TsTypeAliasDecl,
    ) -> Type {
        let type_info = self.process_type(ts_path, &*type_ann);
        Type {
            name: TypeName::for_name(ts_path, &id.sym.to_string()),
            is_exported: false,
            info: type_info,
        }
    }

    fn process_class(&mut self, ts_path: &Path, class: &swc_ecma_ast::Class) -> TypeInfo {
        let swc_ecma_ast::Class {
            body,
            type_params: _,
            ..
        } = class;

        TypeInfo::Class(Class {
            super_class: ClassSuperTypeRef::from(class)
                .map(|super_ref| Box::new(Source::from(self, ts_path, &super_ref).into())),
            members: body
                .iter()
                .filter_map(|member| match member {
                    ClassMember::Constructor(ctor) => Some((
                        ctor.key()
                            .unwrap_or_else(|| panic!("no key for constructor")),
                        Member::Constructor(ctor.to_ctor(ts_path, self)),
                    )),
                    // TODO: need to split Method into methods, getters, and setters
                    ClassMember::Method(method) => Some((
                        method
                            .key()
                            .unwrap_or_else(|| panic!("no key for constructor")),
                        Member::Method(method.to_func(ts_path, self)),
                    )),
                    ClassMember::PrivateMethod(_) => None,
                    ClassMember::ClassProp(prop) => Some((
                        prop.key().expect("we only handle some prop key types"),
                        Member::Property(prop.to_type_info(ts_path, self)),
                    )),
                    ClassMember::PrivateProp(_) => None,
                    ClassMember::TsIndexSignature(_) => None,
                    ClassMember::Empty(_) => None,
                })
                .collect(),
        })
    }

    fn process_class_type(
        &mut self,
        ts_path: &Path,
        ClassDecl { ident, class, .. }: &ClassDecl,
    ) -> Type {
        Type {
            name: TypeName::for_name(ts_path, &ident.sym.to_string()),
            is_exported: false,
            info: self.process_class(ts_path, class),
        }
    }

    fn process_var(&mut self, ts_path: &Path, VarDeclarator { name, .. }: &VarDeclarator) -> Type {
        match name {
            Pat::Ident(BindingIdent { id, type_ann }) => Type {
                name: TypeName::for_name(ts_path, &id.sym.to_string()),
                is_exported: false,
                info: TypeInfo::Var {
                    type_info: Box::new(
                        type_ann
                            .as_ref()
                            .map(|t| self.process_type(ts_path, &t.type_ann))
                            .unwrap_or(TypeInfo::PrimitiveAny {}),
                    ),
                },
            },
            _ => panic!("We only support regular identifier variables"),
        }
    }

    fn process_fn_decl(
        &mut self,
        ts_path: &Path,
        FnDecl {
            ident, function, ..
        }: &FnDecl,
    ) -> Type {
        Type {
            name: TypeName::for_name(ts_path, &ident.sym.to_string()),
            is_exported: false,
            info: function.to_type_info(ts_path, self),
        }
    }

    fn process_decl(&mut self, ts_path: &Path, decl: &Decl) -> Vec<Type> {
        match decl {
            Decl::TsInterface(iface) => vec![self.process_ts_interface(ts_path, iface)],
            Decl::TsEnum(enm) => vec![self.process_ts_enum(ts_path, enm)],
            Decl::TsTypeAlias(alias) => vec![self.process_ts_alias(ts_path, alias)],
            Decl::Class(class) => vec![self.process_class_type(ts_path, class)],
            Decl::Var(VarDecl { decls, .. }) => decls
                .iter()
                .map(|var| self.process_var(ts_path, var))
                .collect(),
            Decl::TsModule(TsModuleDecl { id, body, .. }) => {
                let name = match id {
                    TsModuleName::Ident(ident) => ident.sym.to_string(),
                    TsModuleName::Str(s) => s.value.to_string(),
                };

                let full_ns = {
                    let mut full_ns = self
                        .namespace_stack
                        .last()
                        .unwrap_or(&Default::default())
                        .clone();
                    full_ns.push(name);
                    full_ns
                };
                self.namespace_stack.push(full_ns);

                for b in body {
                    match b {
                        TsNamespaceBody::TsModuleBlock(block) => {
                            self.process_module_items(ts_path, &block.body)
                        }
                        TsNamespaceBody::TsNamespaceDecl(_) => {
                            panic!("what is an inner namespace decl?")
                        }
                    }
                }

                self.namespace_stack.pop();

                Default::default()
            }
            Decl::Fn(fn_decl) => vec![self.process_fn_decl(ts_path, fn_decl)],
        }
    }

    fn process_export_decl(&mut self, ts_path: &Path, ExportDecl { decl, .. }: &ExportDecl) {
        let types = self.process_decl(ts_path, decl);

        types
            .into_iter()
            .map(|mut typ| {
                typ.is_exported = true;
                typ
            })
            .for_each(|typ| {
                let type_name = typ.name.to_name().to_string();

                self.set_type_for_name_for_file(ts_path, TypeIdent::Name(type_name), typ);
            });
    }

    fn process_named_export(
        &mut self,
        ts_path: &Path,
        NamedExport {
            src, specifiers, ..
        }: &NamedExport,
    ) {
        if src.is_none() && specifiers.is_empty() {
            return;
        }

        let src = src.as_ref().expect("need a src").value.to_string();
        let dir = ts_path.parent().expect("All files must have a parent");
        let file = self
            .process_module(Some(dir.to_path_buf()), &src)
            .expect("failed to process module");

        specifiers
            .iter()
            .map(|spec| match spec {
                ExportSpecifier::Named(ExportNamedSpecifier { orig, exported, .. }) => Type {
                    name: TypeName::for_name(
                        ts_path,
                        &exported.as_ref().unwrap_or(orig).sym.to_string(),
                    ),
                    is_exported: true,
                    info: TypeInfo::NamespaceImport(NamespaceImport::Named {
                        src: file.to_path_buf(),
                        name: orig.sym.to_string(),
                    }),
                },
                ExportSpecifier::Default(ExportDefaultSpecifier { exported }) => Type {
                    name: TypeName::for_name(ts_path, &exported.sym.to_string()),
                    is_exported: true,
                    info: TypeInfo::NamespaceImport(NamespaceImport::Default {
                        src: file.to_path_buf(),
                    }),
                },
                ExportSpecifier::Namespace(ExportNamespaceSpecifier { name, .. }) => Type {
                    name: TypeName::for_name(ts_path, &name.sym.to_string()),
                    is_exported: true,
                    info: TypeInfo::NamespaceImport(NamespaceImport::All {
                        src: file.to_path_buf(),
                    }),
                },
            })
            .for_each(|typ| {
                self.set_type_for_name_for_file(ts_path, typ.name.name.clone(), typ);
            });
    }

    fn process_module_decl(&mut self, ts_path: &Path, module_decl: &ModuleDecl) {
        match module_decl {
            ModuleDecl::Import(decl) => self.process_import_decl(ts_path, decl),
            ModuleDecl::ExportDecl(decl) => self.process_export_decl(ts_path, decl),
            ModuleDecl::ExportNamed(decl) => self.process_named_export(ts_path, decl),
            ModuleDecl::ExportDefaultDecl(_decl) => {
                println!("DEFAULT DECL, {:?}", _decl);
            }
            ModuleDecl::ExportDefaultExpr(_decl) => {
                println!("export default expr, {:?}", _decl);
            }
            ModuleDecl::ExportAll(decl) => self.process_export_all(ts_path, decl),
            ModuleDecl::TsImportEquals(_decl) => {
                println!("import equals, {:?}", _decl);
            }
            ModuleDecl::TsExportAssignment(_decl) => {
                println!("export assignment, {:?}", _decl);
            }
            ModuleDecl::TsNamespaceExport(_decl) => {
                println!("export namespace, {:?}", _decl);
            }
        }
    }

    fn process_stmt(&mut self, ts_path: &Path, stmt: &Stmt) {
        if let Stmt::Decl(decl) = stmt {
            self.process_decl(ts_path, decl)
                .into_iter()
                .for_each(|typ| {
                    let type_name = typ.name.to_name().to_string();

                    self.set_type_for_name_for_file(ts_path, TypeIdent::Name(type_name), typ);
                })
        };
    }
}
