use crate::error::{Error, InternalError};
use crate::fs::Fs;
use crate::ir::base::{
    Alias, BaseClass, Class, Ctor, Enum, EnumMember, EnumValue, Func, Indexer, Interface,
    Intersection, LitBoolean, LitNumber, LitString, Member, NamespaceImport, Param, PrimitiveAny,
    PrimitiveBigInt, PrimitiveBoolean, PrimitiveNull, PrimitiveNumber, PrimitiveObject,
    PrimitiveString, PrimitiveSymbol, PrimitiveUndefined, PrimitiveVoid, Tuple, Type, TypeIdent,
    TypeInfo, TypeName, TypeParamConfig, TypeQuery, TypeRef, Union,
};
use crate::module_resolution::{get_ts_path, typings_module_resolver};
use std::collections::{hash_map::Entry, HashMap};
use std::convert::{TryFrom, TryInto};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{io, io::Read};
use swc_common::{sync::Lrc, FileLoader, FilePathMapping, SourceMap, Spanned};
use swc_ecma_ast::*;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

pub type ArcFs = Arc<dyn Fs + Send + Sync>;

pub struct TsTypes {
    errors: Vec<InternalError>,
    types_by_name_by_file: HashMap<PathBuf, HashMap<TypeIdent, Type>>,
    namespace_stack: Vec<Vec<String>>,
    fs: ArcFs,
    source_map: Lrc<SourceMap>,
}

/// Iterator implementation for `ResultIterExt::filter_map_reporting_result`
struct FilterMapReportingResult<I, F> {
    source_iter: I,
    mapper: F,
}

impl<I, F, From, To, Error> Iterator for FilterMapReportingResult<I, F>
where
    I: Iterator<Item = From>,
    F: FnMut(From) -> Result<Option<To>, Error>,
{
    type Item = Result<To, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(item) = self.source_iter.next() {
                let mapped_result = (self.mapper)(item);

                match mapped_result {
                    Ok(Some(res)) => {
                        return Some(Ok(res));
                    }
                    Ok(None) => {
                        continue;
                    }
                    Err(err) => {
                        return Some(Err(err));
                    }
                }
            } else {
                return None;
            }
        }
    }
}

/// Iterator extension trait for dealing with iterators of Results
trait ResultIterExt<From, Error>: Sized {
    /// Given a function that maps iterator items of type `To` to `Result<Option<To>, Error>`, return an
    /// Iterator of `Result<To, Error>` by dropping any Ok(None)
    fn filter_map_reporting_result<F, To>(self, f: F) -> FilterMapReportingResult<Self, F>
    where
        F: FnMut(From) -> Result<Option<To>, Error>;

    fn find_map_reporting_result<F, R>(self, f: F) -> Result<Option<R>, Error>
    where
        F: FnMut(From) -> Option<Result<R, Error>>;
}

impl<I, From, Error> ResultIterExt<From, Error> for I
where
    I: Iterator<Item = From>,
{
    fn filter_map_reporting_result<F, To>(self, f: F) -> FilterMapReportingResult<Self, F>
    where
        F: FnMut(From) -> Result<Option<To>, Error>,
    {
        FilterMapReportingResult {
            source_iter: self,
            mapper: f,
        }
    }

    fn find_map_reporting_result<F, R>(mut self, f: F) -> Result<Option<R>, Error>
    where
        F: FnMut(From) -> Option<Result<R, Error>>,
    {
        self.find_map(f).transpose()
    }
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

impl TypeRefExt for TsEntityName {
    fn entity_name(&self) -> &TsEntityName {
        self
    }

    fn type_args(&self) -> &Option<TsTypeParamInstantiation> {
        &None
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

impl<'a, T: TypeRefExt> TryFrom<Source<'a, T>> for TypeRef {
    type Error = InternalError;

    fn try_from(source: Source<'a, T>) -> Result<TypeRef, Self::Error> {
        let Source {
            ts_types,
            ts_path,
            node,
        } = source;

        match node.entity_name() {
            TsEntityName::Ident(Ident { sym, .. }) => {
                let type_name = TypeName::for_name(ts_path.to_path_buf(), &sym.to_string());
                let type_params = node
                    .type_args()
                    .as_ref()
                    .map(|tps| {
                        tps.params
                            .iter()
                            .map(|tp| ts_types.process_type(ts_path, tp))
                            .collect()
                    })
                    .unwrap_or_else(|| Ok(Default::default()))?;

                Ok(ts_types.make_type_ref(type_name, type_params))
            }
            TsEntityName::TsQualifiedName(qn) => {
                let type_name = ts_types.qualified_name_to_type_name(ts_path, qn);
                let type_params = node
                    .type_args()
                    .as_ref()
                    .map(|tps| {
                        tps.params
                            .iter()
                            .map(|tp| ts_types.process_type(ts_path, tp))
                            .collect()
                    })
                    .unwrap_or_else(|| Ok(Default::default()))?;

                Ok(ts_types.make_type_ref(type_name, type_params))
            }
        }
    }
}

trait KeyedExt {
    fn key(&self) -> Option<String>;
}

fn make_key<K: KeyedExt + Spanned>(k: &K) -> Result<String, InternalError> {
    k.key()
        .ok_or_else(|| InternalError::with_msg_and_span("bad key", k.span()))
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

impl ExprKeyed for TsGetterSignature {
    fn expr_key(&self) -> &Expr {
        &*self.key
    }
}

impl ExprKeyed for TsSetterSignature {
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

impl KeyedExt for TsConstructSignatureDecl {
    fn key(&self) -> Option<String> {
        Some("constructor".to_string())
    }
}

trait PropExt {
    fn item_type(&self) -> Option<&TsType>;

    fn is_optional(&self) -> bool;

    fn to_type_info(
        &self,
        ts_path: &Path,
        ts_types: &mut TsTypes,
    ) -> Result<TypeInfo, InternalError> {
        self.item_type()
            .map(|t| {
                let item_type = ts_types.process_type(ts_path, t)?;
                Ok(if self.is_optional() {
                    TypeInfo::Optional {
                        item_type: Box::new(item_type),
                    }
                } else {
                    item_type
                })
            })
            .unwrap_or_else(|| Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))
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
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError>;
}

impl FnParamExt for TsFnParam {
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        match self {
            TsFnParam::Ident(ident) => ident.to_param(ts_path, i, ts_types),
            TsFnParam::Object(obj) => obj.to_param(ts_path, i, ts_types),
            TsFnParam::Rest(rest) => rest.to_param(ts_path, i, ts_types),
            TsFnParam::Array(array) => array.to_param(ts_path, i, ts_types),
        }
    }
}

impl FnParamExt for swc_ecma_ast::Param {
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        self.pat.to_param(ts_path, i, ts_types)
    }
}

impl FnParamExt for Pat {
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        match self {
            Pat::Ident(ident) => ident.to_param(ts_path, i, ts_types),
            Pat::Object(obj) => obj.to_param(ts_path, i, ts_types),
            Pat::Rest(rest) => rest.to_param(ts_path, i, ts_types),
            Pat::Array(array) => array.to_param(ts_path, i, ts_types),
            _ => Err(InternalError::with_msg_and_span(
                "unhandled param pattern",
                self.span(),
            )),
        }
    }
}

impl FnParamExt for BindingIdent {
    fn to_param(
        &self,
        ts_path: &Path,
        _i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        Ok(Param {
            name: self.id.sym.to_string(),
            is_variadic: false,
            type_info: self
                .type_ann
                .as_ref()
                .map(|t| ts_types.process_type(ts_path, &*t.type_ann))
                .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
        })
    }
}

impl FnParamExt for ObjectPat {
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        Ok(Param {
            name: format!("arg{}", i),
            is_variadic: false,
            type_info: self
                .type_ann
                .as_ref()
                .map(|t| ts_types.process_type(ts_path, &*t.type_ann))
                .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
        })
    }
}

impl FnParamExt for RestPat {
    fn to_param(
        &self,
        ts_path: &Path,
        _i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        let name = match &*self.arg {
            Pat::Ident(id_param) => id_param.id.sym.to_string(),
            _ => "rest".to_string(),
        };

        Ok(Param {
            name,
            is_variadic: true,
            type_info: self
                .type_ann
                .as_ref()
                .map(|t| ts_types.process_type(ts_path, &t.type_ann))
                .map(|t_result| {
                    t_result.map(|t| match t {
                        // rest params should be arrays but, since we handle rest params (is_variadic) explicitly
                        // later, we unpack the array now
                        TypeInfo::Array { item_type } => *item_type,
                        _ => t,
                    })
                })
                .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
        })
    }
}

impl FnParamExt for ArrayPat {
    fn to_param(
        &self,
        ts_path: &Path,
        i: usize,
        ts_types: &mut TsTypes,
    ) -> Result<Param, InternalError> {
        Ok(Param {
            name: format!("arg{}", i),
            is_variadic: false,
            type_info: self
                .type_ann
                .as_ref()
                .map(|t| ts_types.process_type(ts_path, &t.type_ann))
                .unwrap_or_else(|| {
                    Ok(TypeInfo::Array {
                        item_type: Box::new(TypeInfo::PrimitiveAny(PrimitiveAny())),
                    })
                })?,
        })
    }
}

trait CtorExt {
    fn to_ctor(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Ctor, InternalError>;
}

impl CtorExt for Constructor {
    fn to_ctor(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Ctor, InternalError> {
        Ok(Ctor {
            params: self
                .params
                .iter()
                .enumerate()
                .map(|(i, p)| match p {
                    ParamOrTsParamProp::Param(p) => p.to_param(ts_path, i, ts_types),
                    ParamOrTsParamProp::TsParamProp(tsp) => match &tsp.param {
                        TsParamPropParam::Ident(id) => id.to_param(ts_path, i, ts_types),
                        TsParamPropParam::Assign(a) => Err(InternalError::with_msg_and_span(
                            "we don't handle assignment params yet",
                            a.span(),
                        )),
                    },
                })
                .collect::<Result<Vec<Param>, InternalError>>()?,
        })
    }
}

trait FuncExt {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Vec<Param>, InternalError>;

    fn type_params(&self) -> &Option<TsTypeParamDecl>;

    fn return_type(&self) -> Option<&TsType>;

    fn to_func(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Func, InternalError> {
        Ok(Func {
            params: self.params(ts_path, ts_types)?,
            type_params: ts_types.process_fn_type_params(ts_path, self.type_params()),
            return_type: Box::new(
                self.return_type()
                    .map(|t| ts_types.process_type(ts_path, t))
                    .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
            ),
            class_name: None,
        })
    }

    fn to_member_func(
        &self,
        ts_path: &Path,
        ts_types: &mut TsTypes,
        name: &TypeName,
    ) -> Result<Func, InternalError> {
        let mut f = self.to_func(ts_path, ts_types)?;
        f.class_name = Some(name.clone());
        Ok(f)
    }

    fn to_type_info(
        &self,
        ts_path: &Path,
        ts_types: &mut TsTypes,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Func(self.to_func(ts_path, ts_types)?))
    }
}

impl FuncExt for TsMethodSignature {
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Vec<Param>, InternalError> {
        self.params
            .iter()
            .enumerate()
            .map(|(i, param)| param.to_param(ts_path, i, ts_types))
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
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Vec<Param>, InternalError> {
        self.params
            .iter()
            .enumerate()
            .map(|(i, param)| param.to_param(ts_path, i, ts_types))
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
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Vec<Param>, InternalError> {
        self.params
            .iter()
            .enumerate()
            .map(|(i, param)| param.to_param(ts_path, i, ts_types))
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
    fn params(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Vec<Param>, InternalError> {
        self.function
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| param.to_param(ts_path, i, ts_types))
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

    fn to_indexer(&self, ts_path: &Path, ts_types: &mut TsTypes) -> Result<Indexer, InternalError> {
        Ok(Indexer {
            readonly: self.is_readonly(),
            type_info: Box::new(
                self.value_type()
                    .map(|t| ts_types.process_type(ts_path, t))
                    .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
            ),
        })
    }
}

impl IndexerExt for TsIndexSignature {
    fn is_readonly(&self) -> bool {
        self.readonly
    }

    fn value_type(&self) -> Option<&TsType> {
        self.type_ann.as_ref().map(|ta| ta.type_ann.as_ref())
    }
}

trait HasTypeParams {
    fn type_param_config(&self) -> Vec<(String, TypeParamConfig)>;
}

impl HasTypeParams for Option<TsTypeParamDecl> {
    fn type_param_config(&self) -> Vec<(String, TypeParamConfig)> {
        self.as_ref()
            .map(|tps| {
                tps.params
                    .iter()
                    .map(|p| {
                        (
                            p.name.sym.to_string(),
                            TypeParamConfig {
                                constraint: None,
                                default_type_arg: None,
                            },
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }
}

struct FsFileLoader {
    fs: ArcFs,
}

impl FsFileLoader {
    fn new(fs: ArcFs) -> Self {
        FsFileLoader { fs }
    }
}

impl FileLoader for FsFileLoader {
    fn file_exists(&self, path: &Path) -> bool {
        self.fs.exists(path)
    }

    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        Some(self.fs.normalize(path))
    }

    fn read_file(&self, path: &Path) -> Result<String, io::Error> {
        let mut buf = String::new();
        self.fs.open(path)?.read_to_string(&mut buf)?;
        Ok(buf)
    }
}

impl TsTypes {
    /// Given a filesystem, `fs`, and a `module_name` pointing into that filesystem,
    /// return a map from files to a map from type names to types.
    pub fn parse(
        fs: ArcFs,
        module_name: &str,
    ) -> Result<HashMap<PathBuf, HashMap<TypeIdent, Type>>, Error> {
        let file_loader =
            Box::new(FsFileLoader::new(Arc::clone(&fs))) as Box<dyn FileLoader + Send + Sync>;
        let source_map: Lrc<SourceMap> = Lrc::new(SourceMap::with_file_loader(
            file_loader,
            FilePathMapping::empty(),
        ));
        let mut tt = TsTypes {
            types_by_name_by_file: Default::default(),
            namespace_stack: Default::default(),
            errors: Default::default(),
            fs: Arc::clone(&fs),
            source_map,
        };

        if let Err(err) = tt.process_module(None, module_name) {
            tt.record_error(err);
        }

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

        tt.try_into_types_by_name_by_file()
    }

    pub fn try_into_types_by_name_by_file(
        self,
    ) -> Result<HashMap<PathBuf, HashMap<TypeIdent, Type>>, Error> {
        if self.errors.is_empty() {
            Ok(self.types_by_name_by_file)
        } else {
            Err(Error::with_errors(self.errors, &self.source_map))
        }
    }

    fn load_module(&mut self, ts_path: &Path) -> Result<Module, InternalError> {
        let fm = self.source_map.load_file(ts_path)?;
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

        Ok(module)
    }

    fn make_type_ref(&mut self, referent: TypeName, type_params: Vec<TypeInfo>) -> TypeRef {
        TypeRef {
            referent: TypeName {
                file: referent.file,
                name: self.get_possibly_ns_qualified_name(referent.name),
            },
            type_params,
        }
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
    ) -> Result<PathBuf, InternalError> {
        self.process_module_with_items(module_base, module_name, false, |t, p| {
            t.load_module(p).map(|m| m.body)
        })
    }

    fn process_module_with_items<
        G: FnMut(&mut TsTypes, &Path) -> Result<Vec<ModuleItem>, InternalError>,
    >(
        &mut self,
        module_base: Option<PathBuf>,
        module_name: &str,
        // if allow_incremental - reprocess module if necessary. else, skip re-processing
        allow_incremental: bool,
        mut load_module: G,
    ) -> Result<PathBuf, InternalError> {
        let ts_path = get_ts_path(
            &*self.fs,
            module_base,
            module_name,
            &typings_module_resolver,
        )?;
        let ts_path = self.fs.normalize(&ts_path);

        match self.types_by_name_by_file.entry(ts_path.clone()) {
            Entry::Occupied(_) => {
                if !allow_incremental {
                    return Ok(ts_path);
                }
            }
            Entry::Vacant(v) => {
                v.insert(Default::default());
            }
        }

        let module_items = load_module(self, &ts_path)?;
        self.process_module_items(&ts_path, &module_items);

        Ok(ts_path)
    }

    fn get_possibly_ns_qualified_name(&self, name: TypeIdent) -> TypeIdent {
        match self.namespace_stack.last() {
            Some(ns) => {
                let mut ns = ns.clone();
                match name {
                    TypeIdent::Name(s) => {
                        ns.push(s);
                    }
                    TypeIdent::DefaultExport() => {
                        ns.push("default".to_string());
                    }
                    TypeIdent::QualifiedName(mut name) => {
                        ns.append(&mut name);
                    }
                    TypeIdent::TypeEnvironmentParent() => {
                        // TODO: should never get here
                        return name;
                    }
                }

                TypeIdent::QualifiedName(ns)
            }
            None => name,
        }
    }

    fn ns_type_name(&self, name: TypeName) -> TypeName {
        TypeName {
            file: name.file,
            name: self.get_possibly_ns_qualified_name(name.name),
        }
    }

    fn set_type_for_name_for_file(
        &mut self,
        file: &Path,
        name: TypeIdent,
        typ: Result<Type, InternalError>,
    ) {
        match typ {
            Err(e) => self.record_error(e),
            Ok(typ) => {
                self.types_by_name_by_file
                    .entry(file.to_path_buf())
                    .and_modify(|names_to_types: &mut HashMap<TypeIdent, Type>| {
                        names_to_types.insert(name, typ);
                    });
            }
        }
    }

    fn export_type_with_name(
        &mut self,
        ts_path: &Path,
        name: TypeName,
        info: Result<TypeInfo, InternalError>,
    ) {
        let typ = info.map(|info| Type {
            name,
            is_exported: true,
            info,
        });

        self.set_type_for_file(ts_path, typ);
    }

    fn set_type_for_file(&mut self, ts_path: &Path, typ: Result<Type, InternalError>) {
        match typ {
            Ok(t) => {
                self.set_type_for_name_for_file(ts_path, t.name.name.clone(), Ok(t));
            }
            Err(err) => {
                self.record_error(err);
            }
        }
    }

    fn record_error(&mut self, err: InternalError) {
        self.errors.push(err);
    }

    fn process_import_decl(
        &mut self,
        ts_path: &Path,
        ImportDecl {
            specifiers, src, ..
        }: &ImportDecl,
    ) {
        let base = match path_parent(ts_path) {
            Ok(base) => base,
            Err(err) => {
                self.record_error(err);
                return;
            }
        };
        let import = src.value.to_string();

        let file_result = self.process_module(Some(base.to_path_buf()), &import);

        let file = match file_result {
            Ok(file) => file,
            Err(err) => {
                self.record_error(err);
                return;
            }
        };

        specifiers.iter().for_each(|specifier| match specifier {
            ImportSpecifier::Named(ImportNamedSpecifier {
                local, imported, ..
            }) => {
                let name = imported.as_ref().unwrap_or(local).sym.to_string();
                let info = if name == "default" {
                    // import { default as X } from '...'
                    TypeInfo::NamespaceImport(NamespaceImport::Default {
                        src: file.to_path_buf(),
                    })
                } else {
                    TypeInfo::NamespaceImport(NamespaceImport::Named {
                        src: file.to_path_buf(),
                        name,
                    })
                };
                self.set_type_for_file(
                    ts_path,
                    Ok(Type {
                        name: self
                            .ns_type_name(TypeName::for_name(ts_path, &local.sym.to_string())),
                        is_exported: false,
                        info,
                    }),
                );
            }
            ImportSpecifier::Default(ImportDefaultSpecifier { local, .. }) => {
                self.set_type_for_file(
                    ts_path,
                    Ok(Type {
                        name: self
                            .ns_type_name(TypeName::for_name(ts_path, &local.sym.to_string())),
                        is_exported: false,
                        info: TypeInfo::NamespaceImport(NamespaceImport::Default {
                            src: file.to_path_buf(),
                        }),
                    }),
                );
            }
            ImportSpecifier::Namespace(ImportStarAsSpecifier { local, .. }) => {
                self.set_type_for_file(
                    ts_path,
                    Ok(Type {
                        name: self
                            .ns_type_name(TypeName::for_name(ts_path, &local.sym.to_string())),
                        is_exported: false,
                        info: TypeInfo::NamespaceImport(NamespaceImport::All {
                            src: file.to_path_buf(),
                        }),
                    }),
                );
            }
        })
    }

    fn process_export_all(&mut self, ts_path: &Path, export_all: &ExportAll) {
        let s = export_all.src.value.to_string();
        let dir = match path_parent(ts_path) {
            Ok(base) => base,
            Err(err) => {
                self.record_error(err);
                return;
            }
        };

        let file_result = self.process_module(Some(dir.to_path_buf()), &s);

        let file = match file_result {
            Ok(file) => file,
            Err(err) => {
                self.record_error(err);
                return;
            }
        };

        let types_by_name = self.types_by_name_by_file.get(&file);

        let to_export = match types_by_name {
            Some(file) => file,
            None => {
                self.record_error(InternalError::with_msg_and_span(
                    "expected file to have already been processed",
                    export_all.span(),
                ));
                return;
            }
        };
        let to_export = to_export
            .iter()
            .filter(|(_, t)| t.is_exported)
            .filter_map(|(n, t)| match n {
                n @ TypeIdent::Name(_) => Some((n.clone(), t.clone())),
                _ => None,
            })
            .collect::<HashMap<TypeIdent, Type>>();

        to_export.into_iter().for_each(|(name, typ)| {
            self.set_type_for_name_for_file(ts_path, name, Ok(typ));
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

    fn process_type_ref(
        &mut self,
        ts_path: &Path,
        ts_type_ref: &TsTypeRef,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Ref(
            Source::from(self, ts_path, ts_type_ref).try_into()?,
        ))
    }

    fn process_keyword_type(
        &mut self,
        _ts_path: &Path,
        keyword: &TsKeywordType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(match &keyword.kind {
            TsKeywordTypeKind::TsAnyKeyword => TypeInfo::PrimitiveAny(PrimitiveAny()),
            TsKeywordTypeKind::TsUnknownKeyword => {
                return Err(InternalError::with_msg_and_span(
                    "unknown keyword not supported",
                    keyword.span(),
                ))
            }
            TsKeywordTypeKind::TsNumberKeyword => TypeInfo::PrimitiveNumber(PrimitiveNumber()),
            TsKeywordTypeKind::TsObjectKeyword => TypeInfo::PrimitiveObject(PrimitiveObject()),
            TsKeywordTypeKind::TsBooleanKeyword => TypeInfo::PrimitiveBoolean(PrimitiveBoolean()),
            TsKeywordTypeKind::TsBigIntKeyword => TypeInfo::PrimitiveBigInt(PrimitiveBigInt()),
            TsKeywordTypeKind::TsStringKeyword => TypeInfo::PrimitiveString(PrimitiveString()),
            TsKeywordTypeKind::TsSymbolKeyword => TypeInfo::PrimitiveSymbol(PrimitiveSymbol()),
            TsKeywordTypeKind::TsVoidKeyword => TypeInfo::PrimitiveVoid(PrimitiveVoid()),
            TsKeywordTypeKind::TsUndefinedKeyword => {
                TypeInfo::PrimitiveUndefined(PrimitiveUndefined())
            }
            TsKeywordTypeKind::TsNullKeyword => TypeInfo::PrimitiveNull(PrimitiveNull()),
            TsKeywordTypeKind::TsNeverKeyword => {
                return Err(InternalError::with_msg_and_span(
                    "never keyword not supported",
                    keyword.span(),
                ))
            }
            TsKeywordTypeKind::TsIntrinsicKeyword => {
                return Err(InternalError::with_msg_and_span(
                    "intrinsic keyword not supported",
                    keyword.span(),
                ))
            }
        })
    }

    fn process_array_type(
        &mut self,
        ts_path: &Path,
        TsArrayType { elem_type, .. }: &TsArrayType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Array {
            item_type: Box::new(self.process_type(ts_path, elem_type)?),
        })
    }

    fn process_optional_type(
        &mut self,
        ts_path: &Path,
        TsOptionalType { type_ann, .. }: &TsOptionalType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Optional {
            item_type: Box::new(self.process_type(ts_path, type_ann)?),
        })
    }

    fn process_union_type(
        &mut self,
        ts_path: &Path,
        TsUnionType { types, .. }: &TsUnionType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Union(Union {
            types: types
                .iter()
                .map(|t| self.process_type(ts_path, t))
                .collect::<Result<Vec<_>, InternalError>>()?,
        }))
    }

    fn process_intersection_type(
        &mut self,
        ts_path: &Path,
        TsIntersectionType { types, .. }: &TsIntersectionType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Intersection(Intersection {
            types: types
                .iter()
                .map(|t| self.process_type(ts_path, t))
                .collect::<Result<Vec<_>, InternalError>>()?,
        }))
    }

    fn process_type_lit(
        &mut self,
        ts_path: &Path,
        TsTypeLit { members, .. }: &TsTypeLit,
    ) -> Result<TypeInfo, InternalError> {
        if members.len() == 1 && members.first().unwrap().is_ts_index_signature() {
            // mapped param, e.g. function f(a: {[n: string]: string]});
            let mem = members.first().unwrap();
            return if let TsTypeElement::TsIndexSignature(index_sig) = mem {
                Ok(TypeInfo::Mapped {
                    value_type: Box::new(
                        self.process_type(
                            ts_path,
                            &index_sig
                                .type_ann
                                .as_ref()
                                .ok_or_else(|| {
                                    InternalError::with_msg_and_span(
                                        "expected a type for a mapped type",
                                        index_sig.span(),
                                    )
                                })?
                                .type_ann,
                        )?,
                    ),
                })
            } else {
                Err(InternalError::with_msg_and_span(
                    "bad member types for mapped type",
                    mem.span(),
                ))
            };
        }

        Ok(TypeInfo::Interface(Interface {
            indexer: self.process_interface_indexer(ts_path, members)?,
            extends: Default::default(),
            fields: self.process_interface_members(ts_path, members)?,
            type_params: Default::default(),
            constructor: self.process_interface_constructor(ts_path, members)?,
        }))
    }

    fn process_literal_type(
        &mut self,
        _ts_path: &Path,
        lit: &TsLitType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(match &lit.lit {
            TsLit::Number(n) => TypeInfo::LitNumber(LitNumber { n: n.value }),
            TsLit::Str(s) => TypeInfo::LitString(LitString {
                s: s.value.to_string(),
            }),
            TsLit::Bool(b) => TypeInfo::LitBoolean(LitBoolean { b: b.value }),
            TsLit::BigInt(_) => {
                return Err(InternalError::with_msg_and_span(
                    "we don't support literal bigints yet",
                    lit.span(),
                ))
            }
            TsLit::Tpl(_) => {
                return Err(InternalError::with_msg_and_span(
                    "we don't support template literals yet",
                    lit.span(),
                ))
            }
        })
    }

    fn process_params(
        &mut self,
        ts_path: &Path,
        params: &[TsFnParam],
    ) -> Result<Vec<Param>, InternalError> {
        params
            .iter()
            .enumerate()
            .map(|(i, p)| p.to_param(ts_path, i, self))
            .collect()
    }

    fn process_fn_type_params(
        &mut self,
        _ts_path: &Path,
        type_params: &Option<TsTypeParamDecl>,
    ) -> Vec<(String, TypeParamConfig)> {
        type_params.type_param_config()
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
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Func(Func {
            type_params: self.process_fn_type_params(ts_path, type_params),
            params: self.process_params(ts_path, params)?,
            return_type: Box::new(self.process_type(ts_path, &type_ann.type_ann)?),
            class_name: None,
        }))
    }

    fn process_ctor_type(
        &mut self,
        ts_path: &Path,
        TsConstructorType {
            params,
            type_params: _,
            ..
        }: &TsConstructorType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Constructor(Ctor {
            params: self.process_params(ts_path, params)?,
        }))
    }

    fn process_type_predicate(
        &mut self,
        ts_path: &Path,
        TsTypePredicate {
            param_name,
            type_ann,
            ..
        }: &TsTypePredicate,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Func(Func {
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
                    .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
            }],
            return_type: Box::new(TypeInfo::PrimitiveBoolean(PrimitiveBoolean())),
            class_name: None,
        }))
    }

    fn process_tuple(
        &mut self,
        ts_path: &Path,
        TsTupleType { elem_types, .. }: &TsTupleType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(TypeInfo::Tuple(Tuple {
            types: elem_types
                .iter()
                .map(|t| self.process_type(ts_path, &t.ty))
                .collect::<Result<Vec<_>, InternalError>>()?,
        }))
    }

    fn process_type_op(
        &mut self,
        ts_path: &Path,
        TsTypeOperator { op, type_ann, .. }: &TsTypeOperator,
    ) -> Result<TypeInfo, InternalError> {
        Ok(match op {
            TsTypeOperatorOp::KeyOf => TypeInfo::PrimitiveString(PrimitiveString()),
            TsTypeOperatorOp::Unique | TsTypeOperatorOp::ReadOnly => {
                self.process_type(ts_path, &*type_ann)?
            }
        })
    }

    fn process_type_query(
        &mut self,
        ts_path: &Path,
        query: &TsTypeQuery,
    ) -> Result<TypeInfo, InternalError> {
        match &query.expr_name {
            TsTypeQueryExpr::TsEntityName(ent) => Ok(TypeInfo::TypeQuery(TypeQuery::LookupRef(
                Source::from(self, ts_path, ent).try_into()?,
            ))),
            TsTypeQueryExpr::Import(_) => Err(InternalError::with_msg_and_span(
                "type queries for imports are not supported",
                query.span(),
            )),
        }
    }

    fn process_type(
        &mut self,
        ts_path: &Path,
        ts_type: &TsType,
    ) -> Result<TypeInfo, InternalError> {
        Ok(match ts_type {
            TsType::TsTypeRef(type_ref) => self.process_type_ref(ts_path, type_ref)?,
            TsType::TsKeywordType(keyword_type) => {
                self.process_keyword_type(ts_path, keyword_type)?
            }
            TsType::TsArrayType(array_type) => self.process_array_type(ts_path, array_type)?,
            TsType::TsOptionalType(opt_type) => self.process_optional_type(ts_path, opt_type)?,
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(
                union_type,
            )) => self.process_union_type(ts_path, union_type)?,
            TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
                isect_type,
            )) => self.process_intersection_type(ts_path, isect_type)?,
            TsType::TsTypeLit(type_lit) => self.process_type_lit(ts_path, type_lit)?,
            TsType::TsLitType(lit_type) => self.process_literal_type(ts_path, lit_type)?,
            TsType::TsParenthesizedType(TsParenthesizedType { type_ann, .. }) => {
                self.process_type(ts_path, type_ann)?
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(f)) => {
                self.process_fn_type(ts_path, f)?
            }
            TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(ctor)) => {
                self.process_ctor_type(ts_path, ctor)?
            }
            TsType::TsTypePredicate(pred) => self.process_type_predicate(ts_path, pred)?,
            TsType::TsTupleType(tuple) => self.process_tuple(ts_path, tuple)?,
            TsType::TsTypeOperator(op) => self.process_type_op(ts_path, op)?,
            TsType::TsThisType(_) => TypeInfo::PrimitiveAny(PrimitiveAny()), // TODO: this is clearly silly
            TsType::TsTypeQuery(query) => self.process_type_query(ts_path, query)?,
            _ => {
                println!("MISSING {:?} {:?}", ts_path, ts_type);
                TypeInfo::Ref(TypeRef {
                    referent: TypeName::default_export_for(ts_path.to_path_buf()),
                    type_params: Default::default(),
                })
            }
        })
    }

    fn process_interface_indexer(
        &mut self,
        ts_path: &Path,
        members: &[TsTypeElement],
    ) -> Result<Option<Indexer>, InternalError> {
        members.iter().find_map_reporting_result(|el| match el {
            TsTypeElement::TsIndexSignature(indexer) => Some(indexer.to_indexer(ts_path, self)),
            _ => None,
        })
    }

    fn process_interface_members(
        &mut self,
        ts_path: &Path,
        members: &[TsTypeElement],
    ) -> Result<HashMap<String, TypeInfo>, InternalError> {
        members
            .iter()
            .filter_map_reporting_result(|el| {
                Ok(match &el {
                    TsTypeElement::TsPropertySignature(prop) => {
                        Some((make_key(prop)?, prop.to_type_info(ts_path, self)?))
                    }
                    TsTypeElement::TsMethodSignature(method) => {
                        Some((make_key(method)?, method.to_type_info(ts_path, self)?))
                    }
                    TsTypeElement::TsIndexSignature(TsIndexSignature { .. }) => None,
                    TsTypeElement::TsGetterSignature(getter) => Some((
                        make_key(getter)?,
                        getter
                            .type_ann
                            .as_ref()
                            .map(|t| self.process_type(ts_path, &*t.type_ann))
                            .unwrap_or_else(|| Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
                    )),
                    TsTypeElement::TsSetterSignature(setter) => Some((
                        make_key(setter)?,
                        setter.param.to_param(ts_path, 0, self)?.type_info,
                    )),
                    TsTypeElement::TsConstructSignatureDecl(_) => None,
                    TsTypeElement::TsCallSignatureDecl(_) => None, // TODO
                })
            })
            .collect()
    }

    fn process_interface_constructor(
        &mut self,
        ts_path: &Path,
        members: &[TsTypeElement],
    ) -> Result<Option<Ctor>, InternalError> {
        members.iter().find_map_reporting_result(|el| match el {
            TsTypeElement::TsConstructSignatureDecl(ctor) => {
                let params = self.process_params(ts_path, &ctor.params);
                Some(match params {
                    Ok(params) => Ok(Ctor { params }),
                    Err(e) => Err(e),
                })
            }
            _ => None,
        })
    }

    fn process_ts_interface(
        &mut self,
        ts_path: &Path,
        TsInterfaceDecl {
            id,
            type_params,
            extends,
            body,
            ..
        }: &TsInterfaceDecl,
    ) -> Result<Type, InternalError> {
        Ok(Type {
            name: self.ns_type_name(TypeName::for_name(ts_path, &id.sym.to_string())),
            is_exported: false,
            info: TypeInfo::Interface(Interface {
                indexer: self.process_interface_indexer(ts_path, &body.body)?,
                extends: extends
                    .iter()
                    .map(|e| {
                        Ok(BaseClass::Unresolved(
                            Source::from(self, ts_path, e).try_into()?,
                        ))
                    })
                    .collect::<Result<Vec<_>, InternalError>>()?,
                fields: self.process_interface_members(ts_path, &body.body)?,
                type_params: type_params.type_param_config(),
                constructor: self.process_interface_constructor(ts_path, &body.body)?,
            }),
        })
    }

    fn process_ts_enum(
        &mut self,
        ts_path: &Path,
        TsEnumDecl { id, members, .. }: &TsEnumDecl,
    ) -> Result<Type, InternalError> {
        Ok(Type {
            name: self.ns_type_name(TypeName::for_name(ts_path, &id.sym.to_string())),
            is_exported: false,
            info: TypeInfo::Enum(Enum {
                members: members
                    .iter()
                    .scan(
                        None,
                        |last_numeric_discriminator, TsEnumMember { id, init, .. }| {
                            Some(make_enum_member(id, init, last_numeric_discriminator))
                        },
                    )
                    .collect::<Result<Vec<EnumMember>, InternalError>>()?,
            }),
        })
    }

    fn process_ts_alias(
        &mut self,
        ts_path: &Path,
        TsTypeAliasDecl {
            id,
            type_params,
            type_ann,
            ..
        }: &TsTypeAliasDecl,
    ) -> Result<Type, InternalError> {
        let type_info = self.process_type(ts_path, &*type_ann)?;
        Ok(Type {
            name: self.ns_type_name(TypeName::for_name(ts_path, &id.sym.to_string())),
            is_exported: false,
            info: TypeInfo::Alias(Alias {
                target: Box::new(type_info),
                type_params: type_params.type_param_config(),
            }),
        })
    }

    fn process_class(
        &mut self,
        ts_path: &Path,
        name: &TypeName,
        class: &swc_ecma_ast::Class,
    ) -> Result<TypeInfo, InternalError> {
        let swc_ecma_ast::Class {
            body,
            type_params,
            implements,
            ..
        } = class;
        let super_class = ClassSuperTypeRef::from(class)
            .map(|super_ref| Source::from(self, ts_path, &super_ref).try_into());
        let super_class = match super_class {
            Some(super_class) => Some(Box::new(super_class?)),
            None => None,
        };

        Ok(TypeInfo::Class(Class {
            super_class,
            members: body
                .iter()
                .filter_map_reporting_result(|member| {
                    Ok(match member {
                        ClassMember::StaticBlock(_) => None,
                        ClassMember::Constructor(ctor) => Some((
                            make_key(ctor)?,
                            Member::Constructor(ctor.to_ctor(ts_path, self)?),
                        )),
                        ClassMember::Method(method) if method.kind == MethodKind::Method => Some((
                            make_key(method)?,
                            Member::Method(method.to_member_func(ts_path, self, name)?),
                        )),
                        ClassMember::Method(method) if method.kind == MethodKind::Getter => Some((
                            make_key(method)?,
                            Member::Property(
                                *method.to_member_func(ts_path, self, name)?.return_type,
                            ),
                        )),
                        ClassMember::Method(method) if method.kind == MethodKind::Setter => Some((
                            make_key(method)?,
                            Member::Property(
                                method
                                    .to_member_func(ts_path, self, name)?
                                    .params
                                    .pop()
                                    .map(|p| p.type_info)
                                    .unwrap_or_else(|| TypeInfo::PrimitiveAny(PrimitiveAny())),
                            ),
                        )),
                        ClassMember::Method(_) => None,
                        ClassMember::PrivateMethod(_) => None,
                        ClassMember::ClassProp(prop)
                            if prop
                                .accessibility
                                .map(|a| a != Accessibility::Private)
                                .unwrap_or(true) =>
                        {
                            Some((
                                make_key(prop)?,
                                Member::Property(prop.to_type_info(ts_path, self)?),
                            ))
                        }
                        ClassMember::ClassProp(_) => None,
                        ClassMember::PrivateProp(_) => None,
                        ClassMember::TsIndexSignature(_) => None,
                        ClassMember::Empty(_) => None,
                    })
                })
                .collect::<Result<HashMap<String, Member>, InternalError>>()?,
            implements: implements
                .iter()
                .map(|i| Source::from(self, ts_path, i).try_into())
                .collect::<Result<Vec<_>, InternalError>>()?,
            type_params: type_params.type_param_config(),
        }))
    }

    fn process_class_type(
        &mut self,
        ts_path: &Path,
        ClassDecl { ident, class, .. }: &ClassDecl,
    ) -> Result<Type, InternalError> {
        let name = self.ns_type_name(TypeName::for_name(ts_path, &ident.sym.to_string()));
        Ok(Type {
            name: name.clone(),
            is_exported: false,
            info: self.process_class(ts_path, &name, class)?,
        })
    }

    fn process_var(
        &mut self,
        ts_path: &Path,
        VarDeclarator { name, .. }: &VarDeclarator,
    ) -> Result<Type, InternalError> {
        match name {
            Pat::Ident(BindingIdent { id, type_ann }) => Ok(Type {
                name: self.ns_type_name(TypeName::for_name(ts_path, &id.sym.to_string())),
                is_exported: false,
                info: TypeInfo::Var {
                    type_info: Box::new(
                        type_ann
                            .as_ref()
                            .map(|t| self.process_type(ts_path, &t.type_ann))
                            .unwrap_or(Ok(TypeInfo::PrimitiveAny(PrimitiveAny())))?,
                    ),
                },
            }),
            _ => Err(InternalError::with_msg_and_span(
                "we only support regular identifiers for variables",
                name.span(),
            )),
        }
    }

    fn process_fn_decl(
        &mut self,
        ts_path: &Path,
        FnDecl {
            ident, function, ..
        }: &FnDecl,
    ) -> Result<Type, InternalError> {
        Ok(Type {
            name: self.ns_type_name(TypeName::for_name(ts_path, &ident.sym.to_string())),
            is_exported: false,
            info: function.to_type_info(ts_path, self)?,
        })
    }

    fn process_namespace_body(
        &mut self,
        ts_path: &Path,
        declare: bool,
        id: &TsModuleName,
        body: Option<&TsNamespaceBody>,
    ) {
        let name = match id {
            TsModuleName::Ident(ident) => ident.sym.to_string(),
            TsModuleName::Str(s) => s.value.to_string(),
        };

        // TODO: not sure if this heuristic of treating strings as module names
        // and idents as namespaces makes sense...
        if declare && matches!(id, TsModuleName::Str(_)) {
            // declared namespace bodies are interpreted as definitions for separate modules but
            // with access to the surrounding types environment

            if let Some(TsNamespaceBody::TsModuleBlock(block)) = body {
                let file_result = self.process_module_with_items(
                    None, // TODO: should we resolve relative to ts_path?
                    &name,
                    true,
                    |_, _| Ok(block.body.clone()),
                );

                let file = match file_result {
                    Ok(file) => file,
                    Err(err) => {
                        self.record_error(err);
                        return;
                    }
                };

                if file != ts_path {
                    self.set_type_for_file(
                        &file,
                        Ok(Type {
                            name: TypeName {
                                file: ts_path.to_path_buf(),
                                name: TypeIdent::TypeEnvironmentParent(),
                            },
                            is_exported: false,
                            info: TypeInfo::NamespaceImport(NamespaceImport::All {
                                src: file.clone(),
                            }),
                        }),
                    );
                }
            } else {
                println!("Module declaration without body block");
            }

            return;
        }

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

        if let Some(b) = body.as_ref() {
            match b {
                TsNamespaceBody::TsModuleBlock(block) => {
                    self.process_module_items(ts_path, &block.body)
                }
                TsNamespaceBody::TsNamespaceDecl(ns_decl) => self.process_namespace_body(
                    ts_path,
                    ns_decl.declare,
                    &TsModuleName::Ident(ns_decl.id.clone()),
                    Some(&*ns_decl.body),
                ),
            };
        }

        self.namespace_stack.pop();
    }

    fn process_decl(&mut self, ts_path: &Path, decl: &Decl) -> Vec<Result<Type, InternalError>> {
        match decl {
            Decl::TsInterface(iface) => vec![self.process_ts_interface(ts_path, iface)],
            Decl::TsEnum(enm) => vec![self.process_ts_enum(ts_path, enm)],
            Decl::TsTypeAlias(alias) => vec![self.process_ts_alias(ts_path, alias)],
            Decl::Class(class) => vec![self.process_class_type(ts_path, class)],
            Decl::Var(VarDecl { decls, .. }) => decls
                .iter()
                .map(|var| self.process_var(ts_path, var))
                .collect(),
            Decl::TsModule(TsModuleDecl {
                id, declare, body, ..
            }) => {
                self.process_namespace_body(ts_path, *declare, id, body.as_ref());

                Default::default()
            }
            Decl::Fn(fn_decl) => vec![self.process_fn_decl(ts_path, fn_decl)],
        }
    }

    fn process_export_decl(&mut self, ts_path: &Path, ExportDecl { decl, .. }: &ExportDecl) {
        self.process_decl(ts_path, decl)
            .into_iter()
            .map(|typ| match typ {
                Ok(mut t) => {
                    t.is_exported = true;
                    Ok(t)
                }
                Err(e) => Err(e),
            })
            .for_each(|typ| {
                self.set_type_for_file(ts_path, typ);
            });
    }

    fn process_named_export(
        &mut self,
        ts_path: &Path,
        NamedExport {
            src, specifiers, ..
        }: &NamedExport,
    ) {
        if src.is_none() {
            // export { x as y };
            // we need to create an alias for the name
            specifiers.iter().for_each(|spec| match spec {
                ExportSpecifier::Named(ExportNamedSpecifier { orig, exported, .. }) => {
                    let orig = orig.sym.to_string();
                    let orig = self.get_possibly_ns_qualified_name(TypeIdent::Name(orig));

                    if exported.is_none() {
                        // export { x };
                        // we need to just mark the name as exported
                        // TODO: handle the case where export {x} preceeds the definition of x
                        self.types_by_name_by_file
                            .entry(ts_path.to_path_buf())
                            .and_modify(|names_to_types: &mut HashMap<TypeIdent, Type>| {
                                names_to_types.entry(orig).and_modify(|typ: &mut Type| {
                                    typ.is_exported = true;
                                });
                            });
                    } else {
                        // export { x as y };
                        // create an exported alias from y to x
                        let exported = exported.as_ref().unwrap();

                        let typ = Type {
                            name: self.ns_type_name(TypeName::for_name(
                                ts_path,
                                &exported.sym.to_string(),
                            )),
                            is_exported: true,
                            info: TypeInfo::Alias(Alias {
                                target: Box::new(TypeInfo::Ref(self.make_type_ref(
                                    TypeName {
                                        file: ts_path.to_path_buf(),
                                        name: orig,
                                    },
                                    Default::default(),
                                ))),
                                type_params: Default::default(),
                            }),
                        };
                        self.set_type_for_name_for_file(ts_path, typ.name.name.clone(), Ok(typ));
                    }
                }
                _ => {
                    self.record_error(InternalError::with_msg_and_span(
                        "unnamed exports not supported",
                        spec.span(),
                    ));
                }
            });

            return;
        }

        let src = src.as_ref().unwrap();
        let file = {
            let src = src.value.to_string();
            let dir = match path_parent(ts_path) {
                Ok(dir) => dir,
                Err(err) => {
                    self.record_error(err);
                    return;
                }
            };
            let file_result = self.process_module(Some(dir.to_path_buf()), &src);
            match file_result {
                Ok(file) => file,
                Err(err) => {
                    self.record_error(err);
                    return;
                }
            }
        };

        let types: Vec<_> = specifiers
            .iter()
            .map(|spec| match spec {
                ExportSpecifier::Named(ExportNamedSpecifier { orig, exported, .. }) => {
                    let imported_name = orig.sym.to_string();
                    let info = if imported_name == "default" {
                        // export { default as X } from '...'
                        TypeInfo::NamespaceImport(NamespaceImport::Default { src: file.clone() })
                    } else {
                        TypeInfo::NamespaceImport(NamespaceImport::Named {
                            src: file.clone(),
                            name: imported_name,
                        })
                    };

                    Type {
                        name: self.ns_type_name(TypeName::for_name(
                            ts_path,
                            &exported.as_ref().unwrap_or(orig).sym.to_string(),
                        )),
                        is_exported: true,
                        info,
                    }
                }
                ExportSpecifier::Default(ExportDefaultSpecifier { exported }) => Type {
                    name: self.ns_type_name(TypeName::for_name(ts_path, &exported.sym.to_string())),
                    is_exported: true,
                    info: TypeInfo::NamespaceImport(NamespaceImport::Default { src: file.clone() }),
                },
                ExportSpecifier::Namespace(ExportNamespaceSpecifier { name, .. }) => Type {
                    name: self.ns_type_name(TypeName::for_name(ts_path, &name.sym.to_string())),
                    is_exported: true,
                    info: TypeInfo::NamespaceImport(NamespaceImport::All { src: file.clone() }),
                },
            })
            .collect();

        for typ in types {
            self.set_type_for_name_for_file(ts_path, typ.name.name.clone(), Ok(typ));
        }
    }

    fn process_default_alias(&mut self, ts_path: &Path, referent: TypeName) {
        let alias = Type {
            name: self.ns_type_name(TypeName::default_export_for(ts_path.to_path_buf())),
            is_exported: true,
            info: TypeInfo::Alias(Alias {
                target: Box::new(TypeInfo::Ref(
                    self.make_type_ref(referent, Default::default()),
                )),
                type_params: Default::default(),
            }),
        };
        self.set_type_for_name_for_file(ts_path, TypeIdent::DefaultExport(), Ok(alias));
    }

    fn process_default_export(&mut self, ts_path: &Path, def_decl: &DefaultDecl) {
        let mut name_for_ident_with_default_alias = |ident: &Option<Ident>| -> TypeName {
            match ident {
                Some(ident) => {
                    let name = ident.sym.to_string();
                    let type_name = TypeName::for_name(ts_path, &name);
                    self.process_default_alias(ts_path, type_name.clone());
                    type_name
                }
                None => TypeName::default_export_for(ts_path.to_path_buf()),
            }
        };

        match def_decl {
            DefaultDecl::Class(class_expr) => {
                let name = name_for_ident_with_default_alias(&class_expr.ident);
                let info = self.process_class(ts_path, &name, &class_expr.class);
                self.export_type_with_name(ts_path, name, info);
            }
            DefaultDecl::TsInterfaceDecl(interface_decl) => {
                let iface_result = self.process_ts_interface(ts_path, interface_decl);
                match iface_result {
                    Ok(mut iface) => {
                        iface.is_exported = true;
                        let iface_name = iface.name.clone();
                        self.set_type_for_file(ts_path, Ok(iface));
                        self.process_default_alias(ts_path, iface_name);
                    }
                    Err(err) => {
                        self.record_error(err);
                    }
                }
            }
            DefaultDecl::Fn(fn_expr) => {
                let name = name_for_ident_with_default_alias(&fn_expr.ident);
                let info = fn_expr.function.to_type_info(ts_path, self);
                self.export_type_with_name(ts_path, name, info);
            }
        }
    }

    fn export_default_alias(&mut self, ts_path: &Path, decl: &TsExportAssignment) {
        match &*decl.expr {
            Expr::Ident(ident) => {
                let name = ident.sym.to_string();
                let type_name = TypeName::for_name(ts_path, &name);
                self.process_default_alias(ts_path, type_name);
            }
            _ => {
                self.record_error(InternalError::with_msg_and_span(
                    "non-ident assignment exports are not supported",
                    decl.span(),
                ));
            }
        }
    }

    fn process_module_decl(&mut self, ts_path: &Path, module_decl: &ModuleDecl) {
        match module_decl {
            ModuleDecl::Import(decl) => self.process_import_decl(ts_path, decl),
            ModuleDecl::ExportDecl(decl) => self.process_export_decl(ts_path, decl),
            ModuleDecl::ExportNamed(decl) => self.process_named_export(ts_path, decl),
            ModuleDecl::ExportDefaultDecl(decl) => self.process_default_export(ts_path, &decl.decl),
            ModuleDecl::ExportDefaultExpr(_decl) => {
                println!("export default expr, {:?}", _decl);
            }
            ModuleDecl::ExportAll(decl) => self.process_export_all(ts_path, decl),
            ModuleDecl::TsImportEquals(_decl) => {
                println!("import equals, {:?}", _decl);
            }
            ModuleDecl::TsExportAssignment(decl) => {
                self.export_default_alias(ts_path, decl);
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
                    let typ = if self.currently_in_namespace() {
                        // decls in namespaces or modules are exported by default
                        typ.map(|mut t| {
                            t.is_exported = true;
                            t
                        })
                    } else {
                        typ
                    };
                    self.set_type_for_file(ts_path, typ)
                });
        }
    }

    fn currently_in_namespace(&self) -> bool {
        !self.namespace_stack.is_empty()
    }
}

fn path_parent(path: &Path) -> Result<&Path, InternalError> {
    match path.parent() {
        Some(base) => Ok(base),
        None => Err(std::io::Error::new(std::io::ErrorKind::NotFound, "path has no parent").into()),
    }
}

fn make_enum_member(
    id: &TsEnumMemberId,
    init: &Option<Box<Expr>>,
    last_numeric_discriminator: &mut Option<f64>,
) -> Result<EnumMember, InternalError> {
    Ok(EnumMember {
        id: match id {
            TsEnumMemberId::Ident(ident) => ident.sym.to_string(),
            TsEnumMemberId::Str(s) => s.value.to_string(),
        },
        value: init
            .as_ref()
            .and_then(|v| match &**v {
                Expr::Lit(l) => match l {
                    Lit::Str(s) => Some(Ok(EnumValue::Str(s.value.to_string()))),
                    Lit::Num(n) => {
                        *last_numeric_discriminator = Some(n.value);
                        Some(Ok(EnumValue::Num(n.value)))
                    }
                    _ => None,
                },
                _ => Some(Err(InternalError::with_msg_and_span(
                    "enums with non-literal initializers not supported",
                    v.span(),
                ))),
            })
            .or_else(|| {
                last_numeric_discriminator.map(|last| {
                    // if we have a prior EnumValue::Num member, use the prior
                    // value + 1 as our enum value
                    let our_init = last + 1.0;
                    *last_numeric_discriminator = Some(our_init);
                    Ok(EnumValue::Num(our_init))
                })
            })
            .transpose()?,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::fs::MemFs;

    fn get_types_for_code(ts_code: &str) -> Result<HashMap<TypeIdent, Type>, Error> {
        let test_path: &Path = Path::new("/test.d.ts");
        let mut fs: MemFs = Default::default();
        fs.set_cwd(Path::new("/"));
        fs.add_file_at(test_path, ts_code.to_string());

        let mut tbnbf = TsTypes::parse(Arc::new(fs) as ArcFs, &test_path.to_string_lossy())?;

        assert_eq!(tbnbf.len(), 1);

        let types = tbnbf.remove(test_path);

        assert!(types.is_some());

        Ok(types.unwrap())
    }

    macro_rules! test_exported_type {
        ($code:literal, $name:literal, $expected_info:pat, $assertions:block) => {
            {
                let code = $code;
                let name = TypeIdent::Name($name.to_string());
                test_exported_type!(
                    code,
                    name,
                    $expected_info,
                    $assertions
                )
            }
        };
        ($code:ident, $name:ident, $expected_info:pat, $assertions:block) => {
            {
                let types = get_types_for_code($code)?;

                let ty = types.get(&$name);
                assert!(ty.is_some());

                let ty = ty.unwrap();
                assert!(ty.is_exported);

                if let $expected_info = &ty.info $assertions else {
                    assert!(false);
                }

                Ok(())
            }
        };
    }

    #[test]
    fn test_basic_parsing() -> Result<(), Error> {
        let types = get_types_for_code(r#"export type Test = number | string | null;"#)?;

        assert!(types.get(&TypeIdent::Name("Test".to_string())).is_some());

        Ok(())
    }

    #[test]
    fn test_class_skip_private_properties() -> Result<(), Error> {
        test_exported_type!(
            r#"export class A {
                private n: number;
                x: string;
                public y: number;
                protected z: string;
            }"#,
            "A",
            TypeInfo::Class(c),
            {
                assert!(c.super_class.is_none());
                assert_eq!(c.members.len(), 3);
                assert!(c.members.contains_key("x"));
                assert!(c.members.contains_key("y"));
                assert!(c.members.contains_key("z"));
                assert!(!c.members.contains_key("n"));
            }
        )
    }

    #[test]
    fn test_class_property_parsing() -> Result<(), Error> {
        test_exported_type!(
            r#"export declare class A {
                private n: number;

                get thing(): number;

                set thing(n: number);
            }"#,
            "A",
            TypeInfo::Class(c),
            {
                assert!(c.super_class.is_none());
                assert_eq!(c.members.len(), 1);
                let thing = c.members.get("thing");
                assert!(thing.is_some());
                let thing = thing.unwrap();
                assert_eq!(
                    *thing,
                    Member::Property(TypeInfo::PrimitiveNumber(PrimitiveNumber()))
                );
            }
        )
    }

    fn test_first_fn_param(
        ts_code: &str,
        fn_name: &str,
        expected_param: &Param,
    ) -> Result<(), Error> {
        let fn_name = TypeIdent::Name(fn_name.to_string());
        test_exported_type!(ts_code, fn_name, TypeInfo::Func(f), {
            assert_eq!(f.params.len(), 1);
            assert_eq!(f.params.first().unwrap(), expected_param);
        })
    }

    #[test]
    fn test_fn_param_array_destructure() -> Result<(), Error> {
        test_first_fn_param(
            r#"export declare function arrayPat([a, b]: [string, number]);"#,
            "arrayPat",
            &Param {
                name: "arg0".to_string(),
                type_info: TypeInfo::Tuple(Tuple {
                    types: vec![
                        TypeInfo::PrimitiveString(PrimitiveString()),
                        TypeInfo::PrimitiveNumber(PrimitiveNumber()),
                    ],
                }),
                is_variadic: false,
            },
        )?;

        Ok(())
    }

    #[test]
    fn test_fn_param_map_destructure() -> Result<(), Error> {
        test_first_fn_param(
            r#"export declare function mapPat(m: {[n: string]: string});"#,
            "mapPat",
            &Param {
                name: "m".to_string(),
                type_info: TypeInfo::Mapped {
                    value_type: Box::new(TypeInfo::PrimitiveString(PrimitiveString())),
                },
                is_variadic: false,
            },
        )?;

        Ok(())
    }

    #[test]
    fn test_fn_param_obj_destructure() -> Result<(), Error> {
        let mut fields: HashMap<String, TypeInfo> = HashMap::new();
        fields.insert(
            "a".to_string(),
            TypeInfo::PrimitiveNumber(PrimitiveNumber()),
        );
        fields.insert(
            "b".to_string(),
            TypeInfo::PrimitiveString(PrimitiveString()),
        );

        test_first_fn_param(
            r#"export declare function objPat(o: {a: number, b: string});"#,
            "objPat",
            &Param {
                name: "o".to_string(),
                type_info: TypeInfo::Interface(Interface {
                    indexer: None,
                    extends: Default::default(),
                    fields,
                    type_params: Default::default(),
                    constructor: None,
                }),
                is_variadic: false,
            },
        )?;

        Ok(())
    }

    #[test]
    fn test_fn_param_rest() -> Result<(), Error> {
        test_first_fn_param(
            r#"export declare function restPat(...b: string);"#,
            "restPat",
            &Param {
                name: "b".to_string(),
                type_info: TypeInfo::PrimitiveString(PrimitiveString()),
                is_variadic: true,
            },
        )?;

        Ok(())
    }

    #[test]
    fn test_interface_basic_props() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface A { n: number }"#,
            "A",
            TypeInfo::Interface(i),
            {
                assert_eq!(i.fields.len(), 1);
                let n = i.fields.get("n");
                assert!(n.is_some());
                let n = n.unwrap();
                assert_eq!(*n, TypeInfo::PrimitiveNumber(PrimitiveNumber()));
            }
        )
    }

    #[test]
    fn test_interface_computed_props() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface A {
                get num(): number;
                set num(n: number);
                get getter(): string;
                set setter(s: string);
            }"#,
            "A",
            TypeInfo::Interface(i),
            {
                assert_eq!(i.fields.len(), 3);

                let num = i.fields.get("num");
                assert!(num.is_some());
                let num = num.unwrap();
                assert_eq!(*num, TypeInfo::PrimitiveNumber(PrimitiveNumber()));

                let getter = i.fields.get("getter");
                assert!(getter.is_some());
                let getter = getter.unwrap();
                assert_eq!(*getter, TypeInfo::PrimitiveString(PrimitiveString()));

                let setter = i.fields.get("setter");
                assert!(setter.is_some());
                let setter = setter.unwrap();
                assert_eq!(*setter, TypeInfo::PrimitiveString(PrimitiveString()));
            }
        )
    }

    #[test]
    fn test_interface_indexer() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface A {
                [n: string]: number;
            }"#,
            "A",
            TypeInfo::Interface(i),
            {
                assert!(i.fields.is_empty());
                assert!(i.indexer.is_some());
                let indexer = i.indexer.as_ref().unwrap();
                assert_eq!(
                    *indexer.type_info,
                    TypeInfo::PrimitiveNumber(PrimitiveNumber())
                );
            }
        )
    }

    #[test]
    fn test_interface_constructor() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface A {
                new (n: number): A;
            }"#,
            "A",
            TypeInfo::Interface(i),
            {
                assert!(i.fields.is_empty());
                assert!(i.indexer.is_none());
                assert!(i.constructor.is_some());
                let ctor = i.constructor.as_ref().unwrap();
                assert_eq!(ctor.params.len(), 1);
                let param = ctor.params.first().unwrap();
                assert_eq!(
                    *param,
                    Param {
                        name: "n".to_string(),
                        type_info: TypeInfo::PrimitiveNumber(PrimitiveNumber()),
                        is_variadic: false,
                    }
                );
            }
        )
    }

    #[test]
    fn test_raw_enum() -> Result<(), Error> {
        test_exported_type!(
            r#"export enum Enm {
                A,
                B,
                C
            }"#,
            "Enm",
            TypeInfo::Enum(e),
            {
                assert_eq!(e.members.len(), 3);
                assert!(e.members.contains(&EnumMember {
                    id: "A".to_string(),
                    value: None,
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "B".to_string(),
                    value: None,
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "C".to_string(),
                    value: None,
                }));
            }
        )
    }

    #[test]
    fn test_string_enum() -> Result<(), Error> {
        test_exported_type!(
            r#"export enum Enm {
                A = "First",
                B = "Second"
            }"#,
            "Enm",
            TypeInfo::Enum(e),
            {
                assert_eq!(e.members.len(), 2);
                assert!(e.members.contains(&EnumMember {
                    id: "A".to_string(),
                    value: Some(EnumValue::Str("First".to_string())),
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "B".to_string(),
                    value: Some(EnumValue::Str("Second".to_string())),
                }));
            }
        )
    }

    #[test]
    fn test_number_enum() -> Result<(), Error> {
        test_exported_type!(
            r#"export enum Enm {
                A = 1,
                B = 2,
                C,
                D = 5,
                E,
            }"#,
            "Enm",
            TypeInfo::Enum(e),
            {
                assert_eq!(e.members.len(), 5);
                assert!(e.members.contains(&EnumMember {
                    id: "A".to_string(),
                    value: Some(EnumValue::Num(1.0)),
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "B".to_string(),
                    value: Some(EnumValue::Num(2.0)),
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "C".to_string(),
                    value: Some(EnumValue::Num(3.0)),
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "D".to_string(),
                    value: Some(EnumValue::Num(5.0)),
                }));
                assert!(e.members.contains(&EnumMember {
                    id: "E".to_string(),
                    value: Some(EnumValue::Num(6.0)),
                }));
            }
        )
    }

    #[test]
    fn test_this_type() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface Something {
                thisThing: this;
            }"#,
            "Something",
            TypeInfo::Interface(i),
            {
                assert_eq!(i.fields.len(), 1);
                let n = i.fields.get("thisThing");
                assert!(n.is_some());
                let n = n.unwrap();
                assert_eq!(*n, TypeInfo::PrimitiveAny(PrimitiveAny()));
            }
        )
    }

    #[test]
    fn test_type_query() -> Result<(), Error> {
        test_exported_type!(
            r#"class MyClass {
                n: number;
            }
            export interface I {
                c: typeof MyClass;
            }
            "#,
            "I",
            TypeInfo::Interface(i),
            {
                assert_eq!(i.fields.len(), 1);
                let c = i.fields.get("c");
                assert!(c.is_some());
                let c = c.unwrap();
                if let TypeInfo::TypeQuery(TypeQuery::LookupRef(tr)) = c {
                    assert_eq!(tr.referent.name, TypeIdent::Name("MyClass".to_string()));
                } else {
                    assert!(false);
                }
            }
        )
    }

    #[test]
    fn test_module() -> Result<(), Error> {
        let expected_type =
            TypeIdent::QualifiedName(vec!["A".to_string(), "B".to_string(), "Hi".to_string()]);
        let ts = r#"module A.B {
            export class Hi {}
        }
        "#;
        test_exported_type!(ts, expected_type, TypeInfo::Class(c), {
            assert!(c.members.is_empty());
        })
    }

    #[test]
    fn test_indexed() -> Result<(), Error> {
        test_exported_type!(
            r#"export interface A {
                environment: {[key: string]: string};
            }"#,
            "A",
            TypeInfo::Interface(i),
            {
                assert_eq!(i.fields.len(), 1);
                let environment = i.fields.get("environment");
                assert!(environment.is_some());
                let environment = environment.unwrap();
                if let TypeInfo::Mapped { value_type } = environment {
                    assert_eq!(**value_type, TypeInfo::PrimitiveString(PrimitiveString()));
                } else {
                    assert!(false);
                }
            }
        )
    }

    #[test]
    fn test_ns_super() -> Result<(), Error> {
        let ts_code = r#"
            export class A {
                b: B;
            }
            class B {}
            namespace other {
                export class Inner extends A {
                    i: number;
                }
            }
        "#;
        let name = TypeIdent::QualifiedName(vec!["other".to_string(), "Inner".to_string()]);
        test_exported_type!(ts_code, name, TypeInfo::Class(c), {
            assert!(c.super_class.is_some());
        })
    }
}
