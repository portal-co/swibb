//! Hot Module Replacement (HMR) utilities for JavaScript modules.
//!
//! This module provides utilities for working with hot module replacement in JavaScript,
//! particularly for tools like Webpack and Vite. It includes helpers for:
//! - Detecting HMR accept calls
//! - Generating import manifests
//! - Creating HMR-compatible code
//! - Managing global variable access

use crate::*;
use swc_common::Span;
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, BinExpr, BinaryOp, CallExpr, Expr, ExprOrSpread, ExprStmt,
    Ident, IdentName, ImportDecl, ImportNamedSpecifier, KeyValuePatProp, KeyValueProp, Lit,
    MemberExpr, MetaPropExpr, MetaPropKind, ObjectLit, ObjectPat, OptCall, OptChainBase,
    OptChainExpr, Pat, Prop, PropOrSpread, Stmt, Str,
};

/// Standard HMR property names used by module bundlers.
///
/// These are the property names on `import.meta` that provide HMR functionality:
/// - `hot`: Standard HMR API (Webpack, Vite)
/// - `webpackHot`: Webpack-specific HMR API
pub const KINDS: &'static [&'static str] = &["hot", "webpackHot"];

/// Checks if an expression is an HMR accept call.
///
/// Detects patterns like `import.meta.hot.accept(...)` or `import.meta.webpackHot.accept(...)`
/// which are used to register hot module replacement callbacks.
///
/// # Arguments
///
/// * `a` - The expression to check
///
/// # Returns
///
/// `true` if the expression is an HMR accept call, `false` otherwise
pub fn is_meta_hot_accept(a: &Expr) -> bool {
    match match a {
        Expr::Member(m) => m,
        Expr::OptChain(o) => match &*o.base {
            OptChainBase::Member(m) => m,
            _ => return false,
        },
        _ => return false,
    } {
        m => {
            if m.prop.is_ident_with("accept") {
                match match &*m.obj {
                    Expr::Member(m) => m,
                    Expr::OptChain(o) => match &*o.base {
                        OptChainBase::Member(m) => m,
                        _ => return false,
                    },
                    _ => return false,
                } {
                    m => {
                        if m.prop.is_ident_with("hot") {
                            m.obj
                                .as_meta_prop()
                                .is_some_and(|a| a.kind == MetaPropKind::ImportMeta)
                        } else {
                            false
                        }
                    }
                }
            } else {
                false
            }
        }
    }
}

/// Generates an expression that accesses the HMR API.
///
/// Creates an expression like `import.meta.hot` or `import.meta.webpackHot`,
/// using optional chaining for safe access.
///
/// # Arguments
///
/// * `span` - Source location for the generated expression
/// * `kind` - The HMR property name ("hot" or "webpackHot")
///
/// # Returns
///
/// An expression accessing the specified HMR API
pub fn import_meta_hot(span: Span, kind: &str) -> Expr {
    Expr::OptChain(OptChainExpr {
        span,
        optional: false,
        base: Box::new(swc_ecma_ast::OptChainBase::Member(MemberExpr {
            span,
            obj: Box::new(Expr::MetaProp(MetaPropExpr {
                span,
                kind: swc_ecma_ast::MetaPropKind::ImportMeta,
            })),
            prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                span,
                sym: Atom::new(kind),
            }),
        })),
    })
}

/// Generates a disposer function for HMR cleanup.
///
/// Creates an arrow function that calls the dispose method on all configured
/// HMR APIs. This is used to clean up resources when a module is updated.
///
/// The generated function looks like:
/// ```javascript
/// ($) => (import.meta.hot?.dispose?.($), import.meta.webpackHot?.dispose?.($), undefined)
/// ```
///
/// # Arguments
///
/// * `span` - Source location for the generated expression
/// * `ctxt` - Syntax context for the generated identifiers
/// * `kinds` - Optional list of HMR property names (defaults to KINDS)
///
/// # Returns
///
/// An arrow function expression that disposes HMR resources
pub fn disposer(span: Span, ctxt: SyntaxContext, kinds: Option<&[&str]>) -> Expr {
    let kinds = kinds.unwrap_or(KINDS);
    let i = Ident::new(Atom::new("$"), span, ctxt);
    Expr::Arrow(ArrowExpr {
        span,
        ctxt,
        params: [Pat::Ident(i.clone().into())].into_iter().collect(),
        is_async: false,
        is_generator: false,
        type_params: None,
        return_type: None,
        body: Box::new(swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(Expr::Seq(
            SeqExpr {
                span,
                exprs: kinds
                    .iter()
                    .cloned()
                    .map(|kind| {
                        Box::new(Expr::OptChain(OptChainExpr {
                            span,
                            optional: false,
                            base: Box::new(swc_ecma_ast::OptChainBase::Call(OptCall {
                                span,
                                ctxt,
                                args: [ExprOrSpread {
                                    expr: i.clone().into(),
                                    spread: None,
                                }]
                                .into_iter()
                                .collect(),
                                type_args: None,
                                callee: Box::new(Expr::OptChain(OptChainExpr {
                                    span,
                                    optional: false,
                                    base: Box::new(swc_ecma_ast::OptChainBase::Member(
                                        MemberExpr {
                                            span,
                                            obj: Box::new(import_meta_hot(span, kind)),
                                            prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                                span,
                                                sym: Atom::new("dispose"),
                                            }),
                                        },
                                    )),
                                })),
                            })),
                        }))
                    })
                    .chain([Expr::undefined(span)])
                    .collect(),
            },
        )))),
    })
}

/// Manages imports and global variable references for module transformations.
///
/// This struct tracks:
/// - Which identifiers should be imported from which modules
/// - Global variable references that need to be resolved
///
/// It can then generate the appropriate import statements and variable declarations
/// with HMR support for hot module replacement scenarios.
#[derive(Default)]
pub struct ImportManifest {
    /// Map from module path to (export name -> local identifier)
    map: BTreeMap<Atom, BTreeMap<Atom, Id>>,
    /// Map from global property path to local identifier
    /// e.g., ["console", "log"] -> console$log
    globals: BTreeMap<Vec<Atom>, Id>,
}
impl ImportManifest {
    /// Gets or creates an identifier for a module import.
    ///
    /// If this module/name combination hasn't been seen before, creates a new
    /// unique identifier for it. Returns the same identifier on subsequent calls
    /// with the same module and name.
    ///
    /// # Arguments
    ///
    /// * `module` - The module path to import from
    /// * `name` - The export name to import
    ///
    /// # Returns
    ///
    /// A unique identifier for this import
    pub fn get(&mut self, module: Atom, name: Atom) -> Id {
        return self
            .map
            .entry(module)
            .or_default()
            .entry(name.clone())
            .or_insert_with(|| (name.clone(), SyntaxContext::empty().apply_mark(Mark::new())))
            .clone();
    }
    
    /// Gets or creates an identifier for a global variable access path.
    ///
    /// For a path like `["console", "log"]`, creates an identifier that will
    /// be bound to `globalThis.console.log`. Returns the same identifier on
    /// subsequent calls with the same path.
    ///
    /// # Arguments
    ///
    /// * `x` - The property access path (e.g., `["console", "log"]`)
    ///
    /// # Returns
    ///
    /// A unique identifier for this global access
    pub fn global(&mut self, x: &[Atom]) -> Id {
        return self
            .globals
            .entry(x.to_owned())
            .or_insert_with(|| {
                (
                    Atom::new(match x.get(0) {
                        None => format!(""),
                        Some(a) => {
                            let mut v = format!("{a}");
                            for a in &x[1..] {
                                v = format!("{v}${a}");
                            }
                            v
                        }
                    }),
                    SyntaxContext::empty().apply_mark(Mark::new()),
                )
            })
            .clone();
    }
    
    /// Generates import statements and variable declarations for all tracked imports and globals.
    ///
    /// This method produces:
    /// 1. Import statements for each module with all its named imports
    /// 2. Let declarations to create mutable local bindings for the imports
    /// 3. HMR accept calls for each imported module (if kinds is not empty)
    /// 4. Const declarations for global variable accesses
    ///
    /// # Arguments
    ///
    /// * `span` - Source location for generated code
    /// * `kinds` - HMR property names to generate accept calls for (defaults to KINDS)
    ///
    /// # Returns
    ///
    /// An iterator over module items (imports and statements) to be inserted at
    /// the beginning of the module
    pub fn render(
        &self,
        span: swc_common::Span,
        kinds: Option<&[&str]>,
    ) -> impl Iterator<Item = ModuleItem> {
        let kinds = kinds.unwrap_or(KINDS);
        return self
            .map
            .iter()
            .flat_map(move |(a, b)| {
                [
                    ModuleItem::ModuleDecl(swc_ecma_ast::ModuleDecl::Import(ImportDecl {
                        span,
                        specifiers: b
                            .iter()
                            .map(|(c, d)| {
                                swc_ecma_ast::ImportSpecifier::Named(ImportNamedSpecifier {
                                    span,
                                    local: swc_ecma_ast::Ident {
                                        span,
                                        ctxt: d.1,
                                        sym: Atom::new(format!("$import${}", &d.0)),
                                        optional: false,
                                    },
                                    imported: Some(swc_ecma_ast::ModuleExportName::Str(
                                        swc_ecma_ast::Str {
                                            span,
                                            value: c.clone().into(),
                                            raw: None,
                                        },
                                    )),
                                    is_type_only: false,
                                })
                            })
                            .collect(),
                        type_only: false,
                        with: None,
                        phase: Default::default(),
                        src: Box::new(swc_ecma_ast::Str {
                            span,
                            raw: None,
                            value: a.clone().into(),
                        }),
                    })),
                    ModuleItem::Stmt(Stmt::Decl(swc_ecma_ast::Decl::Var(Box::new(VarDecl {
                        span,
                        ctxt: Default::default(),
                        kind: if kinds.len() == 0{
                            swc_ecma_ast::VarDeclKind::Const
                        }else{
                            swc_ecma_ast::VarDeclKind::Let
                        },
                        declare: false,
                        decls: b
                            .iter()
                            .map(|(_, d)| VarDeclarator {
                                span,
                                name: d.clone().into(),
                                init: Some(Box::new(Expr::Ident(swc_ecma_ast::Ident {
                                    span,
                                    ctxt: d.1,
                                    sym: Atom::new(format!("$import${}", &d.0)),
                                    optional: false,
                                }))),
                                definite: false,
                            })
                            .collect(),
                    })))),
                ].into_iter().chain(kinds.iter().cloned().flat_map(move|kind|[ModuleItem::Stmt(Stmt::Expr(ExprStmt {
                        span,
                        expr: Box::new(Expr::OptChain(OptChainExpr {
                            span,
                            optional: false,
                            base: Box::new(swc_ecma_ast::OptChainBase::Call(OptCall {
                                span,
                                ctxt: Default::default(),
                                callee: Box::new(Expr::OptChain(OptChainExpr {
                                    span,
                                    optional: false,
                                    base: Box::new(swc_ecma_ast::OptChainBase::Member(
                                        MemberExpr {
                                            span,
                                            obj: import_meta_hot(span,kind).into(),
                                            prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                                span,
                                                sym: Atom::new("accept"),
                                            }),
                                        },
                                    )),
                                })),
                                args: [
                                    Expr::Lit(Lit::Str(Str{span,raw:None,value:a.clone().into()})),
                                    match Ident::new_private(Atom::new(format!("$new${}",a)), span){
                                        id => Expr::Arrow(ArrowExpr {
                                            span,
                                            ctxt: Default::default(),
                                            params: [Pat::Ident(id.clone().into())].into_iter().collect(),
                                            body: Box::new(Expr::Assign(AssignExpr{
                                                span,
                                                op:AssignOp::Assign,
                                                right:id.into(),
                                                left:swc_ecma_ast::AssignTarget::Pat(swc_ecma_ast::AssignTargetPat::Object(ObjectPat{
                                                    span,
                                                    optional:false,
                                                    type_ann:None,
                                                    props:b.iter().map(|(c,d)|swc_ecma_ast::ObjectPatProp::KeyValue(KeyValuePatProp{
                                                        key: swc_ecma_ast::PropName::Str(Str {
                                                            span,
                                                            value: c.clone().into(),
                                                            raw: None }),
                                                        value: Box::new(Pat::Ident(d.clone().into()))
                                                    })).collect()})
                                                )}).into()),
                                            is_async: false,
                                            is_generator: false,
                                            type_params: None,
                                            return_type: None })
                                    },
                                ].into_iter().map(|a|ExprOrSpread { spread: None, expr: Box::new(a) }).collect(),
                                type_args: None,
                            })),
                        })),
                    })),]))
            })
            .chain(self.globals.iter().map(move |(a, b)| {
                ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(Box::new(
                    VarDecl {
                        span,
                        ctxt: b.1,
                        kind: swc_ecma_ast::VarDeclKind::Const,
                        declare: false,
                        decls: vec![VarDeclarator {
                            span,
                            name: swc_ecma_ast::Pat::Ident(b.clone().into()),
                            init: Some(a.iter().fold(
                                Box::new(swc_ecma_ast::Expr::Ident(swc_ecma_ast::Ident {
                                    span,
                                    ctxt: Default::default(),
                                    sym: Atom::new("globalThis"),
                                    optional: false,
                                })),
                                |e, i| {
                                    Box::new(swc_ecma_ast::Expr::Member(swc_ecma_ast::MemberExpr {
                                        span,
                                        obj: e,
                                        prop: swc_ecma_ast::MemberProp::Ident(
                                            swc_ecma_ast::IdentName {
                                                span,
                                                sym: i.clone(),
                                            },
                                        ),
                                    }))
                                },
                            )),
                            definite: false,
                        }],
                    },
                ))))
            }));
    }
}
