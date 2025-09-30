use swc_common::Span;
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, BinExpr, BinaryOp, CallExpr, Expr, ExprOrSpread, ExprStmt, Ident, IdentName, ImportDecl, ImportNamedSpecifier, KeyValuePatProp, KeyValueProp, Lit, MemberExpr, MetaPropExpr, ObjectLit, ObjectPat, OptCall, OptChainExpr, Pat, Prop, PropOrSpread, Stmt, Str
};

use crate::*;
pub fn import_meta_hot(span: Span) -> Expr {
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
                sym: Atom::new("hot"),
            }),
        })),
    })
}
pub fn prune(span: Span, ctxt: SyntaxContext) -> Expr {
    let i = Ident::new(Atom::new("$"), span, ctxt);
    Expr::Object(ObjectLit {
        span,
        props: [PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
            key: swc_ecma_ast::PropName::Ident(IdentName {
                span,
                sym: Atom::new("prune"),
            }),
            value: Box::new(Expr::Arrow(ArrowExpr {
                span,
                ctxt,
                params: [Pat::Ident(i.clone().into())].into_iter().collect(),
                is_async: false,
                is_generator: false,
                type_params: None,
                return_type: None,
                body: Box::new(swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(
                    Expr::OptChain(OptChainExpr {
                        span,
                        optional: false,
                        base: Box::new(swc_ecma_ast::OptChainBase::Call(OptCall {
                            span,
                            ctxt,
                            args: [ExprOrSpread {
                                expr: i.into(),
                                spread: None,
                            }]
                            .into_iter()
                            .collect(),
                            type_args: None,
                            callee: Box::new(Expr::OptChain(OptChainExpr {
                                span,
                                optional: false,
                                base: Box::new(swc_ecma_ast::OptChainBase::Member(MemberExpr {
                                    span,
                                    obj: Box::new(import_meta_hot(span)),
                                    prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                        span,
                                        sym: Atom::new("dispose"),
                                    }),
                                })),
                            })),
                        })),
                    }),
                ))),
            })),
        })))]
        .into_iter()
        .collect(),
    })
}
#[derive(Default)]
pub struct ImportManifest {
    map: BTreeMap<Atom, BTreeMap<Atom, Id>>,
    globals: BTreeMap<Vec<Atom>, Id>,
}
impl ImportManifest {
    pub fn get(&mut self, module: Atom, name: Atom) -> Id {
        return self
            .map
            .entry(module)
            .or_default()
            .entry(name.clone())
            .or_insert_with(|| (name.clone(), SyntaxContext::empty().apply_mark(Mark::new())))
            .clone();
    }
    pub fn global(&mut self, x: &[Atom]) -> Id {
        return self
            .globals
            .entry(x.to_owned())
            .or_insert_with(|| {
                (
                    Atom::new(match x.get(0){
                        None => format!(""),
                        Some(a) => {
                            let mut v = format!("{a}");
                            for a in &x[1..]{
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
    pub fn render(&self, span: swc_common::Span) -> impl Iterator<Item = ModuleItem> {
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
                                            value: c.clone(),
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
                            value: a.clone(),
                        }),
                    })),
                    ModuleItem::Stmt(Stmt::Decl(swc_ecma_ast::Decl::Var(Box::new(VarDecl {
                        span,
                        ctxt: Default::default(),
                        kind: swc_ecma_ast::VarDeclKind::Let,
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
                    ModuleItem::Stmt(Stmt::Expr(ExprStmt {
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
                                            obj: import_meta_hot(span).into(),
                                            prop: swc_ecma_ast::MemberProp::Ident(IdentName {
                                                span,
                                                sym: Atom::new("accept"),
                                            }),
                                        },
                                    )),
                                })),
                                args: [
                                    Expr::Lit(Lit::Str(Str{span,raw:None,value:a.clone()})),
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
                                                            value: c.clone(), 
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
                    })),
                ]
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
