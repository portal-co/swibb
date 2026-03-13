//! Transforms arrow functions into regular function expressions.
//!
//! Arrow functions in JavaScript have different semantics from regular functions,
//! particularly regarding `this` binding and the `new.target` meta-property. This
//! module provides transformers to convert arrow functions into regular functions
//! while preserving their lexical scope semantics.

use crate::*;
use swc_ecma_ast::{FnExpr, Function, MetaPropExpr, MetaPropKind, Param, ThisExpr};

/// Helper visitor for transforming `this` and `new.target` inside inflated arrow functions.
///
/// When an arrow function is converted to a regular function, references to `this`
/// and `new.target` inside it need to be converted to identifiers that capture the
/// lexical values from the outer scope.
struct InflateInner {
    /// The syntax context used for the generated identifiers
    ctx: SyntaxContext,
}
impl VisitMut for InflateInner {
    fn visit_mut_function(&mut self, node: &mut Function) {}
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        *node = match take(node) {
            Expr::This(t) => Expr::Ident(Ident::new(Atom::new("this"), t.span, self.ctx)),
            Expr::MetaProp(m) if m.kind == MetaPropKind::NewTarget => {
                Expr::Ident(Ident::new(Atom::new("new.target"), m.span, self.ctx))
            }
            node => node,
        }
    }
}

/// Transforms arrow functions into regular function expressions.
///
/// This visitor converts arrow functions to regular functions while preserving their
/// lexical binding semantics. Since arrow functions capture `this` and `new.target`
/// from their enclosing scope, this transformer:
///
/// 1. Converts arrow functions to regular functions
/// 2. Introduces const declarations for `this` and `new.target` that capture the
///    lexical values
/// 3. Replaces references to `this` and `new.target` inside the function with
///    references to these const declarations
///
/// For example:
/// ```javascript
/// const f = (x) => this.x + x;
/// ```
/// Becomes approximately:
/// ```javascript
/// const this$0 = this;
/// const f = function(x) { return this$0.x + x; };
/// ```
#[derive(Default)]
#[non_exhaustive]
pub struct Inflate {
    /// Map of identifiers to be declared as const bindings at the beginning
    /// of the enclosing scope (for capturing `this` and `new.target`)
    pub idents: BTreeMap<Id, Expr>,
}
impl VisitMut for Inflate {
    fn visit_mut_stmts(&mut self, node: &mut Vec<Stmt>) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let idents = replace(&mut self.idents, old);
        node.insert(
            0,
            Stmt::Decl(Decl::Var(Box::new(VarDecl {
                span: Default::default(),
                ctxt: Default::default(),
                kind: VarDeclKind::Const,
                declare: false,
                decls: idents
                    .into_iter()
                    .map(|((a, c), v)| VarDeclarator {
                        span: v.span(),
                        name: Pat::Ident(Ident::new(a, v.span(), c).into()),
                        init: Some(Box::new(v)),
                        definite: false,
                    })
                    .collect(),
            }))),
        );
    }
    fn visit_mut_module(&mut self, node: &mut Module) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let idents = replace(&mut self.idents, old);
        node.body.insert(
            0,
            ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                span: Default::default(),
                ctxt: Default::default(),
                kind: VarDeclKind::Const,
                declare: false,
                decls: idents
                    .into_iter()
                    .map(|((a, c), v)| VarDeclarator {
                        span: v.span(),
                        name: Pat::Ident(Ident::new(a, v.span(), c).into()),
                        init: Some(Box::new(v)),
                        definite: false,
                    })
                    .collect(),
            })))),
        );
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        *node = match take(node) {
            Expr::Arrow(a) => {
                let ctx = SyntaxContext::empty().apply_mark(Mark::new());
                self.idents.insert(
                    (Atom::new("this"), ctx),
                    Expr::This(ThisExpr { span: a.span }),
                );
                self.idents.insert(
                    (Atom::new("new.target"), ctx),
                    Expr::MetaProp(MetaPropExpr {
                        span: a.span,
                        kind: MetaPropKind::NewTarget,
                    }),
                );
                Expr::Fn(FnExpr {
                    ident: None,
                    function: Box::new(Function {
                        params: a
                            .params
                            .into_iter()
                            .map(|a| Param {
                                span: a.span(),
                                decorators: Default::default(),
                                pat: a,
                            })
                            .collect(),
                        decorators: Default::default(),
                        span: a.span,
                        ctxt: ctx,
                        body: {
                            let mut b = a.body;
                            b.visit_mut_with(&mut InflateInner { ctx });
                            Some(match *b {
                                BlockStmtOrExpr::BlockStmt(b) => b,
                                BlockStmtOrExpr::Expr(e) => BlockStmt {
                                    span: e.span(),
                                    ctxt: ctx,
                                    stmts: [Stmt::Return(ReturnStmt {
                                        span: e.span(),
                                        arg: Some(e),
                                    })]
                                    .into_iter()
                                    .collect(),
                                },
                            })
                        },
                        is_generator: a.is_generator,
                        is_async: a.is_async,
                        type_params: a.type_params,
                        return_type: a.return_type,
                    }),
                })
            }
            node => node,
        }
    }
}
