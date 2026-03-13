//! Transforms JavaScript `with` statements into equivalent code.
//!
//! The `with` statement in JavaScript modifies the scope chain, making it difficult
//! to analyze and optimize code. This module provides a transformer that removes
//! `with` statements by explicitly checking if identifiers exist in the with-object
//! and accessing them accordingly.

use crate::*;
use swc_ecma_ast::{IdentName, Tpl, TplElement};

/// Transforms `with` statements into equivalent conditional expressions.
///
/// This visitor removes JavaScript `with` statements by converting free variable
/// references into conditional expressions that check if the property exists in
/// the with-object before accessing it.
///
/// For example, it transforms:
/// ```javascript
/// with (obj) { foo; }
/// ```
/// Into:
/// ```javascript
/// const $with$0 = obj;
/// ('foo' in $with$0) ? $with$0.foo : foo;
/// ```
///
/// This makes the code more explicit and easier to analyze, at the cost of verbosity.
pub struct Wither {
    /// Stack of with-objects and their locally declared variables
    /// Each entry contains the identifier for the with-object and a set of
    /// identifiers declared within that with scope (which shouldn't be looked up
    /// in the with-object)
    pub with_stack: Vec<(Ident, BTreeSet<Id>)>,
    /// Prefix used when generating temporary identifiers for with-objects
    pub ident_prefix: Atom,
}
impl VisitMut for Wither {
    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        node.visit_mut_children_with(self);
        if let Some(s) = self.with_stack.last_mut() {
            collect_idents(&node.name, &mut s.1);
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        *node = match take(node) {
            Expr::Ident(i)
                if self.with_stack.iter().all(|w| !w.1.contains(&i.to_id()))
                    && self.with_stack.len() > 0 =>
            {
                self.with_stack
                    .iter()
                    .fold(Expr::Ident(i.clone()), |a, (b, _)| {
                        Expr::Cond(CondExpr {
                            span: i.span,
                            test: Box::new(Expr::Bin(BinExpr {
                                span: i.span,
                                op: BinaryOp::In,
                                left: Box::new(Expr::Lit(Lit::Str(Str {
                                    span: i.span,
                                    value: i.sym.clone().into(),
                                    raw: None,
                                }))),
                                right: b.clone().into(),
                            })),
                            cons: Box::new(Expr::Member(MemberExpr {
                                span: i.span,
                                obj: b.clone().into(),
                                prop: MemberProp::Ident(IdentName {
                                    span: i.span,
                                    sym: i.sym.clone(),
                                }),
                            })),
                            alt: a.clone().into(),
                        })
                    })
            }
            Expr::Call(mut c)
                if c.callee
                    .as_expr()
                    .and_then(|e| e.as_ident())
                    .is_some_and(|i| i.sym == "eval" && !i.optional)
                    && self.with_stack.len() > 0 =>
            {
                if let Some(p) = c.args.get_mut(0) {
                    if let None = p.spread {
                        let span = p.expr.span();
                        let s = {
                            let mut s = format!("{{");
                            for (w, _) in self.with_stack.iter() {
                                s = format!("with({w}){s}")
                            }
                            s
                        };
                        p.expr = match *take(&mut p.expr) {
                            Expr::Lit(Lit::Str(Str { span, value, raw })) if value.as_str().is_some()=> {
                                Box::new(Expr::Lit(Lit::Str(Str {
                                    span,
                                    value: Atom::new(format!("{s}{}}}",value.as_str().unwrap())).into(),
                                    raw: None,
                                })))
                            }
                            expr => Box::new(Expr::Tpl(Tpl {
                                span,
                                exprs: [Box::new(expr)].into_iter().collect(),
                                quasis: [
                                    TplElement {
                                        span,
                                        tail: false,
                                        cooked: None,
                                        raw: Atom::new({ s }),
                                    },
                                    TplElement {
                                        span,
                                        tail: true,
                                        cooked: None,
                                        raw: Atom::new("}"),
                                    },
                                ]
                                .into_iter()
                                .collect(),
                            })),
                        };
                    }
                }
                Expr::Call(c)
            }
            node => node,
        }
    }
    fn visit_mut_stmt(&mut self, node: &mut Stmt) {
        *node = match take(node) {
            Stmt::With(mut w) => {
                let id = Ident::new_private(
                    Atom::new(format!(
                        "{}$with${}",
                        self.ident_prefix,
                        self.with_stack.len()
                    )),
                    w.span,
                );
                Stmt::Block(BlockStmt {
                    span: w.span,
                    ctxt: Default::default(),
                    stmts: [
                        Stmt::Decl(Decl::Var(Box::new(VarDecl {
                            span: w.span,
                            ctxt: Default::default(),
                            kind: VarDeclKind::Const,
                            declare: false,
                            decls: [VarDeclarator {
                                span: w.span,
                                name: id.clone().into(),
                                init: {
                                    w.obj.visit_mut_with(self);
                                    Some(w.obj)
                                },
                                definite: false,
                            }]
                            .into_iter()
                            .collect(),
                        }))),
                        {
                            self.with_stack.push((id.clone(), Default::default()));
                            w.body.visit_mut_with(self);
                            self.with_stack.pop();
                            *w.body
                        },
                    ]
                    .into_iter()
                    .collect(),
                })
            }
            mut node => {
                node.visit_mut_children_with(self);
                node
            }
        }
    }
}

/// Recursively collects all identifiers declared in a pattern.
///
/// This helper function traverses destructuring patterns and collects all
/// identifiers that are bound by the pattern. This is used to track which
/// variables are declared locally within a with-block and therefore should
/// not be looked up in the with-object.
///
/// # Arguments
///
/// * `name` - The pattern to analyze
/// * `x` - A mutable set to add discovered identifiers to
///
/// # Pattern Types Handled
///
/// - `Ident`: Adds the identifier directly
/// - `Array`: Recursively processes all elements
/// - `Object`: Recursively processes all properties
/// - `Rest`: Recursively processes the rest pattern
/// - `Assign`: Recursively processes the left-hand side
fn collect_idents(name: &Pat, x: &mut BTreeSet<Id>) {
    match name {
        Pat::Ident(binding_ident) => {
            x.insert(binding_ident.id.to_id());
        }
        Pat::Array(array_pat) => {
            for elem in array_pat.elems.iter().filter_map(|a| a.as_ref()) {
                collect_idents(elem, x);
            }
        }
        Pat::Rest(rest_pat) => {
            collect_idents(&rest_pat.arg, x);
        }
        Pat::Object(object_pat) => {
            for elem in object_pat.props.iter() {
                match elem {
                    swc_ecma_ast::ObjectPatProp::KeyValue(key_value_pat_prop) => {
                        collect_idents(&key_value_pat_prop.value, x)
                    }
                    swc_ecma_ast::ObjectPatProp::Assign(assign_pat_prop) => {
                        x.insert(assign_pat_prop.key.id.to_id());
                    }
                    swc_ecma_ast::ObjectPatProp::Rest(rest_pat) => {
                        collect_idents(&rest_pat.arg, x);
                    }
                }
            }
        }
        Pat::Assign(assign_pat) => {
            collect_idents(&assign_pat.left, x);
        }
        Pat::Invalid(invalid) => todo!(),
        Pat::Expr(expr) => todo!(),
    }
}
