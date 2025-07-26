use swc_common::EqIgnoreSpan;
use swc_ecma_ast::Bool;

use crate::*;
#[derive(Default)]
#[non_exhaustive]
pub enum Mode {
    #[default]
    Extract,
    Simplify,
}
#[derive(Default)]
#[non_exhaustive]
pub struct CondFolding {
    pub fold_stmts: bool,
    pub idents: BTreeSet<Id>,
    pub mode: Mode,
}
impl VisitMut for CondFolding {
    fn visit_mut_stmts(&mut self, node: &mut Vec<Stmt>) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.insert(
                0,
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: Span::dummy_with_cmt(),
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: Span::dummy_with_cmt(),
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, Span::dummy_with_cmt(), new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_module(&mut self, node: &mut swc_ecma_ast::Module) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                swc_ecma_ast::ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                })))),
            );
        }
    }
    fn visit_mut_stmt(&mut self, node: &mut Stmt) {
        let mut should_continue = true;
        while take(&mut should_continue) {
            node.visit_mut_children_with(self);
            if self.fold_stmts {
                *node = match take(node) {
                    Stmt::Expr(expr_stmt) if expr_stmt.expr.is_cond() => {
                        should_continue = true;
                        let Expr::Cond(cond_expr) = *expr_stmt.expr else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: cond_expr.span,
                            test: cond_expr.test,
                            cons: Box::new(Stmt::Expr(ExprStmt {
                                span: expr_stmt.span,
                                expr: cond_expr.cons,
                            })),
                            alt: Some(Box::new(Stmt::Expr(ExprStmt {
                                span: expr_stmt.span,
                                expr: cond_expr.alt,
                            }))),
                        })
                    }
                    Stmt::Return(return_stmt)
                        if return_stmt.arg.as_ref().is_some_and(|arg| arg.is_cond()) =>
                    {
                        should_continue = true;
                        let Expr::Cond(c) = *return_stmt.arg.unwrap() else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: c.span,
                            test: c.test,
                            cons: Box::new(Stmt::Return(ReturnStmt {
                                span: return_stmt.span,
                                arg: Some(c.cons),
                            })),
                            alt: Some(Box::new(Stmt::Return(ReturnStmt {
                                span: return_stmt.span,
                                arg: Some(c.alt),
                            }))),
                        })
                    }
                    Stmt::Throw(e) if e.arg.is_cond() => {
                        should_continue = true;
                        let Expr::Cond(c) = *e.arg else {
                            unreachable!()
                        };
                        Stmt::If(IfStmt {
                            span: c.span,
                            test: c.test,
                            cons: Box::new(Stmt::Throw(ThrowStmt {
                                span: e.span,
                                arg: c.cons,
                            })),
                            alt: Some(Box::new(Stmt::Throw(ThrowStmt {
                                span: e.span,
                                arg: c.alt,
                            }))),
                        })
                    }
                    node => node,
                }
            }
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        let mut cont = true;
        let mut skip_children = false;
        while take(&mut cont) {
            constant_propagation().visit_mut_expr(node);
            if !take(&mut skip_children) {
                node.visit_mut_children_with(self);
            }
            *node = match take(node) {
                Expr::Cond(CondExpr {
                    span,
                    test,
                    cons,
                    alt,
                }) => match *test {
                    Expr::Seq(mut sequence_expression) => {
                        let last = take(&mut **sequence_expression.exprs.last_mut().unwrap());
                        **sequence_expression.exprs.last_mut().unwrap() = Expr::Cond(CondExpr {
                            span,
                            test: Box::new(last),
                            cons,
                            alt,
                        });
                        cont = true;
                        Expr::Seq(sequence_expression)
                    }
                    Expr::Bin(binary_expression)
                        if matches!(binary_expression.op, BinaryOp::LogicalAnd) =>
                    {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: binary_expression.left,
                            cons: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: binary_expression.right,
                                cons,
                                alt: alt.clone(),
                            })),
                            alt,
                        })
                    }
                    Expr::Bin(binary_expression)
                        if matches!(binary_expression.op, BinaryOp::LogicalOr) =>
                    {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: binary_expression.left,
                            cons: cons.clone(),
                            alt: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: binary_expression.right,
                                cons,
                                alt,
                            })),
                        })
                    }
                    Expr::Unary(unary_expression)
                        if matches!(unary_expression.op, UnaryOp::Bang) =>
                    {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span,
                            test: unary_expression.arg,
                            cons: alt,
                            alt: cons,
                        })
                    }
                    Expr::Lit(Lit::Bool(boolean_literal)) => {
                        // cont = true;
                        match boolean_literal.value {
                            true => *cons,
                            false => *alt,
                        }
                    }
                    Expr::Cond(d) => {
                        cont = true;
                        Expr::Cond(CondExpr {
                            span: d.span,
                            test: d.test,
                            cons: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: d.cons,
                                cons: cons.clone(),
                                alt: alt.clone(),
                            })),
                            alt: Box::new(Expr::Cond(CondExpr {
                                span,
                                test: d.alt,
                                cons,
                                alt,
                            })),
                        })
                    }
                    test => match (*cons, *alt) {
                        (Expr::Assign(a), Expr::Assign(b))
                            if !(matches!(&self.mode, Mode::Extract))
                                && a.left.eq_ignore_span(&b.left)
                                && a.op == b.op =>
                        {
                            Expr::Assign(AssignExpr {
                                span: a.span,
                                op: a.op,
                                left: a.left,
                                right: Box::new(Expr::Cond(CondExpr {
                                    span,
                                    test: Box::new(test),
                                    cons: a.right,
                                    alt: b.right,
                                })),
                            })
                        }
                        (cons, alt) => Expr::Cond(CondExpr {
                            span,
                            test: Box::new(test),
                            cons: Box::new(cons),
                            alt: Box::new(alt),
                        }),
                    },
                },
                Expr::Bin(binary_expression) if binary_expression.left.is_cond() => {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *binary_expression.left else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            left: conditional_expression.cons,
                            right: binary_expression.right.clone(),
                        })),
                        alt: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            left: conditional_expression.alt,
                            right: binary_expression.right,
                        })),
                    })
                }
                Expr::Bin(binary_expression) if binary_expression.right.is_cond() => {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *binary_expression.right else {
                        unreachable!()
                    };
                    let id = (
                        Atom::new("left"),
                        SyntaxContext::empty().apply_mark(Mark::new()),
                    );
                    self.idents.insert(id.clone());
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: Box::new(Expr::Seq(SeqExpr {
                            span: conditional_expression.span,
                            exprs: vec![
                                Box::new(Expr::Assign(AssignExpr {
                                    span: conditional_expression.span,
                                    op: AssignOp::Assign,
                                    left: AssignTarget::Simple(SimpleAssignTarget::Ident(
                                        BindingIdent {
                                            id: Ident::new(
                                                id.0.clone(),
                                                conditional_expression.span,
                                                id.1,
                                            ),
                                            type_ann: None,
                                        },
                                    )),
                                    right: binary_expression.left,
                                })),
                                conditional_expression.test,
                            ],
                        })),
                        cons: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            right: conditional_expression.cons,
                            left: Box::new(Expr::Ident(Ident::new(
                                id.0.clone(),
                                conditional_expression.span,
                                id.1,
                            ))),
                        })),
                        alt: Box::new(Expr::Bin(BinExpr {
                            span: binary_expression.span,
                            op: binary_expression.op,
                            right: conditional_expression.alt,
                            left: Box::new(Expr::Ident(Ident::new(
                                id.0.clone(),
                                conditional_expression.span,
                                id.1,
                            ))),
                        })),
                    })
                }
                Expr::Bin(b) if b.op == BinaryOp::LogicalAnd || b.op == BinaryOp::LogicalOr => {
                    cont = true;
                    skip_children = true;
                    let span = b.span;
                    Expr::Cond(CondExpr {
                        span: b.span,
                        test: Box::new(Expr::Bin(b)),
                        cons: Box::new(Expr::Lit(Lit::Bool(Bool { span, value: true }))),
                        alt: Box::new(Expr::Lit(Lit::Bool(Bool { span, value: false }))),
                    })
                }
                Expr::Assign(assign_expression)
                    if assign_expression.right.is_cond()
                        && match assign_expression.left {
                            AssignTarget::Simple(SimpleAssignTarget::Ident(_)) => true,
                            _ => false,
                        }
                        && matches!(&self.mode, Mode::Extract) =>
                {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *assign_expression.right else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Assign(AssignExpr {
                            span: assign_expression.span,
                            op: assign_expression.op,
                            left: assign_expression.left.clone(),
                            right: conditional_expression.cons,
                        })),
                        alt: Box::new(Expr::Assign(AssignExpr {
                            span: assign_expression.span,
                            op: assign_expression.op,
                            left: assign_expression.left,
                            right: conditional_expression.alt,
                        })),
                    })
                }
                Expr::Member(m) if m.obj.is_cond() => {
                    cont = true;
                    let Expr::Cond(conditional_expression) = *m.obj else {
                        unreachable!()
                    };
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: conditional_expression.test,
                        cons: Box::new(Expr::Member(MemberExpr {
                            span: m.span,
                            obj: conditional_expression.cons,
                            prop: m.prop.clone(),
                        })),
                        alt: Box::new(Expr::Member(MemberExpr {
                            span: m.span,
                            obj: conditional_expression.alt,
                            prop: m.prop,
                        })),
                    })
                }
                Expr::Member(m) if m.prop.as_computed().is_some_and(|a| a.expr.is_cond()) => {
                    cont = true;
                    let MemberProp::Computed(p) = m.prop else {
                        unreachable!()
                    };
                    let Expr::Cond(conditional_expression) = *p.expr else {
                        unreachable!()
                    };
                    let id = (
                        Atom::new("object"),
                        SyntaxContext::empty().apply_mark(Mark::new()),
                    );
                    self.idents.insert(id.clone());
                    Expr::Cond(CondExpr {
                        span: conditional_expression.span,
                        test: Box::new(Expr::Seq(SeqExpr {
                            span: conditional_expression.span,
                            exprs: vec![
                                Box::new(Expr::Assign(AssignExpr {
                                    span: conditional_expression.span,
                                    op: AssignOp::Assign,
                                    left: AssignTarget::Simple(SimpleAssignTarget::Ident(
                                        BindingIdent {
                                            id: Ident::new(
                                                id.0.clone(),
                                                conditional_expression.span,
                                                id.1,
                                            ),
                                            type_ann: None,
                                        },
                                    )),
                                    right: m.obj,
                                })),
                                conditional_expression.test,
                            ],
                        })),
                        cons: Box::new(Expr::Member(MemberExpr {
                            span: m.span,
                            obj: Box::new(Expr::Ident(Ident::new(
                                id.0.clone(),
                                conditional_expression.span,
                                id.1,
                            ))),
                            prop: MemberProp::Computed(ComputedPropName {
                                span: m.span,
                                expr: conditional_expression.cons,
                            }),
                        })),
                        alt: Box::new(Expr::Member(MemberExpr {
                            span: m.span,
                            obj: Box::new(Expr::Ident(Ident::new(
                                id.0.clone(),
                                conditional_expression.span,
                                id.1,
                            ))),
                            prop: MemberProp::Computed(ComputedPropName {
                                span: m.span,
                                expr: conditional_expression.alt,
                            }),
                        })),
                    })
                }
                a => a,
            }
        }
    }
}
#[derive(Default)]
pub struct ArrowCallPack {
    idents: BTreeSet<Id>,
}
impl VisitMut for ArrowCallPack {
    fn visit_mut_module(&mut self, node: &mut swc_ecma_ast::Module) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                swc_ecma_ast::ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                })))),
            );
        }
    }
    fn visit_mut_script(&mut self, node: &mut swc_ecma_ast::Script) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_block_stmt(&mut self, node: &mut swc_ecma_ast::BlockStmt) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.stmts.insert(
                0,
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.1,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: Ident::new(new.0, node.span, new.1),
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);
        *node = match take(node) {
            Expr::Call(CallExpr {
                span,
                ctxt,
                callee,
                args,
                type_args,
            }) if args.len() == 0
                && callee
                    .as_expr()
                    .and_then(|a| a.as_arrow())
                    .is_some_and(|a| {
                        a.body.is_expr()
                            && a.params
                                .iter()
                                .all(|a| a.as_assign().is_some_and(|x| x.left.is_ident()))
                    }) =>
            {
                let Callee::Expr(e) = callee else {
                    unreachable!()
                };
                let Expr::Arrow(a) = *e else { unreachable!() };
                let BlockStmtOrExpr::Expr(e) = *a.body else {
                    unreachable!()
                };
                let mut x = a
                    .params
                    .into_iter()
                    .map(|a| {
                        let Pat::Assign(a) = a else { unreachable!() };
                        let Pat::Ident(i) = *a.left else {
                            unreachable!()
                        };
                        self.idents.insert(i.id.to_id());
                        Expr::Assign(AssignExpr {
                            span: a.span,
                            op: AssignOp::Assign,
                            left: AssignTarget::Simple(SimpleAssignTarget::Ident(i)),
                            right: a.right,
                        })
                    })
                    .map(Box::new)
                    .chain([e]);
                Expr::Seq(SeqExpr {
                    span,
                    exprs: x.collect(),
                })
            }
            node => node,
        }
    }
}
