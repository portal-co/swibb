use crate::*;

pub struct Brightener {
    pub key: Atom,
    pub global: Expr,
    pub val: bool,
    pub pack: Option<Ident>,
}
impl VisitMut for Brightener {
    fn visit_mut_expr(&mut self, node: &mut swc_ecma_ast::Expr) {
        let mut did = true;
        while take(&mut did) {
            node.visit_mut_children_with(self);
            *node = match take(node) {
                Expr::Member(MemberExpr { span, obj, prop }) if self.pack.is_some() && self.val => {
                    match prop {
                        MemberProp::Ident(i) => {
                            if i.sym == self.key {
                                did = true;
                                match (|obj: Expr| {
                                    Expr::Cond(CondExpr {
                                        span,
                                        test: Box::new(Expr::Bin(BinExpr {
                                            span,
                                            op: BinaryOp::EqEqEq,
                                            left: Box::new(obj.clone()),
                                            right: Box::new(self.global.clone()),
                                        })),
                                        cons: Box::new(Expr::Ident(self.pack.clone().unwrap())),
                                        alt: Box::new(Expr::Member(MemberExpr {
                                            span,
                                            obj: Box::new(obj),
                                            prop: MemberProp::Ident(i),
                                        })),
                                    })
                                }) {
                                    go => {
                                        if obj.idempotent() {
                                            go(*obj)
                                        } else {
                                            match SyntaxContext::empty().apply_mark(Mark::new()) {
                                                ctxt => Expr::Call(CallExpr {
                                                    span,
                                                    ctxt,
                                                    callee: swc_ecma_ast::Callee::Expr(Box::new(
                                                        Expr::Arrow(ArrowExpr {
                                                            span,
                                                            ctxt,
                                                            params: vec![Pat::Assign(AssignPat {
                                                                span,
                                                                left: Box::new(Pat::Ident(
                                                                    BindingIdent {
                                                                        id: Ident::new(
                                                                            Atom::new("obj"),
                                                                            span,
                                                                            ctxt,
                                                                        ),
                                                                        type_ann: None,
                                                                    },
                                                                )),
                                                                right: obj,
                                                            })],
                                                            body: Box::new(
                                                                swc_ecma_ast::BlockStmtOrExpr::Expr(
                                                                    Box::new(go(Expr::Ident(
                                                                        Ident::new(
                                                                            Atom::new("obj"),
                                                                            span,
                                                                            ctxt,
                                                                        ),
                                                                    ))),
                                                                ),
                                                            ),
                                                            is_async: false,
                                                            is_generator: false,
                                                            type_params: None,
                                                            return_type: None,
                                                        }),
                                                    )),
                                                    args: vec![],
                                                    type_args: None,
                                                }),
                                            }
                                        }
                                    }
                                }
                            } else {
                                Expr::Member(MemberExpr {
                                    span,
                                    obj,
                                    prop: MemberProp::Ident(i),
                                })
                            }
                        }
                        MemberProp::PrivateName(p) => Expr::Member(MemberExpr {
                            span,
                            obj,
                            prop: MemberProp::PrivateName(p),
                        }),
                        MemberProp::Computed(mem) => match (|obj: Expr, mem: Expr| {
                            did = true;
                            let cond = Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(mem.clone()),
                                right: Box::new(Expr::Lit(Lit::Str(Str {
                                    span,
                                    value: self.key.clone(),
                                    raw: None,
                                }))),
                            });
                            let mut cond = if obj.as_ident().is_some_and(|a| {
                                self.global
                                    .as_ident()
                                    .is_some_and(|b| a.to_id() == b.to_id())
                            }) {
                                cond
                            } else {
                                Expr::Bin(BinExpr {
                                    span,
                                    op: BinaryOp::LogicalAnd,
                                    left: Box::new(cond),
                                    right: Box::new(Expr::Bin(BinExpr {
                                        span,
                                        op: BinaryOp::EqEqEq,
                                        left: Box::new(obj.clone()),
                                        right: Box::new(self.global.clone()),
                                    })),
                                })
                            };
                            Expr::Cond(CondExpr {
                                span,
                                test: Box::new(cond),
                                cons: Box::new(Expr::Ident(self.pack.clone().unwrap())),
                                alt: Box::new(Expr::Member(MemberExpr {
                                    span,
                                    obj: Box::new(obj),
                                    prop: MemberProp::Computed(ComputedPropName {
                                        span,
                                        expr: Box::new(mem),
                                    }),
                                })),
                            })
                        }) {
                            mut go => match (obj.idempotent(), mem.expr.idempotent()) {
                                (true, true) => go(*obj, *mem.expr),
                                _ => match SyntaxContext::empty().apply_mark(Mark::new()) {
                                    ctxt => Expr::Call(CallExpr {
                                        span,
                                        ctxt,
                                        callee: swc_ecma_ast::Callee::Expr(Box::new(Expr::Arrow(
                                            ArrowExpr {
                                                span,
                                                ctxt,
                                                params: vec![
                                                    Pat::Assign(AssignPat {
                                                        span,
                                                        left: Box::new(Pat::Ident(BindingIdent {
                                                            type_ann: None,
                                                            id: Ident::new(
                                                                Atom::new("obj"),
                                                                span,
                                                                ctxt,
                                                            ),
                                                        })),
                                                        right: obj,
                                                    }),
                                                    Pat::Assign(AssignPat {
                                                        span,
                                                        left: Box::new(Pat::Ident(BindingIdent {
                                                            type_ann: None,
                                                            id: Ident::new(
                                                                Atom::new("mem"),
                                                                span,
                                                                ctxt,
                                                            ),
                                                        })),
                                                        right: mem.expr,
                                                    }),
                                                ],
                                                is_async: false,
                                                is_generator: false,
                                                type_params: None,
                                                return_type: None,
                                                body: Box::new(
                                                    swc_ecma_ast::BlockStmtOrExpr::Expr(Box::new(
                                                        {
                                                            let right = Expr::Ident(Ident::new(
                                                                Atom::new("obj"),
                                                                span,
                                                                ctxt,
                                                            ));
                                                            let left = Expr::Ident(Ident::new(
                                                                Atom::new("mem"),
                                                                span,
                                                                ctxt,
                                                            ));
                                                            go(left, right)
                                                        },
                                                    )),
                                                ),
                                            },
                                        ))),
                                        args: vec![],
                                        type_args: None,
                                    }),
                                },
                            },
                        },
                    }
                }
                Expr::Bin(BinExpr {
                    left,
                    span,
                    right,
                    op: BinaryOp::In,
                }) => match (|left: Expr, right: Expr| {
                    let cond = Expr::Bin(BinExpr {
                        span,
                        op: BinaryOp::EqEqEq,
                        left: Box::new(left.clone()),
                        right: Box::new(Expr::Lit(Lit::Str(Str {
                            span,
                            value: self.key.clone(),
                            raw: None,
                        }))),
                    });
                    let mut cond = if right.as_ident().is_some_and(|a| {
                        self.global
                            .as_ident()
                            .is_some_and(|b| a.to_id() == b.to_id())
                    }) {
                        cond
                    } else {
                        Expr::Bin(BinExpr {
                            span,
                            op: BinaryOp::LogicalAnd,
                            left: Box::new(cond),
                            right: Box::new(Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(right.clone()),
                                right: Box::new(self.global.clone()),
                            })),
                        })
                    };
                    if !self.val {
                        cond = Expr::Unary(UnaryExpr {
                            span,
                            op: swc_ecma_ast::UnaryOp::Bang,
                            arg: Box::new(cond),
                        });
                    };
                    Expr::Bin(BinExpr {
                        span,
                        op: if self.val {
                            BinaryOp::LogicalOr
                        } else {
                            BinaryOp::LogicalAnd
                        },
                        left: Box::new(cond),
                        right: Box::new(Expr::Bin(BinExpr {
                            span,
                            op: BinaryOp::In,
                            left: Box::new(left),
                            right: Box::new(right),
                        })),
                    })
                }) {
                    go => match (left.idempotent(), right.idempotent()) {
                        (true, true) => go(*left, *right),
                        _ => match SyntaxContext::empty().apply_mark(Mark::new()) {
                            ctxt => Expr::Call(CallExpr {
                                span,
                                ctxt,
                                callee: swc_ecma_ast::Callee::Expr(Box::new(Expr::Arrow(
                                    ArrowExpr {
                                        span,
                                        ctxt,
                                        params: vec![
                                            Pat::Assign(AssignPat {
                                                span,
                                                left: Box::new(Pat::Ident(BindingIdent {
                                                    type_ann: None,
                                                    id: Ident::new(Atom::new("obj"), span, ctxt),
                                                })),
                                                right: right,
                                            }),
                                            Pat::Assign(AssignPat {
                                                span,
                                                left: Box::new(Pat::Ident(BindingIdent {
                                                    type_ann: None,
                                                    id: Ident::new(Atom::new("mem"), span, ctxt),
                                                })),
                                                right: left,
                                            }),
                                        ],
                                        is_async: false,
                                        is_generator: false,
                                        type_params: None,
                                        return_type: None,
                                        body: Box::new(swc_ecma_ast::BlockStmtOrExpr::Expr(
                                            Box::new({
                                                let right = Expr::Ident(Ident::new(
                                                    Atom::new("obj"),
                                                    span,
                                                    ctxt,
                                                ));
                                                let left = Expr::Ident(Ident::new(
                                                    Atom::new("mem"),
                                                    span,
                                                    ctxt,
                                                ));
                                                go(left, right)
                                            }),
                                        )),
                                    },
                                ))),
                                args: vec![],
                                type_args: None,
                            }),
                        },
                    },
                },
                n => n,
            }
        }
    }
}
