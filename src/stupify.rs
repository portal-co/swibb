use swc_ecma_ast::{ExprOrSpread, FnExpr, Function, IdentName, Param, RestPat, ThisExpr};

use crate::*;
#[derive(Default)]
#[non_exhaustive]
pub struct ThisWrap {}
impl VisitMut for ThisWrap {
    fn visit_mut_call_expr(&mut self, node: &mut CallExpr) {
        node.visit_mut_children_with(self);
        if let Callee::Expr(e) = &mut node.callee {
            let e = &mut **e;
            *e = match take(e) {
                Expr::Member(m) => thismemb(m),
                a => thiscall(a),
            }
        }
    }
}
fn thiscall(a: Expr) -> Expr {
    let aid = Ident::new_private(Atom::new("wrapped"), a.span());
    let rid = Ident::new_private(Atom::new("args"), a.span());
    let span = a.span();
    Expr::Call(CallExpr {
        span: a.span(),
        ctxt: Default::default(),
        callee: Callee::Expr(Box::new(Expr::Arrow(ArrowExpr {
            span: a.span(),
            ctxt: Default::default(),
            params: vec![Pat::Assign(AssignPat {
                span: a.span(),
                left: Box::new(Pat::Ident(BindingIdent {
                    id: aid.clone(),
                    type_ann: None,
                })),
                right: Box::new(a),
            })],
            body: Box::new(BlockStmtOrExpr::Expr(Box::new(Expr::Fn(FnExpr {
                ident: None,
                function: Box::new(Function {
                    params: vec![Param {
                        span,
                        decorators: vec![],
                        pat: Pat::Rest(RestPat {
                            span,
                            dot3_token: span,
                            type_ann: None,
                            arg: Box::new(Pat::Ident(rid.clone().into())),
                        }),
                    }],
                    decorators: vec![],
                    span,
                    ctxt: Default::default(),
                    body: Some(BlockStmt {
                        span,
                        ctxt: Default::default(),
                        stmts: vec![Stmt::If(IfStmt {
                            span,
                            test: Box::new(Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(Expr::Unary(UnaryExpr {
                                    span,
                                    op: UnaryOp::TypeOf,
                                    arg: Box::new(Expr::Ident(aid.clone())),
                                })),
                                right: Box::new(Expr::Lit(Lit::Str(Str {
                                    span,
                                    value: Atom::new("function"),
                                    raw: None,
                                }))),
                            })),
                            cons: Box::new(Stmt::Return(ReturnStmt {
                                span,
                                arg: Some(Box::new(Expr::Call(CallExpr {
                                    span,
                                    ctxt: Default::default(),
                                    callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                                        span,
                                        obj: Box::new(Expr::Ident(aid)),
                                        prop: MemberProp::Ident(IdentName {
                                            span,
                                            sym: Atom::new("apply"),
                                        }),
                                    }))),
                                    args: vec![
                                        ExprOrSpread {
                                            spread: None,
                                            expr: Box::new(Expr::This(ThisExpr { span })),
                                        },
                                        ExprOrSpread {
                                            spread: None,
                                            expr: Box::new(Expr::Ident(rid)),
                                        },
                                    ],
                                    type_args: None,
                                }))),
                            })),
                            alt: None,
                        })],
                    }),
                    is_async: false,
                    is_generator: false,
                    type_params: None,
                    return_type: None,
                }),
            })))),
            is_async: false,
            is_generator: false,
            type_params: None,
            return_type: None,
        }))),
        args: vec![],
        type_args: None,
    })
}
fn thismemb(a: MemberExpr) -> Expr {
    let aid = Ident::new_private(Atom::new("wrapped"), a.span());
    let oid = Ident::new_private(Atom::new("obj"), a.span);
    let rid = Ident::new_private(Atom::new("args"), a.span());
    let span = a.span();
    Expr::Call(CallExpr {
        span: a.span(),
        ctxt: Default::default(),
        callee: Callee::Expr(Box::new(Expr::Arrow(ArrowExpr {
            span: a.span(),
            ctxt: Default::default(),
            params: vec![
                Pat::Assign(AssignPat {
                    span,
                    left: Box::new(Pat::Ident(oid.clone().into())),
                    right: a.obj,
                }),
                Pat::Assign(AssignPat {
                    span: a.span,
                    left: Box::new(Pat::Ident(BindingIdent {
                        id: aid.clone(),
                        type_ann: None,
                    })),
                    right: Box::new(Expr::Member(MemberExpr {
                        span,
                        obj: Box::new(Expr::Ident(oid.clone())),
                        prop: a.prop,
                    })),
                }),
            ],
            body: Box::new(BlockStmtOrExpr::Expr(Box::new(Expr::Fn(FnExpr {
                ident: None,
                function: Box::new(Function {
                    params: vec![Param {
                        span,
                        decorators: vec![],
                        pat: Pat::Rest(RestPat {
                            span,
                            dot3_token: span,
                            type_ann: None,
                            arg: Box::new(Pat::Ident(rid.clone().into())),
                        }),
                    }],
                    decorators: vec![],
                    span,
                    ctxt: Default::default(),
                    body: Some(BlockStmt {
                        span,
                        ctxt: Default::default(),
                        stmts: vec![Stmt::If(IfStmt {
                            span,
                            test: Box::new(Expr::Bin(BinExpr {
                                span,
                                op: BinaryOp::EqEqEq,
                                left: Box::new(Expr::Unary(UnaryExpr {
                                    span,
                                    op: UnaryOp::TypeOf,
                                    arg: Box::new(Expr::Ident(aid.clone())),
                                })),
                                right: Box::new(Expr::Lit(Lit::Str(Str {
                                    span,
                                    value: Atom::new("function"),
                                    raw: None,
                                }))),
                            })),
                            cons: Box::new(Stmt::Return(ReturnStmt {
                                span,
                                arg: Some(Box::new(Expr::Call(CallExpr {
                                    span,
                                    ctxt: Default::default(),
                                    callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                                        span,
                                        obj: Box::new(Expr::Ident(aid)),
                                        prop: MemberProp::Ident(IdentName {
                                            span,
                                            sym: Atom::new("apply"),
                                        }),
                                    }))),
                                    args: vec![
                                        ExprOrSpread {
                                            spread: None,
                                            expr: Box::new(Expr::Ident(oid)),
                                        },
                                        ExprOrSpread {
                                            spread: None,
                                            expr: Box::new(Expr::Ident(rid)),
                                        },
                                    ],
                                    type_args: None,
                                }))),
                            })),
                            alt: None,
                        })],
                    }),
                    is_async: false,
                    is_generator: false,
                    type_params: None,
                    return_type: None,
                }),
            })))),
            is_async: false,
            is_generator: false,
            type_params: None,
            return_type: None,
        }))),
        args: vec![],
        type_args: None,
    })
}
