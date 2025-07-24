use crate::*;
#[non_exhaustive]
pub struct MemberRemap<'a, 'b: 'a> {
    pub init: &'b mut (dyn FnMut(Expr) -> Expr + 'a),
    pub root: Expr,
    idents: HashSet<Ident>,
}
impl VisitMut for MemberRemap<'_, '_> {
    fn visit_mut_module(&mut self, node: &mut swc_ecma_ast::Module) {
        let old = take(&mut self.idents);
        node.visit_mut_children_with(self);
        let new = replace(&mut self.idents, old);
        for new in new.into_iter() {
            node.body.insert(
                0,
                swc_ecma_ast::ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: node.span,
                    ctxt: new.ctxt,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: new,
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
                    ctxt: new.ctxt,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: new,
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
                    ctxt: new.ctxt,
                    kind: swc_ecma_ast::VarDeclKind::Var,
                    declare: true,
                    decls: vec![VarDeclarator {
                        span: node.span,
                        name: Pat::Ident(BindingIdent {
                            id: new,
                            type_ann: None,
                        }),
                        init: None,
                        definite: false,
                    }],
                }))),
            );
        }
    }
    fn visit_mut_member_expr(&mut self, node: &mut MemberExpr) {
        node.visit_mut_children_with(self);
        if let MemberProp::PrivateName(_) = &node.prop {
            return;
        }
        let obj = Ident::new_private(Atom::new("obj"), node.span);
        self.idents.insert(obj.clone());
        let mem = Ident::new_private(Atom::new("mem"), node.span);
        self.idents.insert(mem.clone());
        node.obj = Box::new(match take(&mut *node.obj) {
            n => Expr::Assign(AssignExpr {
                span: n.span(),
                op: AssignOp::Assign,
                left: AssignTarget::Simple(SimpleAssignTarget::Ident(obj.clone().into())),
                right: Box::new(n),
            }),
        });
        node.prop = MemberProp::Computed(ComputedPropName {
            span: node.span,
            expr: Box::new(Expr::Seq(SeqExpr {
                span: node.span,
                exprs: [
                    Box::new(Expr::Assign(AssignExpr {
                        span: node.span,
                        op: AssignOp::Assign,
                        left: AssignTarget::Simple(SimpleAssignTarget::Ident(mem.clone().into())),
                        right: match take(&mut node.prop) {
                            MemberProp::Ident(ident_name) => Box::new(Expr::Lit(Lit::Str(Str {
                                span: ident_name.span,
                                value: ident_name.sym,
                                raw: None,
                            }))),
                            MemberProp::PrivateName(private_name) => todo!(),
                            MemberProp::Computed(computed_prop_name) => computed_prop_name.expr,
                        },
                    })),
                    Box::new(Expr::Cond(CondExpr {
                        span: node.span,
                        test: Box::new(Expr::Bin(BinExpr {
                            span: node.span,
                            op: BinaryOp::EqEqEq,
                            left: Box::new(Expr::Ident(obj)),
                            right: Box::new(self.root.clone()),
                        })),
                        cons: Box::new((self.init)(Expr::Ident(mem.clone()))),
                        alt: Box::new(Expr::Ident(mem)),
                    })),
                ]
                .into_iter()
                .collect(),
            })),
        })
    }
    fn visit_mut_bin_expr(&mut self, node: &mut BinExpr) {
        node.visit_mut_children_with(self);
        let BinaryOp::In = node.op else {
            return;
        };
        let obj = Ident::new_private(Atom::new("obj"), node.span);
        self.idents.insert(obj.clone());
        let mem = Ident::new_private(Atom::new("mem"), node.span);
        self.idents.insert(mem.clone());
        node.left = Box::new(Expr::Seq(SeqExpr {
            span: node.span,
            exprs: [
                Box::new(Expr::Assign(AssignExpr {
                    span: node.span,
                    op: AssignOp::Assign,
                    left: AssignTarget::Simple(SimpleAssignTarget::Ident(mem.clone().into())),
                    right: take(&mut node.left),
                })),
                Box::new(match replace(&mut *node.right, Expr::Ident(obj.clone())) {
                    n => Expr::Assign(AssignExpr {
                        span: n.span(),
                        op: AssignOp::Assign,
                        left: AssignTarget::Simple(SimpleAssignTarget::Ident(obj.clone().into())),
                        right: Box::new(n),
                    }),
                }),
                Box::new(Expr::Cond(CondExpr {
                    span: node.span,
                    test: Box::new(Expr::Bin(BinExpr {
                        span: node.span,
                        op: BinaryOp::EqEqEq,
                        left: Box::new(Expr::Ident(obj.clone())),
                        right: Box::new(self.root.clone()),
                    })),
                    cons: Box::new((self.init)(Expr::Ident(mem.clone()))),
                    alt: Box::new(Expr::Ident(mem)),
                })),
            ]
            .into_iter()
            .collect(),
        }));
    }
}
