use swc_ecma_ast::FnExpr;

use crate::*;
#[derive(Default)]
#[non_exhaustive]
pub struct AssignCollector {
    pub target: BTreeSet<Id>,
}
impl VisitMut for AssignCollector {
    fn visit_mut_ident(&mut self, node: &mut Ident) {
        node.visit_mut_children_with(self);
        self.target.insert(node.to_id());
    }
    fn visit_mut_var_declarator(&mut self, node: &mut VarDeclarator) {
        if let Some(init) = node.init.as_deref_mut() {
            init.visit_mut_children_with(self);
        }
    }
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        if let Expr::Ident(_) = node {
            return;
        };
        node.visit_mut_children_with(self);
    }
}
impl AssignCollector {
    pub fn to_constifier(self, opts: ConstifierOpts) -> Constifier {
        Constifier { assign: self, opts }
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct ConstifierOpts {}
pub struct Constifier {
    assign: AssignCollector,
    opts: ConstifierOpts,
}
impl VisitMut for Constifier {
    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.visit_mut_children_with(self);
        if node.decls.iter().all(|a| {
            a.name
                .as_ident()
                .is_some_and(|i| !self.assign.target.contains(&i.to_id()))
        }) {
            node.kind = VarDeclKind::Const;
        }
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct ConstCollector {
    pub map: BTreeMap<Id, Box<Expr>>,
}
impl VisitMut for ConstCollector {
    fn visit_mut_fn_decl(&mut self, node: &mut swc_ecma_ast::FnDecl) {
        node.visit_mut_children_with(self);
        self.map.insert(
            node.ident.to_id(),
            Box::new(Expr::Fn(FnExpr {
                ident: Some(node.ident.clone()),
                function: node.function.clone(),
            })),
        );
    }
    fn visit_mut_var_decl(&mut self, node: &mut VarDecl) {
        node.visit_mut_children_with(self);
        let VarDeclKind::Const = node.kind else {
            return;
        };
        for d in node.decls.iter() {
            if let Pat::Ident(i) = &d.name {
                if let Some(e) = d.init.as_ref() {
                    self.map.insert(i.to_id(), e.clone());
                }
            }
        }
    }
}
#[derive(Default)]
#[non_exhaustive]
pub struct VarDeclSplitter {}
impl VisitMut for VarDeclSplitter {
    fn visit_mut_stmts(&mut self, node: &mut Vec<Stmt>) {
        node.visit_mut_children_with(self);
        *node = take(node)
            .into_iter()
            .flat_map(|a| match a {
                Stmt::Decl(Decl::Var(v)) => v
                    .decls
                    .into_iter()
                    .map(|v2| {
                        Stmt::Decl(Decl::Var(Box::new(VarDecl {
                            span: v.span,
                            ctxt: v.ctxt,
                            kind: v.kind,
                            declare: v.declare,
                            decls: vec![v2],
                        })))
                    })
                    .collect(),
                a => vec![a],
            })
            .collect();
    }
    fn visit_mut_module(&mut self, node: &mut swc_ecma_ast::Module) {
        node.visit_mut_children_with(self);
        node.body = take(&mut node.body)
            .into_iter()
            .flat_map(|a| match a {
                ModuleItem::Stmt(Stmt::Decl(Decl::Var(v))) => v
                    .decls
                    .into_iter()
                    .map(|v2| {
                        ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
                            span: v.span,
                            ctxt: v.ctxt,
                            kind: v.kind,
                            declare: v.declare,
                            decls: vec![v2],
                        }))))
                    })
                    .collect(),
                a => vec![a],
            })
            .collect();
    }
}
