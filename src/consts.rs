//! Utilities for analyzing and transforming constant declarations.
//!
//! This module provides visitors and transformers for:
//! - Collecting identifiers that are assigned to (mutated)
//! - Converting `var` declarations to `const` when safe
//! - Collecting constant declarations for inlining
//! - Splitting variable declarations into separate statements

use crate::*;
use swc_ecma_ast::FnExpr;

/// Collects all identifiers that are assigned to in the code.
///
/// This visitor traverses the AST and records all identifiers that appear on the
/// left-hand side of assignments. This information is useful for determining which
/// variables are mutated and therefore cannot be safely converted to `const`.
#[derive(Default)]
#[non_exhaustive]
pub struct AssignCollector {
    /// Set of identifiers that are assigned to (mutated) in the code
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
    /// Converts this `AssignCollector` into a `Constifier` transformer.
    ///
    /// After collecting all assigned identifiers, use this method to create a
    /// transformer that will convert `var` declarations to `const` when safe.
    ///
    /// # Arguments
    ///
    /// * `opts` - Configuration options for the constifier
    ///
    /// # Returns
    ///
    /// A `Constifier` that can be used as a visitor to transform variable declarations
    pub fn to_constifier(self, opts: ConstifierOpts) -> Constifier {
        Constifier { assign: self, opts }
    }
}

/// Options for configuring the `Constifier` transformer.
///
/// Currently this struct has no fields but is marked as `#[non_exhaustive]`
/// to allow for future configuration options without breaking changes.
#[derive(Default)]
#[non_exhaustive]
pub struct ConstifierOpts {}

/// Transforms variable declarations to use `const` when safe.
///
/// This visitor changes `var` declarations to `const` declarations when the
/// variables are never reassigned. It uses information from an `AssignCollector`
/// to determine which variables are safe to convert.
pub struct Constifier {
    /// The collector that tracked which identifiers are assigned to
    assign: AssignCollector,
    /// Configuration options (currently unused but available for future extension)
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

/// Collects constant declarations and their initializer expressions.
///
/// This visitor traverses the AST and builds a map of `const` variable declarations
/// and function declarations to their values. This information can be used for:
/// - Constant propagation
/// - Inlining constant values
/// - Dead code elimination
///
/// The collector stores both `const` variables and function declarations, as both
/// represent immutable bindings that can potentially be inlined.
#[derive(Default)]
#[non_exhaustive]
pub struct ConstCollector {
    /// Map from identifier to its constant initializer expression
    /// 
    /// For `const` declarations, this is the initializer expression.
    /// For function declarations, this is a function expression wrapping the function.
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

/// Splits multi-declarator variable declarations into separate statements.
///
/// This transformer takes variable declarations with multiple declarators like:
/// ```javascript
/// var x = 1, y = 2, z = 3;
/// ```
///
/// And splits them into individual declarations:
/// ```javascript
/// var x = 1;
/// var y = 2;
/// var z = 3;
/// ```
///
/// This can make the code easier to analyze and transform, as each variable
/// gets its own declaration statement.
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
