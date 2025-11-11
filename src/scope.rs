//! Utilities for analyzing variable scope in JavaScript functions.
//!
//! This module provides tools to identify external identifiers referenced within
//! functions, helping to determine which variables are captured from outer scopes.

use super::*;

/// Collects identifier references during AST traversal.
///
/// This visitor walks the AST and collects all identifier references it encounters.
/// It has two modes controlled by the `pat` flag:
/// - When `pat` is `true`, it only collects identifiers that are being bound (e.g., parameters)
/// - When `pat` is `false`, it collects all identifier references
struct IdentCollector {
    /// The set of collected identifier IDs
    ids: BTreeSet<Id>,
    /// Whether to only collect pattern bindings (true) or all references (false)
    pat: bool,
}
impl Visit for IdentCollector {
    fn visit_ident(&mut self, node: &Ident) {
        self.ids.insert(node.to_id());
    }
    fn visit_assign_pat(&mut self, node: &AssignPat) {
        if self.pat {
            node.left.visit_with(self);
        } else {
            node.visit_children_with(self);
        }
    }
}

/// Identifies external identifiers referenced within a function.
///
/// This function analyzes a JavaScript function to determine which identifiers
/// are referenced but not defined within the function (i.e., captured from outer scopes).
/// It works by:
/// 1. Collecting all parameter names (which are local to the function)
/// 2. Collecting all identifier references in the function body
/// 3. Removing the parameters from the references to find external identifiers
///
/// # Arguments
///
/// * `a` - A reference to the function to analyze
///
/// # Returns
///
/// A set of identifier IDs (`Id`) that represent variables referenced but not defined
/// within the function. These are the "external" or "free" variables.
///
/// # Examples
///
/// ```rust
/// use portal_solutions_swibb::scope::externs;
/// use swc_ecma_ast::Function;
///
/// // Given a function like: function foo(x) { return x + y; }
/// // externs() would return a set containing 'y' (but not 'x')
/// // since 'y' is referenced but not defined in the function
/// ```
pub fn externs(a: &Function) -> BTreeSet<Id> {
    let mut params = IdentCollector {
        ids: Default::default(),
        pat: true,
    };
    a.params.visit_with(&mut params);
    let mut code = IdentCollector {
        ids: Default::default(),
        pat: false,
    };
    a.visit_with(&mut code);
    for p in params.ids {
        code.ids.remove(&p);
    }
    return code.ids;
}
