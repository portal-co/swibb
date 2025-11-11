//! Utilities for rewriting and transforming JavaScript code.
//!
//! `portal-solutions-swibb` provides a collection of AST transformers and utilities
//! for JavaScript code manipulation using the SWC compiler infrastructure. It's designed
//! to support tools like Weevy, Jsaw, Jsxx, DumbJS, and other JavaScript processing tools.
//!
//! # Features
//!
//! - **Conditional folding**: Normalize and simplify ternary expressions
//! - **Constant collection and inlining**: Track and inline constant values
//! - **Arrow function inflation**: Convert arrow functions to regular functions
//! - **With statement transformation**: Remove `with` statements
//! - **Hot module replacement**: Utilities for HMR in bundlers
//! - **Scope analysis**: Determine free variables in functions
//! - **Variable declaration optimization**: Convert `var` to `const` when safe
//!
//! # Examples
//!
//! ## Basic Constant Folding
//!
//! ```rust
//! use portal_solutions_swibb::CondFolding;
//! use swc_ecma_visit::VisitMutWith;
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::test::test_load;
//! use swc_common::sync::Lrc;
//! use swc_common::SourceMap;
//!
//! let cm = Lrc::new(SourceMap::default());
//! let mut module = test_load(&cm, "test", "const x = true ? 1 : 2;");
//! module.visit_mut_with(&mut CondFolding::default());
//! # }
//! ```
//!
//! ## Collecting Constants
//!
//! ```rust
//! use portal_solutions_swibb::ConstCollector;
//! use swc_ecma_visit::VisitMutWith;
//! # #[cfg(feature = "test")]
//! # {
//! use portal_solutions_swibb::test::test_load;
//! use swc_common::sync::Lrc;
//! use swc_common::SourceMap;
//!
//! let cm = Lrc::new(SourceMap::default());
//! let mut module = test_load(&cm, "test", "const PI = 3.14; const TAU = PI * 2;");
//! let mut collector = ConstCollector::default();
//! module.visit_mut_with(&mut collector);
//! // collector.map now contains the constant values
//! # }
//! ```

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    mem::{replace, take},
    sync::Mutex,
};
// use base64::Engine;
use swc_atoms::Atom;
use swc_common::{
    FileName, SourceFile, SourceMap, Span, Spanned,
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
};
use swc_common::{Mark, SyntaxContext};
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, AssignPat, AssignTarget, AssignTargetPat, BinExpr, BinaryOp,
    BindingIdent, BlockStmt, BlockStmtOrExpr, CallExpr, Callee, ComputedPropName, CondExpr, Decl,
    Expr, ExprStmt, Function, Id, Ident, IfStmt, Lit, MemberExpr, MemberProp, Module, ModuleItem,
    Pat, Program, ReturnStmt, SeqExpr, SimpleAssignTarget, Stmt, Str, ThrowStmt, UnaryExpr,
    UnaryOp, VarDecl, VarDeclKind, VarDeclarator,
};
use swc_ecma_parser::{Lexer, Parser, Syntax};
use swc_ecma_transforms_base::rename::Renamer;
use swc_ecma_transforms_optimization::simplify::const_propagation::constant_propagation;
use swc_ecma_visit::{VisitMut, VisitMutWith};
// pub mod brighten;
// pub mod amd;
pub mod consts;
pub mod folding;
pub mod inflate;
pub mod inline;
pub mod module;
pub mod scope;
pub mod wither;
// pub mod member_stuffs;
// pub mod stupify;
#[cfg(feature = "test")]
pub mod test;
pub use folding::{ArrowCallPack, CondFolding};
use swc_ecma_visit::{Visit, VisitWith};

/// Converts SWC's `SyntaxContext` to a hierarchical `Mark` structure.
///
/// SWC uses `SyntaxContext` to track identifier scopes and hygiene. This struct
/// provides a way to convert a `SyntaxContext` (which is internally a chain of marks)
/// into an explicit `Mark` that represents the same scope information.
///
/// This is useful when you need to work with marks directly or when building
/// new AST nodes that should have the same scope as existing nodes.
pub struct SyntaxContextToMark {
    /// The root mark representing the empty syntax context
    root: Mark,
    /// Cache mapping (parent_mark, current_mark) pairs to combined marks
    map: HashMap<(Mark, Mark), Mark>,
}
impl SyntaxContextToMark {
    /// Creates a new converter with the specified root mark.
    ///
    /// # Arguments
    ///
    /// * `root` - The mark to use for the empty syntax context
    pub fn new(root: Mark) -> Self {
        Self {
            root,
            map: Default::default(),
        }
    }
    
    /// Converts a `SyntaxContext` to its corresponding `Mark`.
    ///
    /// This method recursively processes the syntax context chain and returns
    /// a mark that represents the same scope. Results are cached for efficiency.
    ///
    /// # Arguments
    ///
    /// * `a` - The syntax context to convert
    ///
    /// # Returns
    ///
    /// A mark representing the same scope as the input syntax context
    pub fn of(&mut self, mut a: SyntaxContext) -> Mark {
        if a == SyntaxContext::empty() {
            return self.root;
        }
        let last = a.remove_mark();
        let next = self.of(a);
        let last = self
            .map
            .entry((last, next))
            .or_insert_with(|| Mark::fresh(next));
        return *last;
    }
}
// pub mod brighten;
// pub use brighten::*;
pub use crate::consts::ConstCollector;

/// Trait for determining if an expression is pure (has no side effects).
///
/// A pure expression is one that:
/// - Does not modify any state
/// - Does not perform I/O
/// - Always produces the same result given the same inputs
/// - Can be safely evaluated multiple times or not at all
///
/// This extends the `Idempotency` trait, as pure expressions are also idempotent.
pub trait Purity: Idempotency {
    /// Returns `true` if the expression is pure (has no side effects).
    fn is_pure(&self) -> bool;
}
impl Purity for Expr {
    fn is_pure(&self) -> bool {
        match self {
            Expr::Ident(_) | Expr::Lit(_) | Expr::This(_) | Expr::Arrow(_) => true,
            Expr::Member(m) => match &m.prop {
                MemberProp::PrivateName(_) => m.obj.is_pure(),
                _ => false,
            },
            Expr::Cond(c) => [&c.test, &c.alt, &c.cons].into_iter().all(|a| a.is_pure()),
            _ => false,
        }
    }
}
impl<T: Purity> Purity for Box<T> {
    fn is_pure(&self) -> bool {
        (&**self).is_pure()
    }
}

/// Trait for determining if an expression is idempotent.
///
/// An idempotent expression is one that can be executed multiple times without
/// changing the result. This is weaker than purity - an idempotent expression may
/// have side effects, but repeated evaluation produces the same result.
///
/// For example, `x = 5` is idempotent (executing it multiple times leaves x as 5)
/// but not pure (it has the side effect of assigning to x).
pub trait Idempotency {
    /// Returns `true` if the expression is idempotent.
    fn idempotent(&self) -> bool;
}
impl<T: Idempotency> Idempotency for Box<T> {
    fn idempotent(&self) -> bool {
        (&**self).idempotent()
    }
}
impl Idempotency for Expr {
    fn idempotent(&self) -> bool {
        match self {
            Expr::Assign(AssignExpr {
                span,
                op,
                left,
                right,
            }) => *op == AssignOp::Assign && left.idempotent() && right.idempotent(),
            Expr::Member(m) => match &m.prop {
                MemberProp::PrivateName(_) => m.obj.idempotent(),
                _ => false,
            },
            Expr::Cond(c) => [&c.test, &c.alt, &c.cons]
                .into_iter()
                .all(|a| a.idempotent()),
            _ => self.is_pure(),
        }
    }
}
impl Idempotency for AssignTarget {
    fn idempotent(&self) -> bool {
        match self {
            swc_ecma_ast::AssignTarget::Simple(simple_assign_target) => {
                match simple_assign_target {
                    SimpleAssignTarget::Ident(_) => true,
                    SimpleAssignTarget::Member(m) => match &m.prop {
                        MemberProp::PrivateName(_) => m.obj.idempotent(),
                        _ => false,
                    },
                    _ => false,
                }
            }
            swc_ecma_ast::AssignTarget::Pat(assign_target_pat) => match assign_target_pat {
                _ => false,
            },
        }
    }
}
struct CondWrapping {}
impl VisitMut for CondWrapping {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        *node = match take(node) {
            Expr::Cond(cond_expr) => {
                let mut call_expr = Expr::Call(CallExpr {
                    span: cond_expr.span,
                    ctxt: Default::default(),
                    callee: Callee::Expr(Box::new(Expr::Arrow(ArrowExpr {
                        span: cond_expr.span,
                        ctxt: Default::default(),
                        params: vec![],
                        body: Box::new(BlockStmtOrExpr::BlockStmt(BlockStmt {
                            span: cond_expr.span,
                            ctxt: Default::default(),
                            stmts: vec![Stmt::Return(ReturnStmt {
                                span: cond_expr.span,
                                arg: Some(Box::new(Expr::Cond(cond_expr))),
                            })],
                        })),
                        is_async: false,
                        is_generator: false,
                        type_params: None,
                        return_type: None,
                    }))),
                    args: vec![],
                    type_args: None,
                });
                match CondFolding::default() {
                    mut cond_folding => {
                        cond_folding.fold_stmts = true;
                        call_expr.visit_mut_with(&mut cond_folding);
                    }
                };
                call_expr
            }
            other => other,
        };
        node.visit_mut_children_with(self);
    }
}
impl ConstCollector {
    pub fn is_weak_map(&self, id: &Id) -> bool {
        return self.map.get(id).is_some_and(|m| {
            m.as_new().is_some_and(|n| {
                n.args.as_ref().is_none_or(|a| a.len() == 0)
                    && n.callee.as_ident().is_some_and(|i| {
                        i.to_id() == (Atom::new("WeakMap"), SyntaxContext::default())
                    })
            })
        });
    }
    pub fn weak_maps(&self, x: &Program) -> BTreeSet<Id> {
        match x {
            Program::Module(module) => module
                .body
                .iter()
                .filter_map(|a| a.as_stmt())
                .filter_map(|a| a.as_decl())
                .flat_map(|d| match d {
                    Decl::Var(v) => v
                        .decls
                        .iter()
                        .filter_map(|a| a.name.as_ident())
                        .map(|a| a.to_id())
                        .filter(|a| self.is_weak_map(&a))
                        .collect(),
                    _ => BTreeSet::default(),
                })
                .collect(),
            Program::Script(script) => script
                .body
                .iter()
                .filter_map(|a| a.as_decl())
                .flat_map(|d| match d {
                    Decl::Var(v) => v
                        .decls
                        .iter()
                        .filter_map(|a| a.name.as_ident())
                        .map(|a| a.to_id())
                        .filter(|a| self.is_weak_map(&a))
                        .collect(),
                    _ => BTreeSet::default(),
                })
                .collect(),
        }
    }
}

/// Detects and removes malicious code patterns used in supply chain attacks.
///
/// **Security**: This visitor identifies specific code patterns that are known to be used
/// in supply chain attacks on npm packages and other JavaScript ecosystems. It should not
/// trigger on benign code, even when optimized.
///
/// ## Detected Pattern
///
/// This removes variable declarations matching the pattern:
/// ```javascript
/// var x = someIdentifier(arg).substr(0, 11);
/// ```
///
/// This specific pattern (`substr(0, 11)` on a single-argument function call) has been
/// observed in malicious packages attempting to extract and exfiltrate sensitive data
/// like environment variables or credentials.
///
/// ## Why This Pattern?
///
/// The pattern is very specific to avoid false positives:
/// - It only matches when ALL declarations in a `var` statement follow this exact pattern
/// - The callee must be a simple identifier (not a complex expression)
/// - The arguments must be exactly `(0, 11)` in that order
/// - The method must be specifically named `substr`
///
/// This specificity ensures the detector only triggers on the known malicious pattern
/// and not on legitimate code that might coincidentally use `substr`.
///
/// ## Usage
///
/// ```rust
/// use portal_solutions_swibb::Cleanse;
/// use swc_ecma_visit::VisitMutWith;
///
/// let mut module = /* ... */;
/// module.visit_mut_with(&mut Cleanse {});
/// // Any detected malicious patterns have been removed
/// ```
pub struct Cleanse {}

impl VisitMut for Cleanse {
    fn visit_mut_stmts(&mut self, node: &mut Vec<Stmt>) {
        node.visit_mut_children_with(self);
        if node.iter().any(|a| {
            a.as_decl().and_then(|d| d.as_var()).is_some_and(|v| {
                v.decls.iter().all(|d| {
                    d.init
                        .as_ref()
                        .and_then(|x| x.as_call())
                        .and_then(|x| match &*x.args {
                            [a, b]
                                if a.expr
                                    .as_lit()
                                    .and_then(|l| l.as_num())
                                    .is_some_and(|n| n.value == 0.0)
                                    && b.expr
                                        .as_lit()
                                        .and_then(|l| l.as_num())
                                        .is_some_and(|n| n.value == 11.0) =>
                            {
                                match &x.callee {
                                    Callee::Expr(e) => Some(&*e),
                                    _ => None,
                                }
                            }
                            _ => None,
                        })
                        .and_then(|x| x.as_member())
                        .and_then(|x| match &x.prop {
                            MemberProp::Ident(i) if i.sym == "substr" => Some(&x.obj),
                            _ => None,
                        })
                        .and_then(|a| a.as_call())
                        .is_some_and(|a| {
                            a.args.len() == 1
                                && match &a.callee {
                                    Callee::Expr(e) => e.is_ident(),
                                    _ => false,
                                }
                        })
                })
            })
        }) {
            take(node);
        }
    }
}

/// Renames identifiers to random strings for obfuscation.
///
/// This renamer uses a random number generator to create unpredictable identifier
/// names, making the code harder to understand. It maintains consistency by caching
/// the mapping from original identifiers to their randomized names.
///
/// This is primarily useful for code obfuscation. Each identifier gets a consistent
/// random name throughout the program.
///
/// Requires the `rand` feature to be enabled.
#[non_exhaustive]
pub struct Garbler<R> {
    /// Random number generator for creating random identifier names
    pub rng: Mutex<R>,
    /// Cache mapping original identifiers to their garbled names
    cache: Mutex<BTreeMap<Id, Atom>>,
}
impl<R> Garbler<R> {
    /// Creates a new garbler with the given random number generator.
    ///
    /// # Arguments
    ///
    /// * `rng` - A random number generator to use for creating random names
    pub fn new(rng: R) -> Self {
        Self {
            rng: Mutex::new(rng),
            cache: Default::default(),
        }
    }
}
#[cfg(feature = "rand")]
const _: () = {
    use rand::Rng;
    use rand::distr::SampleString;
    impl<'a, R: Rng> Renamer for &'a Garbler<R> {
        const RESET_N: bool = true;
        const MANGLE: bool = true;
        fn new_name_for(&self, orig: &Id, n: &mut usize) -> Atom {
            Renamer::new_name_for(&**self, orig, n)
        }
        type Target = Atom;
    }
    impl<R: Rng> Renamer for Garbler<R> {
        const RESET_N: bool = true;
        const MANGLE: bool = true;
        fn new_name_for(&self, orig: &Id, n: &mut usize) -> Atom {
            *n += 1;
            use rand::distr::Alphabetic;
            return self
                .cache
                .lock()
                .unwrap()
                .entry(orig.clone())
                .or_insert_with(|| {
                    let mut rng = self.rng.lock().unwrap();
                    Atom::new(Alphabetic.sample_string(&mut *rng, 4))
                })
                .clone();
        }
        type Target = Atom;
    }
};

/// Renames identifiers by encoding their syntax context.
///
/// This renamer converts identifiers to a mangled form that includes their syntax
/// context information. For identifiers in the empty context that don't start with
/// `_$`, it keeps the original name. Otherwise, it encodes the identifier as
/// `_${context}_${name}`.
///
/// With the `encoding` feature enabled, very long names (>19 characters) can be
/// hashed to a shorter form for compactness.
///
/// This is useful for:
/// - Debugging scope/hygiene issues (the context is visible in the name)
/// - Ensuring unique names across different scopes
/// - Serializing ASTs with scope information preserved
#[derive(Default)]
#[non_exhaustive]
pub struct ManglingRenamer {
    /// Whether to hash long names to shorter forms (requires `encoding` feature)
    #[cfg(feature = "encoding")]
    pub encode: bool,
}
impl Renamer for ManglingRenamer {
    const RESET_N: bool = true;
    const MANGLE: bool = false;
    type Target = Atom;
    fn new_name_for(&self, orig: &Id, n: &mut usize) -> Atom {
        *n += 1;
        #[cfg(not(feature = "encoding"))]
        fn encode(this: &ManglingRenamer, a: String) -> String {
            a
        }
        #[cfg(feature = "encoding")]
        fn encode(this: &ManglingRenamer, a: String) -> String {
            if a.len() > 19 && this.encode {
                format!(
                    "_$h{}",
                    ::hex::encode(&<::sha3::Sha3_256 as ::sha3::Digest>::digest(&a)[..8])
                )
            } else {
                a
            }
        }
        Atom::new(
            match if orig.1 == SyntaxContext::empty() && !(orig.0.starts_with("_$")) {
                format!("{}", &orig.0)
            } else {
                encode(
                    self,
                    format!("_${}_${}", SyntaxContext::as_u32(orig.1), &orig.0),
                )
            } {
                a => a,
            },
        )
    }
}

/// Removes all syntax context information from identifiers.
///
/// This visitor resets all syntax contexts to the default (empty) context,
/// effectively removing all scope and hygiene information. This can be useful
/// after transformations when you want to "flatten" the scope structure.
///
/// **Warning**: This removes important scope information and can cause name
/// collisions if different scopes had identifiers with the same name.
#[derive(Default)]
#[non_exhaustive]
pub struct StripContext {}

impl VisitMut for StripContext {
    fn visit_mut_syntax_context(&mut self, node: &mut swc_common::SyntaxContext) {
        *node = Default::default();
    }
}

/// Re-resolves a module after transformations by renaming and re-analyzing scopes.
///
/// This function performs a three-step process:
/// 1. Renames all identifiers using the provided mangling renamer
/// 2. Strips all syntax context information
/// 3. Re-runs the resolver to rebuild scope information with fresh marks
///
/// This is useful after performing transformations that may have invalidated the
/// scope information, allowing you to rebuild clean scope data.
///
/// # Arguments
///
/// * `module` - The module to re-resolve (modified in place)
/// * `mangle` - The renamer to use for mangling identifier names
/// * `strip` - The context stripper (typically `StripContext::default()`)
/// * `unresolved_mark` - Mark to use for unresolved references
/// * `top_level_mark` - Mark to use for top-level scope
pub fn reresolve(
    module: &mut Module,
    mangle: ManglingRenamer,
    mut strip: StripContext,
    unresolved_mark: Mark,
    top_level_mark: Mark,
) {
    module.visit_mut_with(&mut swc_ecma_transforms_base::rename::renamer(
        Default::default(),
        mangle,
    ));
    module.visit_mut_with(&mut strip);
    module.visit_mut_with(&mut swc_ecma_transforms_base::resolver(
        unresolved_mark,
        top_level_mark,
        false,
    ));
}
