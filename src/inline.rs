//! Constant inlining and global object canonicalization.
//!
//! This module provides utilities for inlining constant expressions and normalizing
//! references to the global object (`globalThis`, `window`, `self`, `global`).

use crate::*;

/// Configuration flags for the inliner.
///
/// These flags control various aspects of constant inlining and global object handling.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug, Hash)]
#[non_exhaustive]
pub struct InlineFlags {
    /// Canonical name to use for global object references (e.g., "globalThis")
    /// When set, all global object references will be normalized to this name
    pub canon: Option<Atom>,
    /// Whether to inline identifiers that reference global objects
    /// (globalThis, window, self, global)
    pub global_this_inlining: bool,
    /// Whether to replace `globalThis.foo` with just `foo`
    pub global_fetching: bool,
}

/// Visitor that performs constant inlining based on collected constant declarations.
///
/// This visitor replaces identifier references with their constant values when safe to do so.
/// It uses a `ConstCollector` to know which identifiers have constant values, and provides
/// hooks for custom logic to determine when inlining is safe.
pub struct Inliner<'a, 'b: 'a, 'c, 'd: 'c> {
    /// The collected constants available for inlining
    inner: ConstCollector,
    /// Configuration flags controlling inlining behavior
    flags: InlineFlags,
    /// Optional user-provided function to determine if an expression is safe to inline
    opt_inline: &'b mut (dyn Probe + 'a),
    /// Optional user-provided function to transform expressions during inlining
    pass: &'c mut (dyn FnMut(&mut Expr) + 'd),
}

/// Wrapper for passing a probe function recursively.
///
/// This wrapper allows the probe function to call itself recursively
/// when checking nested expressions.
#[repr(transparent)]
pub struct ProbeRef<'a, 'b: 'a>(pub &'b mut (dyn Probe + 'a));

/// Trait for custom inlining safety checks.
///
/// A probe is a function that determines whether a particular expression is safe
/// to inline. It receives the expression and a recursive probe reference for checking
/// sub-expressions.
pub trait Probe: FnMut(&Expr, ProbeRef<'_, '_>) -> bool {}
impl<T: FnMut(&Expr, ProbeRef<'_, '_>) -> bool + ?Sized> Probe for T {}

/// Checks if a string is a reference to the global object.
///
/// Returns true for: `globalThis`, `window`, `self`, `global`
fn is_global_this(a: &str) -> bool {
    return ["globalThis", "window", "self", "global"]
        .into_iter()
        .any(|b| a == b);
}
impl<'a, 'b: 'a, 'c, 'd: 'c> VisitMut for Inliner<'a, 'b, 'c, 'd> {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        fn go<'a, 'b: 'a>(
            flags: &InlineFlags,
            c: &ConstCollector,
            mut x: &'b mut (dyn Probe + 'a),
            a: &Expr,
        ) -> bool {
            if x(a, ProbeRef(&mut |e, ProbeRef(x)| go(flags, c, x, e))) {
                return true;
            }
            if a.is_pure() {
                return true;
            }
            match a {
                Expr::Ident(i) => {
                    (is_global_this(&i.sym)
                        && i.ctxt == Default::default()
                        && (flags.global_this_inlining))
                        || c.map.contains_key(&i.to_id())
                }
                Expr::Lit(_) => true,
                Expr::Member(m) => {
                    go(flags, c, x, &m.obj)
                        && match &m.prop {
                            MemberProp::Ident(_) => true,
                            MemberProp::Computed(c2) => go(flags, c, x, &c2.expr),
                            MemberProp::PrivateName(_) => false,
                        }
                }
                _ => false,
            }
        }
        loop {
            (self.pass)(node);
            if let Expr::Member(m) = node {
                if let Expr::Ident(i) = &mut *m.obj {
                    if is_global_this(&i.sym) && i.ctxt == Default::default() {
                        if let MemberProp::Ident(j) = &m.prop {
                            if is_global_this(&j.sym) {
                                *node = take(&mut m.obj);
                                continue;
                            }
                            if self.flags.global_fetching {
                                i.sym = j.sym.clone();
                                *node = take(&mut m.obj);
                                continue;
                            }
                        }
                        if let MemberProp::Computed(c) = &m.prop {
                            if let Expr::Lit(Lit::Str(s)) = &*c.expr {
                                if let Some(v) = s.value.as_str(){
                                if is_global_this(v) {
                                    *node = take(&mut m.obj);
                                    continue;
                                }
                            }
                            }
                        }
                    }
                }
            }
            if let Expr::Ident(i) = node {
                if let Some(c) = self.inner.map.get(&i.to_id()) {
                    if go(&self.flags, &self.inner, &mut *self.opt_inline, &**c) {
                        *node = (&**c).clone();
                        continue;
                    }
                }
                if is_global_this(&i.sym) && i.ctxt == Default::default() {
                    if let Some(c) = self.flags.canon.as_ref() {
                        if is_global_this(c) && c != &i.sym {
                            i.sym = c.clone();
                            continue;
                        }
                    }
                }
            }
            break;
        }
        node.visit_mut_children_with(self);
    }
}

/// Trait for types that can be visited to collect constants and inline them.
///
/// This trait provides a convenient method to perform the two-pass process of:
/// 1. Collecting constant declarations with `ConstCollector`
/// 2. Inlining those constants with `Inliner`
pub trait CollectConstsAndInline:
    VisitMutWith<ConstCollector> + for<'a, 'b, 'c, 'd> VisitMutWith<Inliner<'a, 'b, 'c, 'd>>
{
    /// Collects constants and inlines them in a single call.
    ///
    /// This method performs the complete constant inlining process:
    /// 1. Visits the AST to collect all constant declarations
    /// 2. Visits the AST again to inline constant values where safe
    ///
    /// # Arguments
    ///
    /// * `flags` - Configuration flags controlling the inlining behavior
    ///
    /// # Note
    ///
    /// The method name has a typo ("collet" and "linine") but is kept for
    /// compatibility. Consider using the correctly-spelled name in new code.
    fn collet_consts_and_linine(&mut self, flags: InlineFlags) {
        let mut c = ConstCollector::default();
        self.visit_mut_with(&mut c);
        let mut f = |_: &Expr, _: ProbeRef<'_, '_>| false;
        let mut g = |_: &mut Expr| {};
        let mut c = c.to_inliner(flags, &mut f, &mut g);
        self.visit_mut_with(&mut c);
    }
}
impl<
    T: VisitMutWith<ConstCollector>
        + for<'a, 'b, 'c, 'd> VisitMutWith<Inliner<'a, 'b, 'c, 'd>>
        + ?Sized,
> CollectConstsAndInline for T
{
}

impl ConstCollector {
    /// Converts a `ConstCollector` into an `Inliner` for the inlining pass.
    ///
    /// After collecting constants, use this method to create an inliner that can
    /// replace identifier references with their constant values.
    ///
    /// # Arguments
    ///
    /// * `flags` - Configuration for the inlining behavior
    /// * `probe` - Optional function to determine if an expression is safe to inline
    /// * `pass` - Optional function to transform expressions during inlining
    ///
    /// # Returns
    ///
    /// An `Inliner` that can be used as a visitor to inline constants
    pub fn to_inliner<'a, 'b: 'a, 'c, 'd: 'c>(
        self,
        flags: InlineFlags,
        probe: &'b mut (dyn Probe + 'a),
        pass: &'d mut (dyn FnMut(&mut Expr) + 'c),
    ) -> Inliner<'a, 'b, 'c, 'd> {
        Inliner {
            inner: self,
            flags,
            opt_inline: probe,
            pass,
        }
    }
}
