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
fn is_global_this(identifier: &str) -> bool {
    return ["globalThis", "window", "self", "global"]
        .into_iter()
        .any(|global_name| identifier == global_name);
}
impl<'a, 'b: 'a, 'c, 'd: 'c> VisitMut for Inliner<'a, 'b, 'c, 'd> {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        fn go<'a, 'b: 'a>(
            flags: &InlineFlags,
            const_collector: &ConstCollector,
            mut probe: &'b mut (dyn Probe + 'a),
            expr: &Expr,
        ) -> bool {
            if probe(expr, ProbeRef(&mut |e, ProbeRef(p)| go(flags, const_collector, p, e))) {
                return true;
            }
            if expr.is_pure() {
                return true;
            }
            match expr {
                Expr::Ident(ident) => {
                    (is_global_this(&ident.sym)
                        && ident.ctxt == Default::default()
                        && (flags.global_this_inlining))
                        || const_collector.map.contains_key(&ident.to_id())
                }
                Expr::Lit(_) => true,
                Expr::Member(member) => {
                    go(flags, const_collector, probe, &member.obj)
                        && match &member.prop {
                            MemberProp::Ident(_) => true,
                            MemberProp::Computed(computed) => go(flags, const_collector, probe, &computed.expr),
                            MemberProp::PrivateName(_) => false,
                        }
                }
                _ => false,
            }
        }
        loop {
            (self.pass)(node);
            if let Expr::Member(member_expr) = node {
                if let Expr::Ident(ident) = &mut *member_expr.obj {
                    if is_global_this(&ident.sym) && ident.ctxt == Default::default() {
                        if let MemberProp::Ident(prop_ident) = &member_expr.prop {
                            if is_global_this(&prop_ident.sym) {
                                *node = take(&mut member_expr.obj);
                                continue;
                            }
                            if self.flags.global_fetching {
                                ident.sym = prop_ident.sym.clone();
                                *node = take(&mut member_expr.obj);
                                continue;
                            }
                        }
                        if let MemberProp::Computed(computed_prop) = &member_expr.prop {
                            if let Expr::Lit(Lit::Str(string_lit)) = &*computed_prop.expr {
                                if let Some(value) = string_lit.value.as_str(){
                                if is_global_this(value) {
                                    *node = take(&mut member_expr.obj);
                                    continue;
                                }
                            }
                            }
                        }
                    }
                }
            }
            if let Expr::Ident(ident) = node {
                if let Some(const_value) = self.inner.map.get(&ident.to_id()) {
                    if go(&self.flags, &self.inner, &mut *self.opt_inline, &**const_value) {
                        *node = (&**const_value).clone();
                        continue;
                    }
                }
                if is_global_this(&ident.sym) && ident.ctxt == Default::default() {
                    if let Some(canon_name) = self.flags.canon.as_ref() {
                        if is_global_this(canon_name) && canon_name != &ident.sym {
                            ident.sym = canon_name.clone();
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
