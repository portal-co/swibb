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
    Expr, ExprStmt, Id, Ident, IfStmt, Lit, MemberExpr, MemberProp, Module, ModuleItem, Pat,
    Program, ReturnStmt, SeqExpr, SimpleAssignTarget, Stmt, Str, ThrowStmt, UnaryExpr, UnaryOp,
    VarDecl, VarDeclKind, VarDeclarator,
};
use swc_ecma_parser::{Lexer, Parser, Syntax};
use swc_ecma_transforms_base::rename::Renamer;
use swc_ecma_transforms_optimization::simplify::const_propagation::constant_propagation;
use swc_ecma_visit::{VisitMut, VisitMutWith};
// pub mod brighten;
pub mod consts;
pub mod folding;
// pub mod member_stuffs;
// pub mod stupify;
#[cfg(feature = "test")]
pub mod test;
pub use folding::{ArrowCallPack, CondFolding};
pub struct SyntaxContextToMark {
    root: Mark,
    map: HashMap<(Mark, Mark), Mark>,
}
impl SyntaxContextToMark {
    pub fn new(root: Mark) -> Self {
        Self {
            root,
            map: Default::default(),
        }
    }
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
pub trait Purity: Idempotency {
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
pub trait Idempotency {
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug, Hash)]
#[non_exhaustive]
pub struct InlineFlags {
    // pub tilde: bool,
    pub canon: Option<Atom>,
    pub global_this_inlining: bool,
    pub global_fetching: bool,
}
pub struct Inliner<'a, 'b: 'a, 'c, 'd: 'c> {
    inner: ConstCollector,
    flags: InlineFlags,
    opt_inline: &'b mut (dyn Probe + 'a),
    pass: &'c mut (dyn FnMut(&mut Expr) + 'd),
}
#[repr(transparent)]
pub struct ProbeRef<'a, 'b: 'a>(pub &'b mut (dyn Probe + 'a));
pub trait Probe: FnMut(&Expr, ProbeRef<'_, '_>) -> bool {}
impl<T: FnMut(&Expr, ProbeRef<'_, '_>) -> bool + ?Sized> Probe for T {}
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
                                if is_global_this(&s.value) {
                                    *node = take(&mut m.obj);
                                    continue;
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
impl ConstCollector {
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
pub trait CollectConstsAndInline:
    VisitMutWith<ConstCollector> + for<'a, 'b, 'c, 'd> VisitMutWith<Inliner<'a, 'b, 'c, 'd>>
{
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
#[non_exhaustive]
pub struct Garbler<R> {
    pub rng: Mutex<R>,
    cache: Mutex<BTreeMap<Id, Atom>>,
}
impl<R> Garbler<R> {
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
#[derive(Default)]
#[non_exhaustive]
pub struct ManglingRenamer {
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
#[derive(Default)]
#[non_exhaustive]
pub struct StripContext {}
impl VisitMut for StripContext {
    fn visit_mut_syntax_context(&mut self, node: &mut swc_common::SyntaxContext) {
        *node = Default::default();
    }
}
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
