use crate::*;
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
}
