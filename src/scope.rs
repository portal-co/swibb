use super::*;
struct IdentCollector {
    ids: BTreeSet<Id>,
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