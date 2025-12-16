use impl_trait_for_tuples::impl_for_tuples;

use crate::*;
pub trait PatternMut {
    type Result;
    fn match_mut(&self, expr: &mut Expr) -> Option<Self::Result>;
}
pub trait Pattern: PatternMut {
    fn r#match(&self, expr: &Expr) -> Option<Self::Result>;
}
#[impl_for_tuples(1, 12)]
impl PatternMut for Tuple {
    for_tuples!(type Result = ( #( Tuple::Result ),* ););
    fn match_mut(&self, expr: &mut Expr) -> Option<Self::Result> {
        Some(for_tuples!((
            #( self.Tuple.match_mut(expr)? ),*
        )))
    }
}
#[impl_for_tuples(1, 12)]
impl Pattern for Tuple {
    fn r#match(&self, expr: &Expr) -> Option<Self::Result> {
        Some(for_tuples!((
            #( self.Tuple.r#match(expr)? ),*
        )))
    }
}
