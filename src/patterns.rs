use impl_trait_for_tuples::impl_for_tuples;

use crate::*;
pub trait PatternMut<T> {
    type Result;
    fn match_mut(&self, expr: &mut T) -> Option<Self::Result>;
}
pub trait Pattern<T>: PatternMut<T> {
    fn r#match(&self, expr: &T) -> Option<Self::Result>;
}
#[impl_for_tuples(1, 12)]
impl<T> PatternMut<T> for Tuple {
    for_tuples!(type Result = ( #( Tuple::Result ),* ););
    fn match_mut(&self, expr: &mut T) -> Option<Self::Result> {
        Some(for_tuples!((
            #( self.Tuple.match_mut(expr)? ),*
        )))
    }
}
#[impl_for_tuples(1, 12)]
impl<T> Pattern<T> for Tuple {
    fn r#match(&self, expr: &T) -> Option<Self::Result> {
        Some(for_tuples!((
            #( self.Tuple.r#match(expr)? ),*
        )))
    }
}
