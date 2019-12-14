use std::sync::atomic::{AtomicUsize, Ordering};

lazy_static::lazy_static! {
    static ref COUNTER: AtomicUsize = AtomicUsize::new(0);
}

#[derive(Clone, Copy, Debug)]
pub struct MetaVar(usize);

impl MetaVar {
    pub fn fresh() -> MetaVar {
        MetaVar(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}
