use std::cell::UnsafeCell;
use std::ptr;

#[derive(Debug)]
pub struct Lazy<A, R> {
    inner: UnsafeCell<MemoThunk<A, R>>,
}

#[derive(Debug)]
enum MemoThunk<A, R> {
    Suspended(A),
    Pending,
    Done(R),
}

pub trait Thunk {
    type Result;
    fn thunk(self) -> Self::Result;
}

impl<A, R> Default for Lazy<A, R>
    where R: Default
{
    fn default() -> Self {
        Lazy::evaluated(Default::default())
    }
}

impl<A, R> Lazy<A, R> {
    pub fn suspended(a: A) -> Lazy<A, R> {
        Lazy {
            inner: UnsafeCell::new(MemoThunk::Suspended(a)),
        }
    }

    pub fn evaluated(r: R) -> Lazy<A, R> {
        Lazy {
            inner: UnsafeCell::new(MemoThunk::Done(r)),
        }
    }
}

impl<A, R> Lazy<A, R>
    where A: Thunk<Result = R>
{
    pub fn get(&self) -> &R {
        unsafe {
            match *self.inner.get() {
                MemoThunk::Done(ref r) => return r,
                MemoThunk::Pending => panic!("circular thunk evaluation"),

                // If the computation is suspended we're going to jump through
                // hoops below to justify our movement of data.
                MemoThunk::Suspended(_) => (),
            }

            match ptr::replace(self.inner.get(), MemoThunk::Pending) {
                MemoThunk::Suspended(a) => {
                    let r = a.thunk();
                    *self.inner.get() = MemoThunk::Done(r);
                    match *self.inner.get() {
                        MemoThunk::Done(ref r) => r,
                        _ => unreachable!(),
                    }
                },
                _ => unreachable!(),
            }
        }
    }
}
