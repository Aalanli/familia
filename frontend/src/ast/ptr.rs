use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub struct P<T: ?Sized> {
    data: Rc<T>,
}

#[allow(non_snake_case)]
pub fn P<T>(p: T) -> P<T> {
    return P::new(p);
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pid(usize);

impl<T> P<T> {
    pub fn new(data: T) -> Self {
        P {
            data: Rc::new(data),
        }
    }

    pub fn pid(&self) -> Pid {
        let ptr = Rc::as_ptr(&self.data) as usize;
        Pid(ptr)
    }
}

impl<T: Clone> P<T> {
    pub fn get_mut(&mut self) -> &mut T {
        // self.hash = OnceCell::new();
        Rc::make_mut(&mut self.data)
    }
}

impl<T> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
