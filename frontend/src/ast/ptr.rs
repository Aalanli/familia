use std::ops::Deref;

#[derive(Clone, Debug)]
pub struct P<T: ?Sized> {
    data: Box<T>,
}

#[allow(non_snake_case)]
pub fn P<T>(p: T) -> P<T> {
    return P::new(p);
}

impl<T> P<T> {
    pub fn new(data: T) -> Self {
        P {
            data: Box::new(data),
        }
    }
}

impl<T> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
