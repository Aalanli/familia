use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::ir;

pub trait DefaultIdentity: 'static {
    fn default_hash(&self) -> u64;
    fn default_compare(&self, other: &dyn DefaultIdentity) -> bool;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Hash + Eq + 'static> DefaultIdentity for T {
    fn default_hash(&self) -> u64 {
        use std::hash::Hasher;
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

    fn default_compare(&self, other: &dyn DefaultIdentity) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<T>() {
            self == other
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl PartialEq for dyn DefaultIdentity {
    fn eq(&self, other: &Self) -> bool {
        self.default_compare(other)
    }
}

impl Eq for dyn DefaultIdentity {}

impl Hash for dyn DefaultIdentity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.default_hash().hash(state);
    }
}

pub trait Query: DefaultIdentity + Hash + Eq + Clone {
    type Result: Clone;
    fn query(&self, q: &QueryAnalysis) -> Self::Result;
}

#[derive(Debug, Clone)]
pub enum QueryError {
    QueryCycle,
}

pub use QueryError::*;

pub struct QueryAnalysis<'ir> {
    ir: &'ir ir::IR,
    query: RefCell<HashMap<Box<dyn DefaultIdentity>, Box<dyn Any>>>,
    current_queries: RefCell<HashSet<Box<dyn DefaultIdentity>>>,
}

impl<'ir> QueryAnalysis<'ir> {
    pub fn new(ir: &'ir ir::IR) -> Self {
        QueryAnalysis {
            ir,
            query: RefCell::new(HashMap::new()),
            current_queries: RefCell::new(HashSet::new()),
        }
    }

    pub fn query<T>(&self, q: T) -> Result<T::Result, QueryError>
    where
        T: Query,
    {
        let qr: &dyn DefaultIdentity = &q;
        if let Some(result) = self.query.borrow().get(qr) {
            return Ok(result.downcast_ref::<T::Result>().unwrap().clone());
        }
        if self.current_queries.borrow().contains(qr) {
            return Err(QueryError::QueryCycle);
        }
        let nq = q.clone();
        self.current_queries.borrow_mut().insert(Box::new(nq));
        let result = q.query(self);
        self.current_queries.borrow_mut().remove(qr);
        self.query
            .borrow_mut()
            .insert(Box::new(q), Box::new(result.clone()));
        Ok(result)
    }

    pub fn query_default<T>(
        &self,
        q: T,
        f: impl FnOnce() -> T::Result,
    ) -> Result<T::Result, QueryError>
    where
        T: Query,
    {
        let qr: &dyn DefaultIdentity = &q;
        if let Some(result) = self.query.borrow().get(qr) {
            return Ok(result.downcast_ref::<T::Result>().unwrap().clone());
        }
        // no cycle is possible in this case
        let result = f();
        self.query
            .borrow_mut()
            .insert(Box::new(q), Box::new(result.clone()));
        Ok(result)
    }

    pub fn ir(&self) -> &ir::IR {
        self.ir
    }

    pub fn finish(self) -> QueryAnalysisResult {
        QueryAnalysisResult {
            query: self.query.take(),
        }
    }
}

pub struct QueryAnalysisResult {
    query: HashMap<Box<dyn DefaultIdentity>, Box<dyn Any>>,
}

impl QueryAnalysisResult {
    pub fn get<T: Query>(&self, q: &T) -> Option<T::Result> {
        let qr: &dyn DefaultIdentity = q;
        self.query
            .get(qr)
            .and_then(|x| x.downcast_ref::<T::Result>().cloned())
    }
}
