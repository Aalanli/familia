use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
struct PointerHashKey<'a, K: ?Sized> {
    key: &'a K,
}

#[derive(Debug, Clone, Default)]
pub struct PointerHashMap<'a, K: ?Sized, V> {
    map: HashMap<PointerHashKey<'a, K>, V>,
}

impl<'a, K: ?Sized> Hash for PointerHashKey<'a, K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.key as *const K).hash(state);
    }
}

impl<'a, K: ?Sized> PartialEq for PointerHashKey<'a, K> {
fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(self.key as *const K, other.key as *const K)
    }
}

impl<'a, K: ?Sized> Eq for PointerHashKey<'a, K> {}

impl<'a, K: ?Sized, V> PointerHashMap<'a, K, V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'a K, value: V) -> Option<V> {
        self.map.insert(PointerHashKey { key }, value)
    }

    pub fn get(&self, key: &'a K) -> Option<&V> {
        self.map.get(&PointerHashKey { key })
    }

    pub fn contains_key(&self, key: &'a K) -> bool {
        self.map.contains_key(&PointerHashKey { key })
    }

    pub fn remove(&mut self, key: &'a K) -> Option<V> {
        self.map.remove(&PointerHashKey { key })
    }

    pub fn or_insert_with<F>(&mut self, key: &'a K, default: F) -> &V
    where
        F: FnOnce() -> V,
    {
        self.map
            .entry(PointerHashKey { key })
            .or_insert_with(default)
    }
}
