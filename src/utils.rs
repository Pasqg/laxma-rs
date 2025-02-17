use std::{borrow::Borrow, collections::HashMap, hash::Hash, rc::Rc};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InsertionOrderHashMap<K, V>
where
    K: Hash + Eq,
{
    map: HashMap<Rc<K>, V>,
    keys: Vec<Rc<K>>,
}

impl<K, V> InsertionOrderHashMap<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            keys: Vec::new(),
        }
    }

    pub fn insert(&mut self, k: K, v: V) {
        let rc = Rc::new(k);
        self.keys.push(Rc::clone(&rc));
        self.map.insert(rc, v);
    }

    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        Rc<K>: Borrow<Q>,
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.map.get(k)
    }

    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where
        Rc<K>: Borrow<Q>,
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.map.get(k).is_some()
    }

    pub fn keys(&self) -> &Vec<Rc<K>> {
        &self.keys
    }
}
