use std::{collections::HashMap, hash::Hash};

use nohash_hasher::{IntMap, IsEnabled};

pub(super) fn to_int_map<K, V>(map: HashMap<K, V>) -> IntMap<K, V>
where
    K: Eq + Hash + PartialEq + IsEnabled,
{
    let mut result = IntMap::default();
    result.extend(map);
    result
}
