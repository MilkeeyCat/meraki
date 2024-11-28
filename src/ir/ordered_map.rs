pub trait OrderedMap<K: Eq, V> {
    fn get(&self, k: &K) -> Option<&V>;
}

impl<K: Eq, V> OrderedMap<K, V> for &[(K, V)] {
    fn get(&self, k: &K) -> Option<&V> {
        self.iter()
            .find(|(key, _)| key == k)
            .map(|(_, value)| value)
    }
}
