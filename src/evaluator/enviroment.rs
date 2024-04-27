use std::collections::HashMap;

use super::object::Object;

#[derive(Default)]
pub struct Enviroment {
    store: HashMap<String, Object>,
}

impl Enviroment {
    pub fn get(&self, name: &String) -> Option<Object> {
        self.store.get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
