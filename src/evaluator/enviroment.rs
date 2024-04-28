use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::object::Object;

pub type EnviromentType = Rc<RefCell<Enviroment>>;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Enviroment {
    store: HashMap<String, Object>,
    outer: Option<EnviromentType>,
}

impl Enviroment {
    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(result) => Some(result.clone()),
            _ => match self.outer {
                Some(ref outer) => outer.borrow_mut().get(name),
                _ => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }

    pub fn new_enclosed_enviroment(outer: EnviromentType) -> Self {
        Self {
            store: HashMap::default(),
            outer: Some(outer),
        }
    }
}
