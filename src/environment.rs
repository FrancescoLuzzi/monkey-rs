use std::collections::BTreeMap;

use crate::objects::Object;

#[derive(Default)]
pub struct Environment {
    env: BTreeMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set(&mut self, name: &str, value: Object) {
        self.env.insert(name.into(), value);
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.env.get(name)
    }
}
