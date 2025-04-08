use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use crate::objects::Object;

#[derive(Debug, Default)]
pub struct Environment {
    parent: Option<Arc<RwLock<BTreeMap<String, Object>>>>,
    current: Arc<RwLock<BTreeMap<String, Object>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set(&self, name: &str, value: Object) {
        let mut locked_curr = self.current.write().expect("Environment lock is poisoned");
        if locked_curr.contains_key(name) || self.parent.is_none() {
            locked_curr.insert(name.into(), value);
        } else if let Some(parent) = self.parent.as_ref() {
            parent
                .write()
                .expect("Environment lock is poisoned")
                .insert(name.into(), value);
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let locked_curr = self.current.read().expect("Environment lock is poisoned");
        if locked_curr.contains_key(name) {
            locked_curr.get(name).cloned()
        } else if let Some(parent) = self.parent.as_ref() {
            parent
                .read()
                .expect("Environment lock is poisoned")
                .get(name)
                .cloned()
        } else {
            locked_curr.get(name).cloned()
        }
    }

    pub fn new_derived_env(&self) -> Self {
        Self {
            parent: Some(self.current.clone()),
            current: Arc::default(),
        }
    }
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        Self {
            parent: self.parent.clone(),
            current: self.current.clone(),
        }
    }
}
