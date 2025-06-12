use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use crate::objects::Object;

#[derive(Debug, Clone, Default)]
struct InnerEnvironment {
    parent: Option<Environment>,
    current: Arc<RwLock<BTreeMap<String, Object>>>,
}
unsafe impl Send for InnerEnvironment {}
unsafe impl Sync for InnerEnvironment {}

#[derive(Debug, Clone, Default)]
pub struct Environment(Arc<InnerEnvironment>);

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set(&self, name: &str, value: Object) {
        let mut locked_curr = self
            .0
            .current
            .write()
            .expect("Environment lock is poisoned");
        if locked_curr.contains_key(name) || self.0.parent.is_none() {
            locked_curr.insert(name.into(), value);
        } else if let Some(parent) = self.0.parent.as_ref() {
            parent.set(name, value);
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let locked_curr = self.0.current.read().expect("Environment lock is poisoned");
        if locked_curr.contains_key(name) {
            locked_curr.get(name).cloned()
        } else if let Some(parent) = self.0.parent.as_ref() {
            drop(locked_curr);
            parent.get(name)
        } else {
            None
        }
    }

    pub fn new_derived_env(&self) -> Self {
        Self(Arc::new(InnerEnvironment {
            parent: Some(self.clone()),
            current: Arc::default(),
        }))
    }
}