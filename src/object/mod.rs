use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::ast::{BlockStatement, Identifier};

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Rc<Mutex<Environment>>),
    Null,
}

impl std::cmp::PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Integer(a) => if let Object::Integer(b) = other {
                a == b
            } else { false },
            Object::Boolean(a) => if let Object::Boolean(b) = other {
                a == b
            } else { false },
            Object::Return(a) => if let Object::Return(b) = other {
                *a == *b
            } else { false }
            Object::Function(ai, ab, ae) => if let Object::Function(bi, bb, be) = other {
                ai == bi && ab == bb && Rc::ptr_eq(ae, be)
            } else { false },
            Object::Null => {
                if let Object::Null = other {
                    true
                } else { false }
            }
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let formatted = match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => "Null".to_string(),
            Object::Return(val) => format!("return {}", val),
            Object::Function(params, body, _) => { format!("fn({}) {}", IdentifierVec(params.clone()), body) }
        };
        write!(f, "{}", formatted)
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let formatted = match self {
            Object::Integer(i) => format!("Integer {{value: {}}}", i),
            Object::Boolean(b) => format!("Boolean {{value: {}}}", b),
            Object::Null => "Null".to_string(),
            Object::Return(val) => format!("return {:?}", val),
            Object::Function(params, body, env) => {
                format!("fn({:?}) {:?}\nFunction {}", IdentifierVec(params.clone()), body, env.lock().unwrap())
            }
        };
        write!(f, "{}", formatted)
    }
}

struct IdentifierVec(Vec<Identifier>);

impl fmt::Display for IdentifierVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self.0.iter().map(|id| id.value.clone()).collect::<Vec<String>>().join(", ");
        write!(f, "{s}")
    }
}

impl fmt::Debug for IdentifierVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<Mutex<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: HashMap::new(), outer: None }
    }

    pub fn new_enclosed(outer: Rc<Mutex<Environment>>) -> Self {
        Environment { store: HashMap::new(), outer: Some(outer) }
    }

    pub fn get(&self, name: &str) -> Result<Object> {
        let obj = self.store.get(name);
        if let Some(o) = obj {
            return Ok(o.clone());
        }
        if let Some(outer) = &self.outer {
            return Ok(outer.lock().unwrap().get(name)
                .context("Searching outer environment for identifier.")?
                .clone());
        }

        bail!("Identifier not found: {}", name);
    }

    pub fn set(&mut self, name: &str, obj: Object) -> Result<()> {
        let _old = self.store.insert(name.to_string(), obj);
        // if old.is_some(){
        //     bail!("Identifier {} already defined with value: {}", name, old.unwrap());
        // }
        Ok(())
    }
}

impl std::cmp::PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.store == other.store && if let (Some(a), Some(b)) = (&self.outer, &other.outer) {
            Rc::ptr_eq(a, b)
        } else { self.outer.is_none() == other.outer.is_none() }
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        out.push_str("Environment {\n");
        let vals = self.store.iter()
            .map(|(k, v)| format!("\t| {k}: {v}"))
            .collect::<Vec<String>>()
            .join(",\n");
        out.push_str(&vals);
        if let Some(env) = &self.outer {
            let outer_str = format!("\nouter {}", env.lock().unwrap())
                .replace("\n", "\n\t");
            out.push_str(&outer_str);
        }
        out.push_str("\n}");

        write!(f, "{out}")
    }
}
