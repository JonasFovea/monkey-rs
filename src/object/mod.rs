use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

use anyhow::{bail, Context, Result};

use crate::ast::{BlockStatement, Identifier};

#[derive(Eq, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Environment),
    Null,
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
                format!("fn({:?}) {:?}\nFunction {}", IdentifierVec(params.clone()), body, env)
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

#[derive(Clone, Eq, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: HashMap::new(), outer: None }
    }

    pub fn new_enclosed(outer: Environment) -> Self {
        Environment { store: HashMap::new(), outer: Some(Box::new(outer)) }
    }

    pub fn get(&self, name: &str) -> Result<Object> {
        let obj = self.store.get(name);
        if let Some(o) = obj {
            return Ok(o.clone());
        }
        if let Some(outer) = &self.outer {
            return Ok(outer.get(name).context("Searching outer environment for identifier.")?.clone());
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
            let outer_str = format!("\nouter {}", env)
                .replace("\n", "\n\t");
            out.push_str(&outer_str);
        }
        out.push_str("\n}");

        write!(f, "{out}")
    }
}
