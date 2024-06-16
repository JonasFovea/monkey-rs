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
    String(String),
    Return(Box<Object>),
    Function(Vec<Identifier>, BlockStatement, Rc<Mutex<Environment>>),
    Null,
    Builtin(String),
    Array(Vec<Object>),
}

impl Object {
    pub fn type_str(&self) -> String {
        match self {
            Object::Builtin(_) => "Builtin",
            Object::Integer(_) => "Integer",
            Object::String(_) => "String",
            Object::Function(_, _, _) => "Function",
            Object::Null => "Null",
            Object::Boolean(_) => "Boolean",
            Object::Return(_) => "Return",
            Object::Array(_) => "Array",
        }.to_string()
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Object::Integer(a) => if let Object::Integer(b) = other {
                a == b
            } else { false },
            Object::Boolean(a) => if let Object::Boolean(b) = other {
                a == b
            } else { false },
            Object::String(a) => if let Object::String(b) = other {
                a == b
            } else { false }
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
            Object::Builtin(a) => {
                if let Object::Builtin(b) = other {
                    a == b
                } else { false }
            }
            Object::Array(a) => {
                if let Object::Array(b) = other {
                    a == b
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
            Object::String(s) => format!("{}", s),
            Object::Null => "Null".to_string(),
            Object::Return(val) => format!("return {}", val),
            Object::Function(params, body, _) => { format!("fn({}) {}", IdentifierVec(params.clone()), body) }
            Object::Builtin(f) => format!("{}(...){{builtin...}}", f),
            Object::Array(elems) => {
                format!("[{}]",
                        elems.iter()
                            .map(|e| format!("{}", e))
                            .collect::<Vec<String>>()
                            .join(", ")
                )
            }
        };
        write!(f, "{}", formatted)
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let formatted = match self {
            Object::Integer(i) => format!("Integer {{value: {}}}", i),
            Object::Boolean(b) => format!("Boolean {{value: {}}}", b),
            Object::String(s) => format!("String {{ value: \"{}\"", s),
            Object::Null => "Null".to_string(),
            Object::Return(val) => format!("return {:?}", val),
            Object::Function(params, body, env) => {
                format!("fn({:?}) {:?}\nFunction {}", IdentifierVec(params.clone()), body, env.lock().unwrap())
            }
            Object::Builtin(f) => format!("Builtin function {{ {}(...) }}", f),
            Object::Array(elems) => {
                format!("Array {{Elements: [{:?}]}}",
                        elems.iter()
                            .map(|e| format!("{}", e))
                            .collect::<Vec<String>>()
                            .join(", ")
                )
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


pub fn get_builtin_function(func_name: &str) -> Result<Box<dyn Fn(Vec<Object>) -> Result<Object>>> {
    match func_name {
        "len" => Ok(Box::new(len)),
        "first" => Ok(Box::new(first)),
        "rest" => Ok(Box::new(rest)),
        "push" => Ok(Box::new(push)),
        f => bail!("Builtin function {} unknown!", f)
    }
}

fn len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        bail!("Invalid number of arguments! Expected: 1, Got: {}", args.len());
    }
    match &args[0] {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        Object::Array(a) => Ok(Object::Integer(a.len() as i64)),
        a => bail!("Invalid argument of type: {}", a.type_str())
    }
}

fn first(args: Vec<Object>) -> Result<Object>{
    if args.len() != 1 {
        bail!("Invalid number of arguments! Expected: 1, Got: {}", args.len());
    }
    match &args[0] {
        Object::Array(a) => { 
            if a.len() < 1 {
                return Ok(Object::Null);
            }
            Ok(a[0].clone()) 
        },
        a => bail!("Invalid argument of type: {}", a.type_str())
    }
}

fn rest(args: Vec<Object>) -> Result<Object>{
    if args.len() != 1 {
        bail!("Invalid number of arguments! Expected: 1, Got: {}", args.len());
    }
    match &args[0] {
        Object::Array(a) => {
            if a.len() < 2 {
                return Ok(Object::Null);
            }
            Ok(Object::Array(a[1..].iter().map(|e|e.clone()).collect()))
        },
        a => bail!("Invalid argument of type: {}", a.type_str())
    }
}

fn push(args: Vec<Object>) -> Result<Object> {
    if args.len() != 2 {
        bail!("Invalid number of arguments! Expected: 2, Got: {}", args.len());
    }
    match (&args[0], &args[1]) {
        (Object::Array(a), b) => {
            let mut new_arr = a.clone();
            new_arr.push(b.clone());
            Ok(Object::Array(new_arr))
        },
        (a, b) => bail!("Invalid arguments of type: {}, {}", a.type_str(), b.type_str())
    }
}

