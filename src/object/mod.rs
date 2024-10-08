use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use std::sync::Mutex;

use crate::ast::{BlockStatement, Identifier};
use crate::code::Instructions;
use anyhow::{bail, Context, Result};

#[allow(dead_code)]
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
    Hash(HashMap<HashKey, Object>),
    CompiledFunction(Instructions, usize, usize), // instructions, num_locals, num_params
    Closure(Instructions, usize, usize, Vec<Object>), // instructions, num_locals, num_params, frees
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
            Object::Hash(_) => "Hash",
            Object::CompiledFunction(..) => "CompiledFunction",
            Object::Closure(..) => "Closure"
        }.to_string()
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Object::Boolean(b) => *b,
            Object::Null => false,
            _ => true
        }
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
            Object::Hash(a) => {
                if let Object::Hash(b) = other {
                    a == b
                } else { false }
            }
            Object::CompiledFunction(a, nla, npa) => {
                if let Object::CompiledFunction(b, nlb, npb) = other {
                    a == b && nla == nlb && npa == npb
                } else { false }
            }
            Object::Closure(insa, nla, npa, fra) => if let Object::Closure(insb, nlb, npb, frb) = other {
                insa == insb && nla == nlb && npa == npb && fra == frb
            } else { false }
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
            Object::Hash(map) => {
                format!("{{{}}}", map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", "))
            }
            Object::CompiledFunction(..) => format!("CompiledFunction[{:p}]", &self),
            Object::Closure(..) => format!("Closure[{:p}]", &self),
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
            Object::Hash(map) => {
                format!("Hash {{ Elements: {{{:?}}}}}", map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", "))
            }
            Object::CompiledFunction(ins, locals, params) => format!("CompiledFunction {{instructions: {:?}, num_locals: {}, num_params: {}}}", ins, locals, params),
            Object::Closure(ins, locals, params, frees) => format!("Closure {{ instructions: {:?}, num_locals: {}, num_params: {}, free variables: {:?}}}", ins, locals, params, frees),
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

impl PartialEq for Environment {
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


pub(crate) static BUILTINS: [&'static str; 6] = ["len", "first", "last", "rest", "push", "puts"];

pub fn get_builtin_function(func_name: &str) -> Result<Box<dyn Fn(Vec<Object>) -> Result<Object>>> {
    match func_name {
        "len" => Ok(Box::new(len)),
        "first" => Ok(Box::new(first)),
        "last" => Ok(Box::new(last)),
        "rest" => Ok(Box::new(rest)),
        "push" => Ok(Box::new(push)),
        "puts" => Ok(Box::new(puts)),
        f => bail!("Builtin function {} unknown!", f)
    }
}

fn len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        bail!("wrong number of arguments. got={}, want=1", args.len());
    }
    match &args[0] {
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        Object::Array(a) => Ok(Object::Integer(a.len() as i64)),
        a => bail!("argument to `len` not supported, got {}", a.type_str().to_uppercase())
    }
}

fn first(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        bail!("wrong number of arguments. got={}, want=1", args.len());
    }
    match &args[0] {
        Object::Array(a) => {
            if a.len() < 1 {
                return Ok(Object::Null);
            }
            Ok(a[0].clone())
        }
        a => bail!("argument to `first` must be ARRAY, got {}", a.type_str().to_uppercase())
    }
}

fn last(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        bail!("wrong number of arguments. got={}, want=1", args.len());
    }
    match &args[0] {
        Object::Array(a) => {
            if a.len() < 1 {
                return Ok(Object::Null);
            }
            Ok(a[a.len() - 1].clone())
        }
        a => bail!("argument to `last` must be ARRAY, got {}", a.type_str().to_uppercase())
    }
}

fn rest(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        bail!("wrong number of arguments. got={}, want=1", args.len());
    }
    match &args[0] {
        Object::Array(a) => {
            if a.len() < 2 {
                return Ok(Object::Null);
            }
            Ok(Object::Array(a[1..].iter().map(|e| e.clone()).collect()))
        }
        a => bail!("argument to `rest` must be ARRAY, got {}", a.type_str().to_uppercase())
    }
}

fn push(args: Vec<Object>) -> Result<Object> {
    if args.len() != 2 {
        bail!("wrong number of arguments. got={}, want=2", args.len());
    }
    match (&args[0], &args[1]) {
        (Object::Array(a), b) => {
            let mut new_arr = a.clone();
            new_arr.push(b.clone());
            Ok(Object::Array(new_arr))
        }
        (a, _) => bail!("argument to `push` must be ARRAY, got {}", a.type_str().to_uppercase())
    }
}

fn puts(args: Vec<Object>) -> Result<Object> {
    for arg in args {
        println!("{arg}");
    }
    Ok(Object::Null)
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum HashKey {
    INT(i64),
    STRING(String),
    BOOL(bool),
}

impl HashKey {
    pub fn from_object(obj: &Object) -> Result<Self> {
        match obj {
            Object::String(s) => Ok(HashKey::STRING(s.clone())),
            Object::Integer(i) => Ok(HashKey::INT(*i)),
            Object::Boolean(b) => Ok(HashKey::BOOL(*b)),
            o => {
                bail!("Object of type {} is not hashable! Supported types: Integer, String, Boolean.", o.type_str());
            }
        }
    }
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            HashKey::STRING(s) => format!("{}", s),
            HashKey::INT(i) => format!("{}", i),
            HashKey::BOOL(b) => format!("{}", b)
        })
    }
}
