use std::fmt;
use std::fmt::Formatter;

#[derive(Eq, PartialEq)]
pub enum Object{
    Integer(i64),
    Boolean(bool),
    Return(Box<Object>),
    Null,
}

impl fmt::Display for Object{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let formatted = match self {
            Object::Integer(i) => format!("{}", i),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => "Null".to_string(),
            Object::Return(val) => format!("return {}", val),
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
        };
        write!(f, "{}", formatted)
    }
}