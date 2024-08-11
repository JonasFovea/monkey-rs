use crate::code::Instructions;
use crate::object::Object;
use anyhow::bail;
use anyhow::Result;

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) func: Box<Object>,
    pub(crate) ip: isize,
}

impl Frame {
    pub(crate) fn new(func: Object) -> Result<Self> {
        match func {
            Object::CompiledFunction(_) => Ok(Frame {
                func: Box::new(func),
                ip: -1,
            }),
            _ => bail!("Expected object of type CompiledFunction! Got {} instead!", func.type_str())
        }
    }

    pub(crate) fn get_instructions(&self) -> &Instructions {
        match self.func.as_ref() {
            Object::CompiledFunction(ins) => ins,
            _ => panic!("Frame does not contain compiled function!")
        }
    }
}
