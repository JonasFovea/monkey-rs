use crate::object::Object;
use anyhow::bail;
use anyhow::Result;

#[derive(Debug)]
pub(crate) struct Frame {
    pub(crate) func: Box<Object>,
    pub(crate) ip: isize,
    pub(crate) base_pointer: usize,
}

impl Frame {
    pub(crate) fn new(func: Object, base_pointer: usize) -> Result<Self> {
        match func {
            Object::CompiledFunction(..) => Ok(Frame {
                func: Box::new(func),
                ip: -1,
                base_pointer,
            }),
            _ => bail!("Expected object of type CompiledFunction! Got {} instead!", func.type_str())
        }
    }
}
