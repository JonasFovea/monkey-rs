use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::code::{read_uint16, Instructions, Opcode};
use crate::compiler::Bytecode;
use crate::object::{HashKey, Object};
use crate::vm::frame::Frame;

mod test;
mod frame;

const STACK_SIZE: usize = 2048;
pub(crate) const GLOBALS_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

#[derive(Debug)]
pub(crate) struct VM {
    constants: Vec<Object>,
    stack: [Object; STACK_SIZE],
    sp: usize,
    globals: Rc<Mutex<Box<[Object]>>>,
    frames: [Option<Frame>; MAX_FRAMES],
    frames_index: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        const NULL: Object = Object::Null;

        match bytecode {
            Bytecode { instructions: ins, constants: cons } => {
                const NONE: Option<Frame> = None;
                let mut frames = [NONE; MAX_FRAMES];
                let main_func = Object::CompiledFunction(ins);
                let main_frame = Frame::new(main_func).unwrap();
                frames[0] = Some(main_frame);

                VM {
                    constants: cons,
                    stack: [NULL; STACK_SIZE],
                    sp: 0,
                    globals: Rc::new(Mutex::new(vec![Object::Null; GLOBALS_SIZE].into_boxed_slice())),
                    frames,
                    frames_index: 1,
                }
            }
        }
    }

    pub fn with_global_store(bytecode: Bytecode, globals: Rc<Mutex<Box<[Object]>>>) -> Self {
        const NULL: Object = Object::Null;
        match bytecode {
            Bytecode { instructions: ins, constants: cons } => {
                const NONE: Option<Frame> = None;
                let mut frames = [NONE; MAX_FRAMES];
                let main_func = Object::CompiledFunction(ins);
                let main_frame = Frame::new(main_func).unwrap();
                frames[0] = Some(main_frame);

                VM {
                    constants: cons,
                    stack: [NULL; STACK_SIZE],
                    sp: 0,
                    globals,
                    frames,
                    frames_index: 1,
                }
            }
        }
    }

    #[allow(unused)]
    pub(crate) fn stack_top(&self) -> Result<Object> {
        if self.sp == 0 {
            bail!("Stack is empty!");
        }
        Ok(self.stack[self.sp - 1].clone())
    }

    #[allow(unreachable_patterns)]
    pub(crate) fn run(&mut self) -> Result<()> {
        while self.current_frame().ip + 1 < self.current_func().0.len() as isize {
            self.current_frame().ip += 1;
            let ip = self.current_frame().ip as usize;
            let op = Opcode::try_from(self.current_func().0[ip])
                .context("Decoding fetched instruction byte.")?;

            match op {
                Opcode::OpConstant => {
                    let const_index = read_uint16(&self.current_func().0[ip + 1..]) as usize;
                    self.current_frame().ip += 2;

                    self.push(self.constants[const_index].clone())
                        .with_context(|| format!("Pushing constant with idx {} onto the stack.", const_index))?;
                }
                Opcode::OpAdd | Opcode::OpSub | Opcode::OpDiv | Opcode::OpMul => {
                    self.execute_binary_operation(op)
                        .context("Executing binary operation.")?;
                }
                Opcode::OpPop => { self.pop().context("Executing OpPop.")?; }
                Opcode::OpTrue => {
                    self.push(Object::Boolean(true))
                        .context("Pushing true onto the stack.")?;
                }
                Opcode::OpFalse => {
                    self.push(Object::Boolean(false))
                        .context("Pushing false onto the stack.")?;
                }
                Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan | Opcode::OpGreaterEquals => {
                    self.execute_comparison(op)
                        .context("Executing comparison operator.")?;
                }
                Opcode::OpBang => {
                    self.execute_bang_operator()
                        .context("Executing bang operator.")?;
                }
                Opcode::OpMinus => {
                    self.execute_minus_operator()
                        .context("Executing minus operator.")?;
                }
                Opcode::OpJump => {
                    let pos = read_uint16(&self.current_func().0[ip + 1..]);

                    self.current_frame().ip = pos as isize - 1;
                }
                Opcode::OpJumpNotTruthy => {
                    let pos = read_uint16(&self.current_func().0[ip + 1..]);
                    self.current_frame().ip += 2;

                    let condition = self.pop()
                        .context("Popping condition value.")?;
                    if !condition.is_truthy() {
                        self.current_frame().ip = pos as isize - 1;
                    }
                }
                Opcode::OpNull => {
                    self.push(Object::Null)
                        .context("Pushing Null onto the stack.")?;
                }
                Opcode::OpSetGlobal => {
                    let global_index = read_uint16(&self.current_func().0[ip + 1..]);
                    self.current_frame().ip += 2;

                    self.globals
                        .lock().expect("Failed to access globals.")
                        [global_index as usize] = self.pop()
                        .context("Popping element to store in globals.")?;
                }
                Opcode::OpGetGlobal => {
                    let global_index = read_uint16(&self.current_func().0[ip + 1..]);
                    self.current_frame().ip += 2;
                    let obj = self.globals
                        .lock().expect("Failed to access globals.")
                        [global_index as usize].clone();
                    self.push(obj)
                        .context("Pushing global object onto the stack.")?
                }
                Opcode::OpArray => {
                    let num_elements = read_uint16(&self.current_func().0[ip + 1..]) as usize;
                    self.current_frame().ip += 2;

                    let array = self.build_array(self.sp - num_elements, self.sp);

                    self.sp -= num_elements;

                    self.push(array)
                        .context("Pushing array object.")?;
                }
                Opcode::OpHash => {
                    let num_elements = read_uint16(&self.current_func().0[ip + 1..]) as usize;
                    self.current_frame().ip += 2;

                    let hash = self.build_hash(self.sp - num_elements, self.sp)
                        .context("Build Hash object form stack contents.")?;

                    self.sp -= num_elements;

                    self.push(hash)
                        .context("Pushing array object.")?;
                }
                Opcode::OpIndex => {
                    let index = self.pop()
                        .context("Popping index.")?;
                    let left = self.pop()
                        .context("Popping left side of index expression.")?;

                    self.execute_index_expression(left, index)
                        .context("Evaluating index expression.")?;
                }
                Opcode::OpCall => {
                    let func = &self.stack[self.sp - 1];
                    match func {
                        Object::CompiledFunction(_) => {
                            self.push_frame(Frame::new(func.clone())
                                .context("Building new stack frame.")?)
                                .context("Pushing new stack frame.")?;
                        }
                        _ => bail!("Calling non-function.")
                    }
                }
                Opcode::OpReturnValue => {
                    let return_value = self.pop().context("Popping return value.")?;
                    self.pop_frame();
                    let _ = self.pop();

                    self.push(return_value).context("Pushing returned value.")?;
                }
                Opcode::OpReturn => {
                    self.pop_frame().context("Popping stack frame after OpReturn.")?;
                    self.pop().context("Popping stack element after OpReturn.")?;

                    self.push(Object::Null).context("Pushing Null-Object after OpReturn.")?;
                }
                _ => bail!("Operation {:?} not yet implemented!", op)
            }
        }
        Ok(())
    }

    fn execute_binary_operation(&mut self, opcode: Opcode) -> Result<()> {
        let right = self.pop().context("Popping right operand for OpAdd")?;
        let left = self.pop().context("Popping left operand for OpAdd")?;

        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.execute_binary_integer_operation(opcode, *l, *r)
                    .context("Executing binary integer operation.")?;
            }
            (Object::String(l), Object::String(r)) => {
                self.execute_binary_string_operation(opcode, l, r)
                    .context("Executing binary string operation.")?;
            }
            _ => bail!("Unsupported types for binary operation: {:?} {:?}", left.type_str(), right.type_str())
        }
        Ok(())
    }

    fn execute_binary_integer_operation(&mut self, opcode: Opcode, left: i64, right: i64) -> Result<()> {
        let result = match opcode {
            Opcode::OpAdd => {
                left.checked_add(right)
                    .context("Overflow on integer addition!")?
            }
            Opcode::OpSub => {
                left.checked_sub(right)
                    .context("Overflow on integer subtraction!")?
            }
            Opcode::OpDiv => {
                left.checked_div(right)
                    .context("Error on integer division!")?
            }
            Opcode::OpMul => {
                left.checked_mul(right)
                    .context("Overflow on integer multiplication!")?
            }
            _ => bail!("Operation {:?} not implemented for two integers!", opcode)
        };
        self.push(Object::Integer(result))
            .context("Pushing result of binary integer operation.")?;
        Ok(())
    }

    fn execute_binary_string_operation(&mut self, opcode: Opcode, left: &str, right: &str) -> Result<()> {
        if opcode != Opcode::OpAdd {
            bail!("Unknown string operator: {:?}", opcode);
        }
        let mut new_string = String::from(left);
        new_string.push_str(right);
        self.push(Object::String(new_string))
            .context("Pushing result of binary string operation.")?;

        Ok(())
    }

    fn execute_comparison(&mut self, opcode: Opcode) -> Result<()> {
        let right = self.pop()
            .context("Popping right comparison operand.")?;
        let left = self.pop()
            .context("Popping left comparison operand.")?;

        if let (Object::Integer(l), Object::Integer(r)) = (&left, &right) {
            return self.execute_integer_comparison(opcode, *l, *r)
                .context("Executing integer comparison.");
        }

        let result = match opcode {
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            _ => bail!("Unsupported comparison operator {:?} for {:?} and {:?}", opcode, left.type_str(), right.type_str())
        };
        self.push(result)
            .context("Pushing comparison result onto the stack.")?;

        Ok(())
    }

    fn execute_integer_comparison(&mut self, opcode: Opcode, left: i64, right: i64) -> Result<()> {
        let result = match opcode {
            Opcode::OpEqual => Object::Boolean(left == right),
            Opcode::OpNotEqual => Object::Boolean(left != right),
            Opcode::OpGreaterThan => Object::Boolean(left > right),
            Opcode::OpGreaterEquals => Object::Boolean(left >= right),
            _ => bail!("Unsupported operator for integer comparison: {:?}", opcode)
        };
        self.push(result)
            .context("Pushing comparison result onto the stack.")?;
        Ok(())
    }

    fn execute_bang_operator(&mut self) -> Result<()> {
        let operand = self.pop()
            .context("Popping bang operand.")?;
        let result = match operand {
            Object::Boolean(b) => Object::Boolean(!b),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        };

        self.push(result).context("Pushing bang result onto the stack.")?;

        Ok(())
    }

    fn execute_minus_operator(&mut self) -> Result<()> {
        let operand = self.pop()
            .context("Popping minus operand.")?;

        if let Object::Integer(i) = operand {
            self.push(Object::Integer(-i))
                .context("Pushing minus result onto the stack.")?;
        } else { bail!("Unsupported operand type for prefix minus: {:?}", operand.type_str()) }

        Ok(())
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<()> {
        match (&left, &index) {
            (Object::Array(elems), Object::Integer(i)) => {
                let obj = elems.get(*i as usize)
                    .unwrap_or(&Object::Null)
                    .clone();

                self.push(obj)
                    .context("Pushing array element.")?;
            }
            (Object::Hash(map), idx) => {
                let key = HashKey::from_object(idx)
                    .context("Building HashKey from index object.")?;

                let value = map.get(&key)
                    .unwrap_or(&Object::Null)
                    .clone();

                self.push(value)
                    .context("Pushing hash element.")?;
            }
            _ => bail!("Index operator is not supported for types {}[{}]", left.type_str(), index.type_str())
        }

        Ok(())
    }

    fn build_array(&mut self, start: usize, stop: usize) -> Object {
        let mut elements = vec![Object::Null; stop - start];

        for i in start..stop {
            elements[i - start] = self.stack[i].clone();
        }

        Object::Array(elements)
    }

    fn build_hash(&mut self, start: usize, stop: usize) -> Result<Object> {
        let mut map = HashMap::with_capacity((stop - start) / 2);

        for i in (start..stop).step_by(2) {
            let key = HashKey::from_object(&self.stack[i])
                .context("Building HashKey from stack object.")?;
            let value = self.stack[i + 1].clone();
            map.insert(key, value);
        }

        Ok(Object::Hash(map))
    }

    fn push(&mut self, obj: Object) -> Result<()> {
        if self.sp >= STACK_SIZE {
            bail!("Stack overflow!");
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Result<Object> {
        if self.sp <= 0 {
            bail!("Can't pop from empty stack!");
        }

        let o = self.stack[self.sp - 1].clone();
        self.sp -= 1;

        Ok(o)
    }

    pub(crate) fn last_popped_stack_elem(&self) -> Result<Object> {
        Ok(self.stack.get(self.sp).expect("Retrieving last popped element from the stack.").clone())
    }

    fn current_frame(&mut self) -> &mut Frame {
        self.frames[self.frames_index - 1].as_mut().unwrap()
    }

    fn current_func(&mut self) -> &mut Instructions {
        match self.frames[self.frames_index - 1].as_mut() {
            Some(Frame { func, ip: _ }) => {
                match func.as_mut() {
                    Object::CompiledFunction(ins) => {
                        ins
                    }
                    _ => panic!("Current frame does not contain a compiled function!")
                }
            }
            _ => panic!("Current frame not found!")
        }
    }

    fn push_frame(&mut self, frame: Frame) -> Result<()> {
        if self.frames_index > MAX_FRAMES {
            bail!("Frame stack size exceeded!");
        }

        self.frames[self.frames_index] = Some(frame);
        self.frames_index += 1;

        Ok(())
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        self.frames_index -= 1;

        std::mem::replace(&mut self.frames[self.frames_index], None)
    }
}
