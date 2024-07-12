use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::code::{Instructions, Opcode, read_uint16};
use crate::compiler::Bytecode;
use crate::object::Object;

mod test;

const STACK_SIZE: usize = 2048;
pub(crate) const GLOBALS_SIZE: usize = 65536;

#[derive(Debug)]
pub(crate) struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: [Object; STACK_SIZE],
    sp: usize,
    globals: Rc<Mutex<Box<[Object]>>>,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        const NULL: Object = Object::Null;
        match bytecode {
            Bytecode { instructions: ins, constants: cons } => {
                VM {
                    constants: cons,
                    instructions: ins,
                    stack: [NULL; STACK_SIZE],
                    sp: 0,
                    globals: Rc::new(Mutex::new(vec![Object::Null; GLOBALS_SIZE].into_boxed_slice())),
                }
            }
        }
    }

    pub fn with_global_store(bytecode: Bytecode, globals: Rc<Mutex<Box<[Object]>>>) -> Self {
        const NULL: Object = Object::Null;
        match bytecode {
            Bytecode { instructions: ins, constants: cons } => {
                VM {
                    constants: cons,
                    instructions: ins,
                    stack: [NULL; STACK_SIZE],
                    sp: 0,
                    globals,
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
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = Opcode::try_from(self.instructions[ip])
                .context("Decoding fetched instruction byte.")?;

            match op {
                Opcode::OpConstant => {
                    let const_index = read_uint16(&self.instructions.0[ip + 1..]) as usize;
                    ip += 2;

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
                    let pos = read_uint16(&self.instructions.0[ip + 1..]);

                    ip = pos as usize - 1;
                }
                Opcode::OpJumpNotTruthy => {
                    let pos = read_uint16(&self.instructions.0[ip + 1..]);
                    ip += 2;

                    let condition = self.pop()
                        .context("Popping condition value.")?;
                    if !condition.is_truthy() {
                        ip = pos as usize - 1;
                    }
                }
                Opcode::OpNull => {
                    self.push(Object::Null)
                        .context("Pushing Null onto the stack.")?;
                }
                Opcode::OpSetGlobal => {
                    let global_index = read_uint16(&self.instructions.0[ip + 1..]);
                    ip += 2;

                    self.globals
                        .lock().expect("Failed to access globals.")
                        [global_index as usize] = self.pop()
                        .context("Popping element to store in globals.")?;
                }
                Opcode::OpGetGlobal => {
                    let global_index = read_uint16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    let obj = self.globals
                        .lock().expect("Failed to access globals.")
                        [global_index as usize].clone();
                    self.push(obj)
                        .context("Pushing global object onto the stack.")?
                }
                Opcode::OpArray => {
                    let num_elements = read_uint16(&self.instructions.0[ip + 1..]) as usize;
                    ip += 2;

                    let array = self.build_array(self.sp - num_elements, self.sp);

                    self.sp -= num_elements;

                    self.push(array)
                        .context("Pushing array object.")?;
                }
                _ => bail!("Operation {:?} not yet implemented!", op)
            }

            ip += 1;
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

    fn build_array(&mut self, start: usize, stop: usize) -> Object {
        let mut elements = vec![Object::Null; stop - start];

        for i in start..stop {
            elements[i - start] = self.stack[i].clone();
        }

        Object::Array(elements)
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
}
