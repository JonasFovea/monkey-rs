use anyhow::{bail, Context, Result};

use crate::code::{Instructions, Opcode, read_uint16};
use crate::compiler::Bytecode;
use crate::object::Object;

mod test;

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub(crate) struct VM {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: [Object; STACK_SIZE],
    sp: usize,
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
                }
            }
        }
    }

    pub(crate) fn stack_top(&self) -> Result<Object> {
        if self.sp == 0 {
            bail!("Stack is empty!");
        }
        Ok(self.stack[self.sp - 1].clone())
    }

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
                Opcode::OpAdd => {
                    let right = self.pop().context("Popping right operand for OpAdd")?;
                    let left = self.pop().context("Popping left operand for OpAdd")?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            let result = l.checked_add(*r)
                                .context("Overflow on integer addition!")?;
                            self.push(Object::Integer(result))
                                .context("Pushing result of integer addition.")?;
                        },
                        _ => bail!("Expected two integer operands, got: {:?} and {:?}", &left, &right)
                    }
                }
                _ => todo!("Operation not yet implemented!")
            }

            ip += 1;
        }
        Ok(())
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
}