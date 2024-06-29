use anyhow::{bail, Context, Result};

use crate::ast::{Expression, ExpressionStatement, Program, Statement};
use crate::code::{Instructions, make, Opcode};
use crate::object::Object;
use crate::token::TokenType;

mod test;

#[derive(Debug, Clone)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<()> {
        for stmt in &program.statements {
            self.compile_statement(stmt)
                .context("Compiling program statement.")?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::EXPRESSION(ExpressionStatement { token: _tok, expression: exp }) => {
                self.compile_expression(&exp)
                    .context("Compiling ExpressionStatement expression.")?;
            }
            _ => todo!("Statement can't yet be compiled.")
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<()> {
        match expression {
            Expression::INFIX(a, op, b) => {
                self.compile_expression(a)
                    .context("Compiling first operand of infix expression.")?;
                self.compile_expression(b)
                    .context("Compiling second operand of infix expression.")?;

                match op.token_type {
                    TokenType::PLUS => {
                        self.emit(Opcode::OpAdd, vec![])
                            .context("Emitting OpAdd for infix expression.")?;
                    }
                    _ => bail!("Unknown operator {}", op)
                }
            }
            Expression::INT_LITERAL(_, i) => {
                let int = Object::Integer(*i);
                let const_pos = self.add_constant(int) as u16;
                self.emit(Opcode::OpConstant, vec![const_pos])
                    .context("Emitting code for integer literal.")?;
            }
            _ => todo!("Expression can't yet be compiled.")
        }

        Ok(())
    }

    fn add_constant(&mut self, object: Object) -> usize {
        self.constants.push(object);
        return self.constants.len() - 1;
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();

        self.instructions.0.extend_from_slice(&ins);

        return pos_new_instruction;
    }

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> Result<usize> {
        let ins = make(op, operands)
            .context("Building instruction to emit.")?;
        return Ok(self.add_instruction(ins.0));
    }

    pub(crate) fn bytecode(&self) -> Result<Bytecode> {
        if self.constants.len() == 0 && self.instructions.len() == 0 {
            bail!("Compiler did not produce any bytecode!");
        }
        Ok(Bytecode { instructions: self.instructions.clone(), constants: self.constants.clone() })
    }
}

pub(crate) struct Bytecode {
    pub(crate) instructions: Instructions,
    pub(crate) constants: Vec<Object>,
}