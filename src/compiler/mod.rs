use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::ast::{Expression, ExpressionStatement, LetStatement, Program, Statement};
use crate::code::{Instructions, make, Opcode};
use crate::compiler::symbol_table::SymbolTable;
use crate::object::Object;
use crate::token::TokenType;

mod test;
pub(crate) mod symbol_table;
mod test_symbol_table;

#[derive(Debug, Clone)]
pub struct Compiler {
    instructions: Instructions,
    constants: Rc<Mutex<Vec<Object>>>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
    symbol_table: Rc<Mutex<SymbolTable>>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Rc::new(Mutex::new(Vec::new())),
            last_instruction: None,
            previous_instruction: None,
            symbol_table: Rc::new(Mutex::new(SymbolTable::new())),
        }
    }

    pub fn with_state(symbol_table: Rc<Mutex<SymbolTable>>, constants: Rc<Mutex<Vec<Object>>>) -> Self {
        Compiler {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
            constants,
            symbol_table,
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<()> {
        self.compile_block(&program.statements)
            .context("Compiling program statements.")?;
        Ok(())
    }

    fn compile_block(&mut self, block: &Vec<Statement>) -> Result<()> {
        for stmt in block {
            self.compile_statement(stmt)
                .context("Compiling statement.")?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::EXPRESSION(ExpressionStatement { token: _tok, expression: exp }) => {
                self.compile_expression(&exp)
                    .context("Compiling ExpressionStatement expression.")?;
                self.emit(Opcode::OpPop, vec![])?;
            }
            Statement::LET(LetStatement { token: _, value: exp, identifier: id }) => {
                self.compile_expression(exp)
                    .context("Compiling value of let statement.")?;

                let symbol = self.symbol_table.lock()
                    .expect("Failed to access symbol table")
                    .define(&id.value);

                self.emit(Opcode::OpSetGlobal, vec![symbol.index as u16])
                    .context("Emitting OpSetGlobal for let statement.")?;
            }
            _ => bail!("Statement {:?} can't yet be compiled.", statement)
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression) -> Result<()> {
        match expression {
            Expression::INFIX(a, op, b) => {
                match op.token_type {
                    TokenType::LT => {
                        self.compile_expression(b)
                            .context("Compiling first (switched) operand of infix expression.")?;
                        self.compile_expression(a)
                            .context("Compiling second (switched) operand of infix expression.")?;
                        self.emit(Opcode::OpGreaterThan, vec![])
                            .context("Emitting OpGreaterThan for (switched) infix expression.")?;
                        return Ok(());
                    }
                    TokenType::LEQ => {
                        self.compile_expression(b)
                            .context("Compiling first (switched) operand of infix expression.")?;
                        self.compile_expression(a)
                            .context("Compiling second (switched) operand of infix expression.")?;
                        self.emit(Opcode::OpGreaterEquals, vec![])
                            .context("Emitting OpGreaterEquals for (switched) infix expression.")?;
                        return Ok(());
                    }
                    _ => {}
                }

                self.compile_expression(a)
                    .context("Compiling first operand of infix expression.")?;
                self.compile_expression(b)
                    .context("Compiling second operand of infix expression.")?;

                match op.token_type {
                    TokenType::PLUS => {
                        self.emit(Opcode::OpAdd, vec![])
                            .context("Emitting OpAdd for infix expression.")?;
                    }
                    TokenType::MINUS => {
                        self.emit(Opcode::OpSub, vec![])
                            .context("Emitting OpSub for infix expression.")?;
                    }
                    TokenType::ASTERISK => {
                        self.emit(Opcode::OpMul, vec![])
                            .context("Emitting OpMul for infix expression.")?;
                    }
                    TokenType::SLASH => {
                        self.emit(Opcode::OpDiv, vec![])
                            .context("Emitting OpDiv for infix expression.")?;
                    }
                    TokenType::GT => {
                        self.emit(Opcode::OpGreaterThan, vec![])
                            .context("Emitting OpGreaterThan for infix expression.")?;
                    }
                    TokenType::EQ => {
                        self.emit(Opcode::OpEqual, vec![])
                            .context("Emitting OpEqual for infix expression.")?;
                    }
                    TokenType::NEQ => {
                        self.emit(Opcode::OpNotEqual, vec![])
                            .context("Emitting OpNotEqual for infix expression.")?;
                    }
                    TokenType::GEQ => {
                        self.emit(Opcode::OpGreaterEquals, vec![])
                            .context("Emitting OpGreaterEquals for infix expression.")?;
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
            Expression::BOOL_LITERAL(_, b) => {
                if *b {
                    self.emit(Opcode::OpTrue, vec![])
                        .context("Emitting boolean literal true.")?;
                } else {
                    self.emit(Opcode::OpFalse, vec![])
                        .context("Emitting boolean literal false.")?;
                }
            }
            Expression::PREFIX(op, e) => {
                self.compile_expression(e)
                    .context("Compiling inner expression of prefix expression.")?;
                match op.token_type {
                    TokenType::BANG => self.emit(Opcode::OpBang, vec![])
                        .context("Emitting prefix bang operator.")?,
                    TokenType::MINUS => self.emit(Opcode::OpMinus, vec![])
                        .context("Emitting prefix minus operator.")?,
                    _ => bail!("Unknown prefix operator: {:?}", op.token_type)
                };
            }
            Expression::IF_EXPRESSION(_, condition, consequence, alternative) => {
                self.compile_expression(condition)
                    .context("Compiling condition of if expression")?;

                let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, vec![9999])
                    .context("Emitting conditional jump with placeholder address.")?;

                self.compile_block(&consequence.statements)
                    .context("Compiling consequence block.")?;

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                let jump_pos = self.emit(Opcode::OpJump, vec![9999])
                    .context("Emitting jump instruction after conditional consequence with placeholder address.")?;

                let after_consequence_pos = self.instructions.len();
                self.change_operand(jump_not_truthy_pos, vec![after_consequence_pos as u16])
                    .context("Swapping jump address of conditional.")?;

                if let Some(alt) = alternative {
                    self.compile_block(&alt.statements)
                        .context("Compiling alternative of conditional.")?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(Opcode::OpNull, vec![])
                        .context("Emitting OpNull for empty alternative of conditional.")?;
                }

                let after_alternative_pos = self.instructions.len();

                self.change_operand(jump_pos, vec![after_alternative_pos as u16])
                    .context("Swapping jump address after conditional consequence.")?;
            }
            Expression::IDENT(id) => {
                let symbol = self.symbol_table.lock()
                    .expect("Failed to access symbol table.")
                    .resolve(&id.value)
                    .context("Resolving identifier in symbol table.")?;

                self.emit(Opcode::OpGetGlobal, vec![symbol.index as u16])
                    .context("Emitting OpGetGlobal to load identifier.")?;
            }
            _ => bail!("Expression {:?} can't yet be compiled.", expression)
        }

        Ok(())
    }

    fn add_constant(&mut self, object: Object) -> usize {
        self.constants.lock()
            .expect("Failed to access constants.")
            .push(object);
        return self.constants.lock()
            .expect("Failed to access constants.")
            .len() - 1;
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();

        self.instructions.0.extend_from_slice(&ins);

        return pos_new_instruction;
    }

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> Result<usize> {
        let ins = make(op, operands)
            .context("Building instruction to emit.")?;
        let pos = self.add_instruction(ins.0);

        self.set_last_instruction(op, pos);

        return Ok(pos);
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        self.previous_instruction = self.last_instruction;
        self.last_instruction = Some(EmittedInstruction {
            opcode,
            position,
        });
    }

    fn last_instruction_is_pop(&self) -> bool {
        if let Some(ins) = self.last_instruction {
            return ins.opcode == Opcode::OpPop;
        }
        false
    }

    fn remove_last_pop(&mut self) {
        let new_len = if let Some(ins) = self.last_instruction {
            ins.position
        } else { 0 };

        self.instructions.0.resize(new_len, 0);

        self.last_instruction = self.previous_instruction;
        self.previous_instruction = None;
    }


    fn replace_instruction(&mut self, pos: usize, instructions: Instructions) {
        for (i, repl) in instructions.0.iter().enumerate() {
            self.instructions.0[pos + i] = *repl;
        }
    }

    fn change_operand(&mut self, op_pos: usize, operands: Vec<u16>) -> Result<()> {
        let op = Opcode::try_from(*self.instructions.0.get(op_pos)
            .context("Retrieving opcode to change operand of.")?)
            .context("Converting given instruction byte into opcode.")?;

        let instruction = make(op, operands)
            .context("Constructing new instruction with replaced operands.")?;

        self.replace_instruction(op_pos, instruction);

        Ok(())
    }

    pub(crate) fn bytecode(&self) -> Result<Bytecode> {
        if self.constants.lock()
            .expect("Failed to access constants.")
            .len() == 0 && self.instructions.len() == 0 {
            bail!("Compiler did not produce any bytecode!");
        }
        Ok(Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.lock()
                .expect("Failed to access constants.")
                .clone(),
        })
    }
}

pub(crate) struct Bytecode {
    pub(crate) instructions: Instructions,
    pub(crate) constants: Vec<Object>,
}

#[derive(Copy, Clone, Debug)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}