use std::rc::Rc;
use std::sync::Mutex;

use anyhow::{bail, Context, Result};

use crate::ast::{Expression, ExpressionStatement, LetStatement, Program, ReturnStatement, Statement};
use crate::code::{make, Instructions, Opcode};
use crate::compiler::symbol_table::{Scope, SymbolTable};
use crate::object::{Object, BUILTINS};
use crate::token::TokenType;

mod test;
pub(crate) mod symbol_table;
mod test_symbol_table;

#[derive(Debug, Clone)]
pub struct Compiler {
    constants: Rc<Mutex<Vec<Object>>>,
    symbol_table: Rc<Mutex<SymbolTable>>,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();
        for (i, builtin) in BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, builtin);
        }

        Compiler {
            constants: Rc::new(Mutex::new(Vec::new())),
            symbol_table: Rc::new(Mutex::new(symbol_table)),
            scopes: vec![CompilationScope {
                instructions: Instructions::new(),
                last_instruction: None,
                previous_instruction: None,
            }],
            scope_index: 0,
        }
    }

    pub fn with_state(symbol_table: Rc<Mutex<SymbolTable>>, constants: Rc<Mutex<Vec<Object>>>) -> Self {
        Compiler {
            constants,
            symbol_table,
            scopes: vec![CompilationScope {
                instructions: Instructions::new(),
                last_instruction: None,
                previous_instruction: None,
            }],
            scope_index: 0,
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

                if symbol.scope == Scope::GlobalScope {
                    self.emit(Opcode::OpSetGlobal, vec![symbol.index as u16])
                        .context("Emitting OpSetGlobal for let statement.")?;
                } else {
                    self.emit(Opcode::OpSetLocal, vec![symbol.index as u16])
                        .context("Emitting OpSetLocal for let statement.")?;
                }
            }
            Statement::RETURN(ReturnStatement { return_value: val, .. }) => {
                self.compile_expression(val)
                    .context("Compiling return value.")?;

                self.emit(Opcode::OpReturnValue, vec![])
                    .context("Emitting OpReturnValue.")?;
            }
            // _ => bail!("Statement {:?} can't yet be compiled.", statement)
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

                if self.last_instruction_is(Opcode::OpPop) {
                    self.remove_last_pop();
                }

                let jump_pos = self.emit(Opcode::OpJump, vec![9999])
                    .context("Emitting jump instruction after conditional consequence with placeholder address.")?;

                let after_consequence_pos = self.current_instructions().len();
                self.change_operand(jump_not_truthy_pos, vec![after_consequence_pos as u16])
                    .context("Swapping jump address of conditional.")?;

                if let Some(alt) = alternative {
                    self.compile_block(&alt.statements)
                        .context("Compiling alternative of conditional.")?;

                    if self.last_instruction_is(Opcode::OpPop) {
                        self.remove_last_pop();
                    }
                } else {
                    self.emit(Opcode::OpNull, vec![])
                        .context("Emitting OpNull for empty alternative of conditional.")?;
                }

                let after_alternative_pos = self.current_instructions().len();

                self.change_operand(jump_pos, vec![after_alternative_pos as u16])
                    .context("Swapping jump address after conditional consequence.")?;
            }
            Expression::IDENT(id) => {
                let symbol = self.symbol_table.lock()
                    .expect("Failed to access symbol table.")
                    .resolve(&id.value)
                    .with_context(|| format!("Resolving identifier {:?} in symbol table.", &id.value))?;

                match symbol.scope {
                    Scope::GlobalScope => {
                        self.emit(Opcode::OpGetGlobal, vec![symbol.index as u16])
                            .context("Emitting OpGetGlobal to load identifier.")?;
                    }
                    Scope::BuiltinScope => {
                        self.emit(Opcode::OpGetBuiltin, vec![symbol.index as u16])
                            .context("Emitting OpGetBuiltin to load identifier.")?;
                    }
                    Scope::LocalScope => {
                        self.emit(Opcode::OpGetLocal, vec![symbol.index as u16])
                            .context("Emitting OpGetLocal to load identifier.")?;
                    }
                }
            }
            Expression::STRING_LITERAL(_, s) => {
                let string = Object::String(s.clone());
                let const_pos = self.add_constant(string) as u16;
                self.emit(Opcode::OpConstant, vec![const_pos])
                    .context("Emitting code for string literal.")?;
            }
            Expression::ARRAY_LITERAL(_, elements) => {
                for elem in elements {
                    self.compile_expression(elem)
                        .context("Compiling array element.")?;
                }
                self.emit(Opcode::OpArray, vec![elements.len() as u16])
                    .context("Emmitting Array literal.")?;
            }
            Expression::HASH_LITERAL(_, keys, vals) => {
                for (k, v) in keys.iter().zip(vals) {
                    self.compile_expression(k)
                        .context("Compiling key expression of hash literal.")?;

                    self.compile_expression(v)
                        .context("Compiling value expression of hash literal.")?;
                }

                self.emit(Opcode::OpHash, vec![(keys.len() * 2) as u16])
                    .context("Emitting OpHash for hash literal.")?;
            }
            Expression::INDEX_EXPRESSION(_, left, index) => {
                self.compile_expression(left)
                    .context("Compiling left side of index expression.")?;

                self.compile_expression(index)
                    .context("Compiling index of index expression.")?;

                self.emit(Opcode::OpIndex, vec![])
                    .context("Emitting index operator.")?;
            }
            Expression::FUNCTION(_, params, body) => {
                self.enter_scope();

                for param in params {
                    self.symbol_table.lock().expect("Accessing symbol table.").define(&param.value);
                }

                self.compile_block(&body.statements)
                    .context("Compiling function body.")?;

                if self.last_instruction_is(Opcode::OpPop) {
                    self.replace_last_pop_with_return()
                        .context("Replacing last OpPop with OpReturnValue.")?;
                }

                if !self.last_instruction_is(Opcode::OpReturnValue) {
                    self.emit(Opcode::OpReturn, vec![]).context("Emitting return without value.")?;
                }

                let num_locals = self.symbol_table
                    .lock().expect("Could not access symbol table!")
                    .num_definitions;

                let instructions = self.leave_scope()
                    .context("Leaving function scope.")?;

                let compiled_fun = Object::CompiledFunction(instructions, num_locals, params.len());
                let const_idx = self.add_constant(compiled_fun) as u16;
                self.emit(Opcode::OpConstant, vec![const_idx])
                    .context("Emitting compiled function constant.")?;
            }
            Expression::CALL(_, func, args) => {
                self.compile_expression(func)
                    .context("Compiling function to call.")?;

                for (i, arg) in args.iter().enumerate() {
                    self.compile_expression(arg).with_context(|| format!("Compiling call argument no. {}.", i))?;
                }

                self.emit(Opcode::OpCall, vec![args.len() as u16])
                    .context("Emitting function call.")?;
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
        let pos_new_instruction = self.current_instructions().len();

        self.current_instructions().0.extend_from_slice(&ins);

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
        self.scopes[self.scope_index].previous_instruction = self.scopes[self.scope_index].last_instruction;
        self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction {
            opcode,
            position,
        });
    }

    fn last_instruction_is(&self, opcode: Opcode) -> bool {
        if let Some(ins) = self.scopes[self.scope_index].last_instruction {
            return ins.opcode == opcode;
        }
        false
    }

    fn remove_last_pop(&mut self) {
        let new_len = if let Some(ins) = self.scopes[self.scope_index].last_instruction {
            ins.position
        } else { 0 };

        self.current_instructions().0.resize(new_len, 0);

        self.scopes[self.scope_index].last_instruction = self.scopes[self.scope_index].previous_instruction;
        self.scopes[self.scope_index].previous_instruction = None;
    }

    fn replace_instruction(&mut self, pos: usize, instructions: Instructions) {
        for (i, repl) in instructions.0.iter().enumerate() {
            self.current_instructions().0[pos + i] = *repl;
        }
    }

    fn replace_last_pop_with_return(&mut self) -> Result<()> {
        let last_pos = self.scopes[self.scope_index].last_instruction
            .context("Accessing last instruction in current scope.")?.position;

        self.replace_instruction(last_pos, make(Opcode::OpReturnValue, vec![])
            .context("Building return instruction.")?);

        self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction {
            opcode: Opcode::OpReturnValue,
            position: last_pos,
        });

        Ok(())
    }

    fn change_operand(&mut self, op_pos: usize, operands: Vec<u16>) -> Result<()> {
        let op = Opcode::try_from(*self.current_instructions().0.get(op_pos)
            .context("Retrieving opcode to change operand of.")?)
            .context("Converting given instruction byte into opcode.")?;

        let instruction = make(op, operands)
            .context("Constructing new instruction with replaced operands.")?;

        self.replace_instruction(op_pos, instruction);

        Ok(())
    }

    pub(crate) fn bytecode(&mut self) -> Result<Bytecode> {
        if self.constants.lock()
            .expect("Failed to access constants.")
            .len() == 0 && self.current_instructions().len() == 0 {
            bail!("Compiler did not produce any bytecode!");
        }
        Ok(Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.lock()
                .expect("Failed to access constants.")
                .clone(),
        })
    }

    fn current_instructions(&mut self) -> &mut Instructions {
        &mut self.scopes[self.scope_index].instructions
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::new());
        self.scope_index += 1;

        self.symbol_table = Rc::new(Mutex::new(SymbolTable::new_enclosed(self.symbol_table.clone())));
    }

    fn leave_scope(&mut self) -> Result<Instructions> {
        let instructions = match self.scopes.pop() {
            Some(CompilationScope { instructions: i, previous_instruction: _, last_instruction: _ }) => { i }
            _ => { bail!("No scope to leave!"); }
        };

        self.scope_index -= 1;

        let table = if let Some(outer) = &self.symbol_table
            .lock()
            .expect("Could not access outer symbol table")
            .outer {
            outer.clone()
        } else {
            bail!("No outer symbol table! Cannot leave scope!");
        };

        self.symbol_table = table;

        Ok(instructions)
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

#[derive(Clone, Debug)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    fn new() -> CompilationScope {
        CompilationScope {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }
}
