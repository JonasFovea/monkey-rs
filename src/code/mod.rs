use std::ops::Index;

use anyhow::{bail, Context, Result};

mod test;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Instructions(pub(crate) Vec<u8>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn from_vec(vec: Vec<u8>) -> Self {
        Instructions(vec)
    }

    pub fn join(instruction_sets: Vec<Instructions>) -> Self {
        let mut instruction_sets = instruction_sets;
        let mut ins = Vec::new();
        for _ in 0..instruction_sets.len() {
            let set = instruction_sets.remove(0);
            ins.extend(set.0);
        }
        Instructions(ins)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn to_string(&self) -> Result<String> {
        let mut out = String::new();

        let mut i = 0;
        while i < self.len() {
            let def = lookup(self.0[i]).context("Checking byte for OpCode.")?;

            let (operands, read) = read_operands(def.clone(), Instructions::from_vec(
                self.0[i + 1..].to_vec()
            ));

            out.push_str(&format!("{:04} {}\n", i, Instructions::fmt_instruction(&def, &operands)));

            i += 1 + read;
        }

        Ok(out)
    }

    pub(crate) fn fmt_instruction(definition: &Definition, operands: &Vec<u16>) -> String {
        let operand_count = definition.operand_widths.len();
        if operands.len() != operand_count {
            return format!("ERROR: operand len {} does not match defined {}\n", operands.len(), operand_count);
        }

        return
        match operand_count {
            0 => format!("{}", definition.name),
            1 => format!("{} {}", definition.name, operands[0]),
            _ => format!("ERROR: unhandled operator count for {}", definition.name),
        };
    }
}

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Opcode {
    OpConstant,
    OpAdd,
    OpPop,
    OpSub,
    OpDiv,
    OpMul,
    OpTrue,
    OpFalse,
    OpEqual,
    OpNotEqual,
    OpGreaterThan,
    OpGreaterEquals,
    OpMinus,
    OpBang,
    OpJumpNotTruthy,
    OpJump,
    OpNull,
    OpGetGlobal,
    OpSetGlobal,
}

impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match self {
            Opcode::OpConstant => 1,
            Opcode::OpAdd => 2,
            Opcode::OpPop => 3,
            Opcode::OpSub => 4,
            Opcode::OpDiv => 5,
            Opcode::OpMul => 6,
            Opcode::OpTrue => 7,
            Opcode::OpFalse => 8,
            Opcode::OpEqual => 9,
            Opcode::OpNotEqual => 10,
            Opcode::OpGreaterThan => 11,
            Opcode:: OpGreaterEquals => 12,
            Opcode::OpMinus => 13,
            Opcode::OpBang => 14,
            Opcode::OpJumpNotTruthy => 15,
            Opcode::OpJump => 16,
            Opcode::OpNull => 0,
            Opcode::OpGetGlobal => 17,
            Opcode::OpSetGlobal => 18,
        }
    }
}

impl Into<Definition> for Opcode {
    fn into(self) -> Definition {
        match self {
            Opcode::OpConstant => Definition {
                name: "OpConstant".to_string(),
                operand_widths: vec![2],
            },
            Opcode::OpAdd => Definition {
                name: "OpAdd".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpPop => Definition {
                name: "OpPop".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpSub => Definition {
                name: "OpSub".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpDiv => Definition {
                name: "OpDiv".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpMul => Definition {
                name: "OpMul".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpTrue => Definition {
                name: "OpTrue".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpFalse => Definition {
                name: "OpFalse".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpEqual => Definition{
                name: "OpEqual".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpNotEqual => Definition{
                name: "OpNotEqual".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpGreaterThan => Definition{
                name: "OpGreaterThan".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpGreaterEquals => Definition{
                name: "OpGreaterEquals".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpMinus => Definition{
                name: "OpMinus".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpBang => Definition{
                name: "OpBang".to_string(),
                operand_widths: vec![],
            },
            Opcode::OpJumpNotTruthy => Definition{
                name: "OpJumpNotTruthy".to_string(),
                operand_widths: vec![2]
            },
            Opcode::OpJump => Definition{
                name: "OpJump".to_string(),
                operand_widths: vec![2]
            },
            Opcode::OpNull => Definition{
                name: "OpNull".to_string(),
                operand_widths: vec![]
            },
            Opcode::OpGetGlobal => Definition{
                name: "OpGetGlobal".to_string(),
                operand_widths: vec![2]
            },
            Opcode::OpSetGlobal => Definition{
                name: "OpSetGlobal".to_string(),
                operand_widths: vec![2]
            }
        }
    }
}

impl TryFrom<u8> for Opcode {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> Result<Opcode, anyhow::Error> {
        match value {
            1 => Ok(Opcode::OpConstant),
            2 => Ok(Opcode::OpAdd),
            3 => Ok(Opcode::OpPop),
            4 => Ok(Opcode::OpSub),
            5 => Ok(Opcode::OpDiv),
            6 => Ok(Opcode::OpMul),
            7 => Ok(Opcode::OpTrue),
            8 => Ok(Opcode::OpFalse),
            9 => Ok(Opcode::OpEqual),
            10 => Ok(Opcode::OpNotEqual),
            11 => Ok(Opcode::OpGreaterThan),
            12 => Ok(Opcode::OpGreaterEquals),
            13 => Ok(Opcode::OpMinus),
            14 => Ok(Opcode::OpBang),
            15 => Ok(Opcode::OpJumpNotTruthy),
            16 => Ok(Opcode::OpJump),
            0 => Ok(Opcode::OpNull),
            17 => Ok(Opcode::OpGetGlobal),
            18 => Ok(Opcode::OpSetGlobal),
            _ => bail!("opcode {} undefined", value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Definition {
    name: String,
    operand_widths: Vec<usize>,
}

pub fn lookup(byte: u8) -> Result<Definition> {
    Ok(
        Opcode::try_from(byte)
            .context("Converting byte to opcode.")?
            .into()
    )
}

pub fn make(op: Opcode, operands: Vec<u16>) -> Result<Instructions> {
    let def: Definition = op.into();
    let mut instruction_len = 1;
    for w in &def.operand_widths {
        instruction_len += w;
    }

    let mut instruction: Vec<u8> = Vec::with_capacity(instruction_len);
    instruction.push(op.into());

    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => { instruction.extend_from_slice(&o.to_be_bytes()) }
            _ => bail!("Operand width not supported: {}", width),
        }
    }

    Ok(Instructions(instruction))
}

pub fn read_operands(definition: Definition, instructions: Instructions) -> (Vec<u16>, usize) {
    let mut operands = Vec::with_capacity(definition.operand_widths.len());
    let mut offset = 0;

    for (i, width) in definition.operand_widths.iter().enumerate() {
        match width {
            2 => {
                operands.insert(i, read_uint16(&instructions.0[offset..]));
            }
            _ => {}
        }
        offset += width;
    }

    (operands, offset)
}

pub(crate) fn read_uint16(bytes: &[u8]) -> u16 {
    u16::from_be_bytes([bytes[0], bytes[1]])
}