use anyhow::Context;

use crate::code::{make, Instructions, Opcode};

#[test]
fn test_make() {
    let tests = vec![
        (Opcode::OpConstant, vec![65534], vec![Opcode::OpConstant.into(), 255, 254]),
        (Opcode::OpAdd, vec![], vec![Opcode::OpAdd.into()]),
        (Opcode::OpGetLocal, vec![255], vec![Opcode::OpGetLocal.into(), 255])
    ];

    for (op, operands, expected) in tests {
        let instruction = make(op, operands)
            .context("Building instruction from bytes.");
        if instruction.is_err() {
            assert!(false, "Instruction could not be built: {:?}", &instruction);
        }
        let instruction = instruction.unwrap();

        assert_eq!(instruction.0.len(), expected.len(),
                   "Wrong length of produced instruction!");
        for ((i, ins), exp) in instruction.0.iter().enumerate().zip(expected) {
            assert_eq!(*ins, exp,
                       "Comparing expected and produced instruction byte no {} does not match!", i);
        }
    }
}


#[test]
fn test_instructions_string() {
    let instructions = Instructions::join(vec![
        make(Opcode::OpAdd, vec![]).unwrap(),
        make(Opcode::OpGetLocal, vec![1]).unwrap(),
        make(Opcode::OpConstant, vec![2]).unwrap(),
        make(Opcode::OpConstant, vec![65535]).unwrap(),
    ]);

    let expected = "0000 OpAdd\n0001 OpGetLocal 1\n0003 OpConstant 2\n0006 OpConstant 65535\n";
    assert_eq!(expected, instructions.to_string().unwrap(), "Instructions wrongly formatted.\nwant={:?}\n got={:?}",
               expected, instructions.to_string().unwrap());
}