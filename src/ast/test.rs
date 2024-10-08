#[cfg(test)]
use std::ops::Deref;

use crate::ast::*;

#[test]
fn test_let_statement() {
    let tests = vec![("let x = 4;", "let x = 4"),
                     ("let y = true;", "let y = true"),
                     ("let foobar = y;", "let foobar = y")];
    for (input, expected) in tests {
        let program = Parser::new(Lexer::new(input).unwrap()).parse_program();
        assert!(program.is_ok());
        let program = program.unwrap();
        assert_eq!(program.statements.len(), 1);

        assert_eq!(format!("{}", &program.statements[0]), expected);
    }
}

#[test]
fn test_return_statement() {
    let input = "return 5;\
        return 10;\
        return 298374982;";
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    // println!("The Program: \n{}", program);
    assert_eq!(program.statements.len(), 3);

    for stmnt in program.statements {
        match stmnt {
            Statement::RETURN(rtsmt) => {
                assert_eq!(rtsmt.token.literal, "return");
            }
            _ => {
                eprintln!("Expected return statement, got {:?}", stmnt);
                assert!(false);
            }
        }
    }
}

#[test]
fn test_string() {
    let program = Program {
        statements: vec![
            Statement::LET(LetStatement {
                token: Token { token_type: TokenType::LET, literal: "let".to_string() },
                identifier: Identifier { token: Token { token_type: TokenType::IDENT, literal: "myVar".to_string() }, value: "myVar".to_string() },
                value: Expression::IDENT(Identifier { token: Token { token_type: TokenType::IDENT, literal: "anotherVar".to_string() }, value: "anotherVar".to_string() }),
            })
        ]
    };

    let program_str = format!("{}", &program);
    assert_eq!("let myVar = anotherVar", program_str);
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    // println!("Program: {}", &program);
    assert_eq!(program.statements.len(), 1);

    let stmnt = &program.statements[0];
    if let Statement::EXPRESSION(exp) = stmnt {
        if let Expression::IDENT(ident) = &exp.expression {
            assert_eq!(ident.value, "foobar");
        }
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    // println!("Program: {}", &program);
    assert_eq!(program.statements.len(), 1);

    let stmnt = &program.statements[0];
    if let Statement::EXPRESSION(exp) = stmnt {
        if let Expression::INT_LITERAL(_, val) = &exp.expression {
            assert_eq!(5, *val);
        }
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

    for (lit, op, int) in tests {
        let lex = Lexer::new(lit);
        let mut parser = Parser::new(lex.unwrap());
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();
        // println!("Program: {}", &program);
        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.get(0).unwrap();
        if let Statement::EXPRESSION(exp_stmnt) = statement {
            if let Expression::PREFIX(tok, exp) = &exp_stmnt.expression {
                assert_eq!(tok.literal, op);
                let exp = exp.deref();
                if let Expression::INT_LITERAL(_, i_val) = exp {
                    assert_eq!(*i_val, int);
                } else { assert!(false); }
            } else { assert!(false); }
        } else { assert!(false); }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let tests = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
        ("5 <= 5;", 5, "<=", 5),
        ("5 >= 5;", 5, ">=", 5),
    ];
    for (lit, int_0, op, int_1) in tests {
        let lex = Lexer::new(lit);
        let mut parser = Parser::new(lex.unwrap());
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();
        // println!("Program: {}", &program);
        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.get(0).unwrap();
        if let Statement::EXPRESSION(exp_stmnt) = statement {
            if let Expression::INFIX(left, op_tok, right) = &exp_stmnt.expression {
                if let Expression::INT_LITERAL(_, li) = left.as_ref() {
                    assert_eq!(*li, int_0);
                } else { assert!(false); }

                assert_eq!(op_tok.literal, op);

                if let Expression::INT_LITERAL(_, ri) = right.as_ref() {
                    assert_eq!(*ri, int_1);
                } else { assert!(false); }
            } else { assert!(false); }
        } else { assert!(false); }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b*c) +d", "((a + add((b * c))) + d)"),
        ("add(a,b,1,2*3,4+5,add(6,7*8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
        ("add(a+b+c*d/f+g)", "add((((a + b) + ((c * d) / f)) + g))"),
        ("a * [1,2,3,4][b*c]*d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
        ("add( a* b[2], b[1], 2 * [1,2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"),
    ];

    for (input, expected) in tests {
        expect_parse_result_str(input, expected);
    }
}

#[test]
fn test_bool_literals() {
    let tests = vec![
        ("true;", "true"),
        ("false;", "false"),
        ("let foobar = true;", "let foobar = true"),
        ("let barfoo = false;", "let barfoo = false"),
    ];
    for (input, expected) in tests {
        expect_parse_result_str(input, expected);
    }
}

#[allow(unused)]
fn expect_parse_result_str(input: &str, expected: &str) {
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();
    let actual = format!("{}", program);
    assert_eq!(&actual, expected);
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(expr_stmt) = &program.statements[0] {
        if let Expression::IF_EXPRESSION(_, cond, block, els_block) = &expr_stmt.expression {
            assert_eq!(format!("{}", cond), "(x < y)");
            assert_eq!(block.statements.len(), 1);
            if let Statement::EXPRESSION(block_expr_stmt) = &block.statements[0] {
                if let Expression::IDENT(ident) = &block_expr_stmt.expression {
                    assert_eq!(format!("{}", ident), "x");
                } else { assert!(false); }
            } else { assert!(false); }
            assert!(els_block.is_none());
        } else { assert!(false); }
    } else { assert!(false); }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let lex = Lexer::new(input);
    let mut parser = Parser::new(lex.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(expr_stmt) = &program.statements[0] {
        if let Expression::IF_EXPRESSION(_, cond, block, els_block) = &expr_stmt.expression {
            assert_eq!(format!("{}", cond), "(x < y)");
            assert_eq!(block.statements.len(), 1);
            if let Statement::EXPRESSION(block_expr_stmt) = &block.statements[0] {
                if let Expression::IDENT(ident) = &block_expr_stmt.expression {
                    assert_eq!(format!("{}", ident), "x");
                } else { assert!(false); }
            } else { assert!(false); }

            if let Some(block) = els_block {
                if let Statement::EXPRESSION(block_expr_stmt) = &block.statements[0] {
                    if let Expression::IDENT(ident) = &block_expr_stmt.expression {
                        assert_eq!(format!("{}", ident), "y");
                    } else { assert!(false); }
                } else { assert!(false); }
            } else { assert!(false); }
        } else { assert!(false); }
    } else { assert!(false); }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn (x, y) { x + y; }";
    let mut parser = Parser::new(Lexer::new(input).unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(expr_stmt) = &program.statements[0] {
        if let Expression::FUNCTION(_, params, body, _) = &expr_stmt.expression {
            assert_eq!(params.len(), 2);
            assert_eq!(format!("{}", params[0]), "x");
            assert_eq!(format!("{}", params[1]), "y");
            assert_eq!(body.statements.len(), 1);

            if let Statement::EXPRESSION(innr_expr) = &body.statements[0] {
                assert_eq!(format!("{}", innr_expr), "(x + y)");
            } else { assert!(false); }
        } else { assert!(false); }
    } else { assert!(false); }
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![("fn(){};", Vec::new()), ("fn(x){};", vec!["x"]), ("fn(x,y,z){}", vec!["x", "y", "z"])];

    for (input, expected) in tests {
        let mut parser = Parser::new(Lexer::new(input).unwrap());
        let program = parser.parse_program();

        assert!(program.is_ok());

        let program = program.unwrap();

        assert_eq!(program.statements.len(), 1);

        if let Statement::EXPRESSION(exp_stmnt) = &program.statements[0] {
            if let Expression::FUNCTION(_, params, _, _) = &exp_stmnt.expression {
                for (param, exp) in params.iter().zip(expected) {
                    assert_eq!(param.value, exp);
                }
            } else { assert!(false); }
        } else { assert!(false); }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let mut parser = Parser::new(Lexer::new(input).unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(exp_stmt) = &program.statements[0] {
        if let Expression::CALL(_, func, args) = &exp_stmt.expression {
            assert_eq!(format!("{}", func), "add");

            assert_eq!(args.len(), 3);

            assert_eq!(format!("{}", &args[0]), "1");
            assert_eq!(format!("{}", &args[1]), "(2 * 3)");
            assert_eq!(format!("{}", &args[2]), "(4 + 5)");
        }
    }
}

#[test]
fn test_string_literal_expression() {
    let input = "\"hello world\"";
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    let stmt = &program.statements[0];

    if let Statement::EXPRESSION(
        ExpressionStatement {
            token: _,
            expression: Expression::STRING_LITERAL(_, s)
        }) = stmt {
        assert_eq!(s, "hello world");
    } else { assert!(false) }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(ExpressionStatement { token: _, expression: Expression::ARRAY_LITERAL(_, expressions) }) = &program.statements[0] {
        assert_eq!(expressions.len(), 3);
        assert_eq!("1", format!("{}", &expressions[0]));
        assert_eq!("(2 * 2)", format!("{}", &expressions[1]));
        assert_eq!("(3 + 3)", format!("{}", &expressions[2]));
    } else { assert!(false) }
}

#[test]
fn test_parsing_index_expression() {
    let input = "myArray[1 + 1]";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer.unwrap());
    let program = parser.parse_program();

    assert!(program.is_ok());

    let program = program.unwrap();

    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(
        ExpressionStatement {
            token: _, expression:
        Expression::INDEX_EXPRESSION(_, left, idx)
        }) = &program.statements[0] {
        assert_eq!("myArray", format!("{}", left));
        assert_eq!("(1 + 1)", format!("{}", idx));
    }
}

#[test]
fn test_parsing_hash_literals() {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !program.is_ok() {
        eprintln!("{:?}", program);
        assert!(false);
    }

    let program = program.unwrap();
    assert_eq!(program.statements.len(), 1);

    let exp_keys = vec!["\"one\"", "\"two\"", "\"three\""];
    let exp_vals = vec![1, 2, 3];

    if let Statement::EXPRESSION(
        ExpressionStatement {
            token: _, expression:
        Expression::HASH_LITERAL(_, keys, vals)
        }) = &program.statements[0] {
        assert_eq!(keys.len(), 3);
        assert_eq!(vals.len(), 3);
        for (((k, v), ek), ev) in keys.iter().zip(vals).zip(exp_keys).zip(exp_vals) {
            assert_eq!(format!("{}", k), ek);
            assert_eq!(format!("{}", v), format!("{}", ev));
        }
    } else { assert!(false); }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";
    let lexer = Lexer::new(input).unwrap();
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !program.is_ok() {
        eprintln!("{:?}", program);
        assert!(false);
    }

    let program = program.unwrap();
    assert_eq!(program.statements.len(), 1);

    if let Statement::EXPRESSION(
        ExpressionStatement {
            token: _, expression:
        Expression::HASH_LITERAL(_, k, v)
        }) = &program.statements[0] {
        assert_eq!(k.len(), 0);
        assert_eq!(v.len(), 0);
    } else { assert!(false); }
}

#[test]
fn test_function_literal_with_name() {
    let input = "let myFunction = fn(){};";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);

    let program = p.parse_program().unwrap();

    assert_eq!(1, program.statements.len());

    let stmt = &program.statements[0];
    if let Statement::LET(lts) = stmt {
        if let Expression::FUNCTION(.., name) = &lts.value {
            assert_eq!("myFunction", name, "function literal name wrong. want 'myFunction' got={name:?}");
        } else { assert!(false, "Expression is not a Function!"); }
    } else { assert!(false, "Statement is not a LetStatement, got {:?} instead", stmt); }
}
