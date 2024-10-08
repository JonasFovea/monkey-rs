use std::fmt;
use std::fmt::Formatter;
use std::ops::Deref;

use crate::ast::{BlockStatement, Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement, Statement};

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IDENT(id) => write!(f, "{}", id),
            Expression::PREFIX(a, b) => write!(f, "({}{})", a, b.deref()),
            Expression::INFIX(a, b, c) => write!(f, "({} {} {})", a, b, c),
            Expression::INT_LITERAL(_, b) => write!(f, "{}", b),
            Expression::BOOL_LITERAL(_, val) => write!(f, "{}", val),
            Expression::IF_EXPRESSION(_, cond, block, els_block) => {
                let mut s = String::new();
                s.push_str("if ");
                s.push_str(&format!("{}", cond));
                s.push_str(" ");
                s.push_str(&format!("{}", block));
                if let Some(els) = els_block {
                    s.push_str(&format!("{}", els));
                }
                write!(f, "{}", s)
            }
            Expression::FUNCTION(_, parameters, body, name) => {
                let param_strings: Vec<String> = parameters.iter().map(|ident| format!("{}", ident)).collect();
                let param_string = param_strings.join(", ");

                write!(f, "fn {name} params : ({}) body: {{ {} }}", param_string, body)
            }
            Expression::CALL(_, function, arguments) => {
                let argument_strings: Vec<String> = arguments.iter().map(|a| format!("{}", a)).collect();
                let arguments = argument_strings.join(", ");
                write!(f, "{}({})", function, arguments)
            }
            Expression::STRING_LITERAL(_, s) => { write!(f, "\"{}\"", s) }
            Expression::None => write!(f, "{{NONE Expression}}"),
            Expression::ARRAY_LITERAL(_, elems) => {
                let concated = elems.iter().map(|e| format!("{}", e))
                    .collect::<Vec<String>>().join(", ");
                write!(f, "[{}]", concated)
            }
            Expression::INDEX_EXPRESSION(_, left, idx) => { write!(f, "({}[{}])", left, idx) }
            Expression::HASH_LITERAL(_, keys, values) => {
                let out = keys.iter().zip(values)
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{}}}", out)
            }
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        for i in 0..self.statements.len() {
            out.push_str(format!("{}", &self.statements[i]).as_str());
            // if i != self.statements.len() - 1 { out.push_str("\n") };
        }
        write!(f, "{}", out)
    }
}

impl fmt::Display for Statement {
    #[allow(unreachable_patterns)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();

        match self {
            Statement::LET(let_stmnt) => {
                out.push_str(format!("let {} = {}", let_stmnt.identifier, let_stmnt.value).as_str());
            }
            Statement::RETURN(return_statement) => {
                out.push_str(format!("return {};", return_statement.return_value).as_str());
            }
            Statement::EXPRESSION(exp_statement) => {
                out.push_str(format!("{}", exp_statement.expression).as_str());
            }
            _ => { out.push_str("Unknown Statement"); }
        }
        write!(f, "{}", out)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = String::new();
        s.push_str("{\n");
        for stmt in &self.statements {
            s.push_str(&format!("\t{}", stmt));
        }
        s.push_str("\n}");
        write!(f, "{}", s)
    }
}

pub trait TokenLiteral {
    fn token_literal(&self) -> String;
}

impl TokenLiteral for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::IDENT(i) => i.value.clone(),
            Expression::INFIX(first, op, second) => format!("({} {} {})", first, op, second),
            Expression::PREFIX(op, exp) => format!("({}{})", op, exp),
            Expression::INT_LITERAL(_, val) => format!("{}", val),
            _ => "?".to_string()
        }
    }
}

impl TokenLiteral for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl TokenLiteral for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl TokenLiteral for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}

impl TokenLiteral for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }
}
