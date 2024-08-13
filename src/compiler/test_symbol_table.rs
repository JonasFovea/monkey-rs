use crate::compiler::symbol_table::{Scope, Symbol, SymbolTable};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;

#[test]
fn test_define() {
    let expected: HashMap<String, Symbol> = HashMap::from([
        ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
        ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
        ("c".to_string(), Symbol::new("c", Scope::LocalScope, 0)),
        ("d".to_string(), Symbol::new("d", Scope::LocalScope, 1)),
        ("e".to_string(), Symbol::new("e", Scope::LocalScope, 0)),
        ("f".to_string(), Symbol::new("f", Scope::LocalScope, 1)),
    ]);

    let mut global = SymbolTable::new();

    let a = global.define("a");
    assert_eq!(expected["a"], a, "expected={:?}, got={:?}", expected["a"], a);

    let b = global.define("b");
    assert_eq!(expected["b"], b, "expected={:?}, got={:?}", expected["b"], b);

    let global = Rc::new(Mutex::new(global));

    let mut first_local = SymbolTable::new_enclosed(global.clone());

    let c = first_local.define("c");
    assert_eq!(expected["c"], c, "expected={:?}, got={:?}", expected["c"], c);

    let d = first_local.define("d");
    assert_eq!(expected["d"], d, "expected={:?}, got={:?}", expected["d"], d);

    let first_local = Rc::new(Mutex::new(first_local));

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());

    let e = second_local.define("e");
    assert_eq!(expected["e"], e, "expected={:?}, got={:?}", expected["e"], e);

    let f = second_local.define("f");
    assert_eq!(expected["f"], f, "expected={:?}, got={:?}", expected["f"], f);
}

#[test]
fn test_resolve_global() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let expected: HashMap<String, Symbol> = HashMap::from([
        ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
        ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
    ]);

    for (_, sym) in expected {
        if let Some(s) = global.resolve(&sym.name) {
            assert_eq!(sym, s, "expected {} to resolve to {:?}, got {:?}", sym.name, sym, s);
        } else { assert!(false, "Unable to resolve symbol: {:?}", sym); }
    }
}

#[test]
fn test_resolve_local() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let global = Rc::new(Mutex::new(global));

    let mut local = SymbolTable::new_enclosed(global.clone());
    local.define("c");
    local.define("d");

    let expected: HashMap<String, Symbol> = HashMap::from([
        ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
        ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
        ("c".to_string(), Symbol::new("c", Scope::LocalScope, 0)),
        ("d".to_string(), Symbol::new("d", Scope::LocalScope, 1)),
    ]);

    for (_, sym) in expected {
        if let Some(s) = local.resolve(&sym.name) {
            assert_eq!(sym, s, "expected {} to resolve to {:?}, got {:?}", sym.name, sym, s);
        } else { assert!(false, "Unable to resolve symbol: {:?}", sym); }
    }
}

#[test]
fn test_resolve_nested_local() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let global = Rc::new(Mutex::new(global));

    let mut first_local = SymbolTable::new_enclosed(global.clone());
    first_local.define("c");
    first_local.define("d");

    let first_local = Rc::new(Mutex::new(first_local));

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());
    second_local.define("e");
    second_local.define("f");

    let second_local = Rc::new(Mutex::new(second_local));

    let tests = vec![
        (second_local.clone(), HashMap::from([
            ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
            ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
            ("c".to_string(), Symbol::new("c", Scope::LocalScope, 0)),
            ("d".to_string(), Symbol::new("d", Scope::LocalScope, 1)),
        ])),
        (second_local.clone(), HashMap::from([
            ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
            ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
            ("c".to_string(), Symbol::new("e", Scope::LocalScope, 0)),
            ("d".to_string(), Symbol::new("f", Scope::LocalScope, 1)),
        ]))
    ];

    for (table, expected) in tests {
        for (_, sym) in expected {
            if let Some(s) = table.lock().unwrap().resolve(&sym.name) {
                assert_eq!(sym, s, "expected {} to resolve to {:?}, got {:?}", sym.name, sym, s);
            } else { assert!(false, "Unable to resolve symbol: {:?}", sym); }
        }
    }
}
