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
        (first_local.clone(), HashMap::from([
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

#[test]
fn test_resolve_free() {
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
        (first_local, vec![
            Symbol::new("a", Scope::GlobalScope, 0),
            Symbol::new("b", Scope::GlobalScope, 1),
            Symbol::new("c", Scope::LocalScope, 0),
            Symbol::new("d", Scope::LocalScope, 1),
        ], vec![]),
        (second_local, vec![
            Symbol::new("a", Scope::GlobalScope, 0),
            Symbol::new("b", Scope::GlobalScope, 1),
            Symbol::new("c", Scope::FreeScope, 0),
            Symbol::new("d", Scope::FreeScope, 1),
            Symbol::new("e", Scope::LocalScope, 0),
            Symbol::new("f", Scope::LocalScope, 1),
        ], vec![
            Symbol::new("c", Scope::LocalScope, 0),
            Symbol::new("d", Scope::LocalScope, 1),
        ]),
    ];

    for (table, expected_symbols, expected_free_symbols) in tests {
        for exp_sym in expected_symbols {
            if let Some(sym) = table.lock().unwrap().resolve(&exp_sym.name) {
                assert_eq!(exp_sym, sym, "Expected {:?} to resolve to {:?}, got={:?}", &exp_sym.name, exp_sym, sym);
            } else { assert!(false, "Symbol {:?} not resolvable.", &exp_sym.name); }
        }

        assert_eq!(expected_free_symbols.len(), table.lock().unwrap().free_symbols.len(),
                   "wrong number of free symbols. got={}, want={}",
                   table.lock().unwrap().free_symbols.len(),
                   expected_free_symbols.len());

        for (i, sym) in expected_free_symbols.iter().enumerate() {
            let result = &table.lock().unwrap().free_symbols[i];
            assert_eq!(*result, *sym, "wrong free symbol. got={:?}, want={:?}", *sym, *result);
        }
    }
}

#[test]
fn test_unresolvable_free() {
    let mut global = SymbolTable::new();
    global.define("a");
    let global = Rc::new(Mutex::new(global));

    let mut first_local = SymbolTable::new_enclosed(global.clone());
    first_local.define("c");
    let first_local = Rc::new(Mutex::new(first_local));

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());
    second_local.define("e");
    second_local.define("f");
    let second_local = Rc::new(Mutex::new(second_local));

    let expected = vec![
        Symbol::new("a", Scope::GlobalScope, 0),
        Symbol::new("c", Scope::FreeScope, 0),
        Symbol::new("e", Scope::LocalScope, 0),
        Symbol::new("f", Scope::LocalScope, 1),
    ];

    for exp_sym in expected {
        if let Some(sym) = second_local.lock().unwrap().resolve(&exp_sym.name) {
            assert_eq!(exp_sym, sym, "expected {:?} to resolve to {:?}, got={:?}", exp_sym.name, exp_sym, sym);
        } else { assert!(false, "{:?} not resolvable", exp_sym.name) }
    }

    let expected_unresolvable = vec!["b", "d"];

    for name in expected_unresolvable {
        if let Some(_) = second_local.lock().unwrap().resolve(name) {
            assert!(false, "name {name} resolved, but was expected not to");
        }
    }
}

#[test]
fn test_define_and_resolve_function_name() {
    let mut global = SymbolTable::new();
    global.define_function_name("a");

    let expected = Symbol {
        name: "a".to_string(),
        scope: Scope::FunctionScope,
        index: 0,
    };

    if let Some(res) = global.resolve(&expected.name) {
        assert_eq!(expected, res);
    } else { assert!(false); }
}

#[test]
fn test_shadowing_function_name() {
    let mut global = SymbolTable::new();
    global.define_function_name("a");
    global.define("a");

    let expected = Symbol {
        name: "a".to_string(),
        scope: Scope::GlobalScope,
        index: 0,
    };

    if let Some(res) = global.resolve(&expected.name) {
        assert_eq!(expected, res);
    } else { assert!(false); }
}
