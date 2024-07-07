use std::collections::HashMap;
use crate::compiler::symbol_table::{Scope, Symbol, SymbolTable};

#[test]
fn test_define(){
    let expected: HashMap<String, Symbol> = HashMap::from([
        ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
        ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
    ]);
    
    let mut global = SymbolTable::new();
    
    let a = global.define("a");
    assert_eq!(expected["a"], a, "expected={:?}, got={:?}", expected["a"], a);
    
    let b = global.define("b");
    assert_eq!(expected["b"], b, "expected={:?}, got={:?}", expected["b"], b);
}

#[test]
fn test_resolve_global(){
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let expected: HashMap<String, Symbol> = HashMap::from([
        ("a".to_string(), Symbol::new("a", Scope::GlobalScope, 0)),
        ("b".to_string(), Symbol::new("b", Scope::GlobalScope, 1)),
    ]);
    
    for (_, sym) in expected{
        if let Some(s) = global.resolve(&sym.name) {
            assert_eq!(sym, s, "expected {} to resolve to {:?}, got {:?}", sym.name, sym, s);
        }else { assert!(false, "Unable to resolve symbol: {:?}", sym); }
    }
    
}
