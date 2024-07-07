use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum Scope {
    GlobalScope
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    scope: Scope,
    index: usize,
}

impl Symbol {
    pub fn new(name: &str, scope: Scope, index: usize) -> Self {
        Symbol {
            name: name.to_string(),
            scope,
            index,
        }
    }
}

#[derive(Debug)]
pub(crate) struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol::new(name, Scope::GlobalScope, self.num_definitions);
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.store.get(name) {
            Some(s.clone())
        } else {
            None
        }
    }
}
