use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum Scope {
    GlobalScope,
    LocalScope,
    BuiltinScope,
    FreeScope,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    pub(crate) scope: Scope,
    pub(crate) index: usize,
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

#[derive(Debug, Clone)]
pub(crate) struct SymbolTable {
    store: HashMap<String, Symbol>,
    pub(crate) num_definitions: usize,
    pub(crate) outer: Option<Rc<Mutex<SymbolTable>>>,
    pub(crate) free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: None,
            free_symbols: vec![],
        }
    }

    pub fn new_enclosed(outer: Rc<Mutex<SymbolTable>>) -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
            outer: Some(outer),
            free_symbols: vec![],
        }
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let scope = if self.outer.is_none() { Scope::GlobalScope } else { Scope::LocalScope };

        let symbol = Symbol::new(name, scope, self.num_definitions);
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn define_free(&mut self, original: &Symbol) -> Symbol {
        self.free_symbols.push(original.clone());
        let symbol = Symbol::new(&original.name, Scope::FreeScope, self.free_symbols.len() - 1);

        self.store.insert(original.name.clone(), symbol.clone());
        symbol
    }

    pub(crate) fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let sym = Symbol::new(name, Scope::BuiltinScope, index);

        self.store.insert(name.to_string(), sym.clone());

        sym
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        if let Some(s) = self.store.get(name) {
            Some(s.clone())
        } else if let Some(o) = &self.outer {
            let res = o.lock().unwrap().resolve(name);
            if let Some(sym) = res {
                match sym.scope {
                    Scope::GlobalScope | Scope::BuiltinScope => Some(sym),
                    _ => {
                        let free = self.define_free(&sym);
                        Some(free)
                    }
                }
            } else { None }
        } else {
            None
        }
    }
}
