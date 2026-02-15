use lyra_ast::ErasedAstId;

/// Unique key for a symbol in the semantic store.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// A resolved symbol entry.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: String,
    pub kind: SymbolKind,
    pub ast: ErasedAstId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Port,
    Wire,
    Reg,
    Parameter,
}

/// Frozen symbol table -- built once per analysis pass.
#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn push(&mut self, sym: Symbol) {
        self.symbols.push(sym);
    }

    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id.0 as usize)
    }

    pub fn iter(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.iter()
    }
}
