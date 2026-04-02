// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native type registry for Erlang FFI type information (ADR 0075 Phase 0 spike).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Stores type information extracted from `.beam` abstract code by
//! `beamtalk_spec_reader.erl`. Provides typed signatures for Erlang
//! functions, enabling the LSP to show type info in completions and
//! the type checker to infer return types from FFI calls.
//!
//! This is a prototype — the full implementation will cache per-module
//! and invalidate based on `.beam` file timestamps.

use std::collections::HashMap;

/// A single parameter in a native Erlang function signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeParam {
    /// Beamtalk keyword name (lowercased from spec variable name).
    pub name: String,
    /// Beamtalk type name (e.g., "Integer", "List", "Dynamic").
    pub type_name: String,
}

/// Type signature for a single Erlang function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeFunctionType {
    /// Erlang function name.
    pub name: String,
    /// Erlang arity.
    pub arity: u8,
    /// Parameter types and names.
    pub params: Vec<NativeParam>,
    /// Beamtalk return type name.
    pub return_type: String,
}

impl NativeFunctionType {
    /// Format as a Beamtalk-style type signature for display.
    ///
    /// Example: `reverse: list :: List -> List`
    #[must_use]
    pub fn display_signature(&self) -> String {
        if self.params.is_empty() {
            return format!("{} -> {}", self.name, self.return_type);
        }

        let mut parts = Vec::new();
        for (i, param) in self.params.iter().enumerate() {
            let keyword = if i == 0 {
                format!("{}:", self.name)
            } else {
                format!("{}:", param.name)
            };
            parts.push(format!("{} {} :: {}", keyword, param.name, param.type_name));
        }

        format!("{} -> {}", parts.join(" "), self.return_type)
    }
}

/// Registry of type information for native (Erlang) modules.
///
/// Keyed by module name → function name/arity → type signature.
#[derive(Debug, Clone, Default)]
pub struct NativeTypeRegistry {
    /// Module name → list of function type signatures.
    modules: HashMap<String, Vec<NativeFunctionType>>,
}

impl NativeTypeRegistry {
    /// Creates an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Registers type information for an Erlang module.
    pub fn register_module(&mut self, module_name: &str, functions: Vec<NativeFunctionType>) {
        self.modules.insert(module_name.to_string(), functions);
    }

    /// Looks up the type signature for a specific function.
    #[must_use]
    pub fn lookup(&self, module: &str, function: &str, arity: u8) -> Option<&NativeFunctionType> {
        self.modules
            .get(module)
            .and_then(|fns| fns.iter().find(|f| f.name == function && f.arity == arity))
    }

    /// Returns all function signatures for a module.
    #[must_use]
    pub fn module_functions(&self, module: &str) -> Option<&[NativeFunctionType]> {
        self.modules.get(module).map(Vec::as_slice)
    }

    /// Returns `true` if the registry contains type info for the given module.
    #[must_use]
    pub fn has_module(&self, module: &str) -> bool {
        self.modules.contains_key(module)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_registry_returns_none() {
        let reg = NativeTypeRegistry::new();
        assert!(reg.lookup("lists", "reverse", 1).is_none());
        assert!(!reg.has_module("lists"));
    }

    #[test]
    fn register_and_lookup() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![NativeFunctionType {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![NativeParam {
                    name: "list".to_string(),
                    type_name: "List".to_string(),
                }],
                return_type: "List".to_string(),
            }],
        );

        assert!(reg.has_module("lists"));
        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.return_type, "List");
        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0].name, "list");
    }

    #[test]
    fn display_signature_unary() {
        let sig = NativeFunctionType {
            name: "reverse".to_string(),
            arity: 1,
            params: vec![NativeParam {
                name: "list".to_string(),
                type_name: "List".to_string(),
            }],
            return_type: "List".to_string(),
        };
        assert_eq!(sig.display_signature(), "reverse: list :: List -> List");
    }

    #[test]
    fn display_signature_binary() {
        let sig = NativeFunctionType {
            name: "seq".to_string(),
            arity: 2,
            params: vec![
                NativeParam {
                    name: "from".to_string(),
                    type_name: "Integer".to_string(),
                },
                NativeParam {
                    name: "to".to_string(),
                    type_name: "Integer".to_string(),
                },
            ],
            return_type: "List".to_string(),
        };
        assert_eq!(
            sig.display_signature(),
            "seq: from :: Integer to: to :: Integer -> List"
        );
    }

    #[test]
    fn display_signature_nullary() {
        let sig = NativeFunctionType {
            name: "node".to_string(),
            arity: 0,
            params: vec![],
            return_type: "Symbol".to_string(),
        };
        assert_eq!(sig.display_signature(), "node -> Symbol");
    }

    #[test]
    fn lookup_wrong_arity_returns_none() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![NativeFunctionType {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![NativeParam {
                    name: "list".to_string(),
                    type_name: "List".to_string(),
                }],
                return_type: "List".to_string(),
            }],
        );
        assert!(reg.lookup("lists", "reverse", 2).is_none());
    }

    #[test]
    fn module_functions_returns_all() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![
                NativeFunctionType {
                    name: "reverse".to_string(),
                    arity: 1,
                    params: vec![NativeParam {
                        name: "list".to_string(),
                        type_name: "List".to_string(),
                    }],
                    return_type: "List".to_string(),
                },
                NativeFunctionType {
                    name: "sort".to_string(),
                    arity: 1,
                    params: vec![NativeParam {
                        name: "list".to_string(),
                        type_name: "List".to_string(),
                    }],
                    return_type: "List".to_string(),
                },
            ],
        );

        let fns = reg.module_functions("lists").unwrap();
        assert_eq!(fns.len(), 2);
    }
}
