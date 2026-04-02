// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Native type registry for Erlang FFI type information (ADR 0075).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Stores type information extracted from `.beam` abstract code by
//! `beamtalk_spec_reader.erl`. Provides typed signatures for Erlang
//! functions, enabling the type checker to infer return types from FFI
//! calls and the LSP to show type info in completions/hover.
//!
//! ## Registry Structure
//!
//! ```text
//! module → function_name → arity → FunctionSignature
//! ```
//!
//! ## Resolution Order (ADR 0075)
//!
//! 1. Project-local stubs/ (user overrides)
//! 2. Package-bundled stubs/ (library author)
//! 3. Distribution stubs/ (shipped with compiler)
//! 4. Auto-extracted (.beam `abstract_code`)
//! 5. Dynamic (no type info)
//!
//! This registry handles layers 4 and 5. Stub layers (1–3) will be
//! added in Phase 2 (BT-1847).

use super::types::{InferredType, TypeProvenance};
use ecow::EcoString;
use std::collections::HashMap;

/// Type signature for a single Erlang function.
///
/// Stores parameter types and names alongside the return type,
/// with provenance tracking for diagnostic messages.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    /// Erlang function name.
    pub name: String,
    /// Erlang arity.
    pub arity: u8,
    /// Parameter types with optional keyword names.
    pub params: Vec<ParamType>,
    /// Beamtalk return type.
    pub return_type: InferredType,
    /// Where this signature came from (Extracted for .beam, Declared for stubs).
    pub provenance: TypeProvenance,
}

impl FunctionSignature {
    /// Format as a Beamtalk-style type signature for display.
    ///
    /// Uses keyword names when available, falling back to the param type name.
    ///
    /// ## Examples
    ///
    /// - `reverse: list :: List -> List`
    /// - `seq: from :: Integer to: to :: Integer -> List`
    /// - `node -> Symbol` (nullary)
    #[must_use]
    pub fn display_signature(&self) -> String {
        if self.params.is_empty() {
            let ret_display = self
                .return_type
                .display_name()
                .unwrap_or_else(|| EcoString::from("Dynamic"));
            return format!("{} -> {ret_display}", self.name);
        }

        let mut parts = Vec::new();
        for (i, param) in self.params.iter().enumerate() {
            let keyword = if i == 0 {
                format!("{}:", self.name)
            } else {
                match &param.keyword {
                    Some(kw) => format!("{kw}:"),
                    None => "with:".to_string(),
                }
            };
            let param_name = param.keyword.as_deref().unwrap_or("arg");
            let type_display = param
                .type_
                .display_name()
                .unwrap_or_else(|| EcoString::from("Dynamic"));
            parts.push(format!("{keyword} {param_name} :: {type_display}"));
        }

        let ret_display = self
            .return_type
            .display_name()
            .unwrap_or_else(|| EcoString::from("Dynamic"));
        format!("{} -> {ret_display}", parts.join(" "))
    }
}

/// A single parameter in a native function signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamType {
    /// Beamtalk keyword name (lowercased from spec variable name).
    ///
    /// `None` when no meaningful name is available (positional fallback).
    pub keyword: Option<EcoString>,
    /// Beamtalk type for this parameter.
    pub type_: InferredType,
}

/// Registry of type information for native (Erlang) modules.
///
/// Keyed by module name → list of function type signatures. Provides
/// lookup by (module, function, arity) for the type checker and LSP.
#[derive(Debug, Clone, Default)]
pub struct NativeTypeRegistry {
    /// Module name → list of function type signatures.
    modules: HashMap<String, Vec<FunctionSignature>>,
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
    ///
    /// Replaces any existing signatures for this module.
    pub fn register_module(&mut self, module_name: &str, functions: Vec<FunctionSignature>) {
        self.modules.insert(module_name.to_string(), functions);
    }

    /// Looks up the type signature for a specific function.
    #[must_use]
    pub fn lookup(&self, module: &str, function: &str, arity: u8) -> Option<&FunctionSignature> {
        self.modules
            .get(module)
            .and_then(|fns| fns.iter().find(|f| f.name == function && f.arity == arity))
    }

    /// Returns all function signatures for a module.
    #[must_use]
    pub fn module_functions(&self, module: &str) -> Option<&[FunctionSignature]> {
        self.modules.get(module).map(Vec::as_slice)
    }

    /// Returns `true` if the registry contains type info for the given module.
    #[must_use]
    pub fn has_module(&self, module: &str) -> bool {
        self.modules.contains_key(module)
    }

    /// Returns the number of modules in the registry.
    #[must_use]
    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    /// Returns the total number of function signatures across all modules.
    #[must_use]
    pub fn function_count(&self) -> usize {
        self.modules.values().map(Vec::len).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extracted_sig(
        name: &str,
        arity: u8,
        params: Vec<ParamType>,
        return_type: InferredType,
    ) -> FunctionSignature {
        FunctionSignature {
            name: name.to_string(),
            arity,
            params,
            return_type,
            provenance: TypeProvenance::Extracted,
        }
    }

    fn param(keyword: &str, type_name: &str) -> ParamType {
        ParamType {
            keyword: Some(EcoString::from(keyword)),
            type_: InferredType::known(type_name),
        }
    }

    #[test]
    fn empty_registry_returns_none() {
        let reg = NativeTypeRegistry::new();
        assert!(reg.lookup("lists", "reverse", 1).is_none());
        assert!(!reg.has_module("lists"));
        assert_eq!(reg.module_count(), 0);
        assert_eq!(reg.function_count(), 0);
    }

    #[test]
    fn register_and_lookup() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        assert!(reg.has_module("lists"));
        assert_eq!(reg.module_count(), 1);
        assert_eq!(reg.function_count(), 1);

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.return_type, InferredType::known("List"));
        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0].keyword, Some(EcoString::from("list")));
    }

    #[test]
    fn display_signature_unary() {
        let sig = extracted_sig(
            "reverse",
            1,
            vec![param("list", "List")],
            InferredType::known("List"),
        );
        assert_eq!(sig.display_signature(), "reverse: list :: List -> List");
    }

    #[test]
    fn display_signature_binary() {
        let sig = extracted_sig(
            "seq",
            2,
            vec![param("from", "Integer"), param("to", "Integer")],
            InferredType::known("List"),
        );
        assert_eq!(
            sig.display_signature(),
            "seq: from :: Integer to: to :: Integer -> List"
        );
    }

    #[test]
    fn display_signature_nullary() {
        let sig = extracted_sig("node", 0, vec![], InferredType::known("Symbol"));
        assert_eq!(sig.display_signature(), "node -> Symbol");
    }

    #[test]
    fn display_signature_dynamic_return() {
        let sig = extracted_sig(
            "apply",
            1,
            vec![param("fun", "Block")],
            InferredType::Dynamic,
        );
        assert_eq!(sig.display_signature(), "apply: fun :: Block -> Dynamic");
    }

    #[test]
    fn display_signature_no_keyword() {
        let sig = extracted_sig(
            "foo",
            1,
            vec![ParamType {
                keyword: None,
                type_: InferredType::known("Integer"),
            }],
            InferredType::known("Integer"),
        );
        assert_eq!(sig.display_signature(), "foo: arg :: Integer -> Integer");
    }

    #[test]
    fn lookup_wrong_arity_returns_none() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        assert!(reg.lookup("lists", "reverse", 2).is_none());
    }

    #[test]
    fn module_functions_returns_all() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![
                extracted_sig(
                    "reverse",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
                extracted_sig(
                    "sort",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
            ],
        );

        let fns = reg.module_functions("lists").unwrap();
        assert_eq!(fns.len(), 2);
    }

    #[test]
    fn register_module_replaces_existing() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        assert_eq!(reg.function_count(), 1);

        // Re-register with different functions
        reg.register_module(
            "lists",
            vec![
                extracted_sig(
                    "sort",
                    1,
                    vec![param("list", "List")],
                    InferredType::known("List"),
                ),
                extracted_sig(
                    "nth",
                    2,
                    vec![param("n", "Integer"), param("list", "List")],
                    InferredType::known("Dynamic"),
                ),
            ],
        );
        assert_eq!(reg.function_count(), 2);
        assert!(reg.lookup("lists", "reverse", 1).is_none());
        assert!(reg.lookup("lists", "sort", 1).is_some());
    }

    #[test]
    fn multiple_modules() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );
        reg.register_module(
            "maps",
            vec![extracted_sig(
                "get",
                2,
                vec![param("key", "Dynamic"), param("map", "Dictionary")],
                InferredType::Dynamic,
            )],
        );

        assert_eq!(reg.module_count(), 2);
        assert_eq!(reg.function_count(), 2);
        assert!(reg.has_module("lists"));
        assert!(reg.has_module("maps"));
        assert!(!reg.has_module("string"));
    }

    #[test]
    fn provenance_is_extracted() {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "lists",
            vec![extracted_sig(
                "reverse",
                1,
                vec![param("list", "List")],
                InferredType::known("List"),
            )],
        );

        let sig = reg.lookup("lists", "reverse", 1).unwrap();
        assert_eq!(sig.provenance, TypeProvenance::Extracted);
    }
}
