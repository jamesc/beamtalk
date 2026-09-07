// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Test suite for the compiler port (`beamtalk-compiler-port`'s ETF
//! request/response dispatch), including its ADR 0011 Phase 4
//! property-based tests.
//!
//! Tests are organized into feature-focused sub-modules:
//! - [`dispatch_and_hierarchy_meta`] — `handle_request` command-vocabulary
//!   dispatch, inline class definitions, REPL directive defaults, and
//!   class-hierarchy round-tripping through the ETF wire
//! - [`diagnostics_class_hierarchy`] — class-hierarchy / protocol-registry
//!   interplay in false-positive protocol-mismatch suppression
//! - [`type_aliases`] — `known_type_aliases` threading through
//!   `diagnostics/compile/compile_expression` and diagnostics-overrides
//!   loading
//! - [`completion_and_native_types`] — `resolve_completion_type` (BT-1068)
//!   and `load_native_type_registry_from`
//! - [`span_resolution`] — ADR 0082 Phase 1 (BT-2283) source-span commands
//! - [`class_module_index_and_categorization`] —
//!   `build_class_module_index_in_source` (BT-3441), `categorize_methods`,
//!   `class_state_field_defaults`
//! - [`module_naming_and_misc`] — `reindent_method_source` (BT-2584),
//!   `compile_expression_trace`, module-naming overrides, and standalone
//!   method-definition signature reporting
//! - [`compile_method`] — `compile_method` / method-mode diagnostics
//! - [`compile_roundtrip_proptest`] — `handle_compile()` /
//!   `handle_compile_expression()` never panic on near-valid input
//! - [`type_alias_declarations`] — ADR 0108 Phase 8 (BT-2902) `type Name =
//!   ...` REPL declarations
//! - [`type_string_wire_fidelity`] — BT-3100 type-annotation string
//!   fidelity across the compiler-port wire protocol

use super::*;
pub(crate) use proptest::prelude::*;

/// Fixture source for the ADR 0082 Phase 1 (BT-2283) span-resolution
/// commands, shared with the class-module-index and categorization tests
/// that exercise the same file.
pub(crate) const SPAN_FIXTURE: &str = "\
Object subclass: Counter

  increment => self.value := self.value + 1

  class new => self basicNew
";

/// Build an ETF Map for a `compile` request with the given source.
pub(crate) fn compile_request(source: &str) -> Map {
    Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(source)),
    ])
}

/// Build an ETF Map for a `compile_expression` request with the given source.
pub(crate) fn compile_expression_request(source: &str) -> Map {
    Map::from([
        (atom("command"), atom("compile_expression")),
        (atom("source"), binary(source)),
        (atom("module"), binary("bt@test_module")),
    ])
}

/// Extract the status atom from a response Term.
pub(crate) fn response_status(term: &Term) -> Option<String> {
    if let Term::Map(map) = term {
        map_get(map, "status").and_then(|t| {
            if let Term::Atom(a) = t {
                Some(a.name.clone())
            } else {
                None
            }
        })
    } else {
        None
    }
}

/// Extract the diagnostics list from a response Term.
pub(crate) fn response_diagnostics(term: &Term) -> Option<&List> {
    if let Term::Map(map) = term {
        if let Some(Term::List(list)) = map_get(map, "diagnostics") {
            return Some(list);
        }
    }
    None
}

/// Extract a string-valued (binary) field from a response Term.
pub(crate) fn response_field_str(term: &Term, key: &str) -> Option<String> {
    if let Term::Map(map) = term {
        map_get(map, key).and_then(term_to_string)
    } else {
        None
    }
}

/// Extract a list-of-binary-valued field from a response Term (e.g. `param_types`).
pub(crate) fn response_field_str_list(term: &Term, key: &str) -> Option<Vec<String>> {
    if let Term::Map(map) = term {
        if let Some(Term::List(list)) = map_get(map, key) {
            return Some(list.elements.iter().filter_map(term_to_string).collect());
        }
    }
    None
}

/// Default is 512 cases for standard CI; override via `PROPTEST_CASES` env var
/// for nightly extended runs (e.g., `PROPTEST_CASES=10000`).
pub(crate) fn proptest_config() -> ProptestConfig {
    let default = ProptestConfig::default();
    ProptestConfig {
        // Use at least 512 cases, but allow PROPTEST_CASES to increase beyond that
        cases: default.cases.max(512),
        ..default
    }
}

mod class_module_index_and_categorization;
mod compile_method;
mod compile_roundtrip_proptest;
mod completion_and_native_types;
mod diagnostics_class_hierarchy;
mod dispatch_and_hierarchy_meta;
mod module_naming_and_misc;
mod span_resolution;
mod type_alias_declarations;
mod type_aliases;
mod type_string_wire_fidelity;
