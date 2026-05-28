// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! **THROWAWAY** — ADR 0088 Phase 0c audit only.  Delete with the memo
//! (BT-2316) once the Phase 0 decision is recorded.
//!
//! This file is the experimental rig for the ADR 0088 Phase 0c
//! typed-Document-leaves shrinkage comparison (BT-2316 / epic BT-2313).
//! It mirrors the structure of [`super::cerl_audit`] but measures a
//! different alternative — the **typed-Document-leaves** refactor that
//! Phase 0a's memo flagged as the strongest counter-argument to a full
//! cerl-direct migration.
//!
//! # The thesis
//!
//! The original `Document::String(arbitrary_string)` leaf is the BT-875
//! recurrence vector: any author can stuff an unquoted atom, a misformatted
//! variable name, or a hand-rendered `format!()` fragment into a leaf and
//! the compiler will accept it.  Typed-Document-leaves replaces the open
//! `String` leaf with a tiny "what kind of leaf is this?" API — `atom()`
//! and `var()` — that does the punctuation for you.
//!
//! It captures most of cerl-direct's atom-quote-ceremony savings without
//! changing the wire format and without building a full Core Erlang AST.
//! Phase 0c measures how much of that savings the typed-leaves alternative
//! actually delivers, so the Phase 0 verdict can choose between the two
//! refactors on data rather than rhetoric.
//!
//! # What this file contains
//!
//! 1. A **minimal** `leaf::*` API — two helpers (`atom`, `var`) that
//!    return `Document<'static>` with the appropriate punctuation.  Not a
//!    full Phase-1 typed-leaves mirror; just the leaves the three audit
//!    subjects exercise.  In a production typed-leaves refactor these
//!    would replace `Document::String(...)` at call sites across the
//!    codegen; here they sit alongside it.
//!
//! 2. Three rewrites of the same functions as [`super::cerl_audit`]:
//!    - leaf utility — `beamtalk_class_attribute_typed_leaves`
//!    - medium expression — `logger_log_call_typed_leaves`
//!    - high-complexity construct — `generate_pack_prefix_typed_leaves`
//!
//! 3. Text-equivalence tests: each rewrite renders byte-for-byte identical
//!    to the existing `*_original` functions in [`super::cerl_audit`] —
//!    same fixtures, same `to_pretty_string()` comparison, same sanity
//!    substring assertions.
//!
//! 4. The originals are NOT modified.  They continue to drive production
//!    codegen.  This file reuses the `*_original` functions and the
//!    `PackPrefixInputs` struct from [`super::cerl_audit`] so the three
//!    audits compare the same code against the same fixtures.
//!
//! # Methodology
//!
//! Same conventions as the BT-2314 memo:
//! - LOC counts the *body* of each function (signature + braces excluded).
//! - Branching uses cyclomatic count (1 + decision points).
//! - "Helper-call count" is the number of `docvec!` / `Document::*` calls
//!   in the original vs. typed-leaves-helper calls in the rewrite.
//! - Char count uses non-whitespace bytes (the more honest of the two
//!   metrics — LOC fluctuates with rustfmt collapse choices).

#![allow(dead_code)] // entire module is throwaway scaffolding

use super::cerl_audit::{
    PackPrefixInputs, beamtalk_class_attribute_original, generate_pack_prefix_original,
    logger_log_call_original,
};
use super::document::{Document, join};
use crate::docvec;

// ════════════════════════════════════════════════════════════════════════════
// Minimal leaf::* API
// ════════════════════════════════════════════════════════════════════════════
//
// What this audits: whether replacing `Document::String(arbitrary_string)`
// with a tiny typed-leaf API (`atom`, `var`) — leaving the rest of the
// `Document` pretty-printer untouched — captures the same atom-quote
// ceremony savings that cerl-direct does.
//
// What this is NOT: a full sum-type replacement of `Document::String`.
// In a real typed-leaves refactor, `Document::String` would be removed
// from the enum and `Document::Leaf(Leaf)` (or a set of leaf variants)
// would replace it.  For the audit, the helpers coexist with the open
// `Document::String` leaf so the rewrites can be measured in isolation.
//
// Design choice: helpers return `Document<'static>` directly rather than
// going through an intermediate AST.  This is the entire pitch — typed
// leaves stay inside the existing Document API.  No new render layer.
// ════════════════════════════════════════════════════════════════════════════

pub mod leaf {
    use super::{Document, docvec};

    /// `'atom_name'` — quoted Core Erlang atom.  The atom-quote punctuation
    /// is the entire BT-875 recurrence vector this audit closes.
    #[must_use]
    pub fn atom(name: impl Into<String>) -> Document<'static> {
        docvec!["'", Document::String(name.into()), "'"]
    }

    /// `VarName` — bare Core Erlang variable name.  Authors no longer
    /// reach for `Document::String(...)` directly; the typed `var()`
    /// call sites the intent.
    #[must_use]
    pub fn var(name: impl Into<String>) -> Document<'static> {
        Document::String(name.into())
    }
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #1 (leaf): util.rs::beamtalk_class_attribute
// ════════════════════════════════════════════════════════════════════════════
//
// Compare against:
// - cerl_audit::beamtalk_class_attribute_original (the production shape)
// - cerl_audit::beamtalk_class_attribute_cerl (the cerl-direct rewrite)
//
// Per-class entry in the original: 5 `Document::String`/`Document::Str`
// parts wrapping atom-quote punctuation.  Typed-leaves collapses each
// atom to a single `atom(...)` call:
//
//     // Original
//     docvec!["{'", Document::String(name), "', '", Document::String(superclass), "'}"]
//
//     // Typed-leaves
//     docvec!["{", atom(name), ", ", atom(superclass), "}"]
//
// The structural template (`{`, `, `, `}`) stays as inline strings — they
// are not atoms or variables, they're punctuation.
// ════════════════════════════════════════════════════════════════════════════

/// Audit version of `beamtalk_class_attribute` built on the typed-leaves API.
///
/// Same input shape as [`super::cerl_audit::beamtalk_class_attribute_original`].
#[must_use]
pub fn beamtalk_class_attribute_typed_leaves(classes: &[(String, String)]) -> Document<'static> {
    use leaf::*;
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|(name, superclass)| {
        docvec!["{", atom(name.clone()), ", ", atom(superclass.clone()), "}"]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]",
    ]
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #2 (medium): Logger metadata-map + log-call from intrinsics.rs
// ════════════════════════════════════════════════════════════════════════════
//
// The metadata-map is the worst case for the original: each dynamic atom
// value requires six `Document` parts (open quote, value, close quote,
// comma…).  Typed-leaves collapses each dynamic atom to a single `atom()`.
//
//     // Original
//     "'beamtalk_class' => '", Document::String(ctx_class), "', "
//
//     // Typed-leaves
//     "'beamtalk_class' => ", atom(ctx_class), ", "
//
// The keys (`'domain'`, `'beamtalk_class'`, `'beamtalk_selector'`) and
// the static `call 'logger':'log'(` template stay as inline string
// literals — these are constant atoms hard-coded by the template author,
// not dynamic values where the BT-875 hole exists.
// ════════════════════════════════════════════════════════════════════════════

/// Audit version of the metadata-map + log-call fragments on typed leaves.
#[must_use]
pub fn logger_log_call_typed_leaves(
    level: &str,
    msg_doc_text: &str,
    ctx_class: &str,
    ctx_selector: &str,
) -> Document<'static> {
    use leaf::*;
    let metadata_map_doc = docvec![
        "~{",
        "'domain' => ['beamtalk' | ['user']], ",
        "'beamtalk_class' => ",
        atom(ctx_class),
        ", ",
        "'beamtalk_selector' => ",
        atom(ctx_selector),
        "}~",
    ];
    docvec![
        "call 'logger':'log'(",
        atom(level),
        ", ",
        Document::String(msg_doc_text.to_string()),
        ", ",
        metadata_map_doc,
        ")",
    ]
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #3 (high-complexity): generate_pack_prefix from control_flow/mod.rs
// ════════════════════════════════════════════════════════════════════════════
//
// Each `maps:put` let in the original costs **8** `Document::String`
// punctuation parts.  Typed-leaves collapses to one `var(...)` per
// variable and one `atom(...)` per atom key — same algorithm, smaller
// per-line cost.
//
//     // Original
//     docvec![
//         "let ", Document::String(packed_var), " = call 'maps':'put'('",
//         Document::String(key), "', ", Document::String(core_var), ", ",
//         Document::String(current), ") in ",
//     ]
//
//     // Typed-leaves
//     docvec![
//         "let ", var(packed_var), " = call 'maps':'put'(",
//         atom(key), ", ", var(core_var), ", ", var(current), ") in ",
//     ]
// ════════════════════════════════════════════════════════════════════════════

/// Audit version of `ThreadingPlan::generate_pack_prefix` on typed leaves.
#[must_use]
pub fn generate_pack_prefix_typed_leaves(inputs: &PackPrefixInputs) -> (Document<'static>, String) {
    use leaf::*;
    if inputs.threaded_locals.is_empty() || inputs.use_direct_params || inputs.use_hybrid_params {
        return (Document::Nil, inputs.initial_state_var.clone());
    }
    let mut pack_docs: Vec<Document<'static>> = Vec::new();
    let mut current = if inputs.is_value_type {
        let init_map_var = inputs.fresh_temp();
        pack_docs.push(docvec![
            "let ",
            var(init_map_var.clone()),
            " = call 'maps':'new'() in ",
        ]);
        init_map_var
    } else {
        inputs.initial_state_var.clone()
    };
    for var_name in &inputs.threaded_locals {
        let packed_var = inputs.fresh_temp();
        let core_var = inputs.core_var_for_local[var_name].clone();
        let key = inputs.key_for_local[var_name].clone();
        pack_docs.push(docvec![
            "let ",
            var(packed_var.clone()),
            " = call 'maps':'put'(",
            atom(key),
            ", ",
            var(core_var),
            ", ",
            var(current),
            ") in ",
        ]);
        current = packed_var;
    }
    (Document::Vec(pack_docs), current)
}

// ════════════════════════════════════════════════════════════════════════════
// Text-equivalence tests — same fixtures and assertions as cerl_audit.rs
// ════════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn make_pack_inputs(
        threaded_locals: &[&str],
        is_value_type: bool,
        fresh_temps: &[&str],
    ) -> PackPrefixInputs {
        let mut core_var = std::collections::HashMap::new();
        let mut key = std::collections::HashMap::new();
        for v in threaded_locals {
            // mimic to_core_erlang_var: capitalize first letter
            let mut chars = v.chars();
            let core = match chars.next() {
                None => String::new(),
                Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
            };
            core_var.insert((*v).to_string(), core);
            key.insert((*v).to_string(), format!("__local__{v}"));
        }
        PackPrefixInputs {
            threaded_locals: threaded_locals.iter().map(|s| (*s).to_string()).collect(),
            use_direct_params: false,
            use_hybrid_params: false,
            is_value_type,
            initial_state_var: "State".to_string(),
            core_var_for_local: core_var,
            key_for_local: key,
            fresh_temps: std::cell::RefCell::new(
                fresh_temps.iter().map(|s| (*s).to_string()).collect(),
            ),
        }
    }

    // ─── Rewrite #1: leaf ───────────────────────────────────────────────────

    #[test]
    fn beamtalk_class_attribute_empty_matches() {
        let original = beamtalk_class_attribute_original(&[]);
        let rewrite = beamtalk_class_attribute_typed_leaves(&[]);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        assert_eq!(original.to_pretty_string(), "");
    }

    #[test]
    fn beamtalk_class_attribute_single_class_matches() {
        let classes = vec![("Counter".to_string(), "Object".to_string())];
        let original = beamtalk_class_attribute_original(&classes);
        let rewrite = beamtalk_class_attribute_typed_leaves(&classes);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        assert_eq!(
            original.to_pretty_string(),
            ",\n     'beamtalk_class' = [{'Counter', 'Object'}]"
        );
    }

    #[test]
    fn beamtalk_class_attribute_multi_class_matches() {
        let classes = vec![
            ("Counter".to_string(), "Object".to_string()),
            ("Account".to_string(), "Counter".to_string()),
            ("BankAccount".to_string(), "Account".to_string()),
        ];
        let original = beamtalk_class_attribute_original(&classes);
        let rewrite = beamtalk_class_attribute_typed_leaves(&classes);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
    }

    // ─── Rewrite #2: medium ─────────────────────────────────────────────────

    #[test]
    fn logger_log_call_matches() {
        let level = "info";
        let msg = r#"{"~ts", [_LogArg1]}"#;
        let ctx_class = "Counter";
        let ctx_selector = "tick";
        let original = logger_log_call_original(level, msg, ctx_class, ctx_selector);
        let rewrite = logger_log_call_typed_leaves(level, msg, ctx_class, ctx_selector);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        let text = original.to_pretty_string();
        assert!(text.contains("call 'logger':'log'("));
        assert!(text.contains("'beamtalk_class' => 'Counter'"));
        assert!(text.contains("'beamtalk_selector' => 'tick'"));
        assert!(text.contains("['beamtalk' | ['user']]"));
    }

    #[test]
    fn logger_log_call_various_levels_match() {
        for level in &["debug", "info", "warning", "error"] {
            let original =
                logger_log_call_original(level, r#"{"~ts", [_LogArg1]}"#, "Account", "deposit:");
            let rewrite = logger_log_call_typed_leaves(
                level,
                r#"{"~ts", [_LogArg1]}"#,
                "Account",
                "deposit:",
            );
            assert_eq!(
                original.to_pretty_string(),
                rewrite.to_pretty_string(),
                "level={level}",
            );
        }
    }

    // ─── Rewrite #3: high-complexity ────────────────────────────────────────

    #[test]
    fn pack_prefix_empty_locals_matches() {
        let inputs = make_pack_inputs(&[], false, &[]);
        let (orig_doc, orig_var) = generate_pack_prefix_original(&inputs);
        let inputs2 = make_pack_inputs(&[], false, &[]);
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_typed_leaves(&inputs2);
        assert_eq!(orig_doc.to_pretty_string(), rewrite_doc.to_pretty_string());
        assert_eq!(orig_var, rewrite_var);
        assert_eq!(orig_var, "State");
    }

    #[test]
    fn pack_prefix_actor_context_matches() {
        let temps = ["Packed1", "Packed2"];
        let inputs_a = make_pack_inputs(&["counter", "total"], false, &temps);
        let inputs_b = make_pack_inputs(&["counter", "total"], false, &temps);
        let (orig_doc, orig_var) = generate_pack_prefix_original(&inputs_a);
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_typed_leaves(&inputs_b);
        assert_eq!(orig_doc.to_pretty_string(), rewrite_doc.to_pretty_string());
        assert_eq!(orig_var, rewrite_var);
        let text = orig_doc.to_pretty_string();
        assert!(text.contains("call 'maps':'put'('__local__counter', Counter, State)"));
        assert!(text.contains("call 'maps':'put'('__local__total', Total, Packed1)"));
    }

    #[test]
    fn pack_prefix_value_type_context_matches() {
        let temps = ["InitMap1", "Packed2", "Packed3"];
        let inputs_a = make_pack_inputs(&["x", "y"], true, &temps);
        let inputs_b = make_pack_inputs(&["x", "y"], true, &temps);
        let (orig_doc, orig_var) = generate_pack_prefix_original(&inputs_a);
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_typed_leaves(&inputs_b);
        assert_eq!(orig_doc.to_pretty_string(), rewrite_doc.to_pretty_string());
        assert_eq!(orig_var, rewrite_var);
        let text = orig_doc.to_pretty_string();
        assert!(text.contains("call 'maps':'new'()"));
        assert!(text.contains("call 'maps':'put'('__local__x', X, InitMap1)"));
        assert!(text.contains("call 'maps':'put'('__local__y', Y, Packed2)"));
    }

    #[test]
    fn pack_prefix_direct_params_returns_nil() {
        let mut inputs_a = make_pack_inputs(&["counter"], false, &[]);
        inputs_a.use_direct_params = true;
        let mut inputs_b = make_pack_inputs(&["counter"], false, &[]);
        inputs_b.use_direct_params = true;
        let (orig_doc, orig_var) = generate_pack_prefix_original(&inputs_a);
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_typed_leaves(&inputs_b);
        assert_eq!(orig_doc.to_pretty_string(), rewrite_doc.to_pretty_string());
        assert_eq!(orig_var, rewrite_var);
        assert_eq!(orig_doc.to_pretty_string(), "");
    }
}
