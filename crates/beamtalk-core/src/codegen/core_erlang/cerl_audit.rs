// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! **THROWAWAY** — ADR 0088 Phase 0a audit only.  Delete with the memo (BT-2314)
//! once the gate decision has been made.
//!
//! This file is the experimental rig for the ADR 0088 Phase 0a shrinkage audit
//! (BT-2314 / epic BT-2313).  It contains:
//!
//! 1. A **minimal** `cerl::*` Rust mirror of the Core Erlang AST — only the
//!    node kinds the three audit subjects need.  Not the full Phase 1 mirror;
//!    not production-ready; intentionally lossy on syntactic concerns the audit
//!    does not exercise (e.g. annotations, line numbers).
//!
//! 2. Three rewritten codegen functions, one each from the size tiers required
//!    by the ADR:
//!    - leaf utility — `beamtalk_class_attribute` (util.rs)
//!    - medium expression — Logger metadata-map fragment (intrinsics.rs)
//!    - high-complexity construct — `generate_pack_prefix` (`control_flow/mod.rs`,
//!      the 4.2K-LOC hot spot)
//!
//! 3. Text-equivalence tests: each rewrite renders to the same string the
//!    `Document` original produces on representative inputs.
//!
//! The originals are NOT modified — they continue to drive production codegen.
//! Comparison is byte-for-byte against `to_pretty_string()`.
//!
//! Methodology notes for the memo:
//! - LOC counts use `cargo run --quiet -p tokei` style hand counts of the
//!   *body* of each function (signature + braces excluded), so a 3-line wrapper
//!   doesn't artificially inflate the original's count.
//! - Branching is cyclomatic count (1 + decision points).
//! - "Helper-call count" is the number of `docvec!` / `Document::*` calls in
//!   the original vs. the number of `cerl::*` constructor calls in the rewrite.

#![allow(dead_code)] // entire module is throwaway scaffolding

use super::document::{Document, join};
use crate::docvec;

// ════════════════════════════════════════════════════════════════════════════
// Minimal cerl::* mirror
// ════════════════════════════════════════════════════════════════════════════
//
// What this audits: whether expressing Core Erlang as a typed Rust tree
// (instead of a sea of `Document::String("'") + atom + Document::String("'")`
// ceremony) materially shrinks codegen.
//
// What this is NOT: a full Core Erlang AST.  Notably absent: function defs,
// case/clause, receive, try/catch, primops, binary syntax, fun expressions,
// annotations, source locations.  Phase 1 of ADR 0088 (if it proceeds) would
// build the full thing.
//
// Design choice: `Expr` is a single sum type, like OTP's own `cerl` module.
// `Atom`/`Var`/`Int` are leaves; everything else is a composite.
//
// All `cerl::Expr` values know how to render themselves to a `Document` via
// `to_doc()`.  This is what production cerl-direct codegen would NOT need
// (it would ETF-encode and ship to BEAM as a term).  In the audit, however,
// rendering to text is the cheapest way to prove semantic equivalence with
// the existing text-based pipeline.
// ════════════════════════════════════════════════════════════════════════════

pub mod cerl {
    use super::{Document, docvec, join};

    /// A single Core Erlang expression node.  Phase 0a subset.
    #[derive(Debug, Clone)]
    pub enum Expr {
        /// `'atom_name'` — Core Erlang atom (always quoted).
        Atom(String),
        /// `VarName` — Core Erlang variable.
        Var(String),
        /// Integer literal — printed as decimal.
        Int(i64),
        /// `~{ k1 => v1, k2 => v2 }~` — map literal.  Order-preserving.
        Map(Vec<(Expr, Expr)>),
        /// `{ e1, e2, ... }` — tuple literal.
        Tuple(Vec<Expr>),
        /// `[e1, e2, ... ]` — proper list literal (no improper-tail support).
        List(Vec<Expr>),
        /// `[h | t]` — cons cell (improper list head/tail).
        Cons(Box<Expr>, Box<Expr>),
        /// `call Mod:Fun(Args...)` — remote call.  `module`/`fun` are atoms.
        Call {
            /// The module atom (e.g. `"maps"`).
            module: String,
            /// The function atom (e.g. `"put"`).
            fun: String,
            /// Argument expressions in left-to-right order.
            args: Vec<Expr>,
        },
        /// `let Var = Bound in Body`.
        Let {
            /// The variable name bound by this let.
            var: String,
            /// The expression bound to `var`.
            bound: Box<Expr>,
            /// The expression where `var` is in scope.
            body: Box<Expr>,
        },
        /// `let Var = Bound in ` (no body — caller will splice the rest).
        ///
        /// This models the "open let-chain" pattern that appears throughout the
        /// current codegen, where a function returns a `Document` ending in
        /// `" in "` to be concatenated with sibling output.  In a fully-typed
        /// world this wouldn't exist — every `Let` would carry its `body` —
        /// but the audit must produce identical text, so we permit it for
        /// fidelity with the originals.
        LetOpen {
            /// The variable name bound by this open let.
            var: String,
            /// The expression bound to `var`.
            bound: Box<Expr>,
        },
        /// Concatenation of fragments — composes several `LetOpen` chains
        /// (and optionally a terminal value) into a single expression.
        Seq(Vec<Expr>),
        /// A raw escape hatch for fragments we don't need to model in Phase 0a
        /// (e.g. the binary-string `#{ #<104>... }#` body).  Keeping this small
        /// is part of the audit — the more `Raw` we need, the worse cerl-direct
        /// looks.
        Raw(String),
    }

    impl Expr {
        /// Render this expression to a `Document` for text-equivalence checks.
        ///
        /// **Note for the audit:** production cerl-direct codegen would NOT do
        /// this — it would ETF-encode the tree and pipe it to BEAM.  This
        /// renderer exists only so the audit's rewrites can be byte-for-byte
        /// compared with the originals' `Document` output.
        #[must_use]
        pub fn to_doc(&self) -> Document<'static> {
            match self {
                Self::Atom(name) => docvec!["'", Document::String(name.clone()), "'"],
                Self::Var(name) => Document::String(name.clone()),
                Self::Int(n) => Document::String(n.to_string()),
                Self::Map(pairs) => {
                    let entries = pairs
                        .iter()
                        .map(|(k, v)| docvec![k.to_doc(), " => ", v.to_doc()]);
                    docvec!["~{", join(entries, &Document::Str(", ")), "}~"]
                }
                Self::Tuple(elems) => {
                    let parts = elems.iter().map(Self::to_doc);
                    docvec!["{", join(parts, &Document::Str(", ")), "}"]
                }
                Self::List(elems) => {
                    let parts = elems.iter().map(Self::to_doc);
                    docvec!["[", join(parts, &Document::Str(", ")), "]"]
                }
                Self::Cons(h, t) => docvec!["[", h.to_doc(), " | ", t.to_doc(), "]"],
                Self::Call { module, fun, args } => {
                    let arg_parts = args.iter().map(Self::to_doc);
                    docvec![
                        "call '",
                        Document::String(module.clone()),
                        "':'",
                        Document::String(fun.clone()),
                        "'(",
                        join(arg_parts, &Document::Str(", ")),
                        ")",
                    ]
                }
                Self::Let { var, bound, body } => docvec![
                    "let ",
                    Document::String(var.clone()),
                    " = ",
                    bound.to_doc(),
                    " in ",
                    body.to_doc(),
                ],
                Self::LetOpen { var, bound } => docvec![
                    "let ",
                    Document::String(var.clone()),
                    " = ",
                    bound.to_doc(),
                    " in ",
                ],
                Self::Seq(parts) => Document::Vec(parts.iter().map(Self::to_doc).collect()),
                Self::Raw(s) => Document::String(s.clone()),
            }
        }
    }

    // ─── tiny constructors — the user-facing API ────────────────────────────
    //
    // These are the surface area cerl-direct codegen would actually use.
    // Their brevity vs. `docvec![Document::String("'"), name, Document::String("'")]`
    // is the entire point of the experiment.

    /// `'atom'`
    #[must_use]
    pub fn atom(name: impl Into<String>) -> Expr {
        Expr::Atom(name.into())
    }
    /// `Var`
    #[must_use]
    pub fn var(name: impl Into<String>) -> Expr {
        Expr::Var(name.into())
    }
    /// `call 'mod':'fun'(args...)`
    #[must_use]
    pub fn call(module: impl Into<String>, fun: impl Into<String>, args: Vec<Expr>) -> Expr {
        Expr::Call {
            module: module.into(),
            fun: fun.into(),
            args,
        }
    }
    /// `{e1, e2, ...}`
    #[must_use]
    pub fn tuple(elems: Vec<Expr>) -> Expr {
        Expr::Tuple(elems)
    }
    /// `~{ k => v, ... }~`
    #[must_use]
    pub fn map(pairs: Vec<(Expr, Expr)>) -> Expr {
        Expr::Map(pairs)
    }
    /// `let var = bound in body`
    #[must_use]
    pub fn let_(var: impl Into<String>, bound: Expr, body: Expr) -> Expr {
        Expr::Let {
            var: var.into(),
            bound: Box::new(bound),
            body: Box::new(body),
        }
    }
    /// `let var = bound in `  (no body, caller splices)
    #[must_use]
    pub fn let_open(var: impl Into<String>, bound: Expr) -> Expr {
        Expr::LetOpen {
            var: var.into(),
            bound: Box::new(bound),
        }
    }
    /// Concatenate several fragments (typically `LetOpen` chains, optionally
    /// followed by a terminal value).
    #[must_use]
    pub fn seq(parts: Vec<Expr>) -> Expr {
        Expr::Seq(parts)
    }
    /// Cons cell: `[h | t]`.
    #[must_use]
    pub fn cons(h: Expr, t: Expr) -> Expr {
        Expr::Cons(Box::new(h), Box::new(t))
    }
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #1 (leaf): util.rs::beamtalk_class_attribute
// ════════════════════════════════════════════════════════════════════════════
//
// Original (util.rs lines 77-95, ~19 source lines incl. signature):
//
//     pub(super) fn beamtalk_class_attribute(classes: &[ClassDefinition]) -> Document<'static> {
//         if classes.is_empty() { return Document::Nil; }
//         let entries = classes.iter().map(|c| {
//             docvec![
//                 "{'",
//                 Document::String(c.name.name.to_string()),
//                 "', '",
//                 Document::String(c.superclass_name().to_string()),
//                 "'}"
//             ]
//         });
//         docvec![
//             ",\n     'beamtalk_class' = [",
//             join(entries, &Document::Str(", ")),
//             "]"
//         ]
//     }
//
// Observations:
// - 5 `Document::String`/`Document::Str` calls per class entry (mostly delimiters).
// - The atom-quote dance (`"{'"`, `"', '"`, `"'}"`) is exactly the kind of
//   ceremony cerl-direct removes.
// - The leading `",\n     "` and trailing `"]"` are *not* Core Erlang — they're
//   attribute-list glue.  cerl-direct cannot eliminate them entirely; they
//   become attribute-key/value composition at a higher level.  For the audit,
//   we keep them as `Raw` to be honest about what cerl-direct can and can't do.
// ════════════════════════════════════════════════════════════════════════════

/// Audit version of [`beamtalk_class_attribute`] built on `cerl::Expr`.
///
/// `classes` are `(class_name, superclass_name)` pairs — kept as plain strings
/// so the audit doesn't depend on `ClassDefinition` and the file stays trivial
/// to delete.
#[must_use]
pub fn beamtalk_class_attribute_cerl(classes: &[(String, String)]) -> Document<'static> {
    use cerl::*;
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries: Vec<_> = classes
        .iter()
        .map(|(name, superclass)| {
            tuple(vec![atom(name.clone()), atom(superclass.clone())]).to_doc()
        })
        .collect();
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]",
    ]
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #2 (medium): the metadata-map fragment from
// intrinsics.rs::try_generate_logger_intrinsic
// ════════════════════════════════════════════════════════════════════════════
//
// We pull only the metadata-map and log-call fragments here, not the entire
// 115-LOC dispatch function — the *interesting* shape work is in those two
// fragments.  The selector parsing, arg hoisting, and ProtoObject fallthrough
// are independent of the Document/cerl decision and would look identical in
// both implementations.
//
// Original metadata-map fragment (intrinsics.rs lines 2087-2097, ~11 lines):
//
//     let metadata_map_doc = docvec![
//         "~{",
//         "'domain' => ['beamtalk' | ['user']], ",
//         "'beamtalk_class' => '",
//         Document::String(ctx_class),
//         "', ",
//         "'beamtalk_selector' => '",
//         Document::String(ctx_selector),
//         "'",
//         "}~",
//     ];
//
// Original log-call (no-metadata branch, intrinsics.rs lines 2123-2133, ~11 lines):
//
//     docvec![
//         "call 'logger':'log'('",
//         Document::String(level.to_string()),
//         "', ",
//         msg_doc,
//         ", ",
//         metadata_map_doc,
//         ")"
//     ]
//
// Observations:
// - The map fragment is the worst case: every key and string value requires
//   six `Document` parts (open quote, value, close quote, comma…), and the
//   author has to manually keep the punctuation in sync.
// - cerl-direct collapses to a `Map(vec![(atom, value), …])` constructor.
// - The cons cell `['beamtalk' | ['user']]` is currently a raw string — with
//   cerl-direct it's a tiny `cons(atom("beamtalk"), list(vec![atom("user")]))`.
// ════════════════════════════════════════════════════════════════════════════

/// Audit version of the metadata-map + log-call fragments.
///
/// `msg_doc_text` is the rendered `{"~ts", [<arg>]}` fragment from upstream;
/// the audit substitutes a fixed placeholder string instead of re-running the
/// whole arg-hoisting machinery.  This isolates the *shape* work from the
/// orthogonal sub-expression-capture work.
#[must_use]
pub fn logger_log_call_cerl(
    level: &str,
    msg_doc_text: &str,
    ctx_class: &str,
    ctx_selector: &str,
) -> Document<'static> {
    use cerl::*;
    let metadata = map(vec![
        (
            atom("domain"),
            cons(atom("beamtalk"), Expr::List(vec![atom("user")])),
        ),
        (atom("beamtalk_class"), atom(ctx_class)),
        (atom("beamtalk_selector"), atom(ctx_selector)),
    ]);
    let log_call = call(
        "logger",
        "log",
        vec![atom(level), Expr::Raw(msg_doc_text.to_string()), metadata],
    );
    log_call.to_doc()
}

// ════════════════════════════════════════════════════════════════════════════
// Rewrite #3 (high-complexity): control_flow/mod.rs::generate_pack_prefix
// ════════════════════════════════════════════════════════════════════════════
//
// Original lines 577-618, body ~38 lines.
//
//     pub fn generate_pack_prefix(...) -> (Document<'static>, String) {
//         if self.threaded_locals.is_empty() || self.use_direct_params || self.use_hybrid_params {
//             return (Document::Nil, self.initial_state_var.clone());
//         }
//         let mut pack_docs: Vec<Document<'static>> = Vec::new();
//         let mut current = if matches!(self.context, CodeGenContext::ValueType) {
//             let init_map_var = generator.fresh_temp_var("InitMap");
//             pack_docs.push(docvec![
//                 "let ",
//                 Document::String(init_map_var.clone()),
//                 " = call 'maps':'new'() in ",
//             ]);
//             init_map_var
//         } else {
//             self.initial_state_var.clone()
//         };
//         for var_name in &self.threaded_locals {
//             let packed_var = generator.fresh_temp_var("Packed");
//             let core_var = generator.lookup_var(var_name).cloned()
//                 .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
//             let key = self.state_key(var_name);
//             pack_docs.push(docvec![
//                 "let ",
//                 Document::String(packed_var.clone()),
//                 " = call 'maps':'put'('",
//                 Document::String(key),
//                 "', ",
//                 Document::String(core_var),
//                 ", ",
//                 Document::String(current),
//                 ") in ",
//             ]);
//             current = packed_var;
//         }
//         (Document::Vec(pack_docs), current)
//     }
//
// Observations (the heavy-hitter of the audit):
// - Each `maps:put` let costs **8** `Document::String` punctuation parts.
//   cerl-direct collapses to: `let_open(packed_var, call("maps", "put",
//   vec![atom(key), var(core_var), var(current)]))` — one constructor each.
// - The control flow (early return, ValueType branch, for-loop) is unchanged.
//   This is the audit's most honest answer: cerl-direct doesn't change the
//   *algorithm*, just the per-line cost of each emitted expression.
// - `pack_docs.push(docvec![…])` becomes `pack_exprs.push(let_open(…))` —
//   same shape, smaller payload.
// ════════════════════════════════════════════════════════════════════════════

/// Stripped-down inputs for the audit — no `CoreErlangGenerator` dependency.
pub struct PackPrefixInputs {
    /// Variables to pack into the state map.
    pub threaded_locals: Vec<String>,
    /// Whether this is a direct-params loop (skips packing).
    pub use_direct_params: bool,
    /// Whether this is a hybrid-params loop (skips packing).
    pub use_hybrid_params: bool,
    /// Whether this is a `ValueType` context (fresh `maps:new()` instead of State).
    pub is_value_type: bool,
    /// Name of the variable holding the initial state map.
    pub initial_state_var: String,
    /// Pre-resolved per-local Core Erlang variable names (production code
    /// calls `generator.lookup_var(...)`; the audit substitutes a fixed map).
    pub core_var_for_local: std::collections::HashMap<String, String>,
    /// Pre-resolved per-local state-map keys.
    pub key_for_local: std::collections::HashMap<String, String>,
    /// Pre-generated fresh temp names — the audit replaces the live generator's
    /// counter with a deterministic vec, but the *shape* of the resulting code
    /// is identical.
    pub fresh_temps: std::cell::RefCell<std::collections::VecDeque<String>>,
}

impl PackPrefixInputs {
    pub(super) fn fresh_temp(&self) -> String {
        self.fresh_temps
            .borrow_mut()
            .pop_front()
            .expect("audit harness ran out of fresh temp names")
    }
}

/// Audit version of [`ThreadingPlan::generate_pack_prefix`] built on `cerl::Expr`.
#[must_use]
pub fn generate_pack_prefix_cerl(inputs: &PackPrefixInputs) -> (Document<'static>, String) {
    use cerl::*;
    if inputs.threaded_locals.is_empty() || inputs.use_direct_params || inputs.use_hybrid_params {
        return (Document::Nil, inputs.initial_state_var.clone());
    }
    let mut pack_exprs: Vec<Expr> = Vec::new();
    let mut current = if inputs.is_value_type {
        let init_map_var = inputs.fresh_temp();
        pack_exprs.push(let_open(init_map_var.clone(), call("maps", "new", vec![])));
        init_map_var
    } else {
        inputs.initial_state_var.clone()
    };
    for var_name in &inputs.threaded_locals {
        let packed_var = inputs.fresh_temp();
        let core_var = inputs.core_var_for_local[var_name].clone();
        let key = inputs.key_for_local[var_name].clone();
        pack_exprs.push(let_open(
            packed_var.clone(),
            call("maps", "put", vec![atom(key), var(core_var), var(current)]),
        ));
        current = packed_var;
    }
    (seq(pack_exprs).to_doc(), current)
}

// ════════════════════════════════════════════════════════════════════════════
// Originals — verbatim copies, isolated from the live generator so the audit
// can compare bytes without a compiler instance.
// ════════════════════════════════════════════════════════════════════════════

/// Original `beamtalk_class_attribute`, retargeted to `(String, String)` pairs
/// to match the rewrite's input shape.
#[must_use]
pub fn beamtalk_class_attribute_original(classes: &[(String, String)]) -> Document<'static> {
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|(name, superclass)| {
        docvec![
            "{'",
            Document::String(name.clone()),
            "', '",
            Document::String(superclass.clone()),
            "'}"
        ]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]"
    ]
}

/// Original Logger metadata-map + log-call (no-metadata branch), inlined.
#[must_use]
pub fn logger_log_call_original(
    level: &str,
    msg_doc_text: &str,
    ctx_class: &str,
    ctx_selector: &str,
) -> Document<'static> {
    let metadata_map_doc = docvec![
        "~{",
        "'domain' => ['beamtalk' | ['user']], ",
        "'beamtalk_class' => '",
        Document::String(ctx_class.to_string()),
        "', ",
        "'beamtalk_selector' => '",
        Document::String(ctx_selector.to_string()),
        "'",
        "}~",
    ];
    docvec![
        "call 'logger':'log'('",
        Document::String(level.to_string()),
        "', ",
        Document::String(msg_doc_text.to_string()),
        ", ",
        metadata_map_doc,
        ")"
    ]
}

/// Original `generate_pack_prefix`, retargeted to `PackPrefixInputs`.
#[must_use]
pub fn generate_pack_prefix_original(inputs: &PackPrefixInputs) -> (Document<'static>, String) {
    if inputs.threaded_locals.is_empty() || inputs.use_direct_params || inputs.use_hybrid_params {
        return (Document::Nil, inputs.initial_state_var.clone());
    }
    let mut pack_docs: Vec<Document<'static>> = Vec::new();
    let mut current = if inputs.is_value_type {
        let init_map_var = inputs.fresh_temp();
        pack_docs.push(docvec![
            "let ",
            Document::String(init_map_var.clone()),
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
            Document::String(packed_var.clone()),
            " = call 'maps':'put'('",
            Document::String(key),
            "', ",
            Document::String(core_var),
            ", ",
            Document::String(current),
            ") in ",
        ]);
        current = packed_var;
    }
    (Document::Vec(pack_docs), current)
}

// ════════════════════════════════════════════════════════════════════════════
// Text-equivalence tests
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
        let rewrite = beamtalk_class_attribute_cerl(&[]);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        assert_eq!(original.to_pretty_string(), "");
    }

    #[test]
    fn beamtalk_class_attribute_single_class_matches() {
        let classes = vec![("Counter".to_string(), "Object".to_string())];
        let original = beamtalk_class_attribute_original(&classes);
        let rewrite = beamtalk_class_attribute_cerl(&classes);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        // Sanity-check the literal output
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
        let rewrite = beamtalk_class_attribute_cerl(&classes);
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
        let rewrite = logger_log_call_cerl(level, msg, ctx_class, ctx_selector);
        assert_eq!(original.to_pretty_string(), rewrite.to_pretty_string());
        // Sanity check: ensure the generated text contains the expected shape
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
            let rewrite =
                logger_log_call_cerl(level, r#"{"~ts", [_LogArg1]}"#, "Account", "deposit:");
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
        // Re-create inputs because fresh_temps is consumed by interior mutability.
        let inputs2 = make_pack_inputs(&[], false, &[]);
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_cerl(&inputs2);
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
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_cerl(&inputs_b);
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
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_cerl(&inputs_b);
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
        let (rewrite_doc, rewrite_var) = generate_pack_prefix_cerl(&inputs_b);
        assert_eq!(orig_doc.to_pretty_string(), rewrite_doc.to_pretty_string());
        assert_eq!(orig_var, rewrite_var);
        assert_eq!(orig_doc.to_pretty_string(), "");
    }
}
