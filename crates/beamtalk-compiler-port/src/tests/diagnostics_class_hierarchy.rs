// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! diagnostics/compile class-hierarchy and protocol-registry interplay: a class hierarchy alone still reproducing a false protocol-mismatch warning, and a protocol registry (class- or method-level) suppressing it.

use super::*;

/// ADR 0050 Phase 4: `class_hierarchy` in `compile_expression` request is accepted
/// and does not cause errors (backward-compatible optional key).
#[test]
fn compile_expression_accepts_class_hierarchy_key() {
    use eetf::{FixInteger, List};

    let method_info = Map::from([(
        atom("value"),
        Term::from(Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            (atom("return_type"), atom("Integer")),
        ])),
    )]);
    let counter_meta = Map::from([
        (atom("class"), atom("Counter")),
        (atom("superclass"), atom("Object")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(method_info)),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_variables"), Term::from(List::from(vec![]))),
    ]);
    let class_hierarchy_term = Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (atom("source"), binary("1 + 1.")),
        (atom("module"), binary("bt@test_repl")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
        (atom("class_hierarchy"), class_hierarchy_term),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "compile_expression with class_hierarchy should succeed: {response:?}"
    );
}

/// ADR 0105 Phase 1 (BT-2778): `diagnostics` accepts `class_hierarchy`
/// (like `compile_expression`/`compile_method` already did) and returns
/// severity- and category-tagged diagnostics, so a re-check can tell a
/// removed-selector `Dnu` from a `Type` mismatch without location alone.
#[test]
fn diagnostics_accepts_class_hierarchy_and_reports_category() {
    use eetf::{FixInteger, List};

    let method_info = Map::from([(
        atom("getCount"),
        Term::from(Map::from([
            (atom("arity"), Term::from(FixInteger::from(0))),
            (atom("param_types"), Term::from(List::from(vec![]))),
            // The reload changed Counter>>getCount's return type to
            // String — the caller's `+ 1` is now stale.
            (atom("return_type"), atom("String")),
        ])),
    )]);
    let counter_meta = Map::from([
        (atom("superclass"), atom("Object")),
        (atom("method_info"), Term::from(method_info)),
    ]);
    let class_hierarchy_term = Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (
            atom("source"),
            binary(
                "Object subclass: Dashboard\n  refresh: c :: Counter -> Integer => (c getCount) + 1\n",
            ),
        ),
        (atom("class_hierarchy"), class_hierarchy_term),
    ]);

    let response = handle_diagnostics(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")));
    let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    assert!(
        !diagnostics.elements.is_empty(),
        "expected at least one diagnostic for the now-stale `+ 1`: {response:?}"
    );
    // Every diagnostic carries a `category` key (an atom `undefined` or a
    // binary label) — BT-2778's re-check orchestration filters on it.
    for diag in &diagnostics.elements {
        let Term::Map(dm) = diag else {
            panic!("Expected diagnostic map, got {diag:?}");
        };
        assert!(
            map_get(dm, "category").is_some(),
            "diagnostic missing category key: {diag:?}"
        );
    }
}

/// BT-3473: without `protocol_registry`, a runtime-seeded protocol
/// reaches `analyse_full` only as a zero-method `class_hierarchy` entry
/// (`beamtalk_protocol_registry:create_protocol_class/2`'s actual wire
/// shape has no `superclass`/`method_info` — see
/// `parse_protocol_info_from_meta_term`'s doc), which defeats the
/// BT-2088/BT-3472 nominal-mismatch escape hatch and makes every
/// selector on a protocol-typed receiver look unresolved. This proves
/// the pre-fix shape actually reproduces both false positives — the
/// `TimeoutToken`/`NullTimer` scenario from the issue.
#[test]
fn diagnostics_class_hierarchy_alone_reproduces_false_protocol_mismatch() {
    let class_hierarchy_term = Term::from(Map::from([(
        atom("TimeoutToken"),
        Term::from(Map::from([
            (atom("is_sealed"), atom("true")),
            (atom("is_abstract"), atom("true")),
        ])),
    )]));

    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (
            atom("source"),
            binary(
                "Value subclass: NullTimer\n\
                     \x20 cancel -> Boolean => false\n\
                     \x20 isActive -> Boolean => true\n\
                     \n\
                     typed Object subclass: Pool\n\
                     \x20 make -> TimeoutToken => NullTimer new\n\
                     \x20 use: t :: TimeoutToken -> Boolean => t cancel\n\
                     \x20 go -> Boolean => self use: NullTimer new\n",
            ),
        ),
        (atom("class_hierarchy"), class_hierarchy_term),
    ]);

    let response = handle_diagnostics(&request);
    let messages = diagnostic_messages(&response);
    assert!(
        messages
            .iter()
            .any(|m| m.contains("declares return type TimeoutToken")),
        "expected the pre-fix false type mismatch, got: {messages:?}"
    );
    assert!(
        messages.iter().any(|m| m.contains("does not understand")),
        "expected the pre-fix false Dnu hint, got: {messages:?}"
    );
}

/// BT-3473: the companion fix to the test above — supplying
/// `protocol_registry` alongside `class_hierarchy` lets the existing
/// BT-2088/BT-3472 filter in `analyse_full` recognise `TimeoutToken` as
/// a protocol (not a plain class) the same way it already does for the
/// LSP's `ProjectIndex` path, so `NullTimer`'s structural conformance is
/// correctly recognised and neither false positive fires.
#[test]
fn diagnostics_protocol_registry_suppresses_false_protocol_mismatch() {
    let class_hierarchy_term = Term::from(Map::from([(
        atom("TimeoutToken"),
        Term::from(Map::from([
            (atom("is_sealed"), atom("true")),
            (atom("is_abstract"), atom("true")),
        ])),
    )]));
    let protocol_registry_term = Term::from(Map::from([(
        atom("TimeoutToken"),
        Term::from(Map::from([
            (
                atom("required_methods"),
                Term::from(eetf::List::from(vec![
                    Term::from(Map::from([
                        (atom("selector"), atom("cancel")),
                        (atom("arity"), Term::from(eetf::FixInteger::from(0))),
                    ])),
                    Term::from(Map::from([
                        (atom("selector"), atom("isActive")),
                        (atom("arity"), Term::from(eetf::FixInteger::from(0))),
                    ])),
                ])),
            ),
            (atom("type_params"), Term::from(eetf::List::from(vec![]))),
            (atom("extending"), atom("undefined")),
        ])),
    )]));

    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (
            atom("source"),
            binary(
                "Value subclass: NullTimer\n\
                     \x20 cancel -> Boolean => false\n\
                     \x20 isActive -> Boolean => true\n\
                     \n\
                     typed Object subclass: Pool\n\
                     \x20 make -> TimeoutToken => NullTimer new\n\
                     \x20 use: t :: TimeoutToken -> Boolean => t cancel\n\
                     \x20 go -> Boolean => self use: NullTimer new\n",
            ),
        ),
        (atom("class_hierarchy"), class_hierarchy_term),
        (atom("protocol_registry"), protocol_registry_term),
    ]);

    let response = handle_diagnostics(&request);
    let messages = diagnostic_messages(&response);
    assert!(
        !messages
            .iter()
            .any(|m| m.contains("declares return type TimeoutToken")),
        "NullTimer structurally conforms to TimeoutToken — no nominal mismatch \
             expected, got: {messages:?}"
    );
    assert!(
        !messages.iter().any(|m| m.contains("does not understand")),
        "TimeoutToken's required selectors are known — no Dnu hint expected, \
             got: {messages:?}"
    );
}

/// Extract every diagnostic's `message` string from a `handle_diagnostics`
/// response, for the two BT-3473 tests above.
fn diagnostic_messages(response: &Term) -> Vec<String> {
    let Term::Map(m) = response else {
        panic!("Expected map response: {response:?}");
    };
    let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    diagnostics
        .elements
        .iter()
        .map(|diag| {
            let Term::Map(dm) = diag else {
                panic!("Expected diagnostic map, got {diag:?}");
            };
            map_get(dm, "message")
                .and_then(term_to_string)
                .unwrap_or_default()
        })
        .collect()
}

/// BT-3477: builds the `class_hierarchy` entry for `NullTimer` used by
/// the `compile`/`compile_method` protocol-registry tests below —
/// `NullTimer` stands in for the cross-file class the issue describes
/// (the ambient cache's actual wire shape for a real compiled class,
/// full `method_info` included), so the protocol-conformance check has
/// something to structurally match `TimeoutToken`'s required selectors
/// against without inlining `NullTimer`'s definition into the source
/// under compile — `compile`/`compile_method` (unlike `diagnostics`)
/// runs codegen, which enforces BT-1666's one-class-per-file rule.
fn null_timer_class_info_term() -> Term {
    let no_arg_boolean_method = |return_type: &str| {
        Term::from(Map::from([
            (atom("arity"), Term::from(eetf::FixInteger::from(0))),
            (atom("param_types"), Term::from(eetf::List::from(vec![]))),
            (atom("return_type"), atom(return_type)),
        ]))
    };
    Term::from(Map::from([
        (atom("superclass"), atom("Value")),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("true")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(eetf::List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (
            atom("method_info"),
            Term::from(Map::from([
                (atom("cancel"), no_arg_boolean_method("Boolean")),
                (atom("isActive"), no_arg_boolean_method("Boolean")),
            ])),
        ),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (
            atom("class_variables"),
            Term::from(eetf::List::from(vec![])),
        ),
    ]))
}

/// BT-3477: the `TimeoutToken` `class_hierarchy` entry (mirrors the
/// `diagnostics_*_protocol_mismatch` tests' `class_hierarchy_term` above
/// — the runtime-seeded checker sees a protocol as a zero-method class
/// entry regardless of request kind) and its `protocol_registry` entry
/// (BT-3473's real wire shape: selector/arity-only required methods).
fn timeout_token_terms() -> (Term, Term) {
    let class_entry = Term::from(Map::from([
        (atom("is_sealed"), atom("true")),
        (atom("is_abstract"), atom("true")),
    ]));
    let protocol_entry = Term::from(Map::from([
        (
            atom("required_methods"),
            Term::from(eetf::List::from(vec![
                Term::from(Map::from([
                    (atom("selector"), atom("cancel")),
                    (atom("arity"), Term::from(eetf::FixInteger::from(0))),
                ])),
                Term::from(Map::from([
                    (atom("selector"), atom("isActive")),
                    (atom("arity"), Term::from(eetf::FixInteger::from(0))),
                ])),
            ])),
        ),
        (atom("type_params"), Term::from(eetf::List::from(vec![]))),
        (atom("extending"), atom("undefined")),
    ]));
    (class_entry, protocol_entry)
}

/// BT-3477: extract every string in a `compile`/`compile_method` `ok`
/// response's `warnings` field (plain binaries, unlike `diagnostics`'s
/// `diagnostic_messages` maps above) for the protocol-registry tests
/// below.
fn compile_warning_messages(response: &Term) -> Vec<String> {
    let Term::Map(m) = response else {
        panic!("Expected map response: {response:?}");
    };
    let Some(Term::List(warnings)) = map_get(m, "warnings") else {
        panic!("Expected warnings list: {response:?}");
    };
    warnings
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect()
}

/// BT-3477: without `protocol_registry`, `compile` (not just
/// `diagnostics/3`, which BT-3473 already covers) hits the identical
/// false type-mismatch/Dnu for a cross-file protocol-typed receiver —
/// `NullTimer` is only known via the ambient `class_hierarchy` here
/// (simulating a class compiled in an earlier REPL turn/another file),
/// exactly as `beamtalk_repl_eval`'s live `compile` calls see it.
#[test]
fn compile_class_hierarchy_alone_reproduces_false_protocol_mismatch() {
    let (timeout_token_class, _) = timeout_token_terms();
    let class_hierarchy_term = Term::from(Map::from([
        (atom("NullTimer"), null_timer_class_info_term()),
        (atom("TimeoutToken"), timeout_token_class),
    ]));

    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary(
                "typed Object subclass: Pool\n\
                     \x20 make -> TimeoutToken => NullTimer new\n\
                     \x20 use: t :: TimeoutToken -> Boolean => t cancel\n\
                     \x20 go -> Boolean => self use: NullTimer new\n",
            ),
        ),
        (atom("module_name"), binary("bt@pool")),
        (atom("class_hierarchy"), class_hierarchy_term),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "the false positives are warning/hint severity, not errors: {response:?}"
    );
    let warnings = compile_warning_messages(&response);
    assert!(
        warnings
            .iter()
            .any(|m| m.contains("declares return type TimeoutToken")),
        "expected the pre-fix false type mismatch, got: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|m| m.contains("does not understand")),
        "expected the pre-fix false Dnu hint, got: {warnings:?}"
    );
}

/// BT-3477: the companion fix to the test above — supplying
/// `protocol_registry` alongside `class_hierarchy` to `compile` (mirrors
/// `diagnostics_protocol_registry_suppresses_false_protocol_mismatch`)
/// lets the same BT-2088/BT-3472 filter recognise `TimeoutToken` as a
/// protocol, so neither false positive fires.
#[test]
fn compile_protocol_registry_suppresses_false_protocol_mismatch() {
    let (timeout_token_class, timeout_token_protocol) = timeout_token_terms();
    let class_hierarchy_term = Term::from(Map::from([
        (atom("NullTimer"), null_timer_class_info_term()),
        (atom("TimeoutToken"), timeout_token_class),
    ]));
    let protocol_registry_term =
        Term::from(Map::from([(atom("TimeoutToken"), timeout_token_protocol)]));

    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary(
                "typed Object subclass: Pool\n\
                     \x20 make -> TimeoutToken => NullTimer new\n\
                     \x20 use: t :: TimeoutToken -> Boolean => t cancel\n\
                     \x20 go -> Boolean => self use: NullTimer new\n",
            ),
        ),
        (atom("module_name"), binary("bt@pool")),
        (atom("class_hierarchy"), class_hierarchy_term),
        (atom("protocol_registry"), protocol_registry_term),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "resp: {response:?}"
    );
    let warnings = compile_warning_messages(&response);
    assert!(
        !warnings
            .iter()
            .any(|m| m.contains("declares return type TimeoutToken")),
        "NullTimer structurally conforms to TimeoutToken — no nominal mismatch \
             expected, got: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|m| m.contains("does not understand")),
        "TimeoutToken's required selectors are known — no Dnu hint expected, \
             got: {warnings:?}"
    );
}

/// BT-3477: the `compile_method` sibling of
/// `compile_protocol_registry_suppresses_false_protocol_mismatch` — the
/// live-image write surface (IDE save / `compile:source:` / REPL `>>`)
/// hits the same false positive when patching a method onto an
/// already-installed class. `class_source` carries the pre-fix
/// type-mismatch (`make`); the patched `method_source` carries the
/// pre-fix Dnu (`use:`) — both re-checked together on the merged module.
#[test]
fn compile_method_protocol_registry_suppresses_false_protocol_mismatch() {
    let (timeout_token_class, timeout_token_protocol) = timeout_token_terms();
    let class_hierarchy_term = Term::from(Map::from([
        (atom("NullTimer"), null_timer_class_info_term()),
        (atom("TimeoutToken"), timeout_token_class),
    ]));
    let protocol_registry_term =
        Term::from(Map::from([(atom("TimeoutToken"), timeout_token_protocol)]));

    let request = Map::from([
        (atom("command"), atom("compile_method")),
        (
            atom("class_source"),
            binary(
                "typed Object subclass: Pool\n\
                     \x20 make -> TimeoutToken => NullTimer new\n",
            ),
        ),
        (
            atom("method_source"),
            binary("use: t :: TimeoutToken -> Boolean =>\n  t cancel"),
        ),
        (atom("is_class_method"), atom("false")),
        (atom("module_name"), binary("bt@pool")),
        (atom("class_hierarchy"), class_hierarchy_term),
        (atom("protocol_registry"), protocol_registry_term),
    ]);

    let response = handle_compile_method(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "resp: {response:?}"
    );
    let warnings = compile_warning_messages(&response);
    assert!(
        !warnings
            .iter()
            .any(|m| m.contains("declares return type TimeoutToken")),
        "NullTimer structurally conforms to TimeoutToken — no nominal mismatch \
             expected, got: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|m| m.contains("does not understand")),
        "TimeoutToken's required selectors are known — no Dnu hint expected, \
             got: {warnings:?}"
    );
}
