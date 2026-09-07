// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `compile_method` / diagnostics-in-method-mode tests: bare method-body acceptance, source preservation, declared-signature reporting, module-name override, multi-class rejection, class-side method replacement, patched-method line annotation, and method-relative diagnostic positions.

use super::*;

const COMPILE_METHOD_CLASS: &str = "Actor subclass: EventStore\n  state: events = #{}\n\n  initialize -> Nil =>\n    self.events := #{}";

#[test]
fn diagnostics_method_mode_accepts_a_bare_method_body() {
    // BT-2569: the System Browser method editor sends a BARE method body. Under
    // the default (expression) grammar the `=>` body separator is not a valid
    // top-level token, so the parser reports a false
    // `Unexpected token: expected expression, found ⇒` — the bug. With
    // `mode: "method"` the body parses with `parse_method` and is clean.
    let body = "decrement => self.value := self.value - 1";

    // Default (expression) grammar: the bug — a false parse error on `=>`.
    let expr_request = Term::from(Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary(body)),
    ]));
    let expr_response = handle_request(&expr_request);
    assert_eq!(response_status(&expr_response).as_deref(), Some("ok"));
    assert!(
        !response_diagnostics(&expr_response)
            .expect("diagnostics")
            .elements
            .is_empty(),
        "expression grammar should reject a bare method body (regression guard)"
    );

    // Method grammar: the same body parses clean — no diagnostics.
    let method_request = Term::from(Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary(body)),
        (atom("mode"), binary("method")),
    ]));
    let method_response = handle_request(&method_request);
    assert_eq!(response_status(&method_response).as_deref(), Some("ok"));
    assert!(
        response_diagnostics(&method_response)
            .expect("diagnostics")
            .elements
            .is_empty(),
        "method grammar should accept a valid bare method body: {method_response:?}"
    );
}

#[test]
fn diagnostics_method_mode_still_reports_a_broken_body() {
    // The parse-only method path is not a no-op: a genuinely broken body (`:=`
    // with no right-hand side) still produces diagnostics in method mode.
    let request = Term::from(Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary("decrement => self.value :=")),
        (atom("mode"), binary("method")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    assert!(
        !response_diagnostics(&response)
            .expect("diagnostics")
            .elements
            .is_empty(),
        "a broken method body should still produce diagnostics: {response:?}"
    );
}

#[test]
fn compile_method_preserves_source_and_compiles() {
    // A `// --- … ---` banner over a `///` doc block over the header. The
    // per-method canonical `method_source` keeps the `///` doc block but drops
    // the leading `//` banner — the banner is inter-method file structure, not
    // part of the method's edit unit, and the byte span excludes it (BT-2594).
    // It is preserved in the file via `merged_class_source` (whole-file unparse).
    let method_source = "// --- Execution CRUD ---\n\n/// Store a new workflow execution.\n/// Raises if the workflowId already exists.\ncreateExecution: execution :: Object -> Object =>\n  execution";
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (atom("method_source"), binary(method_source)),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    assert_eq!(
        response_field_str(&response, "selector").as_deref(),
        Some("createExecution:")
    );
    let ms = response_field_str(&response, "method_source").expect("method_source");
    assert!(
        !ms.contains("--- Execution CRUD ---"),
        "leading `//` banner should be dropped from per-method source: {ms}"
    );
    assert!(
        ms.contains("/// Store a new workflow execution."),
        "first doc line lost: {ms}"
    );
    assert!(
        ms.contains("/// Raises if the workflowId already exists."),
        "doc line lost: {ms}"
    );
    assert!(
        response_field_str(&response, "core_erlang").is_some_and(|c| !c.is_empty()),
        "no core erlang emitted"
    );
    // The merged class source (stored for the next patch) is a clean inline
    // class that now contains the new method — no `>>` accumulation.
    let merged = response_field_str(&response, "merged_class_source").expect("merged");
    assert!(
        merged.contains("createExecution:"),
        "merged class missing the new method:\n{merged}"
    );
    assert!(
        merged.contains("Actor subclass: EventStore"),
        "merged class lost its header:\n{merged}"
    );
    assert!(
        !merged.contains(">>"),
        "merged class should be inline, not `>>` extensions:\n{merged}"
    );
}

#[test]
fn compile_method_response_carries_declared_signature() {
    // ADR 0105 Phase 1 (BT-2777): the compile_method response must carry the
    // patched method's declared return/param types so the workspace can
    // capture them into the signature-generation store before install.
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (
            atom("method_source"),
            binary("touch: n :: Integer -> Object =>\n  self"),
        ),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    assert_eq!(
        response_field_str(&response, "return_type").as_deref(),
        Some("Object")
    );
    assert_eq!(
        response_field_str_list(&response, "param_types").as_deref(),
        Some(&["Integer".to_string()][..])
    );
}

#[test]
fn compile_method_response_reports_dynamic_for_unannotated_signature() {
    // No return/param annotations on the patched method → both fields report
    // the "Dynamic" sentinel (never omitted, so the diff always compares two
    // values).
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (atom("method_source"), binary("touch: n =>\n  self")),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    assert_eq!(
        response_field_str(&response, "return_type").as_deref(),
        Some("Dynamic")
    );
    assert_eq!(
        response_field_str_list(&response, "param_types").as_deref(),
        Some(&["Dynamic".to_string()][..])
    );
}

#[test]
fn compile_method_honors_module_name_override() {
    // Package-qualified override must flow into the compiled module name
    // (this is what keeps EventStore as bt@exdura@event_store on a patch).
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (atom("method_source"), binary("touch => self.events")),
        (atom("is_class_method"), atom("false")),
        (atom("module_name"), binary("bt@exdura@event_store")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    assert_eq!(
        response_field_str(&response, "module_name").as_deref(),
        Some("bt@exdura@event_store")
    );
}

#[test]
fn compile_method_rejects_multi_class_source_cleanly() {
    // One-class-per-file (ADR 0040) is enforced by semantic analysis, so a
    // multi-class `class_source` can never load/compile through any path.
    // compile_method must surface that as a clean error (not silently drop a
    // sibling class from the merged source) — the caller then leaves the
    // stored source untouched, so there is no corruption.
    let two = "Actor subclass: Alpha\n  state: a = 1\n\n  va => self.a\n\n\
                   Actor subclass: Beta\n  state: b = 2\n\n  vb => self.b";
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(two)),
        (atom("class_name"), binary("Alpha")),
        (atom("method_source"), binary("bumped => self.a + 1")),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
}

#[test]
fn compile_method_replaces_class_side_method_without_duplicating() {
    // BT-2563 #3: the instance/class side is chosen by which method list the
    // backend merges into (driven by `is_class_method`), NOT by `MethodKind`.
    // A class-side patch must REPLACE the existing class method, never push a
    // duplicate alongside it — there is no "kind trap".
    let class_source = "Actor subclass: Counter\n  state: count = 0\n\n  class create -> Nil =>\n    Counter new\n\n  increment => count := count + 1";
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(class_source)),
        (atom("method_source"), binary("create -> Nil =>\n  42")),
        (atom("is_class_method"), atom("true")),
    ]));
    let response = handle_request(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    assert_eq!(
        response_field_str(&response, "selector").as_deref(),
        Some("create")
    );
    let merged = response_field_str(&response, "merged_class_source").expect("merged");
    // Exactly one class-side `create` — replaced in place, not duplicated.
    assert_eq!(
        merged.matches("class create").count(),
        1,
        "class-side method duplicated instead of replaced:\n{merged}"
    );
    assert!(
        merged.contains("42"),
        "class-side method body not updated:\n{merged}"
    );
    // The instance method is untouched.
    assert!(
        merged.contains("increment"),
        "instance method dropped:\n{merged}"
    );
}

#[test]
fn compile_method_annotates_patched_method_at_its_merged_line() {
    // BT-2563 #1: the patched method is parsed standalone, so its raw span
    // indexes into the bare snippet (line 1). Codegen must annotate the
    // method's message sends with the line they occupy in the MERGED class
    // source (the send carries the BEAM stacktrace line), not line 1.
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (
            atom("method_source"),
            binary("touch => self.events isEmpty"),
        ),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    let merged = response_field_str(&response, "merged_class_source").expect("merged");
    let touch_line = merged
        .lines()
        .position(|l| l.contains("touch"))
        .map(|i| i + 1)
        .expect("touch present in merged source");
    assert!(
        touch_line > 1,
        "expected the patched method below line 1 in the merged source:\n{merged}"
    );
    let core = response_field_str(&response, "core_erlang").expect("core erlang");
    // Bare line annotation (no source_path in this request): `-| [<line>]`.
    assert!(
        core.contains(&format!(" -| [{touch_line}]")),
        "patched method not annotated at its merged line {touch_line}:\n{core}"
    );
}

#[test]
fn compile_method_class_context_diagnostic_resolves_against_merged_source() {
    // BT-2563 #2: a post-merge semantic diagnostic whose span lands in the
    // CLASS (not the patched method body) must resolve against the merged
    // class source, not the short `method_source`. The one-class-per-file
    // error points at the second class declaration, several lines into the
    // merged source; the buggy path rendered it against the 1-line method
    // body and clamped it to line 1.
    let two = "Actor subclass: Alpha\n  state: a = 1\n\n  va => self.a\n\n\
                   Actor subclass: Beta\n  state: b = 2\n\n  vb => self.b";
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(two)),
        (atom("class_name"), binary("Alpha")),
        (atom("method_source"), binary("bumped => self.a + 1")),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
    let list = response_diagnostics(&response).expect("diagnostics");
    let line = list.elements.iter().find_map(|d| {
        if let Term::Map(m) = d {
            if let Some(Term::FixInteger(i)) = map_get(m, "line") {
                return Some(i.value);
            }
        }
        None
    });
    // The second class declaration sits several lines into the merged source
    // (`Beta` is below all of `Alpha` plus the patched method), so a correctly
    // resolved span lands well past the 1-line method body. The buggy path
    // clamped this class-context span into the method body (line 1); requiring
    // it to be clearly past the method body — without hardcoding the exact line,
    // which depends on the unparser's blank-line output — guards the regression.
    assert!(
        line.is_some_and(|l| l >= 5),
        "class-context diagnostic clamped into the method body instead of \
             resolving against the merged source: {response:?}"
    );
}

#[test]
fn compile_method_body_diagnostic_is_method_relative() {
    // BT-2563 #2: a semantic error INSIDE the patched method body must report a
    // line relative to the method snippet the user is editing — not its absolute
    // line in the merged class. `probe` is appended below the multi-line
    // `initialize` method, so its absolute merged line is well past 2; the
    // undefined-variable error on the method's second line must still report
    // line 2 (`beamtalk_repl_compiler:format_diagnostic_text/1` renders it as
    // "Line 2: ...", matching the method editor's view).
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (
            atom("method_source"),
            binary("probe =>\n    undefinedLocal\n    self.events"),
        ),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "resp: {response:?}"
    );
    let list = response_diagnostics(&response).expect("diagnostics");
    let (msg, line) = list
        .elements
        .iter()
        .find_map(|d| {
            if let Term::Map(m) = d {
                let msg = match map_get(m, "message") {
                    Some(Term::Binary(b)) => String::from_utf8_lossy(&b.bytes).into_owned(),
                    _ => return None,
                };
                let line = match map_get(m, "line") {
                    Some(Term::FixInteger(i)) => i.value,
                    _ => return None,
                };
                Some((msg, line))
            } else {
                None
            }
        })
        .expect("a structured diagnostic with message + line");
    assert!(
        msg.contains("undefinedLocal"),
        "expected the undefined-variable diagnostic, got: {msg}"
    );
    // Method-relative: the error is on the method's second line. The absolute
    // line in the merged class is larger (the method is appended below
    // `initialize`); reporting 2 proves the span was rebased into method space.
    assert_eq!(
        line, 2,
        "method-body diagnostic not reported method-relative: {response:?}"
    );
}

#[test]
fn compile_method_rejects_non_method_source() {
    let request = Term::from(Map::from([
        (atom("command"), atom("compile_method")),
        (atom("class_source"), binary(COMPILE_METHOD_CLASS)),
        (atom("method_source"), binary("1 + 1")),
        (atom("is_class_method"), atom("false")),
    ]));
    let response = handle_request(&request);
    assert_eq!(response_status(&response).as_deref(), Some("error"));
}
