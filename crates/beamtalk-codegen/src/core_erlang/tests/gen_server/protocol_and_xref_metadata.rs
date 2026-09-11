// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Protocol-only module registration and the `methodXref`/
//! `stateVarXref` metadata baked into `register_class/0` (ADR 0087).

use super::*;

/// A module with only Protocol definitions (no classes) should still
/// generate `register_class/0` that registers the protocols.
#[test]
fn protocol_only_module_generates_register_class() {
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Displayable", Span::new(0, 0)),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let result = generate_module(&module, CodegenOptions::new("bt@proto_only"));
    assert!(
        result.is_ok(),
        "Protocol-only module should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();

    // Should have register_class/0 in exports
    assert!(
        code.contains("'register_class'/0"),
        "Should export register_class/0. Got:\n{code}"
    );

    // Should have on_load attribute
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute. Got:\n{code}"
    );

    // Should call beamtalk_protocol_registry:register_protocol
    assert!(
        code.contains("'beamtalk_protocol_registry':'register_protocol'"),
        "Should call register_protocol. Got:\n{code}"
    );

    // Should reference the Displayable protocol name
    assert!(
        code.contains("'Displayable'"),
        "Should reference protocol name. Got:\n{code}"
    );

    // Should include required_class_methods key
    assert!(
        code.contains("'required_class_methods'"),
        "Should include required_class_methods key. Got:\n{code}"
    );

    // Should NOT have class builder calls (no classes)
    assert!(
        !code.contains("'beamtalk_class_builder':'register'"),
        "Should not call class_builder:register. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::similar_names)]
fn test_bt_1944_typed_param_does_not_change_actor_codegen() {
    // Type annotations on method params should be erasable — they
    // must NOT change the generated Core Erlang dispatch/body code for actors.
    // Uses a multi-keyword method matching the original reproducer:
    // `executeActivity:selector:args:timeout:` with `:: Integer | Nil` on last param.
    let untyped_src = concat!(
        "Actor subclass: TestActor\n",
        "  state: count = 0\n",
        "  executeActivity: act selector: sel args: a timeout: t =>\n",
        "    self.count := self.count + 1\n",
        "    t\n",
    );
    let typed_src = concat!(
        "Actor subclass: TestActor\n",
        "  state: count = 0\n",
        "  executeActivity: act selector: sel args: a timeout: t :: Integer | Nil =>\n",
        "    self.count := self.count + 1\n",
        "    t\n",
    );

    let tokens_u = beamtalk_core::source_analysis::lex_with_eof(untyped_src);
    let (module_u, _) = beamtalk_core::source_analysis::parse(tokens_u);
    let code_u = generate_module(
        &module_u,
        CodegenOptions::new("test_actor").with_workspace_mode(true),
    )
    .expect("untyped should compile");

    let tokens_t = beamtalk_core::source_analysis::lex_with_eof(typed_src);
    let (module_t, _) = beamtalk_core::source_analysis::parse(tokens_t);
    let code_t = generate_module(
        &module_t,
        CodegenOptions::new("test_actor").with_workspace_mode(true),
    )
    .expect("typed should compile");

    // Strip metadata lines that naturally differ (source text, param types).
    // Everything else — dispatch, body, exports — must be identical.
    let strip_metadata = |code: &str| -> String {
        code.lines()
            .filter(|line| {
                !line.contains("'param_types'")
                    && !line.contains("'methodSource'")
                    && !line.contains("'methodSignatures'")
                    // ADR 0087 Phase 2: the typed param adds a class
                    // reference in its type annotation, which methodXref records.
                    // That is metadata, not dispatch/body, so strip it too.
                    && !line.contains("'methodXref'")
            })
            .collect::<Vec<_>>()
            .join("\n")
    };

    let code_u_stripped = strip_metadata(&code_u);
    let code_t_stripped = strip_metadata(&code_t);

    assert_eq!(
        code_u_stripped, code_t_stripped,
        "BT-1944: Typed param changed dispatch/body code (beyond metadata)"
    );

    // Actor instance methods should NOT generate spec attributes — methods are
    // dispatch clauses inside safe_dispatch/3, not standalone functions.
    assert!(
        !code_t.contains("'spec' ="),
        "BT-1944: Actor instance method should NOT generate spec attribute"
    );
}

/// ADR 0087 Phase 2: `register_class/0` bakes a `methodXref` field
/// into the `BuilderState` map. Each entry records the method's defining line,
/// the selectors it sends (with receiver kind), and class references — all with
/// `source_status => indexed`.
#[test]
fn test_method_xref_baked_into_register_class() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: count = 0\n\n",
        "  increment =>\n",
        "    self.count := self.count + 1\n\n",
        "  class default -> Counter =>\n",
        "    Counter new\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // The methodXref field is present and a list (not a `~{ }~` map).
    assert!(
        code.contains("'methodXref' => ["),
        "Should bake a methodXref list. Got:\n{code}"
    );
    // The instance method `increment` is recorded, instance-side, indexed.
    assert!(
        code.contains("'selector' => 'increment'"),
        "increment entry missing. Got:\n{code}"
    );
    assert!(
        code.contains("'class_side' => 'false'"),
        "instance-side entry should carry 'class_side' => 'false'. Got:\n{code}"
    );
    // The `+` send inside `increment` is recorded with a self receiver kind
    // (it is sent to `self.count`, an `other` receiver — the field access).
    assert!(
        code.contains("'selector' => '+'"),
        "the `+` send should be recorded. Got:\n{code}"
    );
    assert!(
        code.contains("'recv_kind' =>"),
        "sends should carry a recv_kind. Got:\n{code}"
    );
    // The class-side method `default` references `Counter` (return type + body).
    assert!(
        code.contains("'class_side' => 'true'"),
        "class-side entry should carry 'class_side' => 'true'. Got:\n{code}"
    );
    assert!(
        code.contains("'class' => 'Counter'"),
        "the Counter reference should be recorded. Got:\n{code}"
    );
    // The user-authored rows are `indexed`.
    assert!(
        code.contains("'source_status' => 'indexed'"),
        "rows should be tagged indexed. Got:\n{code}"
    );
    // `Counter` no longer carries synthetic class-side rows for
    // `new`/`new:`/`spawn`/`spawnWith:` — those bodies are real,
    // source-backed class methods on `Actor` itself
    // (`stdlib/src/actor.bt`), so a subclass like `Counter` genuinely
    // *inherits* them rather than *defining* them, and its own methodXref
    // carries no row for them at all (the honest Smalltalk answer). Bound the methodXref
    // payload to the next class-info field (`'classState'`) so the assertions
    // below cannot be satisfied by unrelated parts of the generated module.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // The optional synthetic_origin key is omitted for the user-authored indexed
    // rows. Scope the check to the `increment` row (an indexed user method) so it
    // is not tripped by unrelated nested rows. The increment row runs from its
    // `'selector' => 'increment'` key up to the start of the next xref row — NOT
    // the first nested `}~`, which would truncate the slice mid-row inside the
    // `sends` list.
    let inc_pos = mx_seg
        .find("'selector' => 'increment'")
        .expect("increment row present");
    let inc_after = &mx_seg[inc_pos..];
    // A `}~, ~{` sequence is the *row* separator in the methodXref list — it only
    // appears between top-level rows, never inside one (nested `sends`/`references`
    // maps close with `}~]`, not `}~, ~{`). Bounding here keeps the slice to the
    // single increment row instead of truncating at the first nested `}~`.
    let inc_row = match inc_after.find("}~, ~{") {
        Some(end) => &inc_after[..end],
        None => inc_after,
    };
    assert!(
        !inc_row.contains("synthetic_origin"),
        "synthetic_origin must be omitted for the indexed increment row. Got:\n{inc_row}"
    );
    // `new`/`new:`/`spawn`/`spawnWith:` are inherited from `Actor`,
    // not defined by `Counter` — no top-level row for them, synthetic or
    // otherwise. Match on the `class_side` + `selector` pair (not just
    // `'selector' => '<sel>'` in isolation) so a legitimate nested `sends`
    // entry — e.g. the `default` class method's own `Counter new` send,
    // which also carries a `'selector' => 'new'` key — is not a false
    // positive: only top-level methodXref rows carry `class_side`.
    for sel in ["new", "new:", "spawn", "spawnWith:"] {
        assert!(
            !mx_seg.contains(&format!("'class_side' => 'true', 'selector' => '{sel}'")),
            "`{sel}` is inherited from Actor and must not appear as a Counter class-side row. Got:\n{mx_seg}"
        );
    }
}

/// `register_class/0` bakes a `stateVarXref` field into the
/// `BuilderState` map, the state-var analogue of `methodXref` — one row per
/// declared instance variable, carrying its name and 1-based declaration
/// line. Covers both a defaulted `state:` slot and a typed slot with *no*
/// default value (`state: name :: Type`, no `= ...`) — the exact shape the
/// VS Code sidebar's `findStateVarDeclaration` regex fails to match, which
/// is precisely why this baked line data exists: so
/// `beamtalk.navigateToStateVar` no longer needs that regex to succeed.
#[test]
fn test_state_var_xref_baked_into_register_class() {
    let src = concat!(
        "Actor subclass: Widget\n",            // line 1
        "  state: count = 0\n",                // line 2
        "  state: engine :: WorkflowEngine\n", // line 3 (no default, `::` type)
        "\n",
        "  increment =>\n",
        "    self.count := self.count + 1\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("widget").with_source(src))
        .expect("codegen should succeed");

    assert!(
        code.contains("'stateVarXref' => ["),
        "Should bake a stateVarXref list. Got:\n{code}"
    );
    let sv_start = code
        .find("'stateVarXref' => [")
        .expect("stateVarXref present");
    let sv_tail = &code[sv_start..];
    let sv_seg = &sv_tail[..sv_tail.find("'classState'").unwrap_or(sv_tail.len())];

    assert!(
        sv_seg.contains("'name' => 'count', 'line' => 2"),
        "count should be recorded at line 2. Got:\n{sv_seg}"
    );
    assert!(
        sv_seg.contains("'name' => 'engine', 'line' => 3"),
        "a defaultless, `::`-typed slot should still be recorded, at line 3. Got:\n{sv_seg}"
    );
}

/// ADR 0087 Phase 6: compiler-generated auto-accessors for a
/// `Value subclass:` class ride the `method_xref` write path with
/// `source_status => synthetic` and a derived `synthetic_origin` line pointing
/// at the generating slot declaration. They are included by default — the
/// documented parity exception that makes `implementorsOf:` on an auto-accessor
/// non-empty.
#[test]
fn test_method_xref_emits_synthetic_accessors_for_value_class() {
    let src = concat!(
        "Value subclass: Point\n",
        "  state: x :: Integer = 0\n",
        "  state: y :: Integer = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("point")).expect("codegen should succeed");

    // Scope the assertions to the `methodXref` payload so they cannot be
    // satisfied by unrelated parts of the generated module (exports, dispatch,
    // method signatures, etc.). The payload runs from `'methodXref' => [` up to
    // the next class-info field, `'classState'`.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // Synthetic getter rows for both slots, tagged synthetic.
    assert!(
        mx_seg.contains("'source_status' => 'synthetic'"),
        "auto-accessors should be tagged synthetic. Got:\n{mx_seg}"
    );
    // Getter selectors `x` and `y` are both present as synthetic rows.
    assert!(
        mx_seg.contains("'selector' => 'x'"),
        "getter `x` synthetic row missing. Got:\n{mx_seg}"
    );
    assert!(
        mx_seg.contains("'selector' => 'y'"),
        "getter `y` synthetic row missing. Got:\n{mx_seg}"
    );
    // The `with*:` setter selectors are emitted.
    assert!(
        mx_seg.contains("'selector' => 'withX:'") && mx_seg.contains("'selector' => 'withY:'"),
        "setter rows `withX:` / `withY:` missing. Got:\n{mx_seg}"
    );
    // Every synthetic row carries a derived synthetic_origin line.
    assert!(
        mx_seg.contains("'synthetic_origin' =>"),
        "synthetic rows must carry synthetic_origin. Got:\n{mx_seg}"
    );
    // Accessors delegate to runtime map primitives — no Beamtalk sends.
    assert!(
        mx_seg.contains("'sends' => []"),
        "synthetic accessors should have empty sends. Got:\n{mx_seg}"
    );
    // The slot's declared type `Integer` is recorded as a reference on the
    // accessor (return/param type), like a hand-written typed accessor.
    assert!(
        mx_seg.contains("'class' => 'Integer'"),
        "slot type `Integer` should be a reference on the accessor. Got:\n{mx_seg}"
    );
}

/// ADR 0087 Phase 6: an `Object subclass:` (not a value class) gets no
/// auto-accessors, so no synthetic rows are emitted.
#[test]
fn test_method_xref_no_synthetic_rows_for_object_class() {
    let src = concat!(
        "Object subclass: Plain\n",
        "  state: count = 0\n\n",
        "  bump =>\n    count := count + 1\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("plain")).expect("codegen should succeed");

    assert!(
        !code.contains("'source_status' => 'synthetic'"),
        "non-value classes must not emit synthetic accessor rows. Got:\n{code}"
    );
}

/// ADR 0087 Phase 6: a user-defined accessor suppresses the synthetic
/// one for that slot — `compute_auto_slot_methods` already excludes hand-defined
/// selectors, so the synthetic emission must not double-emit. The hand-written
/// `x` getter is `indexed`, and there is no synthetic `x` row.
#[test]
fn test_method_xref_user_accessor_suppresses_synthetic() {
    let src = concat!(
        "Value subclass: Point\n",
        "  state: x :: Integer = 0\n\n",
        "  x =>\n    self.x\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("point")).expect("codegen should succeed");

    // The hand-written `x` getter is an indexed row; the synthetic getter for
    // `x` is suppressed. The `withX:` setter is still synthetic.
    //
    // Scope to the `methodXref` payload (between `'methodXref' => [` and the
    // next class-info field `'classState'`) so a global `'withX:'` substring in
    // exports/dispatch cannot satisfy the assertion.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];

    // Exactly one synthetic row survives.
    let synthetic_count = mx_seg.matches("'source_status' => 'synthetic'").count();
    assert_eq!(
        synthetic_count, 1,
        "only the `withX:` setter should be synthetic (user defined `x`). Got:\n{mx_seg}"
    );

    // Prove that sole synthetic row is the `withX:` setter, not some other
    // selector: isolate the row containing the synthetic marker (each row is a
    // `~{ ... }~` map) and check its `selector`.
    let synth_marker = mx_seg
        .find("'source_status' => 'synthetic'")
        .expect("synthetic marker present");
    // Each row map opens with `~{'class_side' =>`; nested `~{...}~` reference
    // maps do not, so anchor on the row prefix to isolate the owning row.
    let row_start = mx_seg[..synth_marker]
        .rfind("~{'class_side' =>")
        .expect("synthetic row opens with ~{'class_side' =>");
    let row = &mx_seg[row_start..synth_marker];
    assert!(
        row.contains("'selector' => 'withX:'"),
        "the surviving synthetic row must be the `withX:` setter. Got:\n{row}"
    );
}

/// ADR 0087 Phase 2: a send to a selector longer than the 255-byte
/// Erlang atom limit (e.g. a 20-keyword auto-constructor) must be dropped from
/// the xref `sends` list — emitting it as an atom would fail `core_scan` at
/// BEAM-compile time. The generated Core Erlang must still be well-formed.
#[test]
fn test_method_xref_drops_oversized_selectors() {
    // Build a keyword send whose concatenated selector exceeds 255 bytes.
    use std::fmt::Write as _;
    let mut send_parts = String::new();
    for i in 0..40 {
        write!(send_parts, " longKeywordPartNumber{i}: x{i}").unwrap();
    }
    let src = format!(
        concat!(
            "Actor subclass: BigSend\n",
            "  state: count = 0\n\n",
            "  run: target =>\n    target{}\n"
        ),
        send_parts
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("big_send")).expect("codegen should succeed");

    // The methodXref field is still emitted, and the oversized selector is
    // dropped from it: the single method's `sends` list is empty.
    let mx_start = code.find("'methodXref' => [").expect("methodXref present");
    let mx_tail = &code[mx_start..];
    let mx_seg = &mx_tail[..mx_tail.find("'classState'").unwrap_or(mx_tail.len())];
    assert!(
        !mx_seg.contains("longKeywordPartNumber"),
        "oversized selector must be dropped from methodXref. Got:\n{mx_seg}"
    );
    assert!(
        mx_seg.contains("'sends' => []"),
        "the only send was oversized, so sends should be empty. Got:\n{mx_seg}"
    );
}
