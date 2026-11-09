// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4292: the dynamic-site report (`BEAMTALK_CODEGEN_DIAGNOSTICS=1`) names
//! each dynamic send's receiver type and the gate that kept it dynamic, and
//! the routing-table-driven type-pattern guards close the gaps the shared
//! fixture records (the `'__class_mod__'` map, the future tuple).

use super::codegen;

/// The dynamic-dispatch hints of compiling `src` with diagnostics on.
fn report(src: &str) -> Vec<String> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let result = crate::core_erlang::generate_module_with_warnings(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_codegen_diagnostics(true),
    )
    .expect("codegen should succeed");
    result
        .warnings
        .into_iter()
        .map(|w| w.message.to_string())
        .filter(|m| m.contains("dynamic dispatch"))
        .collect()
}

/// The dynamic-dispatch hints of compiling `src` with diagnostics on, after
/// the full analysis pass (`analyse_full`) — needed for anything the plain
/// self-sufficient path does not register, such as a module's type aliases.
fn report_full(src: &str) -> Vec<String> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let analysis = beamtalk_core::semantic_analysis::analyse_full(
        &module,
        beamtalk_core::semantic_analysis::AnalysisContext::default(),
    );
    let result = crate::core_erlang::generate_module_with_warnings(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_analysis(analysis)
            .with_codegen_diagnostics(true),
    )
    .expect("codegen should succeed");
    result
        .warnings
        .into_iter()
        .map(|w| w.message.to_string())
        .filter(|m| m.contains("dynamic dispatch"))
        .collect()
}

/// The one hint for `selector`.
fn hint_for(src: &str, selector: &str) -> String {
    let hints = report(src);
    let needle = format!("Send '{selector}'");
    let matching: Vec<&String> = hints.iter().filter(|h| h.starts_with(&needle)).collect();
    assert_eq!(
        matching.len(),
        1,
        "expected one hint for {selector}, got {hints:?}"
    );
    matching[0].clone()
}

#[test]
fn untyped_receiver_reports_untyped() {
    let hint = hint_for(
        "Object subclass: Caller\n  run: x => x frobnicate\n",
        "frobnicate",
    );
    assert!(hint.contains("reason: untyped"), "{hint}");
    assert!(hint.contains("receiver: Dynamic"), "{hint}");
}

#[test]
fn routed_builtin_send_is_converted() {
    // BT-4294: a built-in with a single route (`Dictionary`: the untagged-map
    // guard -> bt@stdlib@dictionary) takes the representation-guarded direct
    // call, so the send is no longer a dynamic site.
    let hints = report("Object subclass: Caller\n  run: d :: Dictionary => d at: #k put: 1\n");
    assert!(
        !hints.iter().any(|h| h.starts_with("Send 'at:put:'")),
        "{hints:?}"
    );
}

#[test]
fn builtin_receiver_reports_its_routing_row() {
    // BT-4390: a `String` special selector names itself, not the whole row.
    let hint = hint_for(
        "Object subclass: Caller\n  run: s :: String => s asBase64\n",
        "asBase64",
    );
    assert!(
        hint.contains(
            "reason: builtin — String: the dynamic path answers 'asBase64' outside bt@stdlib@string"
        ),
        "{hint}"
    );
    assert!(hint.contains("receiver: String"), "{hint}");
    assert!(hint.contains("declared]"), "{hint}");
}

#[test]
fn string_and_pid_ordinary_selectors_are_converted() {
    // BT-4390: only the fixture's special selectors keep a `String` or `Pid`
    // send dynamic; any other selector takes the guarded direct call.
    let hints = report(concat!(
        "Object subclass: Caller\n",
        "  run: s :: String => s size\n",
        "  go: p :: Pid => p frobnicate\n",
    ));
    assert!(
        !hints
            .iter()
            .any(|h| h.starts_with("Send 'size'") || h.starts_with("Send 'frobnicate'")),
        "{hints:?}"
    );
}

#[test]
fn binary_sends_are_converted() {
    // BT-4510: `Binary`'s two routes (a bare binary, the `String` tag) are
    // both guarded, so a `Binary` send is no longer a dynamic site.
    let hints = report("Object subclass: Caller\n  run: b :: Binary => b frobnicate\n");
    assert!(
        !hints.iter().any(|h| h.starts_with("Send 'frobnicate'")),
        "{hints:?}"
    );
}

#[test]
fn builtin_without_a_single_module_reports_why() {
    let hint = hint_for(
        "Object subclass: Caller\n  run: b :: Boolean => b frobnicate\n",
        "frobnicate",
    );
    assert!(
        hint.contains("reason: builtin — Boolean: true and false"),
        "{hint}"
    );
}

#[test]
fn builtin_special_selector_reports_itself() {
    // `Pid` answers the future protocol through `beamtalk_future`, so
    // `whenResolved:` stays dynamic; the reason names that one selector (the
    // old whole-row wording read "and 0 other selectors" for a one-entry
    // list).
    let hint = hint_for(
        "Object subclass: Caller\n  run: p :: Pid => p whenResolved: [:v | v]\n",
        "whenResolved:",
    );
    assert!(
        hint.contains(
            "reason: builtin — Pid: the dynamic path answers 'whenResolved:' outside bt@stdlib@pid"
        ),
        "{hint}"
    );
}

#[test]
fn open_value_class_send_is_converted() {
    // BT-4293: an open Value class takes the guarded direct call, so the
    // send is no longer a dynamic site (only the guard-miss fallback is).
    let hints = report(concat!(
        "Object subclass: Caller\n",
        "  run: p :: OpenPoint => p frobnicate\n\n",
        "Value subclass: OpenPoint\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !hints.iter().any(|h| h.starts_with("Send 'frobnicate'")),
        "{hints:?}"
    );
}

#[test]
fn object_receiver_reports_root() {
    // `Object` is not abstract, but its static type admits raw terms (an
    // ancestor of representation-routed built-ins), so it stays dynamic.
    let hint = hint_for(
        "Object subclass: Caller\n  run: o :: Object => o frobnicate\n",
        "frobnicate",
    );
    assert!(hint.contains("reason: root — Object"), "{hint}");
}

#[test]
fn abstract_class_reports_abstract() {
    let hint = hint_for(
        concat!(
            "Object subclass: Caller\n",
            "  run: s :: Shape => s area\n\n",
            "abstract Value subclass: Shape\n",
            "  area => self subclassResponsibility\n",
        ),
        "area",
    );
    assert!(hint.contains("reason: abstract — Shape"), "{hint}");
}

#[test]
fn actor_receiver_reports_actor() {
    let hint = hint_for(
        concat!(
            "Object subclass: Caller\n",
            "  run: c :: Ctr => c frobnicate\n\n",
            "Actor subclass: Ctr\n",
            "  state: n = 0\n",
        ),
        "frobnicate",
    );
    assert!(hint.contains("reason: actor — Ctr"), "{hint}");
}

#[test]
fn supervisor_subclass_reports_supervisor() {
    let hint = hint_for(
        concat!(
            "Object subclass: Caller\n",
            "  run: s :: App => s frobnicate\n\n",
            "Supervisor subclass: App\n",
            "  class children => #()\n",
        ),
        "frobnicate",
    );
    assert!(
        hint.contains("reason: supervisor — App instances are Supervisor handles"),
        "{hint}"
    );
}

#[test]
fn unsealed_exception_subclass_reports_exception() {
    // `Error` and most user exceptions are unsealed: lifting the sealed gate
    // would still leave them dynamic, so they count as `exception`, not `open`.
    let hint = hint_for(
        concat!(
            "Object subclass: Caller\n",
            "  run: e :: AppError => e frobnicate\n\n",
            "Error subclass: AppError\n",
        ),
        "frobnicate",
    );
    assert!(
        hint.contains("reason: exception — AppError dispatches through bt@stdlib@exception"),
        "{hint}"
    );
}

#[test]
fn guarded_direct_send_is_not_reported() {
    let hints = report(concat!(
        "Object subclass: Caller\n",
        "  run: w :: SealedWidget => w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        hints.iter().all(|h| !h.starts_with("Send 'x'")),
        "a guarded direct call is not a dynamic site: {hints:?}"
    );
}

#[test]
fn dictionary_type_pattern_rejects_class_mod_maps() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  run: v => v match: [d :: Dictionary -> 1; _ -> 2]\n",
    ));
    assert!(
        code.contains("'is_map_key'('__class_mod__'"),
        "the Dictionary test must reject a '__class_mod__' map. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn tuple_type_pattern_rejects_every_reserved_tag() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  run: v => v match: [t :: Tuple -> 1; _ -> 2]\n",
    ));
    for tag in [
        "beamtalk_string",
        "beamtalk_future",
        "beamtalk_object",
        "beamtalk_supervisor",
    ] {
        assert!(
            code.contains(&format!("<'{tag}'> when 'true' -> 'false'")),
            "the Tuple test must reject the reserved tag {tag}. Got:\n{code}"
        );
    }
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn alias_typed_builtin_send_is_converted() {
    // BT-4412: a receiver declared through a type alias of a single-route
    // built-in carries `Aliased` provenance; it takes the same guarded direct
    // call as the unaliased type instead of reporting `untyped`.
    let hints = report_full(concat!(
        "type Pair = Tuple(Integer, Integer)\n",
        "type Opts = Dictionary(Symbol, Integer)\n\n",
        "Object subclass: Caller\n",
        "  first: p :: Pair => p at: 1\n",
        "  put: d :: Opts => d at: #k put: 1\n",
    ));
    assert!(
        !hints
            .iter()
            .any(|h| h.starts_with("Send 'at:'") || h.starts_with("Send 'at:put:'")),
        "{hints:?}"
    );
}

#[test]
fn alias_typed_value_class_send_is_converted() {
    // BT-4412: an alias naming a tagged-map Value class is trusted like the
    // class name itself.
    let hints = report_full(concat!(
        "type Point = OpenPoint\n\n",
        "Object subclass: Caller\n",
        "  run: p :: Point => p frobnicate\n\n",
        "Value subclass: OpenPoint\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !hints.iter().any(|h| h.starts_with("Send 'frobnicate'")),
        "{hints:?}"
    );
}

#[test]
fn alias_of_a_union_stays_dynamic() {
    // BT-4412: trusting `Aliased` does not widen anything else — an alias
    // whose expansion is a union is not a single `Known` class.
    let hints = report_full(concat!(
        "type Either = Integer | String\n\n",
        "Object subclass: Caller\n",
        "  run: v :: Either => v frobnicate\n",
    ));
    assert!(
        hints.iter().any(|h| h.starts_with("Send 'frobnicate'")),
        "{hints:?}"
    );
}
