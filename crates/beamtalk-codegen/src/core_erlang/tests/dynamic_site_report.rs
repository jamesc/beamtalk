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
fn builtin_receiver_reports_its_routing_row() {
    let hint = hint_for(
        "Object subclass: Caller\n  run: d :: Dictionary => d at: #k put: 1\n",
        "at:put:",
    );
    assert!(
        hint.contains(
            "reason: builtin — representation-routed: untagged_map guard -> bt@stdlib@dictionary"
        ),
        "{hint}"
    );
    assert!(hint.contains("receiver: Dictionary"), "{hint}");
    assert!(hint.contains("declared]"), "{hint}");
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
fn builtin_special_selector_is_named() {
    let hint = hint_for(
        "Object subclass: Caller\n  run: s :: String => s asHex\n",
        "asHex",
    );
    assert!(
        hint.contains("reason: builtin — String: 'asHex' is answered outside bt@stdlib@string"),
        "{hint}"
    );
}

#[test]
fn open_value_class_reports_open() {
    let hint = hint_for(
        concat!(
            "Object subclass: Caller\n",
            "  run: p :: OpenPoint => p frobnicate\n\n",
            "Value subclass: OpenPoint\n",
            "  field: x :: Integer = 0\n",
        ),
        "frobnicate",
    );
    assert!(
        hint.contains("reason: open — OpenPoint is not sealed"),
        "{hint}"
    );
    assert!(hint.contains("[Value, declared]"), "{hint}");
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
