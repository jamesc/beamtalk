// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `match:` pattern-kind codegen: array/map/nil/type/tagged-class
//! patterns, guard chains, and cross-file class-hierarchy pattern
//! resolution.

use super::*;

// match: arms with array and map patterns

#[test]
fn test_match_array_pattern_arm_generates_is_map_guard_chain() {
    // `arr match: [#[h, t] -> h + t; _ -> 0]` should compile to a
    // conditional chain using is_map + maps:get('$beamtalk_class', ..., 'undefined') + size check.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[h, t] -> h + t;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Should emit is_map guard check for array pattern",
        &code,
        &["call 'erlang':'is_map'("],
    );
    assert_has(
        "Should check '$beamtalk_class' key for array type guard",
        &code,
        &["'$beamtalk_class'"],
    );
    assert_has(
        "Should check array size",
        &code,
        &["call 'beamtalk_array':'size'("],
    );
    assert_has(
        "Should extract elements via at: dispatch",
        &code,
        &["'at:'"],
    );
}

#[test]
fn test_match_map_pattern_arm_generates_core_erlang_map_pattern() {
    // `d match: [#{#event => evName} -> evName; _ -> "none"]`
    // should compile to a native Core Erlang map pattern `~{'event' := EvName}~`.
    let src = "Object subclass: Foo\n  test: d =>\n    d match: [\n      #{#event => evName} -> evName;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Should emit Core Erlang map pattern ~{...}~",
        &code,
        &["~{"],
    );
    assert_has(
        "Should use := binding syntax in map pattern",
        &code,
        &[":="],
    );
    assert_has(
        "Should include the 'event' key in the map pattern",
        &code,
        &["'event'"],
    );
}

#[test]
fn test_match_array_pattern_fallthrough_to_wildcard() {
    // When the array pattern fails (wrong type/size), execution must
    // fall through to the next arm — not crash.
    // Wildcard fallback arm should be present in the generated code.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      #[a, b] -> a + b;\n      _ -> 42\n    ]\n";
    let code = codegen_named(src, "foo");

    // The fallback (42) should appear in the generated code
    assert_has(
        "Fallback value should appear in generated code",
        &code,
        &["42"],
    );
    // The is_map check should be present — if false, falls through to 42
    assert_has(
        "Should emit is_map check",
        &code,
        &["call 'erlang':'is_map'("],
    );
}

#[test]
fn test_match_nested_array_pattern_arm() {
    // `arr match: [#[#[a, b], c] -> a+b+c; _ -> 0]`
    // should generate nested is_map + size checks for the inner array.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[#[a, b], c] -> a + b + c;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    // Should have multiple is_map checks (outer + inner array)
    let is_map_count = code.matches("call 'erlang':'is_map'(").count();
    assert!(
        is_map_count >= 2,
        "Should emit at least 2 is_map checks for nested array. Found {is_map_count}. Got:\n{code}"
    );
    // Should have multiple beamtalk_array:size calls (outer + inner)
    let size_count = code.matches("call 'beamtalk_array':'size'(").count();
    assert!(
        size_count >= 2,
        "Should emit at least 2 size checks for nested array. Found {size_count}. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_uses_maps_get_with_default_not_map_get() {
    // The class-tag lookup must use maps:get/3 with a default value so
    // that a plain Erlang map (Beamtalk Dictionary) as the match subject does not
    // crash with {badkey, '$beamtalk_class'} — it should fall through instead.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      #[a, b] -> a + b;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    // Verify the 3-arg form maps:get/3 is used — the key, the map, AND the default 'undefined'.
    // This prevents regression to maps:get/2 or erlang:map_get/2 (both throw on missing key).
    assert!(
        code.contains("call 'maps':'get'('$beamtalk_class',") && code.contains(", 'undefined')"),
        "Should use maps:get/3 with 'undefined' default. Got:\n{code}"
    );
    assert_lacks(
        "Must NOT use erlang:map_get/2 — throws badkey when '$beamtalk_class' absent",
        &code,
        &["call 'erlang':'map_get'("],
    );
}

#[test]
fn test_match_array_pattern_duplicate_variable_emits_equality_check() {
    // `arr match: [#[x, x] -> "equal"; _ -> "differ"]`
    // The second occurrence of `x` must emit an `erlang:=:=` equality check
    // rather than a bare re-binding.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[x, x] -> \"equal\";\n      _ -> \"differ\"\n    ]\n";
    let code = codegen_named(src, "foo");

    // Must emit a strict-equality call for the duplicate variable.
    assert_has(
        "Should emit erlang:=:= equality check for duplicate variable",
        &code,
        &["call 'erlang':'=:='("],
    );
    // The primary binding `let X = ... at: [1]` should appear.
    assert_has(
        "Should extract first element for primary binding",
        &code,
        &["'at:', [1]"],
    );
    // A second extraction for position 2 must also be present (into a temp var).
    assert_has(
        "Should extract second element into temp for equality check",
        &code,
        &["'at:', [2]"],
    );
}

// ADR 0107 Phase A: `Pattern::Nil` and `Pattern::Type` codegen

#[test]
fn test_match_nil_pattern_compiles_to_atom_literal() {
    // ADR 0107 Phase A: `nil` pattern reuses the existing atom-literal
    // codegen path verbatim — no new runtime mechanism.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      _ -> 1\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "nil pattern should compile to the atom-literal case pattern 'nil'",
        &code,
        &["<'nil'>"],
    );
}

// ADR 0107 Phase A: `Pattern::Type` codegen (`generate_type_pattern`)

#[test]
fn test_match_type_pattern_string_compiles_to_tag_test() {
    // BT-4173: `s :: String` compiles to the `{'beamtalk_string', _}`
    // tag test — a bare binary is a `Binary`, never a `String`, so the
    // old `is_binary` BIF test is wrong.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: String -> s;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "String type pattern should test the beamtalk_string tag",
        &code,
        &["'beamtalk_string'"],
    );
    assert_lacks(
        "String type pattern must not test is_binary (bare binaries are Binary)",
        &code,
        &["'erlang':'is_binary'"],
    );
}

#[test]
fn test_match_type_pattern_binary_compiles_to_subclass_aware_test() {
    // BT-4228: `b :: Binary` is subclass-aware (agrees with `isKindOf:
    // Binary`): the `is_binary` BIF test, falling back to the `String`
    // `{'beamtalk_string', _}` tag test for the sealed subclass.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      b :: Binary -> b;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Binary type pattern should test is_binary",
        &code,
        &["'erlang':'is_binary'"],
    );
    assert_has(
        "Binary type pattern must also match the String subclass tag",
        &code,
        &["'beamtalk_string'"],
    );
}

#[test]
fn test_match_type_pattern_bitstring_compiles_to_unaligned_test() {
    // ADR 0141 (BT-4139): `b :: Bitstring` matches exactly the unaligned
    // bitstrings — `is_bitstring` and not `is_binary` — never a map tag.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      b :: Bitstring -> b;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Bitstring type pattern should test is_bitstring",
        &code,
        &["'erlang':'is_bitstring'"],
    );
    assert_has(
        "Bitstring type pattern should exclude byte-aligned binaries",
        &code,
        &["'erlang':'is_binary'"],
    );
    assert_lacks(
        "Bitstring is a raw term, never a tagged-map test",
        &code,
        &["'maps':'get'('$beamtalk_class', X"],
    );
}

#[test]
fn test_match_type_pattern_tagged_class_compiles_to_map_tag_check() {
    // An exact tagged `Value`/sealed class reuses the `'$beamtalk_class'`
    // map-key check `generate_constructor_pattern` already uses for
    // `Result`, generalized to the pattern's `class` field. `generate_module`
    // compiles a single class per call, so the pattern's target class (`Bar`)
    // and the method under test live on the same class.
    let src = "Value subclass: Bar\n  field: n = 0\n\n  test: x =>\n    x match: [\n      b :: Bar -> b;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "bar");

    assert_has(
        "Tagged-class type pattern should test is_map first",
        &code,
        &["'erlang':'is_map'"],
    );
    assert_has(
        "Tagged-class type pattern should check '$beamtalk_class'",
        &code,
        &["'maps':'get'('$beamtalk_class'"],
    );
    assert_has(
        "Tagged-class type pattern should match the class name atom",
        &code,
        &["'Bar'"],
    );
}

#[test]
fn test_match_type_pattern_non_leaf_class_compiles_to_runtime_kind_of_test() {
    // BT-4319 (ADR 0107 Phase B): `Shape` has a subclass (`Circle`, declared
    // in another file and supplied via the superclass index), so `s :: Shape`
    // must match `Circle` instances too. An exact `'$beamtalk_class'` tag
    // test would miss them; the arm instead asks the runtime class table.
    let src = "Value subclass: Shape\n  test: x =>\n    x match: [\n      s :: Shape -> s;\n      _ -> \"none\"\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);

    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("Circle".to_string(), "Shape".to_string());

    let code = generate_module(
        &module,
        CodegenOptions::new("shape").with_class_superclass_index(superclass_index),
    )
    .expect("codegen should succeed");

    assert_has(
        "Non-leaf type pattern should call the runtime kind-of test",
        &code,
        &["call 'beamtalk_behaviour_intrinsics':'valueIsKindOfClass'("],
    );
    assert!(
        code.lines().any(|l| {
            l.find("'valueIsKindOfClass'(")
                .is_some_and(|i| l[i..].contains(", 'Shape')"))
        }),
        "Kind-of test should name the pattern class. Got:\n{code}"
    );

    assert_compiles_through_erlc("shape", &code);
}

#[test]
fn test_match_type_pattern_leaf_class_keeps_exact_tag_fast_path_with_kind_of_fallback() {
    // A class with no visible subclass tests the exact tag first; an instance
    // of a different user class falls back to the runtime kind-of test, so a
    // subclass declared elsewhere (or hot-loaded later) still matches.
    let src = "Value subclass: Bar\n  test: x =>\n    x match: [\n      b :: Bar -> b;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "bar");
    assert_has(
        "Leaf type pattern keeps the exact-tag fast path",
        &code,
        &["'maps':'get'('$beamtalk_class'"],
    );
    assert_has(
        "Leaf type pattern falls back to the runtime kind-of test",
        &code,
        &["'valueIsKindOfClass'("],
    );
    assert_compiles_through_erlc("bar", &code);
}

#[test]
fn test_match_type_pattern_mixed_with_nil_and_native_arms_compiles() {
    // A `match:` mixing `nil`, a primitive `Type` arm, and a wildcard must
    // still compile into one dispatch chain (verifies the
    // dispatch/interleaving layer, not just each strategy in isolation).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      n :: Integer -> n;\n      _ -> -1\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has("Should still contain the nil arm", &code, &["<'nil'>"]);
    assert_has(
        "Should still contain the Integer arm's test",
        &code,
        &["'erlang':'is_integer'"],
    );
}

#[test]
fn test_match_type_pattern_block_compiles_to_is_function_test() {
    // A Beamtalk block compiles to a plain Erlang `fun`, never a map — it
    // needs its own guard-safe BIF entry rather than falling into the
    // tagged-class `is_map` path (which would never match a `fun`).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      blk :: Block -> blk;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Block type pattern should test is_function",
        &code,
        &["'erlang':'is_function'"],
    );
}

#[test]
fn test_match_type_pattern_true_false_nil_use_exact_atom_match() {
    // `True`/`False` (real sealed `Boolean` subclasses) and
    // `Nil`/`UndefinedObject` (the nil class and its legacy alias) all
    // compile to a bare atom, never a map — each needs an exact
    // single-atom test rather than the tagged-class `is_map` check (which
    // would never match a plain atom).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      t :: True -> 1;\n      f :: False -> 2;\n      n :: Nil -> 3;\n      u :: UndefinedObject -> 4;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    // Scope the `is_map` check to the `match:` construct's own compiled
    // function (`'test:'/2`), not the whole module: `dispatch/3` now
    // unconditionally carries a `hasField:` arm (ADR 0124 B4) that uses
    // `is_map` to tell a map-backed instance from a primitive/value term —
    // an unrelated reflection check, not the tagged-class dispatch this
    // test guards against.
    let test_fn = code
        .split("'test:'/2 = fun")
        .nth(1)
        .and_then(|rest| rest.split("\n\n").next())
        .expect("generated code should contain a 'test:'/2 function");
    assert_lacks(
        "None of True/False/Nil/UndefinedObject should use the tagged-class is_map check. Got 'test:'/2 body",
        test_fn,
        &["is_map"],
    );
    assert!(
        test_fn.matches("<'true'>").count() >= 1,
        "Should contain an exact 'true' match for the True arm. Got 'test:'/2 body:\n{test_fn}"
    );
    assert!(
        test_fn.matches("<'false'>").count() >= 1,
        "Should contain an exact 'false' match for the False arm. Got 'test:'/2 body:\n{test_fn}"
    );
    assert!(
        test_fn.matches("<'nil'>").count() >= 2,
        "Should contain an exact 'nil' match for both the Nil and UndefinedObject arms. Got 'test:'/2 body:\n{test_fn}"
    );
}

#[test]
fn test_match_type_pattern_actor_class_uses_tuple_tag_check_and_compiles() {
    // an actor reference is `{'beamtalk_object', ClassAtom,
    // ModuleAtom, Pid}` — a 4-tuple, not a map. Naively generalizing the
    // tagged-class `is_map` check to Actor subclasses would silently never
    // match a live actor instance. Verifies both the generated shape and
    // that it actually compiles through erlc (not just pretty-prints).
    let src = "Actor subclass: Counter\n  state: count = 0\n\n  test: x =>\n    x match: [\n      c :: Counter -> c;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "counter");

    assert_has(
        "Actor-class type pattern should test is_tuple",
        &code,
        &["'erlang':'is_tuple'"],
    );
    assert_has(
        "Actor-class type pattern should check the 'beamtalk_object' tag",
        &code,
        &["'beamtalk_object'"],
    );
    assert_has(
        "Actor-class type pattern should match the class name atom",
        &code,
        &["'Counter'"],
    );

    assert_compiles_through_erlc("counter", &code);
}

#[test]
fn test_match_type_pattern_pid_reference_port_use_guard_safe_bifs() {
    // `Pid`/`Reference`/`Port` are raw BEAM terms (never maps) — each needs
    // its own guard-safe BIF entry rather than the tagged-class `is_map`
    // check (which would never match a bare pid/ref/port).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      p :: Pid -> 1;\n      r :: Reference -> 2;\n      pt :: Port -> 3;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Pid type pattern should test is_pid",
        &code,
        &["'erlang':'is_pid'"],
    );
    assert_has(
        "Reference type pattern should test is_reference",
        &code,
        &["'erlang':'is_reference'"],
    );
    assert_has(
        "Port type pattern should test is_port",
        &code,
        &["'erlang':'is_port'"],
    );
}

#[test]
fn test_match_type_pattern_tuple_excludes_actor_reference_shape() {
    // an actor reference is *also* a plain Erlang tuple
    // structurally (`{'beamtalk_object', ClassAtom, ModuleAtom, Pid}`), so
    // `x :: Tuple` must explicitly exclude the reserved actor/supervisor
    // tags or it would incorrectly match a live actor reference too.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      t :: Tuple -> 1;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");

    assert_has(
        "Tuple type pattern should test is_tuple",
        &code,
        &["'erlang':'is_tuple'"],
    );
    assert_has(
        "Tuple type pattern should exclude the actor-reference tag",
        &code,
        &["'beamtalk_object'"],
    );
    assert_has(
        "Tuple type pattern should exclude the supervisor-reference tag",
        &code,
        &["'beamtalk_supervisor'"],
    );
    assert_has(
        "Tuple type pattern should exclude the String tag (BT-4173)",
        &code,
        &["'beamtalk_string'"],
    );
}

#[test]
fn test_match_type_pattern_supervisor_subclass_uses_tuple_tag_check_and_compiles() {
    // a live Supervisor reference is `{'beamtalk_supervisor', ClassAtom,
    // ModuleAtom, Pid}` — a 4-tuple,
    // not a map, and tagged differently from an actor reference
    // (`'beamtalk_object'`). Naively reusing the tagged-map strategy would
    // silently never match a live supervisor. Verifies both the generated
    // shape and that it actually compiles through erlc.
    // `generate_module` compiles exactly `module.classes.first()`'s methods
    // (`generate_value_type_module`/`generate_actor_module` both key off it);
    // other classes in the same parsed module are still registered (giving
    // `ClassHierarchy` what it needs to resolve `is_supervisor_subclass`)
    // but not themselves compiled. `Foo` must come first so this compiles
    // as a value-type module whose `test:` method contains the pattern
    // match — a concrete Supervisor subclass cannot itself carry compiled
    // custom instance methods (OTP's supervisor behaviour owns `handle_call`).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: WebApp -> s;\n      _ -> \"none\"\n    ]\n\nSupervisor subclass: WebApp\n  childSpecs => #()\n";
    let code = codegen_named(src, "webapp");

    assert_has(
        "Supervisor-class type pattern should test is_tuple",
        &code,
        &["'erlang':'is_tuple'"],
    );
    assert_has(
        "Supervisor-class type pattern should check the 'beamtalk_supervisor' tag",
        &code,
        &["'beamtalk_supervisor'"],
    );
    assert_has(
        "Supervisor-class type pattern should match the class name atom",
        &code,
        &["'WebApp'"],
    );

    assert_compiles_through_erlc("webapp", &code);
}

#[test]
fn test_match_type_pattern_dynamic_supervisor_subclass_uses_tuple_tag_check_and_compiles() {
    // `DynamicSupervisor` subclasses use the same
    // `'beamtalk_supervisor'` tuple shape as
    // `Supervisor` subclasses — verifies the dispatch also routes a
    // `DynamicSupervisor(C)` subclass through the supervisor strategy.
    // See the Supervisor test above for why `Foo` (the class whose method
    // actually gets compiled) must be `module.classes.first()`.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      p :: Pool -> p;\n      _ -> \"none\"\n    ]\n\nObject subclass: Widget\n\nDynamicSupervisor(Widget) subclass: Pool\n  childClass => Widget\n";
    let code = codegen_named(src, "pool");

    assert_has(
        "DynamicSupervisor-class type pattern should test is_tuple",
        &code,
        &["'erlang':'is_tuple'"],
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "DynamicSupervisor-class type pattern should check the 'beamtalk_supervisor' tag. \
         Got:\n{code}"
    );
    assert_has(
        "DynamicSupervisor-class type pattern should match the class name atom",
        &code,
        &["'Pool'"],
    );

    assert_compiles_through_erlc("pool", &code);
}

#[test]
fn test_match_type_pattern_actor_subclass_resolves_through_cross_file_stub_chain() {
    // `LeafActor`'s superclass `MidActor` is *not* declared
    // anywhere in this compilation unit — only `LeafActor subclass:`
    // itself is parsed here. `MidActor` and its own superclass `BaseActor`
    // are supplied purely as `add_external_superclasses` stubs (simulating
    // both ancestors living in separate files), exactly the multi-hop
    // shape `class_superclass_index` produces from a whole-project Pass 1.
    // `is_actor_subclass` must still walk stub -> stub -> builtin `Actor`
    // and dispatch to the actor tuple-tag strategy, not silently fall back
    // to the tagged-map strategy that would never match a live actor.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      c :: LeafActor -> c;\n      _ -> \"none\"\n    ]\n\nMidActor subclass: LeafActor\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);

    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("MidActor".to_string(), "BaseActor".to_string());
    superclass_index.insert("BaseActor".to_string(), "Actor".to_string());

    let code = generate_module(
        &module,
        CodegenOptions::new("leaf_actor").with_class_superclass_index(superclass_index),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Actor-class type pattern should test is_tuple even through a cross-file stub chain. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_object'"),
        "Actor-class type pattern should check the 'beamtalk_object' tag even through a \
         cross-file stub chain. Got:\n{code}"
    );
    assert_has(
        "Actor-class type pattern should match the class name atom",
        &code,
        &["'LeafActor'"],
    );

    assert_compiles_through_erlc("leaf_actor", &code);
}

#[test]
fn test_match_type_pattern_supervisor_subclass_resolves_through_cross_file_stub_chain() {
    // same concern as the actor test above, for the Supervisor
    // strategy. `LeafSupervisor`'s superclass `MidSupervisor` and *its*
    // superclass `BaseSupervisor` are both supplied only as cross-file
    // stubs — neither is declared in this compilation unit. If
    // `is_supervisor_subclass` failed to walk through two stub hops, this
    // would silently fall back to the tagged-map strategy, which never
    // matches a live supervisor reference (a 4-tuple).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: LeafSupervisor -> s;\n      _ -> \"none\"\n    ]\n\nMidSupervisor subclass: LeafSupervisor\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);

    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("MidSupervisor".to_string(), "BaseSupervisor".to_string());
    superclass_index.insert("BaseSupervisor".to_string(), "Supervisor".to_string());

    let code = generate_module(
        &module,
        CodegenOptions::new("leaf_supervisor").with_class_superclass_index(superclass_index),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Supervisor-class type pattern should test is_tuple even through a cross-file stub \
         chain. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "Supervisor-class type pattern should check the 'beamtalk_supervisor' tag even through \
         a cross-file stub chain. Got:\n{code}"
    );
    assert_has(
        "Supervisor-class type pattern should match the class name atom",
        &code,
        &["'LeafSupervisor'"],
    );

    assert_compiles_through_erlc("leaf_supervisor", &code);
}

#[test]
fn test_match_type_pattern_dictionary_and_tagged_class_arms_interleave_in_one_case() {
    // `Dictionary` and an exact tagged class both use an `is_map`-based
    // strategy; a `match:` mixing the two must still compile into one
    // dispatch chain, not collide on temp-var names or short-circuit past
    // the second arm.
    let src = "Value subclass: Bar\n  field: n = 0\n\n  test: x =>\n    x match: [\n      d :: Dictionary -> d;\n      b :: Bar -> b;\n      _ -> \"none\"\n    ]\n";
    let code = codegen_named(src, "bar");

    assert!(
        code.matches("'erlang':'is_map'").count() >= 2,
        "Should contain at least two independent is_map checks (Dictionary arm + Bar arm; \
         other class-boilerplate `is_map` checks may also appear). Got:\n{code}"
    );
    assert_has(
        "Should still contain the Dictionary arm's 'undefined' tag check",
        &code,
        &["'undefined'"],
    );
    assert_has(
        "Should still contain the Bar arm's class-tag check",
        &code,
        &["'Bar'"],
    );
}

#[test]
fn test_match_type_pattern_nested_in_tuple_pattern_lowers_after_shape_match() {
    // BT-5294: a `Pattern::Type` nested in a tuple is hoisted to a plain
    // variable in the native tuple pattern, then class-tested afterwards
    // (here `is_integer`), with a miss falling through to the later arms.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      {_, n :: Integer} -> n;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse diagnostics: {diags:?}");
    let code = generate_module(&module, CodegenOptions::new("foo"))
        .expect("nested Pattern::Type should lower");
    assert_has(
        "type test follows the tuple shape match",
        &code,
        &["'erlang':'is_integer'", "{"],
    );
    let tuple_at = code.find("<{").expect("native tuple pattern");
    let test_at = code.find("'erlang':'is_integer'").expect("is_integer test");
    assert!(
        tuple_at < test_at,
        "shape match must precede the type test. Got:\n{code}"
    );
}

#[test]
fn test_match_wildcard_type_pattern_lowers() {
    // BT-5294: `_ :: T` is a type test that binds nothing; two in one
    // pattern must not collide on a shared `_` variable.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      {_ :: Integer, _ :: String} -> 1;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(diags.is_empty(), "parse diagnostics: {diags:?}");
    let code = generate_module(&module, CodegenOptions::new("foo")).expect("`_ :: T` should lower");
    assert_has(
        "both type tests are emitted",
        &code,
        &["'erlang':'is_integer'", "'beamtalk_string'"],
    );
}

#[test]
fn test_match_top_level_wildcard_type_pattern_emits_no_binding() {
    // BT-5332: a top-level `_ :: T` tests the class and binds nothing, so
    // the arm has no `let _ = Match` (the binding was dead).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      _ :: Integer -> 1;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");
    assert_has(
        "the class test is still emitted",
        &code,
        &["'erlang':'is_integer'"],
    );
    assert_lacks(
        "`_ :: T` must not bind the scrutinee",
        &code,
        &["let _ = ", "let _Match = ", "let __ = "],
    );
}

#[test]
fn test_match_top_level_named_type_pattern_still_binds() {
    // BT-5332: only the discard drops its binding; `n :: T` still binds `n`.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      n :: Integer -> n;\n      _ -> 0\n    ]\n";
    let code = codegen_named(src, "foo");
    assert!(
        code.contains("let N@ = _Match@"),
        "named binding kept. Got:\n{code}"
    );
}

#[test]
fn test_match_guarded_type_patterns_use_guard_position_case() {
    // BT-5332: the guard wrapper shared by the top-level and the nested type
    // arm is `case 'true' of <'true'> when GUARD -> body <'true'> when 'true'
    // -> rest end`, so a guard that raises falls through to the next arm.
    for pattern in [
        "n :: Integer",
        "{_, n :: Integer}",
        "{_ :: Atom, n :: Integer}",
    ] {
        let src = format!(
            "Object subclass: Foo\n  test: x =>\n    x match: [\n      {pattern} when: [n > 5] -> n;\n      _ -> 0\n    ]\n"
        );
        let code = codegen_named(&src, "foo");
        assert_has(
            "guard sits in guard position with a fallthrough clause",
            &code,
            &["case 'true' of <'true'> when ", "<'true'> when 'true' -> "],
        );
    }
}
