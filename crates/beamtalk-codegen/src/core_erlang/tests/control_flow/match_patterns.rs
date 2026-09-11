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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for array pattern match:\n{code}");

    assert!(
        code.contains("call 'erlang':'is_map'("),
        "Should emit is_map guard check for array pattern. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class'"),
        "Should check '$beamtalk_class' key for array type guard. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_array':'size'("),
        "Should check array size. Got:\n{code}"
    );
    assert!(
        code.contains("'at:'"),
        "Should extract elements via at: dispatch. Got:\n{code}"
    );
}

#[test]
fn test_match_map_pattern_arm_generates_core_erlang_map_pattern() {
    // `d match: [#{#event => evName} -> evName; _ -> "none"]`
    // should compile to a native Core Erlang map pattern `~{'event' := EvName}~`.
    let src = "Object subclass: Foo\n  test: d =>\n    d match: [\n      #{#event => evName} -> evName;\n      _ -> \"none\"\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for map pattern match:\n{code}");

    assert!(
        code.contains("~{"),
        "Should emit Core Erlang map pattern ~{{...}}~. Got:\n{code}"
    );
    assert!(
        code.contains(":="),
        "Should use := binding syntax in map pattern. Got:\n{code}"
    );
    assert!(
        code.contains("'event'"),
        "Should include the 'event' key in the map pattern. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_fallthrough_to_wildcard() {
    // When the array pattern fails (wrong type/size), execution must
    // fall through to the next arm — not crash.
    // Wildcard fallback arm should be present in the generated code.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      #[a, b] -> a + b;\n      _ -> 42\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    // The fallback (42) should appear in the generated code
    assert!(
        code.contains("42"),
        "Fallback value should appear in generated code. Got:\n{code}"
    );
    // The is_map check should be present — if false, falls through to 42
    assert!(
        code.contains("call 'erlang':'is_map'("),
        "Should emit is_map check. Got:\n{code}"
    );
}

#[test]
fn test_match_nested_array_pattern_arm() {
    // `arr match: [#[#[a, b], c] -> a+b+c; _ -> 0]`
    // should generate nested is_map + size checks for the inner array.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[#[a, b], c] -> a + b + c;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for nested array pattern match:\n{code}");

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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    // Verify the 3-arg form maps:get/3 is used — the key, the map, AND the default 'undefined'.
    // This prevents regression to maps:get/2 or erlang:map_get/2 (both throw on missing key).
    assert!(
        code.contains("call 'maps':'get'('$beamtalk_class',") && code.contains(", 'undefined')"),
        "Should use maps:get/3 with 'undefined' default. Got:\n{code}"
    );
    assert!(
        !code.contains("call 'erlang':'map_get'("),
        "Must NOT use erlang:map_get/2 — throws badkey when '$beamtalk_class' absent. Got:\n{code}"
    );
}

#[test]
fn test_match_array_pattern_duplicate_variable_emits_equality_check() {
    // `arr match: [#[x, x] -> "equal"; _ -> "differ"]`
    // The second occurrence of `x` must emit an `erlang:=:=` equality check
    // rather than a bare re-binding.
    let src = "Object subclass: Foo\n  test: arr =>\n    arr match: [\n      #[x, x] -> \"equal\";\n      _ -> \"differ\"\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for duplicate-variable array pattern:\n{code}");

    // Must emit a strict-equality call for the duplicate variable.
    assert!(
        code.contains("call 'erlang':'=:='("),
        "Should emit erlang:=:= equality check for duplicate variable. Got:\n{code}"
    );
    // The primary binding `let X = ... at: [1]` should appear.
    assert!(
        code.contains("'at:', [1]"),
        "Should extract first element for primary binding. Got:\n{code}"
    );
    // A second extraction for position 2 must also be present (into a temp var).
    assert!(
        code.contains("'at:', [2]"),
        "Should extract second element into temp for equality check. Got:\n{code}"
    );
}

// ADR 0107 Phase A: `Pattern::Nil` and `Pattern::Type` codegen

#[test]
fn test_match_nil_pattern_compiles_to_atom_literal() {
    // ADR 0107 Phase A: `nil` pattern reuses the existing atom-literal
    // codegen path verbatim — no new runtime mechanism.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      _ -> 1\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for nil pattern match:\n{code}");

    assert!(
        code.contains("<'nil'>"),
        "nil pattern should compile to the atom-literal case pattern 'nil'. Got:\n{code}"
    );
}

// ADR 0107 Phase A: `Pattern::Type` codegen (`generate_type_pattern`)

#[test]
fn test_match_type_pattern_string_compiles_to_is_binary_test() {
    // `s :: String` compiles to a guard-safe `is_binary` case test — the
    // single-level generalization of `generate_array_match_arm`'s `is_map`
    // check to an arbitrary primitive BIF.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: String -> s;\n      _ -> \"none\"\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for String type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_binary'"),
        "String type pattern should test is_binary. Got:\n{code}"
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("bar")).expect("codegen should succeed");

    eprintln!("Generated code for tagged-class type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_map'"),
        "Tagged-class type pattern should test is_map first. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'get'('$beamtalk_class'"),
        "Tagged-class type pattern should check '$beamtalk_class'. Got:\n{code}"
    );
    assert!(
        code.contains("'Bar'"),
        "Tagged-class type pattern should match the class name atom. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_mixed_with_nil_and_native_arms_compiles() {
    // A `match:` mixing `nil`, a primitive `Type` arm, and a wildcard must
    // still compile into one dispatch chain (verifies the
    // dispatch/interleaving layer, not just each strategy in isolation).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      nil -> 0;\n      n :: Integer -> n;\n      _ -> -1\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for mixed nil/type/wildcard match:\n{code}");

    assert!(
        code.contains("<'nil'>"),
        "Should still contain the nil arm. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_integer'"),
        "Should still contain the Integer arm's test. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_block_compiles_to_is_function_test() {
    // A Beamtalk block compiles to a plain Erlang `fun`, never a map — it
    // needs its own guard-safe BIF entry rather than falling into the
    // tagged-class `is_map` path (which would never match a `fun`).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      blk :: Block -> blk;\n      _ -> \"none\"\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Block type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_function'"),
        "Block type pattern should test is_function. Got:\n{code}"
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for True/False/Nil/UndefinedObject type patterns:\n{code}");

    assert!(
        !code.contains("is_map"),
        "None of True/False/Nil/UndefinedObject should use the tagged-class is_map check. Got:\n{code}"
    );
    assert!(
        code.matches("<'true'>").count() >= 1,
        "Should contain an exact 'true' match for the True arm. Got:\n{code}"
    );
    assert!(
        code.matches("<'false'>").count() >= 1,
        "Should contain an exact 'false' match for the False arm. Got:\n{code}"
    );
    assert!(
        code.matches("<'nil'>").count() >= 2,
        "Should contain an exact 'nil' match for both the Nil and UndefinedObject arms. Got:\n{code}"
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    eprintln!("Generated code for actor type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Actor-class type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_object'"),
        "Actor-class type pattern should check the 'beamtalk_object' tag. Got:\n{code}"
    );
    assert!(
        code.contains("'Counter'"),
        "Actor-class type pattern should match the class name atom. Got:\n{code}"
    );

    assert_compiles_through_erlc("counter", &code);
}

#[test]
fn test_match_type_pattern_pid_reference_port_use_guard_safe_bifs() {
    // `Pid`/`Reference`/`Port` are raw BEAM terms (never maps) — each needs
    // its own guard-safe BIF entry rather than the tagged-class `is_map`
    // check (which would never match a bare pid/ref/port).
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      p :: Pid -> 1;\n      r :: Reference -> 2;\n      pt :: Port -> 3;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Pid/Reference/Port type patterns:\n{code}");

    assert!(
        code.contains("'erlang':'is_pid'"),
        "Pid type pattern should test is_pid. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_reference'"),
        "Reference type pattern should test is_reference. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_port'"),
        "Port type pattern should test is_port. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_tuple_excludes_actor_reference_shape() {
    // an actor reference is *also* a plain Erlang tuple
    // structurally (`{'beamtalk_object', ClassAtom, ModuleAtom, Pid}`), so
    // `x :: Tuple` must explicitly exclude the reserved actor/supervisor
    // tags or it would incorrectly match a live actor reference too.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      t :: Tuple -> 1;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("foo")).expect("codegen should succeed");

    eprintln!("Generated code for Tuple type pattern:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Tuple type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_object'"),
        "Tuple type pattern should exclude the actor-reference tag. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "Tuple type pattern should exclude the supervisor-reference tag. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_supervisor_subclass_uses_tuple_tag_check_and_compiles() {
    // a live Supervisor reference is `{'beamtalk_supervisor' |
    // 'beamtalk_supervisor_new', ClassAtom, ModuleAtom, Pid}` — a 4-tuple,
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
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      s :: WebApp -> s;\n      _ -> \"none\"\n    ]\n\nSupervisor subclass: WebApp\n  class children => #()\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("webapp")).expect("codegen should succeed");

    eprintln!("Generated code for Supervisor type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "Supervisor-class type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "Supervisor-class type pattern should check the 'beamtalk_supervisor' tag. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor_new'"),
        "Supervisor-class type pattern should also accept the transient \
         'beamtalk_supervisor_new' tag. Got:\n{code}"
    );
    assert!(
        code.contains("'WebApp'"),
        "Supervisor-class type pattern should match the class name atom. Got:\n{code}"
    );

    assert_compiles_through_erlc("webapp", &code);
}

#[test]
fn test_match_type_pattern_dynamic_supervisor_subclass_uses_tuple_tag_check_and_compiles() {
    // `DynamicSupervisor` subclasses use the same
    // `'beamtalk_supervisor'`/`'beamtalk_supervisor_new'` tuple shape as
    // `Supervisor` subclasses — verifies the dispatch also routes a
    // `DynamicSupervisor(C)` subclass through the supervisor strategy.
    // See the Supervisor test above for why `Foo` (the class whose method
    // actually gets compiled) must be `module.classes.first()`.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      p :: Pool -> p;\n      _ -> \"none\"\n    ]\n\nObject subclass: Widget\n\nDynamicSupervisor(Widget) subclass: Pool\n  class childClass => Widget\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("pool")).expect("codegen should succeed");

    eprintln!("Generated code for DynamicSupervisor type pattern match:\n{code}");

    assert!(
        code.contains("'erlang':'is_tuple'"),
        "DynamicSupervisor-class type pattern should test is_tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_supervisor'"),
        "DynamicSupervisor-class type pattern should check the 'beamtalk_supervisor' tag. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'Pool'"),
        "DynamicSupervisor-class type pattern should match the class name atom. Got:\n{code}"
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

    eprintln!("Generated code for cross-file actor type pattern match:\n{code}");

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
    assert!(
        code.contains("'LeafActor'"),
        "Actor-class type pattern should match the class name atom. Got:\n{code}"
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

    eprintln!("Generated code for cross-file supervisor type pattern match:\n{code}");

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
    assert!(
        code.contains("'LeafSupervisor'"),
        "Supervisor-class type pattern should match the class name atom. Got:\n{code}"
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("bar")).expect("codegen should succeed");

    eprintln!("Generated code for Dictionary + tagged-class match:\n{code}");

    assert!(
        code.matches("'erlang':'is_map'").count() >= 2,
        "Should contain at least two independent is_map checks (Dictionary arm + Bar arm; \
         other class-boilerplate `is_map` checks may also appear). Got:\n{code}"
    );
    assert!(
        code.contains("'undefined'"),
        "Should still contain the Dictionary arm's 'undefined' tag check. Got:\n{code}"
    );
    assert!(
        code.contains("'Bar'"),
        "Should still contain the Bar arm's class-tag check. Got:\n{code}"
    );
}

#[test]
fn test_match_type_pattern_nested_in_tuple_pattern_is_codegen_error() {
    // A `Pattern::Type` nested inside a composite pattern (here a `Tuple`)
    // has no codegen path — `generate_type_pattern` only handles a
    // top-level arm pattern, since its per-class runtime test needs to wrap
    // the whole arm. Verifies this is a clean, reported codegen error, not
    // a panic or silently-wrong output.
    let src = "Object subclass: Foo\n  test: x =>\n    x match: [\n      {s :: String, n} -> n;\n      _ -> 0\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let err = generate_module(&module, CodegenOptions::new("foo"))
        .expect_err("nested Pattern::Type should be a codegen error, not silently accepted");
    let message = err.to_string();
    assert!(
        message.contains("Type pattern") && message.contains("nested"),
        "error should describe the nested Type pattern restriction. Got: {message}"
    );
}
