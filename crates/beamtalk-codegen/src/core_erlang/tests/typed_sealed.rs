// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4069, BT-4293, BT-4294: sends to a receiver typed as a tagged-map
//! (Value or Object) class — sealed or open — or as a representation-routed
//! built-in (`List`, `Dictionary`, `Integer`, …) compile to a guarded direct
//! `Module:dispatch/3` call instead of a dynamic
//! `beamtalk_message_dispatch:send/3`.
//!
//! Positive tests pin the guarded direct path (the `typed_guarded` handler);
//! negative tests (untyped/union, abstract classes, the supervisor family,
//! built-ins without a single route, exception kin, `Character` literals)
//! lock in the dynamic behavior so the handler cannot steal those sends.

use super::codegen;

// ---------------------------------------------------------------------------
// Positive: sealed receivers take the guarded direct path
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_typed_param_sends_direct_with_tag_guard() {
    // NOTE: `generate_module` compiles only the module's first class — the
    // Caller (holding the send under test) comes first, the callee second.
    // The hierarchy and type inference still see both.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    // Direct call to the class module with the literal selector. The
    // module's own `dispatch/3` definition also mentions
    // `bt@sealed_widget':'dispatch'` (its `perform:` arm), but always with a
    // variable selector — the `('x'` literal pins this to the send site.
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call the class module. Got:\n{code}"
    );
    // Tag guard on '$beamtalk_class' with a dynamic-send fallback.
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "guard mismatch should fall back to the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn destructured_tuple_local_sends_direct_with_tag_guard() {
    // BT-4487: `{w, _} := pair` types `w` from `Tuple(SealedWidget, Integer)`,
    // so the send to it takes the guarded direct path.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: pair :: Tuple(SealedWidget, Integer) -> Integer =>\n",
        "    {w, _} := pair\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "destructured typed local should direct-call the class module. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
}

#[test]
fn sealed_object_typed_param_sends_direct() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: SealedCounter -> Integer =>\n",
        "    c answer\n\n",
        "sealed Object subclass: SealedCounter\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_counter':'dispatch'('answer'"),
        "sealed Object-kind receiver should direct-call too (same tagged-map representation). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

// ---------------------------------------------------------------------------
// Negative: everything else stays dynamic
// ---------------------------------------------------------------------------

#[test]
fn sealed_value_keyword_send_binds_args_once() {
    // Keyword sends thread arguments into both the direct and fallback
    // branches from a single let-binding (no double evaluation).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w withX: 3\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('withX:', ["),
        "sealed keyword send should direct-call with args. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('$beamtalk_class'"),
        "direct call should be guarded on the runtime class tag. Got:\n{code}"
    );
    // Exactly one `let` binding per operand: the single argument temp is
    // bound once and read three times (direct args list + both fallback
    // args lists), the receiver temp once and read five times (is_map,
    // maps:get, direct call, both fallback sends). Any inlining or double
    // binding changes these counts.
    assert_eq!(
        code.matches("TTaggedArg").count(),
        4,
        "argument should be bound once and read three times. Got:\n{code}"
    );
    assert_eq!(
        code.matches("TTaggedRecv").count(),
        6,
        "receiver should be bound once and read five times. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_class_side() {
    // Class methods send to values too (every vs-elixir row is class-side):
    // instance `dispatch/3` on a value is pure, so no class-process hazard.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  class useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from class side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_typed_param_sends_direct_actor_side() {
    // Actor methods send to values too: a direct `dispatch/3` on a value
    // executes in-caller exactly like the dynamic send (values never hop
    // processes), and the emission must be verifier-clean — this test runs
    // with debug assertions on, so a `report_threaded_ir_verify_errors`
    // finding would panic rather than pass.
    let code = codegen(concat!(
        "Actor subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Integer =>\n",
        "    w x\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('x'"),
        "sealed typed receiver should direct-call from actor side too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn sealed_value_field_send_as_server_match_scrutinee_is_bracketed() {
    // BT-4136: catalina's `Handler>>handleContinuation:` — a field write, then
    // a sealed typed Value's field getter as a `match:` scrutinee whose arms
    // send to `self`. The guarded direct call still has a dynamic-send
    // fallback, so it is bracketed like any other call, never an
    // `UnbracketedUserCodeSend`: debug assertions are on, so a verifier
    // finding panics here rather than passing.
    let code = codegen(concat!(
        "Server subclass: Handler\n",
        "  state: userState :: Object = nil\n\n",
        "  handleContinuation: result :: HandlerResult -> Nil =>\n",
        "    self.userState := result state\n",
        "    result kind match: [\n",
        "      #continue -> self continueReading: result;\n",
        "      #close -> self closeLocally;\n",
        "      _ -> self failWith: result reason\n",
        "    ]\n",
        "    nil\n\n",
        "  continueReading: result :: HandlerResult -> Nil =>\n",
        "    self.userState := result timeout\n",
        "    nil\n\n",
        "  closeLocally -> Nil => nil\n\n",
        "  failWith: reason -> Nil => nil\n\n",
        "sealed typed Value subclass: HandlerResult\n",
        "  field: kind :: Symbol = #continue\n",
        "  field: state :: Object = nil\n",
        "  field: timeout :: Integer = 0\n",
        "  field: reason :: Object = nil\n",
    ));
    assert!(
        code.contains("call 'bt@handler_result':'dispatch'('kind'"),
        "the scrutinee should take the sealed direct path. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_value_typed_param_sends_direct_with_tag_guard() {
    // BT-4293: `sealed` adds nothing to soundness — the guard matches the
    // exact class tag, so a subclass instance misses it and takes the
    // dynamic fallback (ADR 0025 amendment §1).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: OpenWidget -> Integer =>\n",
        "    w x\n\n",
        "Value subclass: OpenWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("call 'bt@open_widget':'dispatch'('x'"),
        "open Value receiver should direct-call the class module. Got:\n{code}"
    );
    assert!(
        code.contains("<'OpenWidget'> when 'true' ->"),
        "the direct call should be guarded on the exact class tag. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "guard mismatch should fall back to the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_object_typed_param_sends_direct() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: OpenCounter -> Integer =>\n",
        "    c answer\n\n",
        "Object subclass: OpenCounter\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        code.contains("call 'bt@open_counter':'dispatch'('answer'"),
        "open Object-kind receiver should direct-call too. Got:\n{code}"
    );
}

#[test]
fn open_value_with_subclass_guards_on_the_declared_class() {
    // The receiver is typed as the parent: the guard names the parent, so a
    // `Sub` instance misses it and dispatches dynamically (its own override
    // or the inherited method, exactly as `send/3` would).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: b :: OpenBase -> Integer =>\n",
        "    b x\n\n",
        "Value subclass: OpenBase\n",
        "  field: x :: Integer = 0\n\n",
        "OpenBase subclass: OpenSub\n",
        "  x -> Integer => 7\n",
    ));
    assert!(
        code.contains("<'OpenBase'> when 'true' ->")
            && code.contains("call 'bt@open_base':'dispatch'('x'"),
        "parent-typed receiver should guard on the parent's tag. Got:\n{code}"
    );
    assert!(
        !code.contains("<'OpenSub'> when 'true' ->"),
        "the guard must name the declared class only. Got:\n{code}"
    );
}

#[test]
fn open_value_self_send_sends_direct() {
    // BT-4293: in a Value class `self` is typed `Known{C}` (Inferred), so an
    // own-class self-send takes the guarded call; a subclass instance running
    // the inherited method misses the guard and stays dynamic. The unit's own
    // class targets the module the unit emits ('test' here), never a derived
    // name: `beamtalk test` emits a test class unqualified with no index entry.
    let code = codegen(concat!(
        "Value subclass: OpenPoint\n",
        "  field: x :: Integer = 0\n",
        "  doubled -> Integer => self x * 2\n",
    ));
    // BT-4473: the own class's arm is called directly, as a local call.
    assert!(
        code.contains("<'OpenPoint'> when 'true' ->") && code.contains("apply 'x'/1("),
        "an open Value self-send should take the guarded direct call. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn abstract_value_typed_param_stays_dynamic() {
    // No instance carries an abstract class's tag: a guard would never hit.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: s :: Shape -> Integer =>\n",
        "    s area\n\n",
        "abstract Value subclass: Shape\n",
        "  area -> Integer => 0\n",
    ));
    assert!(
        !code.contains("'dispatch'('area'"),
        "abstract receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "abstract receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn abstract_builtin_typed_param_stays_dynamic() {
    // `Object` and `Value` are ancestors of representation-routed built-ins:
    // their static types admit raw terms (integers, lists, …) that no class
    // tag matches, so they stay dynamic (the `RepresentationAncestor` gate).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: o :: Object -> Object =>\n",
        "    o frobnicate\n",
        "  useValue: v :: Value -> Object =>\n",
        "    v frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "abstract builtin receivers must stay dynamic. Got:\n{code}"
    );
}

#[test]
fn supervisor_subclass_typed_param_stays_dynamic() {
    // Supervisor-family instances are `{beamtalk_supervisor, …}` tuples
    // (routing fixture), never tagged maps — whether the declared class is
    // the abstract root or a concrete subclass.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: s :: AppSup -> Object =>\n",
        "    s frobnicate\n",
        "  useRoot: s :: Supervisor -> Object =>\n",
        "    s frobnicate\n",
        "  usePool: p :: AppPool -> Object =>\n",
        "    p frobnicate\n\n",
        "Supervisor subclass: AppSup\n",
        "  class childSpecs => #()\n\n",
        "DynamicSupervisor(Object) subclass: AppPool\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "supervisor-family receivers must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "supervisor sends should still exist as dynamic sends. Got:\n{code}"
    );
}

#[test]
fn untyped_param_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w =>\n",
        "    w ping\n",
    ));
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "untyped receiver send should stay dynamic. Got:\n{code}"
    );
}

/// BT-4595: a union whose members all have routes compiles to a guard chain.
#[test]
fn routed_union_typed_param_sends_direct_with_guard_chain() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Gadget | Integer =>\n",
        "    w ping\n\n",
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Gadget\n",
        "  ping => 1\n",
    ));
    for fragment in [
        "call 'bt@widget':'dispatch'('ping'",
        "call 'bt@gadget':'dispatch'('ping'",
        "call 'bt@stdlib@integer':'dispatch'('ping'",
        "call 'erlang':'is_integer'(",
        "<'Widget'> when 'true'",
        "<'Gadget'> when 'true'",
        "beamtalk_message_dispatch':'send'(",
    ] {
        assert!(
            code.contains(fragment),
            "missing {fragment:?}. Got:\n{code}"
        );
    }
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn union_with_duplicate_members_dedupes_arms() {
    // `Widget(Integer) | Widget(String)`-style duplicates collapse; with a
    // single distinct class the send is the plain tagged guard.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Widget | Gadget =>\n",
        "    w ping\n\n",
        "Object subclass: Widget\n",
        "  ping => 1\n\n",
        "Object subclass: Gadget\n",
        "  ping => 1\n",
    ));
    assert_eq!(code.matches("<'Widget'> when 'true'").count(), 1, "{code}");
    assert_eq!(code.matches("<'Gadget'> when 'true'").count(), 1, "{code}");
}

#[test]
fn union_with_unrouted_member_stays_dynamic() {
    // `Boolean` has no single built-in route (true/false dispatch to
    // different modules), so the whole union is dynamic.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: Widget | Boolean =>\n",
        "    w ping\n\n",
        "Object subclass: Widget\n",
        "  ping => 1\n",
    ));
    assert!(!code.contains("'dispatch'('ping'"), "{code}");
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "{code}"
    );
}

#[test]
fn union_over_the_arm_cap_stays_dynamic() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: A1 | A2 | A3 | A4 | A5 =>\n",
        "    w ping\n\n",
        "Object subclass: A1\n  ping => 1\n\n",
        "Object subclass: A2\n  ping => 1\n\n",
        "Object subclass: A3\n  ping => 1\n\n",
        "Object subclass: A4\n  ping => 1\n\n",
        "Object subclass: A5\n  ping => 1\n",
    ));
    assert!(!code.contains("'dispatch'('ping'"), "{code}");
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "{code}"
    );
}

// ---------------------------------------------------------------------------
// BT-4294: built-in receivers take a representation-guarded direct call
// ---------------------------------------------------------------------------

/// Compiles `recv frobnicate` with `recv :: <ty>` and returns the code.
/// (`frobnicate` is intentionally undefined: no earlier handler claims it,
/// and DNU only matters at run time.)
fn builtin_typed_send(ty: &str) -> String {
    codegen(&format!(
        "Object subclass: Caller\n  useIt: r :: {ty} -> Object =>\n    r frobnicate\n"
    ))
}

#[test]
fn builtin_typed_params_send_direct_to_the_fixture_module() {
    // Every row of the routing fixture with a single route, by its static
    // type, guard (one distinctive fragment of its rendering) and module.
    let cases: &[(&str, &str, &str)] = &[
        (
            "Integer",
            "call 'erlang':'is_integer'(",
            "bt@stdlib@integer",
        ),
        ("Float", "call 'erlang':'is_float'(", "bt@stdlib@float"),
        (
            "List(Integer)",
            "call 'erlang':'is_list'(",
            "bt@stdlib@list",
        ),
        ("Block", "call 'erlang':'is_function'(", "bt@stdlib@block"),
        (
            "Reference",
            "call 'erlang':'is_reference'(",
            "bt@stdlib@reference",
        ),
        ("Port", "call 'erlang':'is_port'(", "bt@stdlib@port"),
        // BT-4390: `String` and `Pid` are routed for every selector outside
        // their fixture `special_selectors` (`frobnicate` is one).
        ("Pid", "call 'erlang':'is_pid'(", "bt@stdlib@pid"),
        (
            "String",
            "<'beamtalk_string'> when 'true' -> call 'bt@stdlib@string'",
            "bt@stdlib@string",
        ),
        ("Symbol", ", 'nil')", "bt@stdlib@symbol"),
        (
            "True",
            "<'true'> when 'true' -> call 'bt@stdlib@true'",
            "bt@stdlib@true",
        ),
        (
            "False",
            "<'false'> when 'true' -> call 'bt@stdlib@false'",
            "bt@stdlib@false",
        ),
        (
            "UndefinedObject",
            "<'nil'> when 'true' -> call 'bt@stdlib@undefined_object'",
            "bt@stdlib@undefined_object",
        ),
        (
            "Dictionary(Symbol, Integer)",
            "is_map_key'('__class_mod__'",
            "bt@stdlib@dictionary",
        ),
        (
            "Tuple",
            "<'beamtalk_object'> when 'true' -> 'false'",
            "bt@stdlib@tuple",
        ),
    ];
    for (ty, guard, module) in cases {
        let code = builtin_typed_send(ty);
        assert!(
            code.contains(&format!("call '{module}':'dispatch'('frobnicate'")),
            "{ty} receiver should direct-call {module}. Got:\n{code}"
        );
        assert!(
            code.contains(guard),
            "{ty} direct call should stand behind its representation guard {guard:?}. Got:\n{code}"
        );
        assert!(
            code.contains("beamtalk_message_dispatch':'send'("),
            "{ty} guard miss should fall back to the dynamic send. Got:\n{code}"
        );
        super::assert_compiles_through_erlc("test", &code);
    }
}

#[test]
fn tuple_guard_rejects_reserved_heads_at_every_size() {
    // The dynamic path sends any tuple of size >= 2 headed `beamtalk_object`
    // to `gen_server:call/2`, not only the 4-tuple the type-pattern test
    // rejects, so the send guard rejects every reserved head at every size:
    // no `tuple_size` comparison but the empty-tuple check.
    let code = builtin_typed_send("Tuple");
    for tag in [
        "beamtalk_string",
        "beamtalk_future",
        "beamtalk_object",
        "beamtalk_supervisor",
    ] {
        assert!(
            code.contains(&format!("<'{tag}'> when 'true' -> 'false'")),
            "Tuple guard should reject the reserved head {tag}. Got:\n{code}"
        );
    }
    assert!(
        code.contains("tuple_size'(_TTaggedRecv"),
        "Tuple guard should check for the empty tuple. Got:\n{code}"
    );
    for size in [2, 4] {
        assert!(
            !code.contains(&format!("), {size}) of")),
            "Tuple send guard must not be size-specific. Got:\n{code}"
        );
    }
}

#[test]
fn builtin_typed_keyword_send_binds_args_once() {
    // `Dictionary at:put:` is the largest built-in group in Catalina; the
    // dict intrinsic only claims literal receivers, so a typed parameter
    // reaches the guarded call with each operand bound once.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: d :: Dictionary(Symbol, Integer) -> Object =>\n",
        "    d frobnicate: 1 with: 2\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@dictionary':'dispatch'('frobnicate:with:', ["),
        "keyword send to a Dictionary should direct-call. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn builtin_special_selectors_stay_dynamic() {
    // BT-4390: every selector the fixture lists as special for a row (the
    // `Pid` future protocol through `beamtalk_future`, the `String`
    // byte-level selectors `module_for_value/2` sends to `bt@stdlib@binary`)
    // stays a dynamic send, read from the routing table rather than a copy.
    let routed = crate::core_erlang::routing::rows()
        .iter()
        .filter(|row| crate::core_erlang::routing::is_non_tagged_map_class(row.class));
    for row in routed {
        for special in row.special_selectors {
            let arity = special.matches(':').count();
            let send = if arity == 0 {
                (*special).to_string()
            } else {
                special
                    .split_terminator(':')
                    .map(|part| format!("{part}: 1"))
                    .collect::<Vec<_>>()
                    .join(" ")
            };
            let code = codegen(&format!(
                "Object subclass: Caller\n  useIt: r :: {} -> Object =>\n    r {send}\n",
                row.class
            ));
            let module = row.target_module.expect("special rows have a module");
            assert!(
                !code.contains(&format!("call '{module}':'dispatch'('{special}'")),
                "{}>>{special} must stay dynamic. Got:\n{code}",
                row.class
            );
            // Unary `await`/`awaitForever` are claimed earlier, by the await
            // intrinsics, which call `beamtalk_future` directly.
            assert!(
                code.contains("beamtalk_message_dispatch':'send'(")
                    || code.contains("call 'beamtalk_future':"),
                "{}>>{special} should be a dynamic send. Got:\n{code}",
                row.class
            );
        }
    }
}

#[test]
fn builtin_typed_keyword_send_to_pid_and_string_is_routed() {
    // BT-4390: a non-special keyword selector on each special-selector row.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  usePid: p :: Pid -> Object =>\n",
        "    p frobnicate: 1\n",
        "  useString: s :: String -> Object =>\n",
        "    s frobnicate: 1\n",
    ));
    for module in ["bt@stdlib@pid", "bt@stdlib@string"] {
        assert!(
            code.contains(&format!("call '{module}':'dispatch'('frobnicate:', [")),
            "keyword send should direct-call {module}. Got:\n{code}"
        );
    }
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn builtins_without_a_single_route_stay_dynamic() {
    // From the routing fixture: `Boolean`, `Bitstring`, `Character`,
    // `Class`, `Metaclass` and `ErlangModule` carry a `dynamic_only` reason.
    for ty in [
        "Boolean",
        "Bitstring",
        "Character",
        "Class",
        "Metaclass",
        "ErlangModule",
    ] {
        let code = builtin_typed_send(ty);
        assert!(
            !code.contains("'dispatch'('frobnicate'"),
            "{ty} receiver must stay dynamic. Got:\n{code}"
        );
        assert!(
            code.contains("beamtalk_message_dispatch':'send'("),
            "{ty} receiver send should still exist as a dynamic send. Got:\n{code}"
        );
    }
}

#[test]
fn binary_typed_send_routes_bare_binaries_and_strings() {
    // BT-4510: `Binary`'s test admits a bare binary (routed to
    // `bt@stdlib@binary`) or the `String` tag (routed to
    // `bt@stdlib@string`), so a `Binary`-typed send guards on each in turn
    // before the dynamic send.
    let code = builtin_typed_send("Binary");
    assert!(
        code.contains("call 'erlang':'is_binary'(")
            && code.contains("call 'bt@stdlib@binary':'dispatch'('frobnicate'"),
        "a bare binary should direct-call bt@stdlib@binary. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_string'")
            && code.contains("call 'bt@stdlib@string':'dispatch'('frobnicate'"),
        "a String should direct-call bt@stdlib@string. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "anything else should take the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);

    // A `String` special selector (answered by `bt@stdlib@binary` for a
    // `String` unless an extension says otherwise) keeps only the
    // bare-binary route.
    let code =
        codegen("Object subclass: Caller\n  useIt: r :: Binary -> Object =>\n    r toBytes\n");
    assert!(
        code.contains("call 'bt@stdlib@binary':'dispatch'('toBytes'")
            && !code.contains("call 'bt@stdlib@string':'dispatch'('toBytes'"),
        "a String special selector must not direct-call bt@stdlib@string. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn rewritten_selector_on_typed_receiver_stays_dynamic() {
    // `perform:withArguments:timeout:` is rewritten by
    // `beamtalk_message_dispatch:send/3` into a `send/4` of the target
    // selector before the receiver is classified, so no `dispatch/3` sees it.
    for ty in ["List(Integer)", "SealedWidget"] {
        let code = codegen(&format!(
            "Object subclass: Caller\n  useIt: r :: {ty} -> Object =>\n    r perform: #size withArguments: #() timeout: 100\n\nsealed Value subclass: SealedWidget\n  field: x :: Integer = 0\n"
        ));
        assert!(
            !code.contains("'dispatch'('perform:withArguments:timeout:'"),
            "{ty}: a rewritten selector must stay dynamic. Got:\n{code}"
        );
    }
}

#[test]
fn sealed_exception_subclass_stays_dynamic() {
    // Dynamic sends to exceptions route via `bt@stdlib@exception:dispatch`,
    // not the leaf module — a direct leaf call would diverge.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: e :: SealedBoom -> Object =>\n",
        "    e frobnicate\n\n",
        "sealed typed Exception subclass: SealedBoom\n",
        "  answer -> Integer => 42\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_boom':'dispatch'('frobnicate'"),
        "exception-kin receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "exception-kin send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn field_at_put_on_sealed_receiver_never_direct() {
    // The object-reflection intrinsic claims `fieldAt:put:` before dispatch
    // selection (the handler has no exclusion of its own since BT-4282; the
    // hints agree on every path, BT-4273), so this pins the end state: no
    // direct call, ever.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w fieldAt: #x put: 1\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_widget':'dispatch'('fieldAt:put:'"),
        "fieldAt:put: must never take the direct path. Got:\n{code}"
    );
}

#[test]
fn binary_send_on_sealed_receiver_never_direct() {
    // Binary selectors never reach the dynamic fallthrough (it raises an
    // internal error for them); the direct path must not claim them either.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: SealedWidget -> Object =>\n",
        "    w + 1\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        !code.contains("call 'bt@sealed_widget':'dispatch'('+"),
        "binary selector must never take the direct path. Got:\n{code}"
    );
}

#[test]
fn erlang_module_typed_param_stays_dynamic() {
    // `ErlangModule` instances are tagged maps, but dynamic dispatch routes
    // them to the hand-written native proxy, not a compiled module — a
    // direct call would run the wrong code. Exclusion is correctness here.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: m :: ErlangModule -> Object =>\n",
        "    m frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "native-proxied receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "native-proxied receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn class_object_typed_param_stays_dynamic() {
    // `Class`/`Metaclass` instances are `#beamtalk_object{}` records, not
    // maps: the tag guard could never match.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: c :: Class -> Object =>\n",
        "    c frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "record receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "record receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn sealed_stdlib_result_sends_direct() {
    // A sealed stdlib Value converts too (vs-elixir row 9b's shape): the
    // guard matches the tag and the direct call hits `bt@stdlib@result`.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: r :: Result(Integer, Symbol) -> Object =>\n",
        "    r isOk\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@result':'dispatch'('isOk'"),
        "sealed stdlib receiver should direct-call. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn value_to_a_typed_non_block_receiver_is_a_typed_send_not_block_evaluation() {
    // BT-4510: `r value` on a `Result` is `Result>>value`. The block-value
    // intrinsic used to claim it (is_function tests, then a dynamic send);
    // now it yields to the guarded direct call, for `value` and `value:`.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: r :: Result(Integer, Symbol) -> Object =>\n",
        "    r value\n",
        "  useWidget: w :: SealedWidget -> Object =>\n",
        "    w value: 1\n",
        "  useBlock: b :: Block -> Object =>\n",
        "    b value\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
        "  value: n :: Integer -> Integer => n\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@result':'dispatch'('value', []"),
        "Result-typed `value` should direct-call bt@stdlib@result. Got:\n{code}"
    );
    assert!(
        code.contains("call 'bt@sealed_widget':'dispatch'('value:', ["),
        "SealedWidget-typed `value:` should direct-call its module. Got:\n{code}"
    );
    // A `Block`-typed receiver keeps the intrinsic's fun application.
    assert!(
        code.contains("call 'erlang':'is_function'(") && code.contains("apply "),
        "Block-typed `value` should keep block evaluation. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn future_typed_param_stays_dynamic() {
    // `Future` is the runtime-only builtin: no compiled module, so no
    // `dispatch/3` to direct-call (its dynamic sends route via
    // `beamtalk_future`, not a class module at all).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: f :: Future -> Object =>\n",
        "    f frobnicate\n",
    ));
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "runtime-only receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "runtime-only receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

/// A sealed `Object`-kind class arriving via the ambient (live-image)
/// hierarchy — the shape a REPL inline class definition sees for a
/// `ClassBuilder` class after a live edit. `module_less` selects the
/// module-less (fun-backed, dispatches via `runtime_instance_method`)
/// flavor versus the compiled flavor.
fn ambient_sealed_object(
    module_less: bool,
    package: Option<&str>,
) -> beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
    ClassInfo {
        name: "AmbientWidget".into(),
        superclass: Some("Object".into()),
        is_sealed: true,
        is_abstract: false,
        is_typed: true,
        is_internal: false,
        package: package.map(Into::into),
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: module_less,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        state_kinds: std::collections::HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

fn codegen_with_ambient(
    src: &str,
    ambient: beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo,
) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test").with_class_hierarchy(vec![ambient]),
    )
    .expect("codegen should succeed")
}

#[test]
fn module_less_ambient_sealed_class_stays_dynamic() {
    // A1: a sealed module-less (`ClassBuilder`) class seen through a REPL
    // inline definition's ambient hierarchy must NOT direct-call — there
    // is no `Module:dispatch/3` (dynamic dispatch runs the fun via
    // `runtime_instance_method`), so a direct call would `undef`.
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(true, None),
    );
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "module-less receiver must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "module-less receiver send should still exist as a dynamic send. Got:\n{code}"
    );
}

#[test]
fn module_ful_ambient_sealed_class_sends_direct() {
    // Positive control for the module-less test above: the same ambient
    // sealed class WITH a compiled module still takes the direct path, so
    // the negative cannot pass vacuously (e.g. by the checker failing to
    // resolve the pre-loaded annotation).
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(false, None),
    );
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "module-ful ambient receiver should direct-call. Got:\n{code}"
    );
}

#[test]
fn packaged_ambient_sealed_class_resolves_qualified_module() {
    // The `package = Some` resolution branch: registry miss falls back to
    // the deterministic `bt@{package}@{snake}` composition.
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  useIt: w :: AmbientWidget -> Object =>\n",
            "    w frobnicate\n",
        ),
        ambient_sealed_object(false, Some("live")),
    );
    assert!(
        code.contains("call 'bt@live@ambient_widget':'dispatch'('frobnicate'"),
        "packaged ambient receiver should direct-call the qualified module. Got:\n{code}"
    );
}

#[test]
fn packaged_sealed_class_with_unqualified_index_entry_resolves_emitted_module() {
    // BT-4152: `beamtalk test` emits fixture (and test-file) modules
    // unqualified (`bt@<stem>`) while the file's own class carries
    // `package = Some(current_package)`. The direct-call target must be
    // the module this compilation unit actually emits (the unit index
    // entry), not the re-derived `bt@<package>@<class>` — a module that
    // is never emitted, so the direct call would `undef` at runtime
    // where the dynamic path resolves the loaded `bt@<stem>` module.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "bt@ambient_widget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@caller_fixture")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("buccaneer"))])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "packaged receiver with an unqualified index entry should direct-call the emitted module. Got:\n{code}"
    );
    assert!(
        !code.contains("bt@buccaneer@ambient_widget"),
        "must not target the never-emitted qualified module. Got:\n{code}"
    );
}

#[test]
fn packaged_sealed_class_with_index_entry_keeps_package_mode_target() {
    // BT-4152 (no-change control): in package mode the unit's own
    // registry entry and the `info.package` re-derivation name the same
    // module, so resolving through the registry first must not change
    // the direct-call target there — including a subdirectory class the
    // closed-form `bt@{package}@{snake}` convention could never rebuild.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert(
        "AmbientWidget".to_string(),
        "bt@sicp@scheme@env".to_string(),
    );
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@sicp@main")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("sicp"))])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@sicp@scheme@env':'dispatch'('frobnicate'"),
        "package-mode receiver should direct-call the indexed module. Got:\n{code}"
    );
}

// ---------------------------------------------------------------------------
// BT-4151: a class name declared by more than one package stays dynamic
// ---------------------------------------------------------------------------

/// Generates the shared `Caller` (a typed send to `AmbientWidget`) against
/// `ambient` — the cross-file/dependency declarations a batch build or
/// `beamtalk test` hands codegen through `with_class_hierarchy`.
fn codegen_caller_with_scope(
    ambient: Vec<beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo>,
) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@app@caller").with_class_hierarchy(ambient),
    )
    .expect("codegen should succeed")
}

#[test]
fn sealed_class_exported_by_two_packages_stays_dynamic() {
    // `json@AmbientWidget` and `xml@AmbientWidget` coexist under qualified
    // references but collide into one last-wins runtime class: the tag guard
    // cannot tell their instances apart, so the send must not pin either
    // package's module.
    let code = codegen_caller_with_scope(vec![
        ambient_sealed_object(false, Some("json")),
        ambient_sealed_object(false, Some("xml")),
    ]);
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "a class name exported by two packages must stay dynamic. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'("),
        "the shadowed receiver send should be a dynamic send. Got:\n{code}"
    );
}

#[test]
fn sealed_class_shadowing_a_package_less_class_stays_dynamic() {
    // A workspace/single-file class and a package's class of the same name
    // collide at runtime just like two packages' classes.
    let code = codegen_caller_with_scope(vec![
        ambient_sealed_object(false, None),
        ambient_sealed_object(false, Some("json")),
    ]);
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "a class name shared with a package-less class must stay dynamic. Got:\n{code}"
    );
}

#[test]
fn own_sealed_class_shadowed_by_a_dependency_stays_dynamic() {
    // The unit's own class counts as a declaration in scope: a dependency
    // exporting the same name collides with it at runtime, so even a send
    // inside the class's own module stays dynamic.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "sealed Object subclass: AmbientWidget\n",
        "  frobnicate -> Object => nil\n",
        "  poke: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@ambient_widget")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("json"))]),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "an own class shadowed by a dependency must stay dynamic. Got:\n{code}"
    );
}

#[test]
fn own_sealed_class_without_a_rival_sends_direct() {
    // Positive control for the test above: the same own class with no
    // same-named declaration elsewhere still direct-calls its own module,
    // so the negative cannot pass vacuously.
    let code = codegen(concat!(
        "sealed Object subclass: AmbientWidget\n",
        "  frobnicate -> Object => nil\n",
        "  poke: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    assert!(
        code.contains("apply 'frobnicate'/1("),
        "an unshadowed own sealed class should direct-call. Got:\n{code}"
    );
}

#[test]
fn own_sealed_class_recompiled_over_its_live_self_sends_direct() {
    // No overcorrection on the REPL/`compile:source:` reload path: the
    // ambient (live) hierarchy still carries the class being recompiled,
    // declared by the same package the unit's module name says it belongs
    // to, so it is not a rival declaration.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "sealed Object subclass: AmbientWidget\n",
        "  frobnicate -> Object => nil\n",
        "  poke: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@live@ambient_widget")
            .with_class_hierarchy(vec![ambient_sealed_object(false, Some("live"))]),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("apply 'frobnicate'/1("),
        "recompiling a class over its own live entry should still direct-call. Got:\n{code}"
    );
}

#[test]
fn sealed_class_repeated_within_one_package_sends_direct() {
    // No overcorrection: the same package declaring the class twice (e.g. a
    // class duplicated across the package's own files, BT-3679) is still a
    // single export, and another package's differently-named class is no
    // collision.
    let mut other = ambient_sealed_object(false, Some("xml"));
    other.name = "OtherWidget".into();
    let code = codegen_caller_with_scope(vec![
        ambient_sealed_object(false, Some("json")),
        ambient_sealed_object(false, Some("json")),
        other,
    ]);
    assert!(
        code.contains("call 'bt@json@ambient_widget':'dispatch'('frobnicate'"),
        "a single package's sealed class should still direct-call. Got:\n{code}"
    );
}

/// BT-4069 (review F1), BT-4293: concrete Value/Object builtins whose
/// instances are `'$beamtalk_class'`-tagged maps with a compiled module — the
/// complement of the routing table's non-tagged-map rows
/// ([`is_non_tagged_map_class`](crate::core_erlang::routing::is_non_tagged_map_class),
/// generated from the shared routing fixture) over the concrete builtins
/// outside the exception and supervisor families. Every entry is
/// `.bt`-defined (generated builtin: a compiled `bt@stdlib@…` module
/// exporting `dispatch/3` exists), so a tag match routes to that same module
/// dynamically and statically alike. A newly added builtin fails
/// `all_builtins_are_classified_for_static_dispatch` until its author puts it
/// here or in the routing fixture — with the routing reason — so an
/// `ErlangModule`-like (native proxy) or `Exception`-like (alternate entry)
/// addition cannot slip through unclassified.
const KNOWN_TAGGED_BUILTINS: &[&str] = &[
    "ActorSpawned",
    "ActorStopped",
    "AnnouncementNavigation",
    "Announcer",
    "Array",
    "AtomicCounter",
    "Bag",
    "Beamtalk",
    "BindingChanged",
    "BindingsView",
    "ChangeEntry",
    "ChangeLog",
    "ClassLoaded",
    "ClassRemoved",
    "CompiledMethod",
    "Console",
    "DateTime",
    "Digest",
    "Duration",
    "Erlang",
    "Ets",
    "File",
    "FileHandle",
    "FlushCompleted",
    "Inspector",
    "InspectorField",
    "Interval",
    "Json",
    "Logger",
    "Node",
    "NodeDown",
    "NodeShapeSkew",
    "NodeUp",
    "OS",
    "ObjectStateChanged",
    "Package",
    "Parallel",
    "ProcessDictionary",
    "ProcessNavigation",
    "Program",
    "Protocol",
    "Queue",
    "Random",
    "Regex",
    "ReplyTo",
    "Result",
    "RetryPolicy",
    "Session",
    "Set",
    "StackFrame",
    "Stream",
    "Subscription",
    "SubscriptionNode",
    "SupervisionChildAdded",
    "SupervisionChildCrashed",
    "SupervisionNode",
    "SupervisionSpec",
    "SupervisionTree",
    "SupervisorChild",
    "System",
    "SystemAnnouncer",
    "SystemNavigation",
    "Telemetry",
    "TestCase",
    "TestResult",
    "TestRunner",
    "Time",
    "Timer",
    "Tls",
    "Tracing",
    "Transcript",
    "Uuid",
    "Workspace",
];

#[test]
fn all_builtins_are_classified_for_static_dispatch() {
    use beamtalk_core::ast::ClassKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
    let module = beamtalk_core::ast::Module::new(
        Vec::new(),
        beamtalk_core::source_analysis::Span::new(0, 0),
    );
    let (hierarchy_result, _) = ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("builtin hierarchy should build");
    let mut unclassified = Vec::new();
    for name in hierarchy.class_names() {
        let info = hierarchy
            .get_class(name)
            .unwrap_or_else(|| panic!("{name} should still resolve"));
        // BT-4293: open classes are candidates too; abstract classes,
        // ancestors of representation-routed built-ins, the supervisor family
        // and the exception family are gated separately (each with its own
        // negative test).
        if info.is_abstract
            || crate::core_erlang::dispatch_codegen::is_representation_ancestor(&hierarchy, name)
            || crate::core_erlang::dispatch_codegen::supervisor_family_root(&hierarchy, name)
                .is_some()
            || crate::core_erlang::dispatch_codegen::is_exception_family(&hierarchy, name)
        {
            continue;
        }
        if !matches!(
            hierarchy.resolve_class_kind(name),
            ClassKind::Value | ClassKind::Object
        ) {
            continue;
        }
        if info.is_module_less {
            // Separately gated (dispatch condition 5, with its own negative
            // tests): only runtime-only `Future` and synthetic protocol roots
            // may be module-less here.
            assert!(
                name == "Future" || hierarchy.is_protocol_class(name),
                "{name} is newly module-less — confirm the dispatch gates exclude it"
            );
            continue;
        }
        if !(crate::core_erlang::routing::is_non_tagged_map_class(name.as_str())
            || KNOWN_TAGGED_BUILTINS.contains(&name.as_str()))
        {
            unclassified.push(name.to_string());
        }
    }
    unclassified.sort();
    assert!(
        unclassified.is_empty(),
        "new builtins {unclassified:?} — classify each: a row of \
         tagged_map_routing_conformance.json (never a tagged map of its own \
         name) or KNOWN_TAGGED_BUILTINS \
         (.bt-defined: compiled module + tagged-map instances)"
    );
}

#[test]
fn send_handlers_keep_priority_positions() {
    // BT-4069 (review F2): `HANDLERS` order is load-bearing — `typed_guarded`
    // must stay last (it converts only would-be-dynamic sends) and
    // `character_typed` first. A handler appended after `typed_guarded`
    // would silently never fire for statically typed receivers.
    let handlers = crate::core_erlang::dispatch_codegen::HANDLERS;
    assert_eq!(
        handlers.first().map(|(name, _)| *name),
        Some("character_typed"),
        "character_typed must stay first"
    );
    assert_eq!(
        handlers.last().map(|(name, _)| *name),
        Some("typed_guarded"),
        "typed_guarded must stay last"
    );
}

#[test]
fn non_tagged_exclusions_are_sealed_builtin_values() {
    // Every non-tagged-map row of the routing table must still be a sealed
    // builtin of Value- or Object-kind. The routing table keeps these classes
    // off the typed-tagged path (and gives them a representation-guarded
    // route instead, BT-4294), so a row that became open must have its
    // subclasses' representation re-checked: a subclass whose instances are
    // tagged maps would need the tagged path, not the row's.
    use beamtalk_core::ast::ClassKind;
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
    let module = beamtalk_core::ast::Module::new(
        Vec::new(),
        beamtalk_core::source_analysis::Span::new(0, 0),
    );
    let (hierarchy_result, _) = ClassHierarchy::build(&module);
    let hierarchy = hierarchy_result.expect("builtin hierarchy should build");
    for name in crate::core_erlang::routing::non_tagged_map_classes() {
        let info = hierarchy
            .get_class(name)
            .unwrap_or_else(|| panic!("{name} should still be a builtin class"));
        assert!(
            info.is_sealed,
            "{name} is no longer sealed — re-check its routing fixture row"
        );
        assert!(
            matches!(
                hierarchy.resolve_class_kind(name),
                ClassKind::Value | ClassKind::Object
            ),
            "{name} is no longer Value/Object-kind — re-check its representation"
        );
    }
}

#[test]
fn character_literal_keeps_character_dispatch() {
    // `$A` recurses through `HANDLERS` (including the new trailing entry)
    // before falling back to `generate_character_typed_dispatch`: the new
    // handler must not steal it (`Character` is a raw integer at runtime).
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  m -> Object =>\n",
        "    $A uppercase\n",
    ));
    assert!(
        code.contains("call 'bt@stdlib@character':'dispatch'('uppercase'"),
        "Character literal should still reach the Character module. Got:\n{code}"
    );
    assert!(
        !code.contains("beamtalk_message_dispatch':'send'("),
        "Character literal send should not go dynamic. Got:\n{code}"
    );
}

// ---------------------------------------------------------------------------
// BT-4200: hardening residuals
// ---------------------------------------------------------------------------

#[test]
fn own_class_redefined_into_new_module_targets_the_emitted_module() {
    // BT-4200 (1): the index is captured before the unit loads (REPL: the live
    // `build_class_module_index/0`), so a class compiled as `bt@pkg@pt` and
    // redefined inline into `bt@pt` is still indexed under the old module.
    // The unit's own class must resolve to the module the unit emits, or the
    // guarded direct call lands in the stale old module.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "sealed Value subclass: Pt\n",
        "  field: x :: Integer = 0\n",
        "  sameX: other :: Pt -> Integer =>\n",
        "    other x\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("Pt".to_string(), "bt@pkg@pt".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@pt").with_class_module_index(index),
    )
    .expect("codegen should succeed");
    // BT-4473: a local call of the arm's function, so always this module.
    assert!(
        code.contains("apply 'x'/1("),
        "typed send must target the unit's own emitted module. Got:\n{code}"
    );
    assert!(
        !code.contains("bt@pkg@pt"),
        "must not target the stale module from the pre-load index. Got:\n{code}"
    );
}

#[test]
fn stale_compiled_metadata_for_module_less_redefinition_stays_dynamic() {
    // BT-4200 (2): a `ClassBuilder ... register` over an existing compiled
    // class never refreshes compiler-server metadata (`is_module_less` stays
    // `false`), while the live index now maps the class to its own name (the
    // module-less convention). The fresher index must win: no direct call.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "AmbientWidget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_class_hierarchy(vec![ambient_sealed_object(false, None)])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("'dispatch'('frobnicate'"),
        "index says module-less: must stay dynamic despite stale metadata. Got:\n{code}"
    );
}

#[test]
fn stale_module_less_metadata_for_compiled_redefinition_goes_direct() {
    // BT-4200 (2), converse: metadata says module-less but the live index maps
    // the class to a compiled `bt@` module (a later compiled load). One
    // consistent source: the index decides.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  useIt: w :: AmbientWidget -> Object =>\n",
        "    w frobnicate\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let mut index = std::collections::HashMap::new();
    index.insert("AmbientWidget".to_string(), "bt@ambient_widget".to_string());
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("test")
            .with_class_hierarchy(vec![ambient_sealed_object(true, None)])
            .with_class_module_index(index),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("call 'bt@ambient_widget':'dispatch'('frobnicate'"),
        "index says compiled: should direct-call. Got:\n{code}"
    );
}

#[test]
fn abstract_value_new_stays_a_plain_builder_for_subclass_chaining() {
    // BT-4200 (4), documented-not-fixed: a direct `Mod:new()` skips the
    // gen_server abstract rejection, and the callee cannot add one — a Value
    // subclass's `new/0` chains through its superclass module's `new/0` to
    // collect inherited field defaults, so an abstract class's `new/0` must
    // stay a plain map builder. Pin both halves of that dependency so a future
    // "raise in abstract new/0" change cannot silently break subclass `new`.
    let parent = codegen(concat!(
        "abstract Value subclass: Shape\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        parent.contains("'new'/0 = fun () ->") && !parent.contains("instantiation_error"),
        "abstract Value new/0 must remain a plain builder. Got:\n{parent}"
    );
    let child = codegen(concat!(
        "Shape subclass: Circle\n",
        "  field: r :: Integer = 0\n\n",
        "abstract Value subclass: Shape\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        child.contains(":'new'() in"),
        "subclass new/0 must chain through the superclass module's new/0. Got:\n{child}"
    );
}

// BT-4200 (3)'s exclusion-set-vs-fixture test is structural since BT-4292:
// the exclusion set is generated from the shared fixture
// (`routing::routing_table_tests::routing_table_is_generated_from_fixture`).

// ---------------------------------------------------------------------------
// BT-4127: `obj class` on a typed tagged-map receiver builds the class object
// inline behind the same tag guard; every other receiver keeps the plain
// `class_of_object/1` call.
// ---------------------------------------------------------------------------

/// The plain `obj class` lowering (and the inline shape's miss arms).
const CLASS_OF_OBJECT: &str = "call 'beamtalk_primitive':'class_of_object'(";

/// Asserts `code` lowers `obj class` to the plain `class_of_object/1` call,
/// with no inline class object.
fn assert_plain_class_of(code: &str, why: &str) {
    assert!(
        code.contains(CLASS_OF_OBJECT),
        "{why}: `class` should stay a class_of_object/1 call. Got:\n{code}"
    );
    assert!(
        !code.contains("TClassRecv"),
        "{why}: `class` must not take the inline path. Got:\n{code}"
    );
}

#[test]
fn sealed_value_typed_class_is_inlined_behind_the_tag_guard() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  classOf: w :: SealedWidget -> Object =>\n",
        "    w class\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("maps':'get'('$beamtalk_class', _TClassRecv"),
        "inline class should be guarded on the runtime class tag. Got:\n{code}"
    );
    assert!(
        code.contains(
            "<'SealedWidget'> when 'true' -> case call 'erlang':'whereis'('beamtalk_class_SealedWidget') of"
        ),
        "a tag match should look the class pid up by its compile-time registered name. Got:\n{code}"
    );
    assert!(
        code.contains("{'beamtalk_object', 'SealedWidget class', 'bt@sealed_widget', _TClassPid"),
        "a live class should answer the class object with the pinned module. Got:\n{code}"
    );
    // Miss arms: the inner tag mismatch, the non-map arm and a removed class
    // (`whereis` answers 'undefined') each fall back to the dynamic call.
    assert_eq!(
        code.matches("call 'beamtalk_primitive':'class_of_object'(_TClassRecv")
            .count(),
        3,
        "every miss arm should fall back to class_of_object/1. Got:\n{code}"
    );
    assert!(
        !code.contains("class_of_object_by_name") && !code.contains("class_object_tag"),
        "the inline path builds no tag at runtime. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn typed_class_binds_the_receiver_once() {
    // The receiver is a send: it must run exactly once, before the guard, and
    // every arm reads the bound temp.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  classOf: f :: Factory -> Object =>\n",
        "    f make class\n\n",
        "Object subclass: Factory\n",
        "  make -> SealedWidget => SealedWidget new\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    // Bound once, read by is_map, maps:get and the three fallbacks.
    assert_eq!(
        code.matches("TClassRecv").count(),
        6,
        "receiver should be bound once and read five times. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn open_value_typed_class_is_inlined() {
    // BT-4293: the guard matches the exact tag, so a subclass instance
    // misses it and takes `class_of_object/1` — `sealed` is not needed.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  classOf: w :: OpenWidget -> Object =>\n",
        "    w class\n\n",
        "Value subclass: OpenWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("{'beamtalk_object', 'OpenWidget class', 'bt@open_widget', _TClassPid"),
        "open typed receiver should inline the class object too. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn typed_class_inside_an_actor_is_verifier_clean() {
    // Debug assertions are on, so a ThreadedIr verifier finding panics here.
    let code = codegen(concat!(
        "Actor subclass: Caller\n",
        "  state: last :: Object = nil\n",
        "  classOf: w :: SealedWidget -> Object =>\n",
        "    self.last := w class\n",
        "    self.last\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert!(
        code.contains("{'beamtalk_object', 'SealedWidget class', 'bt@sealed_widget', _TClassPid"),
        "actor-side typed receiver should inline the class object. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

#[test]
fn untyped_class_stays_plain() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  classOf: w -> Object =>\n",
        "    w class\n\n",
        "sealed Value subclass: SealedWidget\n",
        "  field: x :: Integer = 0\n",
    ));
    assert_plain_class_of(&code, "untyped receiver");
}

#[test]
fn class_of_non_tagged_and_excluded_receivers_stays_plain() {
    // Built-ins (even ones with a guarded send route), abstract classes, the
    // exception family and representation ancestors: no inline class object.
    for (ty, extra) in [
        ("Integer", ""),
        ("List(Integer)", ""),
        ("String", ""),
        (
            "AbstractShape",
            "abstract Value subclass: AbstractShape\n  field: x :: Integer = 0\n",
        ),
        (
            "SealedBoom",
            "sealed typed Exception subclass: SealedBoom\n  answer -> Integer => 42\n",
        ),
        ("Error", ""),
        ("Object", ""),
    ] {
        let code = codegen(&format!(
            "Object subclass: Caller\n  classOf: r :: {ty} -> Object =>\n    r class\n\n{extra}"
        ));
        assert_plain_class_of(&code, ty);
    }
}

#[test]
fn module_less_ambient_class_of_stays_plain() {
    let src = concat!(
        "Object subclass: Caller\n",
        "  classOf: w :: AmbientWidget -> Object =>\n",
        "    w class\n",
    );
    let code = codegen_with_ambient(src, ambient_sealed_object(true, None));
    assert_plain_class_of(&code, "module-less receiver");
    // Positive control: the same class with a compiled module is inlined, so
    // the negative above cannot pass vacuously.
    let code = codegen_with_ambient(src, ambient_sealed_object(false, None));
    assert!(
        code.contains("{'beamtalk_object', 'AmbientWidget class', 'bt@ambient_widget', _TClassPid"),
        "module-ful ambient receiver should inline the class object. Got:\n{code}"
    );
}

#[test]
fn shadowed_class_of_stays_plain() {
    // BT-4151: two packages' `AmbientWidget` collide into one last-wins
    // runtime class, so no module can be pinned.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(concat!(
        "Object subclass: Caller\n",
        "  classOf: w :: AmbientWidget -> Object =>\n",
        "    w class\n",
    ));
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@app@caller").with_class_hierarchy(vec![
            ambient_sealed_object(false, Some("json")),
            ambient_sealed_object(false, Some("xml")),
        ]),
    )
    .expect("codegen should succeed");
    assert_plain_class_of(&code, "shadowed class name");
}

#[test]
fn packaged_ambient_class_of_pins_the_qualified_module() {
    // The module pin is `typed_tagged_target`'s, so it follows the same
    // resolution as the direct `dispatch/3` call.
    let code = codegen_with_ambient(
        concat!(
            "Object subclass: Caller\n",
            "  classOf: w :: AmbientWidget -> Object =>\n",
            "    w class\n",
        ),
        ambient_sealed_object(false, Some("live")),
    );
    assert!(
        code.contains(
            "{'beamtalk_object', 'AmbientWidget class', 'bt@live@ambient_widget', _TClassPid"
        ),
        "packaged ambient receiver should pin the qualified module. Got:\n{code}"
    );
}

#[test]
fn interpolation_segment_display_string_uses_the_typed_route() {
    // BT-4510: each interpolation segment's `displayString` is the same
    // typed-send decision as a written `x displayString`: a typed segment
    // calls its class's `dispatch/3` behind the guard, an untyped one
    // sends dynamically.
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  show: n :: Integer name: s :: String -> String =>\n",
        "    \"{n} {s}\"\n",
        "  showAny: x -> String =>\n",
        "    \"{x}\"\n",
    ));
    for module in ["bt@stdlib@integer", "bt@stdlib@string"] {
        assert!(
            code.contains(&format!("call '{module}':'dispatch'('displayString', []")),
            "a typed segment should direct-call {module}. Got:\n{code}"
        );
    }
    assert!(
        code.contains("'displayString', [])"),
        "the untyped segment (and every miss arm) keeps the dynamic send. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}

/// BT-4595: `-spec`-extracted (`Extracted`) provenance is accepted for the
/// members of a union chain only: a union-typed FFI result takes the guard
/// chain, while a single-class `Extracted` receiver stays dynamic.
#[test]
fn extracted_provenance_is_trusted_for_union_members_only() {
    use beamtalk_core::semantic_analysis::type_checker::{FunctionSignature, NativeTypeRegistry};
    use beamtalk_core::semantic_analysis::{InferredType, TypeProvenance};

    let extracted = |name: &str| InferredType::Known {
        class_name: ecow::EcoString::from(name),
        type_args: vec![],
        provenance: TypeProvenance::Extracted,
    };
    let signature = |name: &str, return_type: InferredType| FunctionSignature {
        name: name.to_string(),
        arity: 0,
        params: vec![],
        return_type,
        provenance: TypeProvenance::Extracted,
        line: None,
    };
    let mut registry = NativeTypeRegistry::new();
    registry.register_module(
        "ffi_mod",
        vec![
            signature(
                "either",
                InferredType::Union {
                    members: vec![extracted("Integer"), extracted("List")],
                    provenance: TypeProvenance::Extracted,
                },
            ),
            signature("one", extracted("Integer")),
        ],
    );
    let compile = |call: &str| {
        let src = format!(
            "Object subclass: Caller\n  useIt =>\n    (Erlang ffi_mod {call}) frobnicate\n"
        );
        let module = beamtalk_core::source_analysis::parse(
            beamtalk_core::source_analysis::lex_with_eof(&src),
        )
        .0;
        crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test")
                .with_native_type_registry(Some(std::sync::Arc::new(registry.clone()))),
        )
        .expect("codegen should succeed")
    };
    let union = compile("either");
    assert!(
        union.contains("'bt@stdlib@integer':'dispatch'('frobnicate'")
            && union.contains("'bt@stdlib@list':'dispatch'('frobnicate'"),
        "extracted union receiver should take the guard chain. Got:\n{union}"
    );
    let single = compile("one");
    assert!(
        !single.contains("'bt@stdlib@integer':'dispatch'('frobnicate'"),
        "single-class extracted receiver must stay dynamic. Got:\n{single}"
    );
}

/// BT-4595: a worst-case union of built-in guards binds the dynamic send once
/// instead of repeating it in every miss arm of every guard.
#[test]
fn builtin_union_chain_emits_the_dynamic_send_once() {
    let code = codegen(concat!(
        "Object subclass: Caller\n",
        "  useIt: r :: Symbol | String | Binary | Tuple -> Object =>\n",
        "    r frobnicate\n",
    ));
    assert_eq!(
        code.matches("call 'beamtalk_message_dispatch':'send'(")
            .count(),
        1,
        "dynamic send should be bound once. Got:\n{code}"
    );
    super::assert_compiles_through_erlc("test", &code);
}
