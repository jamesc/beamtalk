// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4030: generated `new/1` (value types) and `init/1` (actors) validate
//! the initialiser map against the merged defaults via
//! `beamtalk_class_instantiation:check_init_args/5`, raising `#unknown_field`
//! for undeclared keys instead of silently merging them. The allowed set
//! derives from the runtime state map — never a codegen-baked field list —
//! so inherited fields stay accepted even when the ancestor is invisible at
//! compile time (standalone file builds, REPL snippets).

use super::*;

// ── value `new/1` ─────────────────────────────────────────────────────

/// A `Value subclass:` `new/1` validates `InitArgs` against the default
/// state's keys and merges the checked map.
#[test]
fn test_value_new_validates_init_args_against_default_state() {
    let src = concat!(
        "Value subclass: Conf\n",
        "  field: port = 4000\n",
        "  field: handler = nil\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("conf")).expect("codegen should succeed");

    let new = extract_core_fn(&code, "'new'/1 = fun").expect("new/1 must be generated");
    assert!(
        new.contains(
            "'beamtalk_class_instantiation':'check_init_args'\
             ('Conf', 'new:', DefaultState, InitArgs, [])"
        ),
        "new/1 must validate InitArgs against DefaultState. Got:\n{new}"
    );
    assert!(
        new.contains("call 'maps':'merge'(DefaultState, CheckedArgs)"),
        "new/1 must merge the validated CheckedArgs. Got:\n{new}"
    );
}

// ── actor `init/1` ────────────────────────────────────────────────────

/// A base actor's `init/1` validates `InitArgs` against `DefaultState`
/// before merging — leaf call only, since only the leaf sees the full key
/// set (parent-helper calls carry the leaf's own keys plus the
/// `__skip_initialize__` flag and must skip).
#[test]
fn test_actor_init_base_validates_init_args_leaf_only() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    let init = extract_core_fn(&code, "'init'/1 = fun").expect("init/1 must be generated");
    assert!(
        init.contains(
            "'beamtalk_class_instantiation':'check_init_args'\
             ('Counter', 'spawnWith:', DefaultState, InitArgs, ['$bt_init_ack'])"
        ),
        "init/1 must validate InitArgs against DefaultState. Got:\n{init}"
    );
    assert!(
        init.contains("call 'maps':'merge'(DefaultState, CheckedArgs)"),
        "init/1 must merge the validated CheckedArgs. Got:\n{init}"
    );
    // The parent-helper call (flag present) skips validation: its InitArgs
    // legitimately carries keys outside this class's own set.
    assert!(
        init.contains("<'true'> when 'true' -> InitArgs"),
        "init/1 must pass parent-helper InitArgs through unvalidated. Got:\n{init}"
    );
}

/// A subclass's `init/1` validates against the *merged* state — parent
/// defaults plus its own fields — with no baked field list anywhere, so a
/// subclass compiled standalone (ancestor invisible, the
/// `beamtalk_codegen_simulation_tests` shape) still accepts inherited keys.
#[test]
fn test_actor_init_subclass_validates_against_merged_state() {
    // LoggingCounter extends Counter (itself an Actor subclass).
    // Compiling only LoggingCounter — Counter's AST is absent.
    let src = concat!(
        "Counter subclass: LoggingCounter\n",
        "  state: logCount = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_counter"))
        .expect("codegen should succeed");

    let init = extract_core_fn(&code, "'init'/1 = fun").expect("init/1 must be generated");
    assert!(
        init.contains(
            "'beamtalk_class_instantiation':'check_init_args'\
             ('LoggingCounter', 'spawnWith:', MergedState, InitArgs, ['$bt_init_ack'])"
        ),
        "subclass init/1 must validate against MergedState. Got:\n{init}"
    );
    assert!(
        init.contains("call 'maps':'merge'(MergedState, CheckedArgs)"),
        "subclass init/1 must merge the validated CheckedArgs. Got:\n{init}"
    );
    assert!(
        !init.contains("['logCount']"),
        "init/1 must not bake a literal field list (it would miss \
         ancestor-invisible fields). Got:\n{init}"
    );
}

// ── actor `spawn/1` error arm ─────────────────────────────────────────

/// `spawn/1`'s start-failure arm re-raises `init/1`'s own `#unknown_field`
/// transparently while every other failure keeps the
/// `instantiation_error` wrap (a throwing `initialize` must stay wrapped —
/// see `actor_initialize_test.bt`).
#[test]
fn test_spawn_reraises_init_unknown_field_keeps_other_wraps() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    let spawn = extract_core_fn(&code, "'spawn'/1 = fun").expect("spawn/1 must be generated");
    assert!(
        spawn.contains("call 'beamtalk_error':'extract_beamtalk_error'(Reason, 4)"),
        "spawn/1 must look for a beamtalk_error in the start reason. Got:\n{spawn}"
    );
    assert!(
        spawn.contains("<'true'> when 'true' -> call 'beamtalk_error':'raise'(InitErr)"),
        "spawn/1 must re-raise init/1's unknown_field unwrapped. Got:\n{spawn}"
    );
    assert!(
        spawn.contains("'instantiation_error'"),
        "spawn/1 must keep the instantiation_error wrap for other failures. Got:\n{spawn}"
    );
}
