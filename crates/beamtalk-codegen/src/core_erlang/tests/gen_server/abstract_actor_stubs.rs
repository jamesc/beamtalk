// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Codegen for abstract actor classes: stub `spawn`, `handle_continue`,
//! `handle_cast`/`handle_call`, and safe-dispatch passthrough.

use super::*;

// ── Abstract actor codegen ──────────────────────────────────
//
// abstract Actor subclass generates:
//  - spawn/0 and spawn/1 as instantiation_error stubs (not safe_spawn calls)
//  - minimal gen_server callback stubs (noreply/reply-nil, no initialize chain)
//  - simple safe_dispatch pass-through (no try-catch error isolation)
//
// None of these paths were exercised by the parse-based codegen() helper before
// these tests were added; only AST-constructed module tests touched is_abstract.

#[test]
fn test_abstract_actor_spawn_raises_instantiation_error() {
    // abstract classes generate spawn/0 as an error stub, not safe_spawn.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'instantiation_error'"),
        "abstract spawn/0 must raise instantiation_error. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_error':'with_hint'("),
        "abstract spawn/0 must call with_hint (hint text included). Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_actor':'safe_spawn'"),
        "abstract spawn/0 must NOT call beamtalk_actor:safe_spawn. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_spawn_1_is_error_stub() {
    // abstract classes generate spawn/1 as an error stub too.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'spawn'/1 = fun (_InitArgs) ->"),
        "abstract spawn/1 should be declared as fun (_InitArgs). Got:\n{code}"
    );
    assert!(
        code.contains("'spawnWith:'"),
        "abstract spawn/1 error stub must set the 'spawnWith:' selector on the error. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_has_stub_handle_continue() {
    // abstract classes emit a minimal handle_continue/2 stub with no
    // initialize dispatch chain — the chain would be unreachable anyway since
    // abstract actors can never be instantiated.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'handle_continue'/2 = fun (_Continue, State) -> {'noreply', State}"),
        "abstract actor handle_continue/2 must be a noreply stub. Got:\n{code}"
    );
    assert!(
        !code.contains("'initialize'"),
        "abstract actor must not emit an initialize dispatch in handle_continue. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_has_stub_handle_cast_and_call() {
    // abstract actor handle_cast/2 and handle_call/3 are stubs.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'handle_cast'/2 = fun (_Msg, State) -> {'noreply', State}"),
        "abstract actor handle_cast/2 must be a noreply stub. Got:\n{code}"
    );
    assert!(
        code.contains("'handle_call'/3 = fun (_Msg, _From, State) -> {'reply', 'nil', State}"),
        "abstract actor handle_call/3 must be a reply-nil stub. Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_safe_dispatch_is_simple_passthrough() {
    // abstract actor safe_dispatch/3 is a plain call to dispatch/4, not
    // the try-catch error-isolation wrapper used by concrete actors.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    assert!(
        code.contains("'safe_dispatch'/3"),
        "abstract actor must still export safe_dispatch/3. Got:\n{code}"
    );
    assert!(
        !code.contains("catch <Type, Error, Stacktrace>"),
        "abstract actor safe_dispatch must NOT have a try-catch (uses simple passthrough). Got:\n{code}"
    );
}

#[test]
fn test_abstract_actor_exports_include_spawn_and_has_method() {
    // Abstract actors still export spawn/0, spawn/1 and has_method/1 so
    // reflection works; the spawn bodies raise errors rather than starting a process.
    let src = "abstract Actor subclass: AbstractShape\n  area => 0\n";
    let code = codegen(src);
    let exports = extract_module_exports(&code);
    assert!(
        exports.contains("'spawn'/0"),
        "abstract actor must still export spawn/0. Exports:\n{exports}"
    );
    assert!(
        exports.contains("'spawn'/1"),
        "abstract actor must still export spawn/1. Exports:\n{exports}"
    );
    assert!(
        exports.contains("'has_method'/1"),
        "abstract actor must still export has_method/1. Exports:\n{exports}"
    );
}

#[test]
fn test_abstract_actor_with_class_method_exports_class_method() {
    // Actors with class-side methods must export them. This exercises the
    // build_class_method_export_doc path for an abstract actor with a class method.
    let src = concat!(
        "abstract Actor subclass: AbstractShape\n",
        "  area => 0\n",
        "\n",
        "  class create => self spawn\n",
    );
    let code = codegen(src);
    let exports = extract_module_exports(&code);
    assert!(
        exports.contains("'class_create'/"),
        "abstract actor class method 'create' must be exported as 'class_create'/N. Exports:\n{exports}"
    );
    assert!(
        code.contains("'class_create'/"),
        "abstract actor's class method must appear in the generated code. Got:\n{code}"
    );
}
