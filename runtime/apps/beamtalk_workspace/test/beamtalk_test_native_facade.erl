%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_native_facade).

-moduledoc """
Test double for an ADR 0056 `native:` class facade (BT-2732).

Mirrors the shape the compiler's `native_facade.rs` emits for a class declared
`native: <module>`: a `__beamtalk_meta/0` reflection function reporting the
backing module, and one `dispatch_<selector>` function per `self delegate`
instance method. The bodies are irrelevant — `delegate_callers_of_native_module/1`
only reads `module_info(exports)` and `__beamtalk_meta/0` — so they are stubs.

The `spawn/0` export stands in for a non-delegate function that must NOT be
mistaken for a delegate selector.
""".

-export([
    '__beamtalk_meta'/0,
    dispatch_increment/1,
    'dispatch_incrementBy:'/2,
    spawn/0
]).

%% The backing native module this facade delegates into. A dedicated test atom
%% that no real class uses, so `delegate_callers_of_native_module/1` isolates on
%% it deterministically.
'__beamtalk_meta'() ->
    #{native => true, backing_module => bt_test_native_backing}.

dispatch_increment(_Self) ->
    ok.

'dispatch_incrementBy:'(_N, _Self) ->
    ok.

spawn() ->
    ok.
