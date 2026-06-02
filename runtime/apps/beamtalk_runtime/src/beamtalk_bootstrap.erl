%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bootstrap).

%%% **DDD Context:** Object System Context

-moduledoc """
Bootstrap the runtime environment.

BT-446: Bootstrap no longer creates class processes for ProtoObject,
Object, and Actor. Those are now registered by their compiled stdlib
modules' on_load via beamtalk_stdlib:load_compiled_stdlib_modules().

Bootstrap only ensures pg (process group) is started, which is needed
by the class registry (beamtalk_object_class) before any classes load.

ADR 0032 Phase 0 (BT-732): Bootstrap registers the 'Class' stub
(beamtalk_class_bt) immediately after pg starts, before stdlib loads.
This is safe because dispatch works via beamtalk_class_bt:has_method/1
and dispatch/4 even before the superclass (Object) is registered.
See beamtalk_class_bt for the stub details.

ADR 0036 Phase 1 (BT-802): Bootstrap also registers the 'Metaclass' stub
(beamtalk_metaclass_bt) immediately after 'Class', completing the bootstrap
order: ProtoObject → Object → Behaviour → Class → Metaclass → Actor → user modules.

ADR 0038 Phase 1 (BT-835): Bootstrap also registers the 'ClassBuilder' stub
(beamtalk_class_builder_bt) immediately after 'Metaclass', before Actor.
New sequence: ProtoObject → Object → Behaviour → Class → Metaclass → ClassBuilder → Actor.
""".

-export([start_link/0, init/1]).

-doc """
Start the bootstrap process.

This is typically called during application startup to ensure pg is
running before class modules load.
""".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

-doc """
Initialize the runtime environment.

Starts pg (process group) which is required by the class registry.
Class processes are created by compiled stdlib modules' on_load.

ADR 0032 Phase 0 (BT-732): Also registers the 'Class' stub so the
class chain dispatch fallthrough is available from startup.

ADR 0036 Phase 1 (BT-802): Also registers the 'Metaclass' stub so that
metaclass objects dispatch through the Metaclass → Class chain.

ADR 0038 Phase 1 (BT-835): Also registers the 'ClassBuilder' stub so that
compiled class on_load hooks can call ClassBuilder before user modules load.
Bootstrap order: ProtoObject → Object → Behaviour → Class → Metaclass → ClassBuilder → Actor.
""".
-spec init(pid()) -> no_return().
init(Parent) ->
    %% Ensure pg is started (required by beamtalk_object_class for class registry)
    case whereis(pg) of
        undefined ->
            {ok, _Pid} = pg:start_link();
        _ ->
            ok
    end,

    %% ADR 0032 Phase 0 (BT-732): Register the 'Class' stub.
    %% This is safe to call before stdlib loads — the module is part of
    %% beamtalk_runtime, and dispatch works via beamtalk_class_bt:has_method/1
    %% and dispatch/4 even before the superclass (Object) is registered.
    beamtalk_class_bt:register_class(),

    %% ADR 0036 Phase 1 (BT-802): Register the 'Metaclass' stub after 'Class'.
    %% Metaclass has superclass 'Class', so Class must be registered first.
    beamtalk_metaclass_bt:register_class(),

    %% ADR 0038 Phase 1 (BT-835): Register the 'ClassBuilder' stub after 'Metaclass'.
    %% ClassBuilder has superclass 'Actor' (which may not yet be registered — safe
    %% because class references are resolved lazily). ClassBuilder must be wired
    %% before any user module loads so that on_load hooks can call ClassBuilder.
    beamtalk_class_builder_bt:register_class(),

    %% ADR 0087 Phase 2 (BT-2298): The Erlang stub classes above do not compile
    %% through codegen, so their `register_class/0` carries no baked method_xref
    %% (it defaults to []). Their methods are genuine sourceless runtime funs —
    %% record them explicitly with `source_status => unindexed_runtime_fun` so
    %% navigation queries report "defined here, sends not analysable" rather than
    %% treating the selectors as absent. xref is the first non-pg supervisor child
    %% (ahead of bootstrap), so it is alive by the time this runs.
    register_stub_class_xref(),

    proc_lib:init_ack(Parent, {ok, self()}),
    bootstrap_loop().

-doc """
Register `unindexed_runtime_fun` xref rows for the Erlang stub classes
(ADR 0087 Phase 2, BT-2298).

The metaclass-tower scaffolding (`Behaviour`, `Class`, `Metaclass`,
`ClassBuilder`) is hand-coded in Erlang, not compiled from `.bt` source, so
its methods have no analysable Beamtalk body. Each is registered with empty
`sends` / `references` and `source_status => unindexed_runtime_fun`, the
genuine narrow sourceless category. `Behaviour` and `ClassBuilder` expose no
stub methods, so they contribute no rows.

Skipped silently if `beamtalk_xref` is not running (e.g. a minimal embedded
runtime without the index).
""".
-spec register_stub_class_xref() -> ok.
register_stub_class_xref() ->
    case whereis(beamtalk_xref) of
        undefined ->
            ok;
        _Pid ->
            %% 'Class': instance-side classBuilder (via has_method/1), class-side superclass.
            ok = beamtalk_xref:register_class('Class', [
                stub_method_entry(false, 'classBuilder'),
                stub_method_entry(true, 'superclass')
            ]),
            %% 'Metaclass': instance-side isMeta / isClass / isMetaclass (via has_method/1).
            ok = beamtalk_xref:register_class('Metaclass', [
                stub_method_entry(false, 'isMeta'),
                stub_method_entry(false, 'isClass'),
                stub_method_entry(false, 'isMetaclass')
            ]),
            ok
    end.

-doc """
Build a single `unindexed_runtime_fun` method_xref entry for a stub-class
method. The `line` is a placeholder (`1`): these methods have no source, but
`beamtalk_xref` requires a `pos_integer()`.
""".
-spec stub_method_entry(boolean(), atom()) -> map().
stub_method_entry(ClassSide, Selector) ->
    #{
        class_side => ClassSide,
        selector => Selector,
        line => 1,
        sends => [],
        references => [],
        source_status => unindexed_runtime_fun,
        provenance => class_body
    }.

-doc "Bootstrap process loop - stays alive as part of the supervision tree".
bootstrap_loop() ->
    receive
        _Any -> bootstrap_loop()
    end.
