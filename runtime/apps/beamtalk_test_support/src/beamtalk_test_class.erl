%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_class).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit class-process fixtures (BT-4677): start and stop a class
gen_server registered through `beamtalk_object_class`, and load a fixture
module on demand. Replaces the per-file `start_class/N`, `stop_class/1`,
`ensure_fixture_loaded/1` copies.
""".

-export([start/2, start/3, stop/1, ensure_fixture_loaded/2, ensure_counter_loaded/0]).

-doc "`start/3` with the default `ClassInfo` overrides (none).".
-spec start(atom(), map()) -> pid().
start(Name, ClassInfo) -> start(Name, ClassInfo, #{}).

-doc """
Start class `Name` with `ClassInfo` merged over `Overrides` (overrides win),
unlink it from the caller so a crash in the test body does not take the class
down mid-teardown, and return its pid.
""".
-spec start(atom(), map(), map()) -> pid().
start(Name, ClassInfo, Overrides) ->
    {ok, Pid} = beamtalk_object_class:start_link(Name, maps:merge(ClassInfo, Overrides)),
    unlink(Pid),
    Pid.

-doc """
Stop a class started by `start/2,3`: unwatch from the class monitor, kill and
wait on the `'DOWN'` (see `beamtalk_test_class_teardown`). Accepts a pid or
class name; dead/unknown values are ignored.
""".
-spec stop(pid() | atom()) -> ok.
stop(Name) when is_atom(Name) ->
    case beamtalk_class_registry:whereis_class(Name) of
        undefined -> ok;
        Pid -> stop(Pid)
    end;
stop(Pid) ->
    beamtalk_test_class_teardown:stop_class_process(Pid).

-doc """
Ensure fixture `Module` (a compiled class module, e.g. `'bt@counter'`) is
loaded and `ClassName` is registered: loads it (from the code path, or from a
`.beam` path when `Module` is a string), calls its `register_class/0` when the
class is not yet registered, then waits for the registration. Idempotent;
crashes the test (never skips) if the class does not register.
""".
-spec ensure_fixture_loaded(atom(), atom() | file:filename()) -> ok.
ensure_fixture_loaded(ClassName, Module) when is_atom(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    register_if_needed(ClassName, Module);
ensure_fixture_loaded(ClassName, BeamPath) ->
    {module, Module} = code:load_abs(BeamPath),
    register_if_needed(ClassName, Module).

register_if_needed(ClassName, Module) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            case erlang:function_exported(Module, register_class, 0) of
                true ->
                    Module:register_class(),
                    beamtalk_test_wait:class(ClassName);
                false ->
                    %% Registered by the module's own on_load, if at all.
                    beamtalk_test_wait:class(ClassName)
            end;
        _ ->
            ok
    end.

-doc "Ensure the compiled `Counter` fixture (`bt@counter`) is loaded and registered.".
-spec ensure_counter_loaded() -> ok.
ensure_counter_loaded() ->
    ensure_fixture_loaded('Counter', 'bt@counter').
