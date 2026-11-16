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

-export([
    start/2, start/3,
    start_or_get/2,
    stop/1,
    with_class/2,
    ensure_fixture_loaded/2,
    ensure_counter_loaded/0,
    reload_fixture/2,
    stop_graceful/1,
    snapshot_instances/1,
    stop_instances_since/1
]).

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
Like `start/2`, but tolerates a class already registered under `Name` (returns
the live pid instead of crashing). Started with `beamtalk_object_class:start/2`,
so it is not linked to the caller.
""".
-spec start_or_get(atom(), map()) -> pid().
start_or_get(Name, ClassInfo) ->
    case beamtalk_object_class:start(Name, ClassInfo) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

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
Run `Fun()` and always stop the class process(es) afterwards (via
`beamtalk_test_proc:stop/1`: a graceful stop that tolerates an already-dead
class). `Pids` is one pid or a list, stopped in list order (children before
parents). Returns `Fun()`'s value; an exception from `Fun` propagates after the
stops. Replaces the hand-written `try Body after stop(Pid) end` wrapper.
""".
-spec with_class(pid() | [pid()], fun(() -> Result)) -> Result.
with_class(Pid, Fun) when is_pid(Pid) ->
    with_class([Pid], Fun);
with_class(Pids, Fun) when is_list(Pids) ->
    try
        Fun()
    after
        lists:foreach(fun beamtalk_test_proc:stop/1, Pids)
    end.

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
            register_fixture(ClassName, Module);
        Pid ->
            %% A class of this name is registered. It is only the fixture if the
            %% fixture module backs it; an earlier test may have left a
            %% different definition under the same name (BT-4934), which would
            %% make this a silent no-op and the caller's asserts fail on the
            %% wrong class. Replace such a stray with the fixture.
            case beamtalk_object_class:module_name_safe(Pid) of
                Module ->
                    ok;
                _Other ->
                    ok = stop_graceful(ClassName),
                    ok = beamtalk_test_wait:until(fun() ->
                        beamtalk_class_registry:whereis_class(ClassName) =:= undefined
                    end),
                    register_fixture(ClassName, Module)
            end
    end.

register_fixture(ClassName, Module) ->
    case erlang:function_exported(Module, register_class, 0) of
        true ->
            Module:register_class(),
            beamtalk_test_wait:class(ClassName);
        false ->
            %% Registered by the module's own on_load, if at all.
            beamtalk_test_wait:class(ClassName)
    end.

-doc """
Register `ClassName` from `Module` freshly: stop a registered class of that name
(deliberately, so the class monitor does not revive it) and register the fixture
again. `ensure_fixture_loaded/2` treats any class backed by `Module` as the
fixture, which holds for a class revived by the class monitor from compiled
metadata but not for its published reflection data (instance variables ...); a
test that asserts on that data starts from a fresh registration (BT-4934).
""".
-spec reload_fixture(atom(), atom()) -> ok.
reload_fixture(ClassName, Module) ->
    ok = stop_graceful(ClassName),
    ok = beamtalk_test_wait:until(fun() ->
        beamtalk_class_registry:whereis_class(ClassName) =:= undefined
    end),
    ensure_fixture_loaded(ClassName, Module).

-doc "Ensure the compiled `Counter` fixture (`bt@counter`) is loaded and registered.".
-spec ensure_counter_loaded() -> ok.
ensure_counter_loaded() ->
    ensure_fixture_loaded('Counter', 'bt@counter').

-doc """
Deliberately stop the class registered as `Name` (a normal `gen_server:stop`),
if any. Unlike `stop/1` this runs the class's `terminate` path, which removes
its registration and metadata rows (BT-4622) instead of leaving them for crash
recovery.
""".
-spec stop_graceful(atom()) -> ok.
stop_graceful(Name) ->
    case beamtalk_class_registry:whereis_class(Name) of
        Pid when is_pid(Pid) ->
            unlink(Pid),
            gen_server:stop(Pid);
        _ ->
            ok
    end.

-doc """
Remember which actor instances are alive now, under `Key`. Pair with
`stop_instances_since/1` in a suite whose tests spawn actors through the REPL
evaluator or a compiled class and cannot stop each one: an actor left running
stays in the instance registry and shows up in every later module's
`bottlenecks`/`actor_health` scans (BT-4934).
""".
-spec snapshot_instances(term()) -> ok.
snapshot_instances(Key) ->
    persistent_term:put({?MODULE, instances, Key}, live_instances()).

-doc """
Stop every actor instance that was not alive when `snapshot_instances(Key)`
ran, and forget the snapshot. A no-op when no snapshot exists.
""".
-spec stop_instances_since(term()) -> ok.
stop_instances_since(Key) ->
    case persistent_term:get({?MODULE, instances, Key}, undefined) of
        undefined ->
            ok;
        Before ->
            _ = persistent_term:erase({?MODULE, instances, Key}),
            lists:foreach(fun beamtalk_test_proc:stop/1, live_instances() -- Before)
    end.

live_instances() ->
    case ets:info(beamtalk_instance_class) of
        undefined -> [];
        _ -> [Pid || {Pid, _Class} <- ets:tab2list(beamtalk_instance_class), is_process_alive(Pid)]
    end.
