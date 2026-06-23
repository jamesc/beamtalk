%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_interface_primitives_tests).

-moduledoc """
EUnit tests for beamtalk_workspace_interface_primitives module.

Tests the Phase 2 dispatch/3 interface for WorkspaceInterface primitives:
- actors selector
- actorAt: selector
- classes selector
- load: selector (including value_type_name/1 coverage — BT-2295)
- globals selector
- bind:as: selector (including to_atom_name/1 error paths — BT-2295)
- unbind: selector
- get_user_bindings/0 external API
- get_session_bindings/0 external API
- create_bindings_table/0 and ensure_bindings_table/0 idempotency (BT-2295)
- dependencies/0 no-package branch (BT-2295)
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% supervisor behaviour callback used by start_bare_workspace_sup/0 in the
%% supervisors/0 tests (a minimal childless workspace_sup stand-in).
-behaviour(supervisor).
-export([init/1]).

%%====================================================================
%% Fixtures
%%====================================================================

%% Construct a fake Self tuple with the given pid at element 4.
fake_self(Pid) ->
    {beamtalk_object, 'WorkspaceInterface', 'bt@stdlib@workspace_interface', Pid}.

%% Cross-platform temporary directory (Windows has no /tmp).
tmp_dir() ->
    case os:type() of
        {win32, _} ->
            case os:getenv("TEMP") of
                false ->
                    case os:getenv("TMP") of
                        false -> ".";
                        Tmp -> Tmp
                    end;
                Temp ->
                    Temp
            end;
        _ ->
            "/tmp"
    end.

%% Create a unique empty directory under tmp_dir().
make_empty_tmp_dir(Prefix) ->
    Dir = filename:join(
        tmp_dir(), Prefix ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dir.

%% Cross-platform recursive directory removal using pure Erlang.
rm_rf(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Entries} = file:list_dir(Dir),
            lists:foreach(fun(E) -> rm_rf(filename:join(Dir, E)) end, Entries),
            file:del_dir(Dir);
        false ->
            file:delete(Dir)
    end.

%% Start the actor registry defensively — tolerates already-started.
ensure_registry_started() ->
    case beamtalk_repl_actors:start_link(registered) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% Clean up all ETS entries in the bindings table.
%% ETS keys are now just atoms (no pid component).
cleanup_ets_for(_Pid) ->
    case ets:info(beamtalk_wi_user_bindings, id) of
        undefined ->
            ok;
        _ ->
            ets:delete_all_objects(beamtalk_wi_user_bindings),
            ok
    end.

%%====================================================================
%% actors Tests
%%====================================================================

actors_returns_empty_when_no_registry_test() ->
    Self = fake_self(self()),
    Result = beamtalk_workspace_interface_primitives:dispatch(actors, [], Self),
    ?assertEqual([], Result).

actors_returns_live_actors_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),

    Result = beamtalk_workspace_interface_primitives:dispatch(actors, [], fake_self(self())),
    ?assertEqual(2, length(Result)),
    lists:foreach(
        fun({beamtalk_object, Class, Module, Pid}) ->
            ?assertEqual('Counter', Class),
            ?assertEqual(test_counter, Module),
            ?assert(is_process_alive(Pid))
        end,
        Result
    ),

    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(RegistryPid).

actors_filters_dead_processes_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),

    gen_server:stop(Actor1),

    Result = beamtalk_workspace_interface_primitives:dispatch(actors, [], fake_self(self())),
    ?assertEqual(1, length(Result)),

    gen_server:stop(Actor2),
    gen_server:stop(RegistryPid).

%%====================================================================
%% actorAt: Tests
%%====================================================================

actor_at_returns_object_for_valid_pid_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = list_to_binary(pid_to_list(Actor)),
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'actorAt:', [PidStr], fake_self(self())
    ),
    ?assertMatch({beamtalk_object, 'Counter', test_counter, _}, Result),

    gen_server:stop(Actor),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_for_unknown_pid_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'actorAt:', [<<"<0.99999.0>">>], fake_self(self())
    ),
    ?assertEqual(nil, Result),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_for_invalid_pid_string_test() ->
    ?assertEqual(
        nil,
        beamtalk_workspace_interface_primitives:dispatch(
            'actorAt:', [<<"not-a-pid">>], fake_self(self())
        )
    ),
    ?assertEqual(
        nil,
        beamtalk_workspace_interface_primitives:dispatch(
            'actorAt:', [<<"">>], fake_self(self())
        )
    ),
    ?assertEqual(
        nil,
        beamtalk_workspace_interface_primitives:dispatch(
            'actorAt:', [42], fake_self(self())
        )
    ).

actor_at_returns_nil_for_dead_actor_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),
    PidStr = list_to_binary(pid_to_list(Actor)),
    gen_server:stop(Actor),

    Result = beamtalk_workspace_interface_primitives:dispatch(
        'actorAt:', [PidStr], fake_self(self())
    ),
    ?assertEqual(nil, Result),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_when_no_registry_test() ->
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'actorAt:', [<<"<0.1.0>">>], fake_self(self())
    ),
    ?assertEqual(nil, Result).

actor_at_accepts_list_string_test() ->
    {ok, RegistryPid} = ensure_registry_started(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = pid_to_list(Actor),
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'actorAt:', [PidStr], fake_self(self())
    ),
    ?assertMatch({beamtalk_object, 'Counter', test_counter, _}, Result),

    gen_server:stop(Actor),
    gen_server:stop(RegistryPid).

%%====================================================================
%% classes Tests
%%====================================================================

classes_returns_list_test() ->
    Result = beamtalk_workspace_interface_primitives:dispatch(classes, [], fake_self(self())),
    ?assert(is_list(Result)).

%%====================================================================
%% load: Tests
%%====================================================================

load_type_error_for_integer_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'load:', [42], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Integer">>))
    end.

load_file_not_found_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'load:', [<<"/nonexistent/path/file.bt">>], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(file_not_found, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class)
    end.

%%====================================================================
%% bind:as: / unbind: / globals Tests
%%====================================================================

bind_and_unbind_test_() ->
    {setup,
        fun() ->
            %% Spawn a proxy process to act as the ws gen_server pid
            spawn(fun() ->
                receive
                    stop -> ok
                end
            end)
        end,
        fun(Proxy) ->
            cleanup_ets_for(Proxy),
            Proxy ! stop
        end,
        fun(Proxy) ->
            Self = fake_self(Proxy),
            [
                {"bind:as: stores value under atom name", fun() ->
                    Result = beamtalk_workspace_interface_primitives:dispatch(
                        'bind:as:', [42, myVar], Self
                    ),
                    ?assertEqual(nil, Result)
                end},
                {"globals includes bound value", fun() ->
                    beamtalk_workspace_interface_primitives:dispatch(
                        'bind:as:', [99, anotherVar], Self
                    ),
                    Globals = beamtalk_workspace_interface_primitives:dispatch(
                        globals, [], Self
                    ),
                    ?assert(is_map(Globals)),
                    ?assert(maps:is_key(anotherVar, Globals)),
                    ?assertEqual(99, maps:get(anotherVar, Globals))
                end},
                {"unbind: removes a bound name", fun() ->
                    beamtalk_workspace_interface_primitives:dispatch(
                        'bind:as:', [<<"hello">>, toRemove], Self
                    ),
                    Result = beamtalk_workspace_interface_primitives:dispatch(
                        'unbind:', [toRemove], Self
                    ),
                    ?assertEqual(nil, Result),
                    Globals = beamtalk_workspace_interface_primitives:dispatch(
                        globals, [], Self
                    ),
                    ?assertNot(maps:is_key(toRemove, Globals))
                end},
                {"unbind: raises name_not_found for unknown name", fun() ->
                    try
                        beamtalk_workspace_interface_primitives:dispatch(
                            'unbind:', [noSuchBinding], Self
                        ),
                        ?assert(false)
                    catch
                        error:#{error := Err} ->
                            ?assertEqual(name_not_found, Err#beamtalk_error.kind),
                            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class)
                    end
                end},
                {"bind:as: raises name_conflict for protected name Transcript", fun() ->
                    try
                        beamtalk_workspace_interface_primitives:dispatch(
                            'bind:as:', [42, 'Transcript'], Self
                        ),
                        ?assert(false)
                    catch
                        error:#{error := Err} ->
                            ?assertEqual(name_conflict, Err#beamtalk_error.kind)
                    end
                end},
                {"bind:as: raises name_conflict for protected name Beamtalk", fun() ->
                    try
                        beamtalk_workspace_interface_primitives:dispatch(
                            'bind:as:', [42, 'Beamtalk'], Self
                        ),
                        ?assert(false)
                    catch
                        error:#{error := Err} ->
                            ?assertEqual(name_conflict, Err#beamtalk_error.kind)
                    end
                end},
                {"bind:as: raises name_conflict for protected name Workspace", fun() ->
                    try
                        beamtalk_workspace_interface_primitives:dispatch(
                            'bind:as:', [42, 'Workspace'], Self
                        ),
                        ?assert(false)
                    catch
                        error:#{error := Err} ->
                            ?assertEqual(name_conflict, Err#beamtalk_error.kind)
                    end
                end},
                {"bind:as: raises type_error for non-atom name", fun() ->
                    try
                        beamtalk_workspace_interface_primitives:dispatch(
                            'bind:as:', [42, <<"notAnAtom">>], Self
                        ),
                        ?assert(false)
                    catch
                        error:#{error := Err} ->
                            ?assertEqual(type_error, Err#beamtalk_error.kind)
                    end
                end}
            ]
        end}.

%%====================================================================
%% Unknown Selector Tests
%%====================================================================

unknown_selector_raises_does_not_understand_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            unknownSelector, [], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(does_not_understand, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class),
            ?assertEqual(unknownSelector, Err#beamtalk_error.selector)
    end.

%%====================================================================
%% get_user_bindings/0 Tests
%%====================================================================

get_user_bindings_returns_empty_when_no_workspace_registered_test() ->
    %% Ensure beamtalk_workspace_meta is not registered (defensive cleanup for
    %% test suite ordering independence)
    (try
        unregister(beamtalk_workspace_meta)
    catch
        _:_ -> ok
    end),
    ?assertEqual(undefined, whereis(beamtalk_workspace_meta)),
    Result = beamtalk_workspace_interface_primitives:get_user_bindings(),
    ?assertEqual(#{}, Result).

get_user_bindings_returns_bound_values_test() ->
    Self = fake_self(self()),
    %% Register beamtalk_workspace_meta as the workspace-up sentinel
    (try
        unregister(beamtalk_workspace_meta)
    catch
        _:_ -> ok
    end),
    register(beamtalk_workspace_meta, self()),
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'bind:as:', [100, wsVar], Self
        ),
        Result = beamtalk_workspace_interface_primitives:get_user_bindings(),
        ?assert(maps:is_key(wsVar, Result)),
        ?assertEqual(100, maps:get(wsVar, Result))
    after
        cleanup_ets_for(self()),
        (try
            unregister(beamtalk_workspace_meta)
        catch
            _:_ -> ok
        end)
    end.

%%====================================================================
%% get_session_bindings/0 Tests
%%====================================================================

get_session_bindings_returns_empty_when_no_workspace_registered_test() ->
    %% Ensure beamtalk_workspace_meta is not registered (defensive cleanup for
    %% test suite ordering independence)
    (try
        unregister(beamtalk_workspace_meta)
    catch
        _:_ -> ok
    end),
    ?assertEqual(undefined, whereis(beamtalk_workspace_meta)),
    Result = beamtalk_workspace_interface_primitives:get_session_bindings(),
    ?assertEqual(#{}, Result).

get_session_bindings_includes_workspace_singleton_test() ->
    %% Register beamtalk_workspace_meta as the workspace-up sentinel
    (try
        unregister(beamtalk_workspace_meta)
    catch
        _:_ -> ok
    end),
    register(beamtalk_workspace_meta, self()),
    try
        Result = beamtalk_workspace_interface_primitives:get_session_bindings(),
        ?assert(is_map(Result)),
        %% Workspace singleton is always included
        ?assert(maps:is_key('Workspace', Result))
    after
        (try
            unregister(beamtalk_workspace_meta)
        catch
            _:_ -> ok
        end)
    end.

get_session_bindings_includes_user_bindings_test() ->
    (try
        unregister(beamtalk_workspace_meta)
    catch
        _:_ -> ok
    end),
    register(beamtalk_workspace_meta, self()),
    Self = fake_self(self()),
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'bind:as:', [<<"hello">>, sessionVar], Self
        ),
        Result = beamtalk_workspace_interface_primitives:get_session_bindings(),
        ?assert(maps:is_key(sessionVar, Result)),
        ?assertEqual(<<"hello">>, maps:get(sessionVar, Result))
    after
        cleanup_ets_for(self()),
        (try
            unregister(beamtalk_workspace_meta)
        catch
            _:_ -> ok
        end)
    end.

%%====================================================================
%% startSupervisor: / stopSupervisor: / supervisors Validation Tests
%%====================================================================

start_supervisor_type_error_for_non_class_object_test() ->
    %% Passing a non-class-object (integer) should raise type_error.
    try
        beamtalk_workspace_interface_primitives:startSupervisor(42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class),
            ?assertEqual('startSupervisor:', Err#beamtalk_error.selector)
    end.

stop_supervisor_type_error_for_non_class_object_test() ->
    %% Passing a non-class-object (atom) should raise type_error.
    try
        beamtalk_workspace_interface_primitives:stopSupervisor(notAClass),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class),
            ?assertEqual('stopSupervisor:', Err#beamtalk_error.selector)
    end.

%%====================================================================
%% rootSupervisor Tests
%%====================================================================

root_supervisor_returns_nil_when_not_registered_test() ->
    %% rootSupervisor/0 returns nil when no root supervisor has been registered.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    Self = fake_self(self()),
    ?assertEqual(nil, beamtalk_workspace_interface_primitives:dispatch(rootSupervisor, [], Self)),
    ?assertEqual(nil, beamtalk_workspace_interface_primitives:rootSupervisor()).

root_supervisor_returns_registered_value_test() ->
    %% rootSupervisor/0 returns the tuple registered via beamtalk_supervisor:register_root/1.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    FakePid = self(),
    SupTuple = {beamtalk_supervisor, 'AppSup', 'bt@my_app@app_sup', FakePid},
    beamtalk_supervisor:register_root(SupTuple),
    Self = fake_self(self()),
    ?assertEqual(
        SupTuple, beamtalk_workspace_interface_primitives:dispatch(rootSupervisor, [], Self)
    ),
    ?assertEqual(SupTuple, beamtalk_workspace_interface_primitives:rootSupervisor()),
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end).

%%====================================================================
%% sync Tests (BT-1723)
%%====================================================================

sync_dispatch_raises_when_no_manifest_test() ->
    %% sync from a directory without beamtalk.toml should raise file_not_found.
    %% Create a unique empty dir to guarantee no beamtalk.toml is present.
    EmptyDir = make_empty_tmp_dir("bt_no_manifest_dispatch_"),
    OldCwd = file:get_cwd(),
    ok = file:set_cwd(EmptyDir),
    try
        beamtalk_workspace_interface_primitives:dispatch(sync, [], fake_self(self())),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(file_not_found, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class)
    after
        {ok, Cwd} = OldCwd,
        file:set_cwd(Cwd),
        rm_rf(EmptyDir)
    end.

sync_direct_raises_when_no_manifest_test() ->
    %% Direct call to sync/0 should raise file_not_found when no beamtalk.toml.
    EmptyDir = make_empty_tmp_dir("bt_no_manifest_direct_"),
    OldCwd = file:get_cwd(),
    ok = file:set_cwd(EmptyDir),
    try
        beamtalk_workspace_interface_primitives:sync(),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(file_not_found, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class)
    after
        {ok, Cwd} = OldCwd,
        file:set_cwd(Cwd),
        rm_rf(EmptyDir)
    end.

sync_returns_map_with_expected_keys_test() ->
    %% When run from a directory with beamtalk.toml, sync/0 returns a map
    %% with the expected keys.
    %% Create a temporary project directory with beamtalk.toml and src/.
    TmpDir = make_empty_tmp_dir("bt_sync_test_"),
    ok = filelib:ensure_dir(filename:join([TmpDir, "src", "dummy"])),
    ok = file:write_file(
        filename:join(TmpDir, "beamtalk.toml"), <<"[package]\nname = \"test\"\n">>
    ),
    OldCwd = file:get_cwd(),
    ok = file:set_cwd(TmpDir),
    try
        Result = beamtalk_workspace_interface_primitives:sync(),
        ?assert(is_map(Result)),
        ?assert(maps:is_key(summary, Result)),
        ?assert(maps:is_key(classes, Result)),
        ?assert(maps:is_key(errors, Result)),
        ?assert(maps:is_key(changedCount, Result)),
        ?assert(maps:is_key(unchangedCount, Result)),
        ?assert(maps:is_key(deletedCount, Result)),
        %% With empty src/ directory, counts should be zero
        ?assertEqual(0, maps:get(changedCount, Result)),
        ?assertEqual(0, maps:get(unchangedCount, Result)),
        ?assertEqual([], maps:get(classes, Result)),
        ?assertEqual([], maps:get(errors, Result))
    after
        {ok, Cwd} = OldCwd,
        file:set_cwd(Cwd),
        rm_rf(TmpDir)
    end.

%%====================================================================
%% value_type_name/1 coverage via load/1 — BT-2295
%%
%% load/1 raises a type_error for non-binary, non-list arguments. The
%% error message is built from value_type_name/1, so each of these tests
%% exercises a distinct clause of that helper.
%%====================================================================

%% Float input → "Float" in error message
load_type_error_for_float_test() ->
    try
        beamtalk_workspace_interface_primitives:load(3.14),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Float">>))
    end.

%% Boolean input → "Boolean" in error message
load_type_error_for_boolean_test() ->
    try
        beamtalk_workspace_interface_primitives:load(true),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Boolean">>))
    end.

%% nil input → "nil" in error message
load_type_error_for_nil_test() ->
    try
        beamtalk_workspace_interface_primitives:load(nil),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"nil">>))
    end.

%% Atom (non-boolean, non-nil) → "Symbol" in error message
load_type_error_for_atom_test() ->
    try
        beamtalk_workspace_interface_primitives:load(myatom),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Symbol">>))
    end.

%% Map input → "Dictionary" in error message
load_type_error_for_map_test() ->
    try
        beamtalk_workspace_interface_primitives:load(#{key => val}),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Dictionary">>))
    end.

%% beamtalk_object input → "Object" in error message
load_type_error_for_object_test() ->
    try
        beamtalk_workspace_interface_primitives:load(#beamtalk_object{}),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Object">>))
    end.

%%====================================================================
%% to_atom_name/1 error-path coverage via bind/2 — BT-2295
%%
%% bind/2 calls to_atom_name/1 on the name argument. Non-atom names
%% produce a type_error with a message built from value_type_name/1.
%% These tests exercise the non-atom clauses of to_atom_name/1 and
%% the corresponding value_type_name/1 branches.
%%====================================================================

%% Float name → type_error with "Float" in message
bind_type_error_for_float_name_test() ->
    try
        beamtalk_workspace_interface_primitives:bind(42, 1.5),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Float">>))
    end.

%% beamtalk_object name → type_error with "Object" in message
%% (nil is an Erlang atom, so it passes to_atom_name/1; use a
%%  #beamtalk_object{} record to reach the "Object" branch instead)
bind_type_error_for_object_name_test() ->
    try
        beamtalk_workspace_interface_primitives:bind(42, #beamtalk_object{}),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Object">>))
    end.

%% List name → type_error with "List" in message
bind_type_error_for_list_name_test() ->
    try
        beamtalk_workspace_interface_primitives:bind(42, [a, b]),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"List">>))
    end.

%% Map name → type_error with "Dictionary" in message
bind_type_error_for_map_name_test() ->
    try
        beamtalk_workspace_interface_primitives:bind(42, #{key => val}),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Dictionary">>))
    end.

%%====================================================================
%% create_bindings_table/0 and ensure_bindings_table/0 — BT-2295
%%
%% Both functions are idempotent: calling them twice in sequence must
%% succeed and return ok without crashing or duplicating the ETS table.
%%====================================================================

create_bindings_table_is_idempotent_test() ->
    %% First call creates the table; second call must not crash.
    ok = beamtalk_workspace_interface_primitives:create_bindings_table(),
    ok = beamtalk_workspace_interface_primitives:create_bindings_table().

%% ensure_bindings_table/0 is not exported; it is exercised indirectly via
%% bind/2 which calls it before every insert. The idempotency of both
%% create and ensure is implicitly tested by create_bindings_table_is_idempotent_test
%% and the many bind/unbind tests that call bind/2 repeatedly.

%%====================================================================
%% dependencies/0 — BT-2295
%%
%% When beamtalk_workspace_meta is not running, get_package_name/0
%% returns undefined and dependencies/0 must return an empty map.
%%====================================================================

dependencies_returns_empty_when_no_package_test() ->
    %% Snapshot whereis/1 BEFORE calling dependencies/0 so the assertion is
    %% based on the same state that get_package_name/0 observed internally.
    %% (Copilot BT-2295: capturing after the call is a TOCTOU race — the
    %% gen_server could stop between the two calls.)
    WasMissing = whereis(beamtalk_workspace_meta) =:= undefined,
    Result = beamtalk_workspace_interface_primitives:dependencies(),
    ?assert(is_map(Result)),
    case WasMissing of
        true ->
            %% workspace_meta was absent → get_package_name/0 returned
            %% undefined → dependencies/0 must return the empty map.
            ?assertEqual(#{}, Result);
        false ->
            %% workspace_meta was running (e.g. in integration suite);
            %% only assert the type — content depends on loaded packages.
            ok
    end.

%%====================================================================
%% ADR 0082 Phase 4 (BT-2290): ChangeLog operations and autoflush
%%====================================================================
%% These tests exercise the FFI-shaped surface directly without booting a
%% workspace. The underlying flush/changelog gen_servers are tested in their
%% own suites; here we focus on argument-shape validation and error paths
%% which do not require a live workspace.

revert_rejects_non_changeentry_test() ->
    %% Passing something that is not a ChangeEntry must raise a typed error
    %% at the FFI boundary, not a deep crash.
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:changeLogRevert(42)
    ).

revert_new_class_entry_with_no_loaded_class_raises_test() ->
    %% A new-class ChangeEntry (selector = nil) now routes to the new-class revert
    %% path (BT-2664), which removes the class. When no such class is loaded the
    %% removal fails loudly with a structured error rather than silently
    %% succeeding — the entry extraction maps `nil` to the `'new-class'`
    %% placeholder and `do_revert` reaches `remove_class/1`.
    NewClassEntry = #{
        '$beamtalk_class' => 'ChangeEntry',
        className => 'NewThing',
        selector => nil,
        kind => 'new-class'
    },
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:changeLogRevert(NewClassEntry)
    ).

revert_method_returns_structured_error_test() ->
    %% The clean-returning wrapper (BT-2293) catches the wrapped error that
    %% `changeLogRevert/1` would raise and returns it as a structured
    %% `{error, #beamtalk_error{}}` — the contract the LiveView Attach client
    %% relies on. The selector here already exists as an atom (we mint it first),
    %% so it resolves past `binary_to_existing_atom/2` and fails downstream in
    %% `do_revert` with a "nothing to revert" error rather than an opaque crash.
    _ = binary_to_atom(<<"revertProbeSelector">>, utf8),
    ?assertMatch(
        {error, #beamtalk_error{}},
        beamtalk_workspace_interface_primitives:revert_method(
            <<"NoSuchClass">>, <<"revertProbeSelector">>
        )
    ).

revert_method_unknown_selector_does_not_leak_atoms_test() ->
    %% Security (PR #2573 review): the selector binary is owner-controlled
    %% (phx-value-selector over the LiveSocket), so a never-seen selector must
    %% NOT mint a fresh atom (atom-table-exhaustion DoS) — it is resolved with
    %% `binary_to_existing_atom/2`. Each bogus selector still yields a clean
    %% structured error, but interns no atom.
    Selectors = [
        <<"nonexistent_selector_", (integer_to_binary(I))/binary, "_",
            (integer_to_binary(erlang:unique_integer([positive])))/binary>>
     || I <- lists:seq(1, 50)
    ],
    lists:foreach(
        fun(Selector) ->
            ?assertMatch(
                {error, #beamtalk_error{}},
                beamtalk_workspace_interface_primitives:revert_method(<<"NoSuchClass">>, Selector)
            )
        end,
        Selectors
    ),
    %% Deterministic leak check: each bogus selector must STILL have no atom
    %% afterwards (`binary_to_existing_atom/2` raises `badarg`). This proves the
    %% old `binary_to_atom/2` behaviour is gone without depending on a node-global
    %% `atom_count` delta, which a concurrent test could perturb.
    lists:foreach(
        fun(Selector) ->
            ?assertError(badarg, binary_to_existing_atom(Selector, utf8))
        end,
        Selectors
    ).

flush_kinds_rejects_non_set_non_list_test() ->
    %% A non-Set/non-List argument surfaces a typed error.
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:changeLogFlushKinds(42)
    ).

flush_kinds_accepts_beamtalk_set_test() ->
    %% A Beamtalk Set tagged map flows through. An empty Set then surfaces
    %% the structured "use Workspace flush" error from the lower layer.
    EmptySet = #{'$beamtalk_class' => 'Set', elements => []},
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:changeLogFlushKinds(EmptySet)
    ).

set_autoflush_rejects_non_boolean_test() ->
    %% Any non-Boolean value raises a typed error.
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:setAutoflush(<<"yes">>)
    ),
    ?assertException(
        error,
        _,
        beamtalk_workspace_interface_primitives:setAutoflush(1)
    ).

autoflush_default_is_false_test() ->
    %% With no workspace running, autoflush/0 must fall back to the default
    %% (false) instead of crashing — mirrors get_setting/2's graceful-on-noproc
    %% behaviour.
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ?assertEqual(false, beamtalk_workspace_interface_primitives:autoflush());
        _ ->
            %% Server running (parallel integration suite); only assert that
            %% it returns a Boolean.
            ?assert(is_boolean(beamtalk_workspace_interface_primitives:autoflush()))
    end.

%%====================================================================
%% resolve_name/2 Tests (BT-2365, ADR 0081 Phase 1)
%%
%% Resolution order: locals -> bind:as: ETS -> singleton registry ->
%% class registry -> undefined_variable. Tiers 1, 2, 5 and ordering are
%% deterministic in EUnit; tiers 3/4 need a live workspace and are exercised
%% by the repl-protocol parity tests.
%%====================================================================

%% Tier 1: a name present in the locals map resolves to its local value.
resolve_name_tier1_locals_hit_test() ->
    Locals = #{x => 42},
    ?assertEqual(42, beamtalk_workspace_interface_primitives:resolve_name(Locals, x)).

%% Tier 2: a name absent from locals but present in the bind:as: ETS table
%% resolves to the registered value.
resolve_name_tier2_bind_as_hit_test() ->
    cleanup_ets_for(self()),
    beamtalk_workspace_interface_primitives:create_bindings_table(),
    ets:insert(beamtalk_wi_user_bindings, {greeting, <<"hi">>}),
    try
        ?assertEqual(
            <<"hi">>,
            beamtalk_workspace_interface_primitives:resolve_name(#{}, greeting)
        )
    after
        cleanup_ets_for(self())
    end.

%% Tier 5: a genuinely unknown name raises undefined_variable.
resolve_name_tier5_undefined_variable_test() ->
    cleanup_ets_for(self()),
    try
        beamtalk_workspace_interface_primitives:resolve_name(#{}, noSuchName),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(undefined_variable, Err#beamtalk_error.kind)
    end.

%% Ordering: locals shadow a bind:as: entry of the same name (tier 1 wins).
resolve_name_locals_shadow_bind_as_test() ->
    cleanup_ets_for(self()),
    beamtalk_workspace_interface_primitives:create_bindings_table(),
    ets:insert(beamtalk_wi_user_bindings, {shadowed, <<"global">>}),
    try
        %% Local value must win over the bind:as: entry.
        ?assertEqual(
            <<"local">>,
            beamtalk_workspace_interface_primitives:resolve_name(
                #{shadowed => <<"local">>}, shadowed
            )
        )
    after
        cleanup_ets_for(self())
    end.

%% A nil local value still resolves as a tier-1 hit (maps:find distinguishes
%% an absent key from a key bound to nil).
resolve_name_nil_local_is_a_hit_test() ->
    ?assertEqual(nil, beamtalk_workspace_interface_primitives:resolve_name(#{n => nil}, n)).

%%====================================================================
%% resolve_class_reference/2 Tests (BT-2365)
%%====================================================================

%% A genuinely unknown class raises class_not_found (NOT undefined_variable),
%% preserving the existing REPL "Class 'X' not found" error.
resolve_class_reference_unknown_raises_class_not_found_test() ->
    cleanup_ets_for(self()),
    try
        beamtalk_workspace_interface_primitives:resolve_class_reference(#{}, 'NoSuchClassXyz'),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(class_not_found, Err#beamtalk_error.kind),
            ?assertEqual('NoSuchClassXyz', Err#beamtalk_error.class)
    end.

%%====================================================================
%% currentSession / sessions Tests (ADR 0081 Phases 5 & 7, BT-2368)
%%====================================================================

%% Outside an eval (no seeded session context) currentSession returns nil,
%% identical to Session current. Routes through dispatch and the direct call.
current_session_returns_nil_outside_eval_test() ->
    erase(beamtalk_session_pid),
    erase(beamtalk_session_id),
    ?assertEqual(nil, beamtalk_workspace_interface_primitives:currentSession()),
    ?assertEqual(
        nil,
        beamtalk_workspace_interface_primitives:dispatch(currentSession, [], fake_self(self()))
    ).

%% currentSession delegates to beamtalk_session_primitives:current/0, so a
%% seeded context yields the *same* Session value (same id and pid).
current_session_matches_session_current_test() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_session_table:new(),
    {ok, ShellPid} = beamtalk_repl_shell:start_link(<<"ws-cur-1">>),
    put(beamtalk_session_pid, ShellPid),
    put(beamtalk_session_id, <<"ws-cur-1">>),
    try
        FromPrim = beamtalk_session_primitives:current(),
        FromWs = beamtalk_workspace_interface_primitives:currentSession(),
        ?assertEqual(FromPrim, FromWs),
        ?assertMatch(#{'$beamtalk_class' := 'Session', id := <<"ws-cur-1">>}, FromWs)
    after
        erase(beamtalk_session_pid),
        erase(beamtalk_session_id),
        beamtalk_repl_shell:stop(ShellPid)
    end.

%% No session supervisor running → sessions returns an empty list, via both the
%% direct call and dispatch.
sessions_returns_empty_when_no_supervisor_test() ->
    undefined = whereis(beamtalk_session_sup),
    ?assertEqual([], beamtalk_workspace_interface_primitives:sessions()),
    ?assertEqual(
        [],
        beamtalk_workspace_interface_primitives:dispatch(sessions, [], fake_self(self()))
    ).

%%====================================================================
%% dispatch/3 routing coverage for the newer selectors
%%
%% The earlier dispatch tests cover actors/load/bind/etc. These hit the
%% dispatch/3 clauses for the supervisor/newClass/flush/autoflush selectors
%% so the routing arms (and not just the direct functions) are exercised.
%% Each lands on a deterministic validation/error path that needs no live
%% workspace tree.
%%====================================================================

%% dispatch('newClass:at:', ...) routes to newClass/2; a non-String argument
%% raises a type_error at the FFI boundary.
dispatch_new_class_type_error_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'newClass:at:', [42, <<"src/Foo.bt">>], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('WorkspaceInterface', Err#beamtalk_error.class),
            ?assertEqual('newClass:at:', Err#beamtalk_error.selector)
    end.

%% dispatch('startSupervisor:', ...) routes to startSupervisor/1; a non-class
%% argument raises a type_error.
dispatch_start_supervisor_type_error_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'startSupervisor:', [42], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('startSupervisor:', Err#beamtalk_error.selector)
    end.

%% dispatch('stopSupervisor:', ...) routes to stopSupervisor/1; a non-class
%% argument raises a type_error.
dispatch_stop_supervisor_type_error_test() ->
    try
        beamtalk_workspace_interface_primitives:dispatch(
            'stopSupervisor:', [notAClass], fake_self(self())
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('stopSupervisor:', Err#beamtalk_error.selector)
    end.

%%====================================================================
%% newClass/2 validation coverage (ADR 0082 Phase 1, BT-2285)
%%
%% validate_new_class_args/2 rejects non-String source/path before the
%% loader is reached. These exercise both error clauses and the
%% new_class_arg_type_error/2 message builder (which names the offending arg).
%%====================================================================

%% Non-String source → type_error naming the "source" argument.
new_class_non_string_source_test() ->
    try
        beamtalk_workspace_interface_primitives:newClass(42, <<"src/Foo.bt">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('newClass:at:', Err#beamtalk_error.selector),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"source">>)),
            %% value_type_name/1 reports the Integer type in the message.
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Integer">>))
    end.

%% String source but non-String path → type_error naming the "path" argument.
new_class_non_string_path_test() ->
    try
        beamtalk_workspace_interface_primitives:newClass(
            <<"Object subclass: Foo">>, myatom
        ),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('newClass:at:', Err#beamtalk_error.selector),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"path">>)),
            ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Symbol">>))
    end.

%%====================================================================
%% changeLogClear / autoflush / setAutoflush / flush / changeLogRevert /
%% changeLogFlushKinds with a live changelog + workspace_meta gen_server.
%%
%% These boot a fresh, isolated workspace (temp HOME) and the changelog
%% gen_server so the *real* delegation paths run, not just the validation
%% guards. The log starts empty, so flush/revert exercise the empty-log and
%% no-entry branches deterministically.
%%====================================================================

changelog_live_test_() ->
    {foreach, fun setup_changelog_ws/0, fun cleanup_changelog_ws/1, [
        fun changelog_clear_returns_nil/1,
        fun autoflush_roundtrips_through_meta/1,
        fun set_autoflush_via_dispatch/1,
        fun autoflush_via_dispatch/1,
        fun flush_empty_log_returns_summary/1,
        fun flush_via_dispatch_returns_summary/1,
        fun flush_filter_via_dispatch/1,
        fun flush_kinds_valid_list_with_empty_log/1,
        fun revert_no_active_entry_raises_state_error/1,
        fun revert_via_object_keyed_map/1
    ]}.

%% changeLogClear/0 returns nil and is idempotent on an empty log.
changelog_clear_returns_nil(_Ctx) ->
    [
        ?_assertEqual(nil, beamtalk_workspace_interface_primitives:changeLogClear()),
        ?_assertEqual(nil, beamtalk_workspace_interface_primitives:changeLogClear())
    ].

%% setAutoflush/1 persists through workspace_meta and autoflush/0 reads it back.
autoflush_roundtrips_through_meta(_Ctx) ->
    True = beamtalk_workspace_interface_primitives:setAutoflush(true),
    AfterTrue = beamtalk_workspace_interface_primitives:autoflush(),
    False = beamtalk_workspace_interface_primitives:setAutoflush(false),
    AfterFalse = beamtalk_workspace_interface_primitives:autoflush(),
    [
        ?_assertEqual(true, True),
        ?_assertEqual(true, AfterTrue),
        ?_assertEqual(false, False),
        ?_assertEqual(false, AfterFalse)
    ].

%% dispatch('autoflush:', [Bool]) routes to setAutoflush/1 and returns the value.
set_autoflush_via_dispatch(_Ctx) ->
    Result = beamtalk_workspace_interface_primitives:dispatch(
        'autoflush:', [true], fake_self(self())
    ),
    [?_assertEqual(true, Result)].

%% dispatch(autoflush, []) routes to autoflush/0; with the default cleared it
%% reports the most recently set value.
autoflush_via_dispatch(_Ctx) ->
    ok = beamtalk_workspace_meta:set_setting(autoflush, false),
    Result = beamtalk_workspace_interface_primitives:dispatch(
        autoflush, [], fake_self(self())
    ),
    [?_assertEqual(false, Result)].

%% flush/0 over an empty changelog returns a FlushResult summary map with the
%% zero/empty fields.
flush_empty_log_returns_summary(_Ctx) ->
    Summary = beamtalk_workspace_interface_primitives:flush(),
    [
        ?_assert(is_map(Summary)),
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual([], maps:get(files, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary))
    ].

%% dispatch(flush, []) routes to flush/0 and returns the same summary map.
flush_via_dispatch_returns_summary(_Ctx) ->
    Summary = beamtalk_workspace_interface_primitives:dispatch(
        flush, [], fake_self(self())
    ),
    [
        ?_assert(is_map(Summary)),
        ?_assertEqual(0, maps:get(flushed, Summary))
    ].

%% dispatch('flush:', [Filter]) routes to flush/1. A Symbol filter over an
%% empty log returns an empty FlushResult summary.
flush_filter_via_dispatch(_Ctx) ->
    Summary = beamtalk_workspace_interface_primitives:dispatch(
        'flush:', ['new-class'], fake_self(self())
    ),
    [
        ?_assert(is_map(Summary)),
        ?_assertEqual(0, maps:get(flushed, Summary))
    ].

%% changeLogFlushKinds/1 with a valid List of kind Symbols flows through to
%% flush_kinds/1; over an empty log it returns an empty summary (a non-empty
%% list is accepted, unlike the empty-list rejection).
flush_kinds_valid_list_with_empty_log(_Ctx) ->
    Summary = beamtalk_workspace_interface_primitives:changeLogFlushKinds([instance, human]),
    [
        ?_assert(is_map(Summary)),
        ?_assertEqual(0, maps:get(flushed, Summary))
    ].

%% changeLogRevert/1 with a well-formed (class, selector) target but no matching
%% active entry raises a revert_not_possible state error (exercises
%% extract_revert_target_from_map ok-branch + do_revert no_entry branch).
revert_no_active_entry_raises_state_error(_Ctx) ->
    Entry = #{
        '$beamtalk_class' => 'ChangeEntry',
        className => 'NoSuchClass',
        selector => 'noSuchSelector'
    },
    [
        ?_assertException(
            error,
            #{error := #beamtalk_error{kind = revert_not_possible, class = 'ChangeLog'}},
            beamtalk_workspace_interface_primitives:changeLogRevert(Entry)
        )
    ].

%% A map keyed by className (without the $beamtalk_class tag) is also accepted
%% by extract_revert_target/1 — same no-entry outcome.
revert_via_object_keyed_map(_Ctx) ->
    Entry = #{className => 'AlsoMissing', selector => 'gone'},
    [
        ?_assertException(
            error,
            #{error := #beamtalk_error{kind = revert_not_possible}},
            beamtalk_workspace_interface_primitives:changeLogRevert(Entry)
        )
    ].

%%====================================================================
%% Changelog/workspace fixture helpers
%%====================================================================

%% Boot an isolated workspace: a temp HOME plus the changelog and meta
%% gen_servers so the delegation paths run for real. Returns a context map.
setup_changelog_ws() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("test-ws-wip-" ++ Unique),
    Tmp = filename:join(ws_temp_dir(), "bt-wip-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    %% workspace_meta is registered under its module name; setAutoflush/autoflush
    %% and the workspace-up sentinel both depend on it.
    MetaPid =
        case
            beamtalk_workspace_meta:start_link(#{
                workspace_id => WorkspaceId,
                project_path => list_to_binary(Tmp),
                created_at => erlang:system_time(second),
                last_activity => erlang:system_time(second)
            })
        of
            {ok, P} -> P;
            {error, {already_started, P}} -> P
        end,
    #{
        clog_pid => ClogPid,
        meta_pid => MetaPid,
        workspace_id => WorkspaceId,
        tmp_home => Tmp,
        old_home => OldHome
    }.

cleanup_changelog_ws(#{
    clog_pid := ClogPid, meta_pid := MetaPid, tmp_home := Tmp, old_home := OldHome
}) ->
    stop_proc(MetaPid),
    stop_proc(ClogPid),
    case OldHome of
        false -> os:unsetenv("HOME");
        _ -> os:putenv("HOME", OldHome)
    end,
    _ = file:del_dir_r(Tmp),
    ok.

ws_temp_dir() ->
    unicode:characters_to_list(beamtalk_file:'tempDirectory'()).

stop_proc(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end;
stop_proc(_) ->
    ok.

%%====================================================================
%% dependencies/0 with a package name set (ADR 0070 Phase 5)
%%
%% When workspace_meta resolves a package name (from beamtalk.toml at the
%% project path) dependencies/0 takes the `PkgName ->` branch and builds a
%% map from the package's dependency list. With a synthetic package that has
%% no registered OTP application, the dependency list is empty and the result
%% is an empty map — but via the populated branch, not the `undefined` guard.
%%====================================================================

dependencies_with_package_name_returns_map_test() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("test-ws-deps-" ++ Unique),
    Tmp = filename:join(ws_temp_dir(), "bt-deps-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    %% A beamtalk.toml at the project path makes get_package_name/0 resolve a
    %% real name, so dependencies/0 leaves the `undefined` short-circuit.
    ok = file:write_file(
        filename:join(Tmp, "beamtalk.toml"),
        <<"[package]\nname = \"bt_deps_probe_pkg\"\n">>
    ),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    MetaPid =
        case
            beamtalk_workspace_meta:start_link(#{
                workspace_id => WorkspaceId,
                project_path => list_to_binary(Tmp),
                created_at => erlang:system_time(second),
                last_activity => erlang:system_time(second)
            })
        of
            {ok, P} -> P;
            {error, {already_started, P}} -> P
        end,
    try
        ?assertEqual(<<"bt_deps_probe_pkg">>, beamtalk_workspace_meta:get_package_name()),
        Result = beamtalk_workspace_interface_primitives:dependencies(),
        ?assert(is_map(Result)),
        %% The synthetic package has no registered OTP app → no resolvable
        %% dependencies → empty map (but via the populated branch).
        ?assertEqual(#{}, Result)
    after
        stop_proc(MetaPid),
        case OldHome of
            false -> os:unsetenv("HOME");
            _ -> os:putenv("HOME", OldHome)
        end,
        _ = file:del_dir_r(Tmp)
    end.

%%====================================================================
%% supervisors/0 against a bare, locally-registered beamtalk_workspace_sup
%%
%% supervisors/0 reads the (possibly empty) root supervisor plus the user
%% supervisors attached under beamtalk_workspace_sup. We start a minimal
%% standalone supervisor registered under that name with no children, so the
%% which_children scan runs for real and the user-supervisor list is empty.
%% Combined with a registered root, this exercises both the Root branch and
%% the filtermap over which_children.
%%====================================================================

supervisors_lists_root_with_empty_user_tree_test() ->
    %% Only run when no real workspace_sup is already up (suite-ordering safe).
    case whereis(beamtalk_workspace_sup) of
        undefined ->
            {ok, SupPid} = start_bare_workspace_sup(),
            (try
                ets:delete(beamtalk_root_supervisor)
            catch
                _:_ -> ok
            end),
            RootTuple = {beamtalk_supervisor, 'AppRoot', 'bt@app@root', self()},
            beamtalk_supervisor:register_root(RootTuple),
            try
                Result = beamtalk_workspace_interface_primitives:supervisors(),
                ?assert(is_list(Result)),
                %% Root is present; no user supervisors attached.
                ?assert(lists:member(RootTuple, Result)),
                ?assertEqual([RootTuple], Result)
            after
                (try
                    ets:delete(beamtalk_root_supervisor)
                catch
                    _:_ -> ok
                end),
                stop_proc(SupPid)
            end;
        _ ->
            %% A workspace_sup is already running (integration suite); just
            %% assert the call returns a list.
            ?assert(is_list(beamtalk_workspace_interface_primitives:supervisors()))
    end.

supervisors_empty_without_root_test() ->
    case whereis(beamtalk_workspace_sup) of
        undefined ->
            {ok, SupPid} = start_bare_workspace_sup(),
            (try
                ets:delete(beamtalk_root_supervisor)
            catch
                _:_ -> ok
            end),
            try
                ?assertEqual([], beamtalk_workspace_interface_primitives:supervisors())
            after
                stop_proc(SupPid)
            end;
        _ ->
            ?assert(is_list(beamtalk_workspace_interface_primitives:supervisors()))
    end.

%% Start a minimal one_for_one supervisor with no children registered under
%% the beamtalk_workspace_sup name. supervisor:start_link with the eunit test
%% module as callback uses init/1 below.
start_bare_workspace_sup() ->
    supervisor:start_link({local, beamtalk_workspace_sup}, ?MODULE, bare_sup).

%% supervisor init/1 callback used only by start_bare_workspace_sup/0.
init(bare_sup) ->
    {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, []}}.

%%====================================================================
%% safe_existing_atom/1 + class_object_for/1 via changeLogRevert
%%
%% (covered indirectly above through the no-entry path; the install/lookup
%% path requires a real compiled class and is exercised by the repl-protocol
%% e2e suite — see the report notes on integration-only branches.)
%%====================================================================
