%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_workspace_interface_primitives module.
%%%
%%% Tests the Phase 2 dispatch/3 interface for WorkspaceInterface primitives:
%%% - actors selector
%%% - actorAt: selector
%%% - classes selector
%%% - load: selector
%%% - globals selector
%%% - bind:as: selector
%%% - unbind: selector
%%% - get_user_bindings/0 external API
%%% - get_session_bindings/0 external API

-module(beamtalk_workspace_interface_primitives_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

%% Construct a fake Self tuple with the given pid at element 4.
fake_self(Pid) ->
    {beamtalk_object, 'WorkspaceInterface', 'bt@stdlib@workspace_interface', Pid}.

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
    catch unregister(beamtalk_workspace_meta),
    ?assertEqual(undefined, whereis(beamtalk_workspace_meta)),
    Result = beamtalk_workspace_interface_primitives:get_user_bindings(),
    ?assertEqual(#{}, Result).

get_user_bindings_returns_bound_values_test() ->
    Self = fake_self(self()),
    %% Register beamtalk_workspace_meta as the workspace-up sentinel
    catch unregister(beamtalk_workspace_meta),
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
        catch unregister(beamtalk_workspace_meta)
    end.

%%====================================================================
%% get_session_bindings/0 Tests
%%====================================================================

get_session_bindings_returns_empty_when_no_workspace_registered_test() ->
    %% Ensure beamtalk_workspace_meta is not registered (defensive cleanup for
    %% test suite ordering independence)
    catch unregister(beamtalk_workspace_meta),
    ?assertEqual(undefined, whereis(beamtalk_workspace_meta)),
    Result = beamtalk_workspace_interface_primitives:get_session_bindings(),
    ?assertEqual(#{}, Result).

get_session_bindings_includes_workspace_singleton_test() ->
    %% Register beamtalk_workspace_meta as the workspace-up sentinel
    catch unregister(beamtalk_workspace_meta),
    register(beamtalk_workspace_meta, self()),
    try
        Result = beamtalk_workspace_interface_primitives:get_session_bindings(),
        ?assert(is_map(Result)),
        %% Workspace singleton is always included
        ?assert(maps:is_key('Workspace', Result))
    after
        catch unregister(beamtalk_workspace_meta)
    end.

get_session_bindings_includes_user_bindings_test() ->
    catch unregister(beamtalk_workspace_meta),
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
        catch unregister(beamtalk_workspace_meta)
    end.
