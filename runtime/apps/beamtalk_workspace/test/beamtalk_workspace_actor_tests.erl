%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for Workspace actor singleton.
%%%
%%% Tests the beamtalk_workspace_actor gen_server which provides
%%% actor introspection API (actors, actorAt:, actorsOf:).

-module(beamtalk_workspace_actor_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ===========================================================================
%%% Lifecycle Tests
%%% ===========================================================================

starts_and_stops_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%% ===========================================================================
%%% has_method Tests
%%% ===========================================================================

has_method_returns_true_for_supported_selectors_test() ->
    ?assert(beamtalk_workspace_actor:has_method(actors)),
    ?assert(beamtalk_workspace_actor:has_method('actorAt:')),
    ?assert(beamtalk_workspace_actor:has_method('actorsOf:')).

has_method_returns_false_for_unknown_selectors_test() ->
    ?assertNot(beamtalk_workspace_actor:has_method(foo)),
    ?assertNot(beamtalk_workspace_actor:has_method(version)),
    ?assertNot(beamtalk_workspace_actor:has_method('doesNotExist:')).

%%% ===========================================================================
%%% class_info Tests
%%% ===========================================================================

class_info_returns_valid_metadata_test() ->
    Info = beamtalk_workspace_actor:class_info(),
    ?assertEqual('Workspace', maps:get(name, Info)),
    ?assertEqual(beamtalk_workspace_actor, maps:get(module, Info)),
    ?assertEqual('Actor', maps:get(superclass, Info)),
    Methods = maps:get(instance_methods, Info),
    ?assert(maps:is_key(actors, Methods)),
    ?assert(maps:is_key('actorAt:', Methods)),
    ?assert(maps:is_key('actorsOf:', Methods)).

%%% ===========================================================================
%%% Sync (handle_call) Tests — actors
%%% ===========================================================================

actors_returns_empty_when_no_registry_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    Result = gen_server:call(Pid, {actors, []}),
    ?assertEqual([], Result),
    gen_server:stop(Pid).

actors_returns_live_actors_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),

    Result = gen_server:call(Pid, {actors, []}),
    ?assertEqual(2, length(Result)),

    %% Verify results are beamtalk_object tuples
    lists:foreach(fun({beamtalk_object, Class, Module, ActorPid}) ->
        ?assertEqual('Counter', Class),
        ?assertEqual(test_counter, Module),
        ?assert(is_process_alive(ActorPid))
    end, Result),

    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actors_filters_dead_processes_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),

    %% Kill one actor — registry auto-unregisters via monitor, but
    %% test wrap_actor filtering by checking immediately before monitor fires
    gen_server:stop(Actor1),

    %% Give monitor time to process
    timer:sleep(50),

    Result = gen_server:call(Pid, {actors, []}),
    ?assertEqual(1, length(Result)),

    gen_server:stop(Actor2),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Sync (handle_call) Tests — actorAt:
%%% ===========================================================================

actor_at_returns_object_for_valid_pid_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = list_to_binary(pid_to_list(Actor)),
    Result = gen_server:call(Pid, {'actorAt:', [PidStr]}),
    ?assertMatch({beamtalk_object, 'Counter', test_counter, _}, Result),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_for_unknown_pid_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    Result = gen_server:call(Pid, {'actorAt:', [<<"<0.99999.0>">>]}),
    ?assertEqual(nil, Result),

    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_for_invalid_pid_string_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    ?assertEqual(nil, gen_server:call(Pid, {'actorAt:', [<<"not-a-pid">>]})),
    ?assertEqual(nil, gen_server:call(Pid, {'actorAt:', [<<"">>]})),
    ?assertEqual(nil, gen_server:call(Pid, {'actorAt:', [42]})),

    gen_server:stop(Pid).

actor_at_returns_nil_for_dead_process_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = list_to_binary(pid_to_list(Actor)),
    gen_server:stop(Actor),
    timer:sleep(50),

    Result = gen_server:call(Pid, {'actorAt:', [PidStr]}),
    ?assertEqual(nil, Result),

    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actor_at_accepts_list_strings_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = pid_to_list(Actor),
    Result = gen_server:call(Pid, {'actorAt:', [PidStr]}),
    ?assertMatch({beamtalk_object, 'Counter', test_counter, _}, Result),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actor_at_returns_nil_when_no_registry_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    Result = gen_server:call(Pid, {'actorAt:', [<<"<0.1.0>">>]}),
    ?assertEqual(nil, Result),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Sync (handle_call) Tests — actorsOf:
%%% ===========================================================================

actors_of_filters_by_class_atom_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    {ok, Actor3} = test_counter:start_link(20),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Timer', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor3, 'Counter', test_counter),

    Counters = gen_server:call(Pid, {'actorsOf:', ['Counter']}),
    ?assertEqual(2, length(Counters)),

    Timers = gen_server:call(Pid, {'actorsOf:', ['Timer']}),
    ?assertEqual(1, length(Timers)),

    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(Actor3),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actors_of_returns_empty_for_unknown_class_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    Result = gen_server:call(Pid, {'actorsOf:', ['NonexistentClass']}),
    ?assertEqual([], Result),

    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actors_of_accepts_binary_class_name_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    %% 'Counter' atom must already exist for binary_to_existing_atom
    Result = gen_server:call(Pid, {'actorsOf:', [<<"Counter">>]}),
    ?assertEqual(1, length(Result)),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actors_of_returns_empty_for_nonexistent_binary_class_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    %% binary_to_existing_atom will fail for non-existent atom
    Result = gen_server:call(Pid, {'actorsOf:', [<<"TotallyFakeClassXYZ123">>]}),
    ?assertEqual([], Result),

    gen_server:stop(Pid).

actors_of_accepts_class_object_tuple_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    %% Start a class process to simulate a real class object reference
    ClassInfo = #{name => 'Counter', module => test_counter, superclass => 'Actor',
                  instance_methods => #{}, class_methods => #{}, instance_variables => []},
    {ok, ClassPid} = beamtalk_object_class:start_link('Counter', ClassInfo),

    ClassObj = {beamtalk_object, 'Counter class', test_counter, ClassPid},
    Result = gen_server:call(Pid, {'actorsOf:', [ClassObj]}),
    ?assertEqual(1, length(Result)),

    gen_server:stop(ClassPid),
    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

actors_of_returns_empty_for_invalid_type_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    ?assertEqual([], gen_server:call(Pid, {'actorsOf:', [42]})),
    ?assertEqual([], gen_server:call(Pid, {'actorsOf:', [{not_a_class}]})),

    gen_server:stop(Pid).

actors_of_returns_empty_when_no_registry_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    Result = gen_server:call(Pid, {'actorsOf:', ['Counter']}),
    ?assertEqual([], Result),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Sync (handle_call) Tests — unknown selectors
%%% ===========================================================================

unknown_selector_returns_error_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    {error, Error} = gen_server:call(Pid, {foo, []}),
    ?assertMatch(#beamtalk_error{kind = does_not_understand, class = 'Workspace'}, Error),

    gen_server:stop(Pid).

malformed_request_returns_error_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    {error, Error} = gen_server:call(Pid, not_a_tuple),
    ?assertMatch(#beamtalk_error{kind = does_not_understand}, Error),

    gen_server:stop(Pid).

%%% ===========================================================================
%%% Async (handle_cast) Tests
%%% ===========================================================================

async_actors_resolves_future_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {actors, [], Future}),
    Result = beamtalk_future:await(Future, 1000),
    ?assertEqual(1, length(Result)),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

async_actor_at_resolves_future_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    PidStr = list_to_binary(pid_to_list(Actor)),
    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {'actorAt:', [PidStr], Future}),
    Result = beamtalk_future:await(Future, 1000),
    ?assertMatch({beamtalk_object, 'Counter', test_counter, _}, Result),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

async_actors_of_resolves_future_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(registered),
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    {ok, Actor} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor, 'Counter', test_counter),

    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {'actorsOf:', ['Counter'], Future}),
    Result = beamtalk_future:await(Future, 1000),
    ?assertEqual(1, length(Result)),

    gen_server:stop(Actor),
    gen_server:stop(Pid),
    gen_server:stop(RegistryPid).

async_unknown_selector_rejects_future_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),

    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {badMethod, [], Future}),

    %% Future should be rejected — await throws {future_rejected, Error}
    ?assertException(throw, {future_rejected, _}, beamtalk_future:await(Future, 1000)),

    gen_server:stop(Pid).

%%% ===========================================================================
%%% Terminate Tests
%%% ===========================================================================

named_terminate_cleans_registration_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link({local, 'Workspace'}),

    %% Verify name is registered
    ?assertEqual(Pid, whereis('Workspace')),

    gen_server:stop(Pid),

    %% Verify name is cleaned up
    ?assertEqual(undefined, whereis('Workspace')).

non_named_terminate_test() ->
    {ok, Pid} = beamtalk_workspace_actor:start_link(),
    gen_server:stop(Pid),
    %% Should not crash
    ok.
