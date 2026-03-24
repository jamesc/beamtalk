%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_message_dispatch module (BT-430).
%%%
%%% Tests unified dispatch routing for actors, class objects, and primitives.
%%% Note: Compiled stdlib modules (bt@stdlib@integer etc.) require `just build-stdlib`.
%%% These tests focus on routing logic that works without compiled stdlib.
-module(beamtalk_message_dispatch_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").
-export([init/1]).

%% ============================================================================
%% Actor dispatch tests (need bootstrap)
%% ============================================================================

actor_setup() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    {ok, _} = beamtalk_bootstrap:start_link(),
    %% BT-446: Bootstrap only starts pg. Classes registered by compiled stdlib.
    beamtalk_stdlib:init(),
    ok.

actor_teardown(_) ->
    ok.

actor_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        %% BT-918 / ADR 0043: sync-by-default — actor dispatch returns value directly, not future
        {"actor dispatch returns value directly (sync)", fun actor_returns_value_directly/0},
        {"actor dispatch dead actor raises error", fun actor_dead_raises_error/0},
        {"class object dispatch returns value", fun class_object_returns_value/0}
    ]}.

actor_returns_value_directly() ->
    %% BT-918: Actor sends now use gen_server:call and return values directly.
    %% Use a real gen_server (test_counter) since dummy procs don't handle gen_server calls.
    {ok, Counter} = test_counter:start_link(0),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    %% Send returns the direct value, not a future
    Result = beamtalk_message_dispatch:send(Obj, 'getValue', []),
    ?assertNot(beamtalk_future:is_future(Result)),
    gen_server:stop(Counter).

actor_dead_raises_error() ->
    %% BT-918: Sending to a dead actor raises an actor_dead exception.
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_message_dispatch:send(Obj, 'getValue', [])
    ).

class_object_returns_value() ->
    ClassPid = beamtalk_class_registry:whereis_class('Object'),
    ?assert(is_pid(ClassPid)),
    ClassTag = beamtalk_class_registry:class_object_tag('Object'),
    ClassMod = beamtalk_object_class:module_name(ClassPid),
    ClassObj = #beamtalk_object{class = ClassTag, class_mod = ClassMod, pid = ClassPid},
    Result = beamtalk_message_dispatch:send(ClassObj, name, []),
    ?assertEqual('Object', Result).

%% ============================================================================
%% BT-1190: send/4 (explicit timeout) dispatch tests
%% ============================================================================

send4_actor_succeeds_with_timeout() ->
    {ok, Counter} = test_counter:start_link(42),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    %% slowGet: sleeps 50ms; 2000ms timeout is plenty
    Result = beamtalk_message_dispatch:send(Obj, 'slowGet:', [50], 2000),
    ?assertEqual(42, Result),
    gen_server:stop(Counter).

send4_actor_timeout_raises_error() ->
    {ok, Counter} = test_counter:start_link(42),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    %% slowGet: sleeps 500ms; 50ms timeout should fail
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = timeout}},
        beamtalk_message_dispatch:send(Obj, 'slowGet:', [500], 50)
    ),
    gen_server:stop(Counter).

send4_value_type_ignores_timeout() ->
    %% Value types have no timeout semantics — just dispatches normally
    Result = beamtalk_message_dispatch:send(2, '+', [3], 30000),
    ?assertEqual(5, Result).

perform_with_timeout_intercept_succeeds() ->
    %% perform:withArguments:timeout: is intercepted in send/3 and rewrites to send/4
    {ok, Counter} = test_counter:start_link(42),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    Result = beamtalk_message_dispatch:send(
        Obj, 'perform:withArguments:timeout:', [getValue, [], 2000]
    ),
    ?assertEqual(42, Result),
    gen_server:stop(Counter).

perform_with_timeout_intercept_times_out() ->
    {ok, Counter} = test_counter:start_link(42),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = timeout}},
        beamtalk_message_dispatch:send(
            Obj, 'perform:withArguments:timeout:', ['slowGet:', [500], 50]
        )
    ),
    gen_server:stop(Counter).

send4_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"send/4 actor succeeds with timeout", fun send4_actor_succeeds_with_timeout/0},
        {"send/4 actor timeout raises error", fun send4_actor_timeout_raises_error/0},
        {"send/4 value type ignores timeout", fun send4_value_type_ignores_timeout/0},
        {"perform:withArguments:timeout: intercept succeeds",
            fun perform_with_timeout_intercept_succeeds/0},
        {"perform:withArguments:timeout: intercept times out",
            fun perform_with_timeout_intercept_times_out/0}
    ]}.

%% ============================================================================
%% Primitive dispatch tests (no bootstrap needed)
%% ============================================================================

primitive_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"primitive integer dispatch", fun primitive_integer_dispatch/0},
        {"primitive list dispatch", fun primitive_list_dispatch/0}
    ]}.

primitive_integer_dispatch() ->
    %% Integer addition via primitive dispatch path
    Result = beamtalk_message_dispatch:send(2, '+', [3]),
    ?assertEqual(5, Result).

primitive_list_dispatch() ->
    %% List size via primitive dispatch path
    Result = beamtalk_message_dispatch:send([1, 2, 3], size, []),
    ?assertEqual(3, Result).

%% ============================================================================
%% Supervisor dispatch tests (ADR 0059)
%% ============================================================================

%% @private Minimal OTP supervisor callback for dispatch tests.
init({SupFlags, ChildSpecs}) ->
    {ok, {SupFlags, ChildSpecs}}.

%% @private Start an anonymous OTP supervisor with no children.
anon_sup() ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, []}),
    Pid.

supervisor_is_alive_true_test() ->
    %% isAlive returns true for a running supervisor.
    SupPid = anon_sup(),
    Sup = {beamtalk_supervisor, 'TestSup', test_mod, SupPid},
    ?assertEqual(true, beamtalk_message_dispatch:send(Sup, isAlive, [])),
    gen_server:stop(SupPid).

supervisor_is_alive_false_test() ->
    %% isAlive returns false after the supervisor is stopped.
    SupPid = anon_sup(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Sup = {beamtalk_supervisor, 'TestSup', test_mod, SupPid},
    ?assertEqual(false, beamtalk_message_dispatch:send(Sup, isAlive, [])).

supervisor_stop_returns_nil_test() ->
    %% stop returns nil and terminates the supervisor process.
    SupPid = anon_sup(),
    Sup = {beamtalk_supervisor, 'TestSup', test_mod, SupPid},
    Result = beamtalk_message_dispatch:send(Sup, stop, []),
    ?assertEqual(nil, Result),
    timer:sleep(50),
    ?assertEqual(false, is_process_alive(SupPid)).

supervisor_stop_stale_handle_test() ->
    %% stop on a dead supervisor raises a structured runtime_error (not a raw OTP exit).
    SupPid = anon_sup(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Sup = {beamtalk_supervisor, 'TestSup', test_mod, SupPid},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
        beamtalk_message_dispatch:send(Sup, stop, [])
    ).
