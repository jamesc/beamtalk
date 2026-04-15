%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_message_dispatch_tests).

-moduledoc """
EUnit tests for beamtalk_message_dispatch module (BT-430).

Tests unified dispatch routing for actors, class objects, and primitives.
Note: Compiled stdlib modules (bt@stdlib@integer etc.) require `just build-stdlib`.
These tests focus on routing logic that works without compiled stdlib.
""".
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

-doc "Minimal OTP supervisor callback for dispatch tests.".
init({SupFlags, ChildSpecs}) ->
    {ok, {SupFlags, ChildSpecs}}.

-doc "Start an anonymous OTP supervisor with no children.".
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

%% ============================================================================
%% BT-1970: cast/3 dispatch tests
%% ============================================================================

cast_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"cast to actor sends message", fun cast_actor_sends_message/0},
        {"cast to metaclass returns ok", fun cast_metaclass_returns_ok/0},
        {"cast to class object returns ok", fun cast_class_object_returns_ok/0},
        {"cast to dead actor returns ok", fun cast_dead_actor_returns_ok/0},
        {"cast to primitive returns ok", fun cast_primitive_returns_ok/0}
    ]}.

cast_actor_sends_message() ->
    %% Cast to a live actor should return ok (fire-and-forget)
    {ok, Counter} = test_counter:start_link(0),
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = Counter},
    Result = beamtalk_message_dispatch:cast(Obj, increment, []),
    ?assertEqual(ok, Result),
    gen_server:stop(Counter).

cast_metaclass_returns_ok() ->
    %% Cast to a Metaclass object is silently ignored
    MetaObj = #beamtalk_object{class = 'Metaclass', class_mod = undefined, pid = self()},
    ?assertEqual(ok, beamtalk_message_dispatch:cast(MetaObj, anySelector, [])).

cast_class_object_returns_ok() ->
    %% Cast to a class object is silently ignored
    ClassPid = beamtalk_class_registry:whereis_class('Object'),
    ?assert(is_pid(ClassPid)),
    ClassTag = beamtalk_class_registry:class_object_tag('Object'),
    ClassMod = beamtalk_object_class:module_name(ClassPid),
    ClassObj = #beamtalk_object{class = ClassTag, class_mod = ClassMod, pid = ClassPid},
    ?assertEqual(ok, beamtalk_message_dispatch:cast(ClassObj, name, [])).

cast_dead_actor_returns_ok() ->
    %% Cast to an actor with invalid pid is silently ignored (fire-and-forget)
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = undefined},
    ?assertEqual(ok, beamtalk_message_dispatch:cast(Obj, increment, [])).

cast_primitive_returns_ok() ->
    %% Cast to a primitive value is silently ignored
    ?assertEqual(ok, beamtalk_message_dispatch:cast(42, '+', [1])),
    ?assertEqual(ok, beamtalk_message_dispatch:cast("hello", size, [])),
    ?assertEqual(ok, beamtalk_message_dispatch:cast([1, 2], size, [])).

%% ============================================================================
%% BT-1970: is_actor/1 edge cases (tested indirectly via send/cast routing)
%% ============================================================================

is_actor_edge_cases_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"non-tuple receiver dispatches as primitive", fun non_tuple_as_primitive/0},
        {"wrong-size tuple dispatches as primitive", fun wrong_size_tuple_as_primitive/0},
        {"wrong-tag tuple dispatches as primitive", fun wrong_tag_tuple_as_primitive/0}
    ]}.

non_tuple_as_primitive() ->
    %% Atoms, binaries, pids — all non-tuple values route to primitive dispatch.
    %% Test with an integer (known to work via beamtalk_primitive).
    ?assertEqual(5, beamtalk_message_dispatch:send(2, '+', [3])).

wrong_size_tuple_as_primitive() ->
    %% A 3-element tuple (not 4) should NOT be treated as an actor.
    %% This goes to beamtalk_primitive:send which handles tuples.
    Result = beamtalk_message_dispatch:send({a, b, c}, size, []),
    ?assertEqual(3, Result).

wrong_tag_tuple_as_primitive() ->
    %% A 4-element tuple with wrong first element is not an actor.
    Result = beamtalk_message_dispatch:send({not_beamtalk, a, b, c}, size, []),
    ?assertEqual(4, Result).

%% ============================================================================
%% BT-1970: send/3 actor with invalid PID
%% ============================================================================

invalid_pid_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"actor with undefined pid raises actor_dead", fun actor_undefined_pid_raises_error/0}
    ]}.

actor_undefined_pid_raises_error() ->
    %% When an actor record has a non-pid value (e.g. undefined), send/3 raises actor_dead.
    Obj = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = undefined},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_message_dispatch:send(Obj, increment, [])
    ).

%% ============================================================================
%% BT-1970: send/4 supervisor timeout delegation
%% ============================================================================

send4_supervisor_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"send/4 supervisor ignores timeout and delegates to send/3",
            fun send4_supervisor_delegates/0}
    ]}.

send4_supervisor_delegates() ->
    %% Supervisors dispatch via hierarchy walk (in-process); timeout is ignored.
    %% isAlive is handled directly by send/3 before hierarchy walk.
    SupPid = anon_sup(),
    Sup = {beamtalk_supervisor, 'TestSup', test_mod, SupPid},
    Result = beamtalk_message_dispatch:send(Sup, isAlive, [], 5000),
    ?assertEqual(true, Result),
    gen_server:stop(SupPid).

%% ============================================================================
%% BT-1970: Metaclass dispatch path
%% ============================================================================

metaclass_dispatch_test_() ->
    {setup, fun actor_setup/0, fun actor_teardown/1, [
        {"metaclass dispatches via primitive send", fun metaclass_dispatch_via_primitive/0},
        {"send/4 metaclass ignores timeout", fun send4_metaclass_ignores_timeout/0}
    ]}.

metaclass_dispatch_via_primitive() ->
    %% ADR 0036: Metaclass objects dispatch synchronously via beamtalk_primitive:send.
    %% Metaclass is a special class tag that bypasses actor gen_server dispatch.
    %% Use the real Object class pid, tagged as Metaclass so dispatch routes correctly.
    ClassPid = beamtalk_class_registry:whereis_class('Object'),
    ?assert(is_pid(ClassPid)),
    MetaObj = #beamtalk_object{class = 'Metaclass', class_mod = undefined, pid = ClassPid},
    %% Sending 'name' to a Metaclass-tagged object goes through primitive dispatch
    %% which calls metaclass_send_dispatch and returns the class name as a binary.
    Result = beamtalk_message_dispatch:send(MetaObj, name, []),
    ?assertEqual(<<"Object class">>, Result).

send4_metaclass_ignores_timeout() ->
    %% send/4 with Metaclass receiver falls through to beamtalk_primitive:send
    ClassPid = beamtalk_class_registry:whereis_class('Object'),
    ?assert(is_pid(ClassPid)),
    MetaObj = #beamtalk_object{class = 'Metaclass', class_mod = undefined, pid = ClassPid},
    Result = beamtalk_message_dispatch:send(MetaObj, name, [], 5000),
    ?assertEqual(<<"Object class">>, Result).
