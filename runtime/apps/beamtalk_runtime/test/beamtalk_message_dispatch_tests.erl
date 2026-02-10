%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_message_dispatch module (BT-430).
%%%
%%% Tests unified dispatch routing for actors, class objects, and primitives.
%%% Note: Compiled stdlib modules (beamtalk_integer etc.) require `just build-stdlib`.
%%% These tests focus on routing logic that works without compiled stdlib.
-module(beamtalk_message_dispatch_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
    ok.

actor_teardown(_) ->
    ok.

actor_test_() ->
    {setup,
     fun actor_setup/0,
     fun actor_teardown/1,
     [
         {"actor dispatch returns future", fun actor_returns_future/0},
         {"actor dispatch creates future pid", fun actor_future_is_pid/0},
         {"class object dispatch returns value", fun class_object_returns_value/0}
      ]}.

actor_returns_future() ->
    %% Spawn a dummy process that discards messages (avoids noisy mailbox on self())
    Dummy = spawn(fun() -> receive _ -> ok end end),
    Obj = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_object, pid = Dummy},
    %% Send returns a future (pid), not a direct value
    Result = beamtalk_message_dispatch:send(Obj, 'testMsg', []),
    ?assert(is_pid(Result)),
    exit(Dummy, kill).

actor_future_is_pid() ->
    %% Future returned by actor dispatch should be a new pid (not the actor pid)
    Dummy = spawn(fun() -> receive _ -> ok end end),
    Obj = #beamtalk_object{class = 'TestClass', class_mod = beamtalk_object, pid = Dummy},
    Future = beamtalk_message_dispatch:send(Obj, 'someMsg', []),
    ?assert(is_pid(Future)),
    ?assertNotEqual(Dummy, Future),
    exit(Dummy, kill).

class_object_returns_value() ->
    ClassPid = beamtalk_object_class:whereis_class('Object'),
    ?assert(is_pid(ClassPid)),
    ClassTag = beamtalk_object_class:class_object_tag('Object'),
    ClassMod = beamtalk_object_class:module_name(ClassPid),
    ClassObj = #beamtalk_object{class = ClassTag, class_mod = ClassMod, pid = ClassPid},
    Result = beamtalk_message_dispatch:send(ClassObj, class_name, []),
    ?assertEqual('Object', Result).

%% ============================================================================
%% Primitive dispatch tests (no bootstrap needed)
%% ============================================================================

primitive_test_() ->
    {setup,
     fun actor_setup/0,
     fun actor_teardown/1,
     [
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
