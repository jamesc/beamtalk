%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Bootstrap tests.
%%
%% BT-446: Bootstrap only starts pg now. Class registration is done by
%% compiled stdlib modules. These tests verify the bootstrap + stdlib
%% flow produces the correct class hierarchy.
-module(beamtalk_bootstrap_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Start pg server if not already running
    case whereis(pg) of
        undefined ->
            {ok, Pid} = pg:start_link(),
            Pid;
        Pid ->
            Pid
    end.

teardown(_) ->
    %% Clean up all registered class processes
    Members = try
        pg:get_members(beamtalk_classes)
    catch
        _:_ -> []
    end,
    
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    case process_info(Pid, registered_name) of
                        {registered_name, Name} when Name =/= [] ->
                            try unregister(Name) catch _:_ -> ok end;
                        _ ->
                            ok
                    end,
                    exit(Pid, kill);
                false ->
                    ok
            end;
        (_) ->
            ok
        end,
        Members
    ),
    
    timer:sleep(50),
    ok.

%%====================================================================
%% Bootstrap Tests (BT-446)
%%====================================================================

%% Bootstrap only starts pg — no class registration
bootstrap_starts_pg_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     ?assert(is_pid(whereis(pg)))
                 end)
          ]
     end}.

%%====================================================================
%% Stdlib Class Registration Tests (BT-446)
%%
%% These tests verify that compiled stdlib modules correctly register
%% the three foundational classes. They call beamtalk_stdlib:init/0
%% after bootstrap to simulate the full startup sequence.
%%====================================================================

protoobject_class_exists_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ProtoObjectClassPid = beamtalk_class_registry:whereis_class('ProtoObject'),
                     ?assert(is_pid(ProtoObjectClassPid)),
                     ?assert(is_process_alive(ProtoObjectClassPid)),
                     
                     Superclass = beamtalk_object_class:superclass(ProtoObjectClassPid),
                     ?assertEqual(none, Superclass)
                 end)
          ]
     end}.

object_class_exists_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ObjectClassPid = beamtalk_class_registry:whereis_class('Object'),
                     ?assert(is_pid(ObjectClassPid)),
                     ?assert(is_process_alive(ObjectClassPid)),
                     
                     Superclass = beamtalk_object_class:superclass(ObjectClassPid),
                     ?assertEqual('ProtoObject', Superclass)
                 end)
          ]
     end}.

actor_class_exists_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ActorClassPid = beamtalk_class_registry:whereis_class('Actor'),
                     ?assert(is_pid(ActorClassPid)),
                     ?assert(is_process_alive(ActorClassPid)),
                     
                     Superclass = beamtalk_object_class:superclass(ActorClassPid),
                     ?assertEqual('Object', Superclass)
                 end)
          ]
     end}.

three_level_hierarchy_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ActorClassPid = beamtalk_class_registry:whereis_class('Actor'),
                     ?assertEqual('Object', beamtalk_object_class:superclass(ActorClassPid)),
                     
                     ObjectClassPid = beamtalk_class_registry:whereis_class('Object'),
                     ?assertEqual('ProtoObject', beamtalk_object_class:superclass(ObjectClassPid)),
                     
                     ProtoObjectClassPid = beamtalk_class_registry:whereis_class('ProtoObject'),
                     ?assertEqual(none, beamtalk_object_class:superclass(ProtoObjectClassPid))
                 end)
          ]
     end}.

all_three_classes_in_pg_group_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     Members = pg:get_members(beamtalk_classes),
                     
                     ProtoObjectClassPid = beamtalk_class_registry:whereis_class('ProtoObject'),
                     ObjectClassPid = beamtalk_class_registry:whereis_class('Object'),
                     ActorClassPid = beamtalk_class_registry:whereis_class('Actor'),
                     
                     ?assert(lists:member(ProtoObjectClassPid, Members)),
                     ?assert(lists:member(ObjectClassPid, Members)),
                     ?assert(lists:member(ActorClassPid, Members))
                 end)
          ]
     end}.

%%====================================================================
%% Method Lookup Tests
%%====================================================================

protoobject_methods_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ProtoObjectClassPid = beamtalk_class_registry:whereis_class('ProtoObject'),
                     Methods = beamtalk_object_class:methods(ProtoObjectClassPid),
                     
                     ?assert(lists:member(class, Methods)),
                     ?assert(lists:member('doesNotUnderstand:args:', Methods)),
                     ?assert(lists:member('==', Methods)),
                     ?assert(lists:member('/=', Methods))
                 end)
          ]
     end}.

object_methods_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ObjectClassPid = beamtalk_class_registry:whereis_class('Object'),
                     Methods = beamtalk_object_class:methods(ObjectClassPid),
                     
                     ?assert(lists:member(isNil, Methods)),
                     ?assert(lists:member(notNil, Methods)),
                     ?assert(lists:member('ifNil:', Methods)),
                     ?assert(lists:member('ifNotNil:', Methods)),
                     ?assert(lists:member(inspect, Methods)),
                     ?assert(lists:member(describe, Methods))
                 end)
          ]
     end}.

actor_methods_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     beamtalk_stdlib:init(),
                     
                     ActorClassPid = beamtalk_class_registry:whereis_class('Actor'),
                     Methods = beamtalk_object_class:methods(ActorClassPid),
                     
                     ?assert(lists:member(describe, Methods))
                 end)
          ]
     end}.
