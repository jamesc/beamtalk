%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

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
%% Three-Level Hierarchy Tests
%%====================================================================

protoobject_class_exists_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Start bootstrap
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     
                     %% Verify ProtoObject class exists
                     ProtoObjectClassPid = beamtalk_class:whereis_class('ProtoObject'),
                     ?assert(is_pid(ProtoObjectClassPid)),
                     ?assert(is_process_alive(ProtoObjectClassPid)),
                     
                     %% Verify it has no superclass (true root)
                     Superclass = beamtalk_class:superclass(ProtoObjectClassPid),
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
                     %% Start bootstrap
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     
                     %% Verify Object class exists
                     ObjectClassPid = beamtalk_class:whereis_class('Object'),
                     ?assert(is_pid(ObjectClassPid)),
                     ?assert(is_process_alive(ObjectClassPid)),
                     
                     %% Verify it inherits from ProtoObject class
                     Superclass = beamtalk_class:superclass(ObjectClassPid),
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
                     %% Start bootstrap
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     
                     %% Verify Actor class exists
                     ActorClassPid = beamtalk_class:whereis_class('Actor'),
                     ?assert(is_pid(ActorClassPid)),
                     ?assert(is_process_alive(ActorClassPid)),
                     
                     %% Verify it inherits from Object class
                     Superclass = beamtalk_class:superclass(ActorClassPid),
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
                     %% Start bootstrap
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     
                     %% Walk the hierarchy from Actor -> Object -> ProtoObject -> none
                     ActorClassPid = beamtalk_class:whereis_class('Actor'),
                     ?assertEqual('Object', beamtalk_class:superclass(ActorClassPid)),
                     
                     ObjectClassPid = beamtalk_class:whereis_class('Object'),
                     ?assertEqual('ProtoObject', beamtalk_class:superclass(ObjectClassPid)),
                     
                     ProtoObjectClassPid = beamtalk_class:whereis_class('ProtoObject'),
                     ?assertEqual(none, beamtalk_class:superclass(ProtoObjectClassPid))
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
                     %% Start bootstrap
                     {ok, _Pid} = beamtalk_bootstrap:start_link(),
                     
                     %% All three classes should be in the beamtalk_classes pg group
                     Members = pg:get_members(beamtalk_classes),
                     
                     ProtoObjectClassPid = beamtalk_class:whereis_class('ProtoObject'),
                     ObjectClassPid = beamtalk_class:whereis_class('Object'),
                     ActorClassPid = beamtalk_class:whereis_class('Actor'),
                     
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
                     
                     ProtoObjectClassPid = beamtalk_class:whereis_class('ProtoObject'),
                     Methods = beamtalk_class:methods(ProtoObjectClassPid),
                     
                     %% ProtoObject should have its core methods
                     ?assert(lists:member(class, Methods)),
                     ?assert(lists:member('doesNotUnderstand:args:', Methods)),
                     ?assert(lists:member('==', Methods)),
                     ?assert(lists:member('~=', Methods))
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
                     
                     ObjectClassPid = beamtalk_class:whereis_class('Object'),
                     Methods = beamtalk_class:methods(ObjectClassPid),
                     
                     %% Object should have nil testing methods
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
                     
                     ActorClassPid = beamtalk_class:whereis_class('Actor'),
                     
                     %% Actor should have class methods for spawning
                     Methods = beamtalk_class:methods(ActorClassPid),
                     
                     %% Check that instance methods exist
                     %% (spawn methods are class methods, stored separately)
                     ?assert(lists:member(describe, Methods))
                 end)
          ]
     end}.
