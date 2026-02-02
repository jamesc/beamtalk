%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_tests).
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
    %% Clean up all registered class processes by getting all from pg group
    Members = try
        pg:get_members(beamtalk_classes)
    catch
        _:_ -> []
    end,
    
    %% Terminate each class process
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    %% Get the registered name if any and unregister
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
    
    %% Wait for all processes to actually die
    timer:sleep(50),
    
    %% Double-check and force unregister any stragglers
    lists:foreach(
        fun(Name) ->
            try unregister(Name) catch _:_ -> ok end
        end,
        [beamtalk_class_TestClass, beamtalk_class_TestClassA, beamtalk_class_TestClassB,
         beamtalk_class_Counter, beamtalk_class_ProtoObject, beamtalk_class_LoggingCounter]
    ),
    ok.

%%====================================================================
%% Basic Lifecycle Tests
%%====================================================================

start_link_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'TestClass',
                         module => test_class,
                         superclass => 'Object',
                         instance_methods => #{},
                         instance_variables => []
                     },
                     {ok, Pid} = beamtalk_class:start_link('TestClass', ClassInfo),
                     ?assert(is_pid(Pid)),
                     ?assert(is_process_alive(Pid))
                 end)
         ]
     end}.

registry_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'TestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, _Pid} = beamtalk_class:start_link('TestClass', ClassInfo),
                     RegPid = beamtalk_class:whereis_class('TestClass'),
                     ?assert(is_pid(RegPid)),
                     ?assertEqual(RegPid, whereis(beamtalk_class_TestClass))
                 end)
         ]
     end}.

pg_group_membership_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'TestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, Pid} = beamtalk_class:start_link('TestClass', ClassInfo),
                     Members = pg:get_members(beamtalk_classes),
                     ?assert(lists:member(Pid, Members))
                 end)
         ]
     end}.

%%====================================================================
%% Introspection Tests
%%====================================================================

methods_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     Methods = #{
                         increment => #{arity => 0},
                         add => #{arity => 1},
                         getValue => #{arity => 0}
                     },
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => Methods
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     Result = beamtalk_class:methods(Pid),
                     ?assertEqual(lists:sort([increment, add, getValue]), lists:sort(Result))
                 end)
         ]
     end}.

superclass_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         superclass => 'Object'
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     ?assertEqual('Object', beamtalk_class:superclass(Pid))
                 end),
          ?_test(begin
                     ClassInfo = #{
                         name => 'ProtoObject',
                         module => proto_object,
                         superclass => none
                     },
                     {ok, Pid} = beamtalk_class:start_link('ProtoObject', ClassInfo),
                     ?assertEqual(none, beamtalk_class:superclass(Pid))
                 end)
         ]
     end}.

method_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     Methods = #{
                         increment => #{arity => 0, block => fun() -> ok end}
                     },
                     MethodSource = #{
                         increment => <<"increment => self.count := self.count + 1">>
                     },
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => Methods,
                         method_source => MethodSource
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     MethodObj = beamtalk_class:method(Pid, increment),
                     ?assertEqual('CompiledMethod', maps:get('__class__', MethodObj)),
                     ?assertEqual(increment, maps:get('__selector__', MethodObj)),
                     ?assertEqual(<<"increment => self.count := self.count + 1">>,
                                  maps:get('__source__', MethodObj))
                 end),
          ?_test(begin
                     ClassInfo = #{
                         name => 'CounterNoMethods',
                         module => counter,
                         instance_methods => #{}
                     },
                     {ok, Pid} = beamtalk_class:start_link('CounterNoMethods', ClassInfo),
                     ?assertEqual(nil, beamtalk_class:method(Pid, nonexistent))
                 end)
         ]
     end}.

instance_variables_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_variables => [count, name]
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     IVars = beamtalk_class:instance_variables(Pid),
                     ?assertEqual([count, name], IVars)
                 end)
         ]
     end}.

%%====================================================================
%% Hot Patching Tests
%%====================================================================

put_method_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => #{}
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     
                     %% Add a new method
                     NewFun = fun() -> 42 end,
                     Source = <<"newMethod => 42">>,
                     ok = beamtalk_class:put_method(Pid, newMethod, NewFun, Source),
                     
                     %% Verify it's in the method list
                     Methods = beamtalk_class:methods(Pid),
                     ?assert(lists:member(newMethod, Methods)),
                     
                     %% Verify we can retrieve it
                     MethodObj = beamtalk_class:method(Pid, newMethod),
                     ?assertEqual('CompiledMethod', maps:get('__class__', MethodObj)),
                     ?assertEqual(newMethod, maps:get('__selector__', MethodObj)),
                     ?assertEqual(Source, maps:get('__source__', MethodObj))
                 end)
         ]
     end}.

replace_method_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     OldFun = fun() -> old_behavior end,
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => #{
                             someMethod => #{block => OldFun}
                         }
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     
                     %% Replace with new implementation
                     NewFun = fun() -> new_behavior end,
                     ok = beamtalk_class:put_method(Pid, someMethod, NewFun, <<"new">>),
                     
                     %% Verify replacement
                     MethodObj = beamtalk_class:method(Pid, someMethod),
                     MethodInfo = maps:get('__method_info__', MethodObj),
                     RetrievedFun = maps:get(block, MethodInfo),
                     ?assertEqual(NewFun, RetrievedFun)
                 end)
         ]
     end}.

%%====================================================================
%% Method Combinations (Flavors Pattern)
%%====================================================================

add_before_method_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => #{}
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     
                     BeforeFun = fun() -> io:format("Before!~n") end,
                     ok = beamtalk_class:add_before(Pid, increment, BeforeFun),
                     
                     %% Can't easily verify without accessing internal state,
                     %% but at least verify the call succeeds
                     ?assert(true)
                 end)
         ]
     end}.

add_after_method_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter,
                         instance_methods => #{}
                     },
                     {ok, Pid} = beamtalk_class:start_link('Counter', ClassInfo),
                     
                     AfterFun = fun() -> io:format("After!~n") end,
                     ok = beamtalk_class:add_after(Pid, increment, AfterFun),
                     
                     ?assert(true)
                 end)
         ]
     end}.

%%====================================================================
%% All Classes Enumeration
%%====================================================================

all_classes_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Start multiple class processes with unique names
                     ClassInfo1 = #{
                         name => 'TestClassA',
                         module => test_class,
                         superclass => none
                     },
                     ClassInfo2 = #{
                         name => 'TestClassB',
                         module => counter,
                         superclass => 'Object'
                     },
                     {ok, Pid1} = beamtalk_class:start_link('TestClassA', ClassInfo1),
                     {ok, Pid2} = beamtalk_class:start_link('TestClassB', ClassInfo2),
                     
                     %% Verify both are in the all_classes list
                     AllClasses = beamtalk_class:all_classes(),
                     ?assert(lists:member(Pid1, AllClasses)),
                     ?assert(lists:member(Pid2, AllClasses)),
                     
                     %% Clean up immediately after test
                     exit(Pid1, kill),
                     exit(Pid2, kill)
                 end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

duplicate_registration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'TestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, _Pid1} = beamtalk_class:start_link('TestClass', ClassInfo),
                     
                     %% Try to register the same name again
                     Result = beamtalk_class:start_link('TestClass', ClassInfo),
                     ?assertMatch({error, _}, Result)
                 end)
         ]
     end}.

whereis_nonexistent_class_test() ->
    Result = beamtalk_class:whereis_class('NonexistentClass'),
    ?assertEqual(undefined, Result).
