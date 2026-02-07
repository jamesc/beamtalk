%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_class_tests).
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
    
    %% Terminate each class process and wait for them to die
    %% gen_server will auto-unregister names when processes terminate
    lists:foreach(
        fun(Pid) when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    MRef = monitor(process, Pid),
                    exit(Pid, kill),
                    receive
                        {'DOWN', MRef, process, Pid, _} -> ok
                    after 1000 ->
                        ok  %% Timeout waiting for process to die
                    end;
                false ->
                    ok
            end;
        (_) ->
            ok
        end,
        Members
    ),
    
    %% Force unregister any stragglers that didn't auto-unregister
    %% (shouldn't be necessary with gen_server, but defensive cleanup)
    lists:foreach(
        fun(Name) ->
            try unregister(Name) catch _:_ -> ok end
        end,
        [beamtalk_class_StartLinkTestClass, beamtalk_class_RegistryTestClass,
         beamtalk_class_PgMembershipTestClass, beamtalk_class_DuplicateTestClass,
         beamtalk_class_TestClassA, beamtalk_class_TestClassB,
         beamtalk_class_Counter, beamtalk_class_ProtoObject, beamtalk_class_LoggingCounter,
         beamtalk_class_CounterNoMethods,
         beamtalk_class_RootTestClass, beamtalk_class_TestParentClass,
         beamtalk_class_TestChildClass, beamtalk_class_TestArityParent,
         beamtalk_class_AsyncTestClass, beamtalk_class_AsyncSuperTest,
         beamtalk_class_AsyncNameTest, beamtalk_class_AsyncModTest,
         beamtalk_class_AsyncUnkTest, beamtalk_class_AsyncIgnoreTest,
         beamtalk_class_InfoTestClass, beamtalk_class_TermTestClass]
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
                         name => 'StartLinkTestClass',
                         module => test_class,
                         superclass => 'Object',
                         instance_methods => #{},
                         instance_variables => []
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('StartLinkTestClass', ClassInfo),
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
                         name => 'RegistryTestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, _Pid} = beamtalk_object_class:start_link('RegistryTestClass', ClassInfo),
                     RegPid = beamtalk_object_class:whereis_class('RegistryTestClass'),
                     ?assert(is_pid(RegPid)),
                     ?assertEqual(RegPid, whereis(beamtalk_class_RegistryTestClass))
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
                         name => 'PgMembershipTestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('PgMembershipTestClass', ClassInfo),
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     Result = beamtalk_object_class:methods(Pid),
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     ?assertEqual('Object', beamtalk_object_class:superclass(Pid))
                 end),
          ?_test(begin
                     ClassInfo = #{
                         name => 'ProtoObject',
                         module => proto_object,
                         superclass => none
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('ProtoObject', ClassInfo),
                     ?assertEqual(none, beamtalk_object_class:superclass(Pid))
                 end)
         ]
     end}.

class_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     ?assertEqual('Counter', beamtalk_object_class:class_name(Pid))
                 end),
          ?_test(begin
                     ClassInfo = #{
                         name => 'Beamtalk',
                         module => 'Beamtalk'
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('Beamtalk', ClassInfo),
                     ?assertEqual('Beamtalk', beamtalk_object_class:class_name(Pid))
                 end)
         ]
     end}.

module_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
                     ClassInfo = #{
                         name => 'Counter',
                         module => counter
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     ?assertEqual(counter, beamtalk_object_class:module_name(Pid))
                 end),
          ?_test(begin
                     ClassInfo = #{
                         name => 'Beamtalk',
                         module => 'Beamtalk'
                     },
                     {ok, Pid} = beamtalk_object_class:start_link('Beamtalk', ClassInfo),
                     ?assertEqual('Beamtalk', beamtalk_object_class:module_name(Pid))
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     MethodObj = beamtalk_object_class:method(Pid, increment),
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
                     {ok, Pid} = beamtalk_object_class:start_link('CounterNoMethods', ClassInfo),
                     ?assertEqual(nil, beamtalk_object_class:method(Pid, nonexistent))
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     IVars = beamtalk_object_class:instance_variables(Pid),
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     
                     %% Add a new method
                     NewFun = fun() -> 42 end,
                     Source = <<"newMethod => 42">>,
                     ok = beamtalk_object_class:put_method(Pid, newMethod, NewFun, Source),
                     
                     %% Verify it's in the method list
                     Methods = beamtalk_object_class:methods(Pid),
                     ?assert(lists:member(newMethod, Methods)),
                     
                     %% Verify we can retrieve it
                     MethodObj = beamtalk_object_class:method(Pid, newMethod),
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     
                     %% Replace with new implementation
                     NewFun = fun() -> new_behavior end,
                     ok = beamtalk_object_class:put_method(Pid, someMethod, NewFun, <<"new">>),
                     
                     %% Verify replacement
                     MethodObj = beamtalk_object_class:method(Pid, someMethod),
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     
                     BeforeFun = fun() -> io:format("Before!~n") end,
                     ok = beamtalk_object_class:add_before(Pid, increment, BeforeFun),
                     
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
                     {ok, Pid} = beamtalk_object_class:start_link('Counter', ClassInfo),
                     
                     AfterFun = fun() -> io:format("After!~n") end,
                     ok = beamtalk_object_class:add_after(Pid, increment, AfterFun),
                     
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
                     %% Test that all_classes returns a list
                     %% More thorough testing happens in other tests
                     AllClasses = beamtalk_object_class:all_classes(),
                     ?assert(is_list(AllClasses))
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
                         name => 'DuplicateTestClass',
                         module => test_class,
                         superclass => none
                     },
                     {ok, _Pid1} = beamtalk_object_class:start_link('DuplicateTestClass', ClassInfo),
                     
                     %% Try to register the same name again
                     Result = beamtalk_object_class:start_link('DuplicateTestClass', ClassInfo),
                     ?assertMatch({error, _}, Result)
                 end)
         ]
     end}.

whereis_nonexistent_class_test() ->
    Result = beamtalk_object_class:whereis_class('NonexistentClass'),
    ?assertEqual(undefined, Result).

%%====================================================================
%% BT-344: Super dispatch tests
%%====================================================================

super_dispatch_missing_class_field_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              %% State without __class__ field
              State = #{value => 0},
              Result = beamtalk_object_class:super_dispatch(State, increment, []),
              ?assertMatch({error, missing_class_field_in_state}, Result)
          end)]
     end}.

super_dispatch_class_not_found_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              %% State with class that doesn't exist
              State = #{'__class__' => 'NonexistentClass999'},
              Result = beamtalk_object_class:super_dispatch(State, increment, []),
              ?assertMatch({error, {class_not_found, 'NonexistentClass999'}}, Result)
          end)]
     end}.

super_dispatch_no_superclass_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              %% Create a root class (no superclass)
              ClassInfo = #{
                  name => 'RootTestClass',
                  module => test_class,
                  superclass => none
              },
              {ok, _Pid} = beamtalk_object_class:start_link('RootTestClass', ClassInfo),
              State = #{'__class__' => 'RootTestClass'},
              Result = beamtalk_object_class:super_dispatch(State, someMethod, []),
              ?assertMatch({error, {no_superclass, 'RootTestClass', someMethod}}, Result)
          end)]
     end}.

%%====================================================================
%% BT-344: create_subclass tests
%%====================================================================

create_subclass_nonexistent_superclass_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              Result = beamtalk_object_class:create_subclass(
                  'NoSuchSuperclass', 'TestChild', #{instance_variables => []}),
              ?assertMatch({error, {superclass_not_found, 'NoSuchSuperclass'}}, Result)
          end)]
     end}.

create_subclass_success_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              %% Create a parent class first
              ParentInfo = #{
                  name => 'TestParentClass',
                  module => test_class,
                  superclass => none
              },
              {ok, _ParentPid} = beamtalk_object_class:start_link('TestParentClass', ParentInfo),

              %% Create subclass
              Result = beamtalk_object_class:create_subclass(
                  'TestParentClass', 'TestChildClass', #{
                      instance_variables => [x],
                      instance_methods => #{
                          'getX' => fun(_Self, [], State) ->
                              {reply, maps:get(x, State, nil), State}
                          end
                      }
                  }),
              ?assertMatch({ok, _Pid}, Result),

              %% Verify child class is registered
              ?assertNotEqual(undefined, beamtalk_object_class:whereis_class('TestChildClass')),

              %% Verify superclass
              ChildPid = beamtalk_object_class:whereis_class('TestChildClass'),
              ?assertEqual('TestParentClass', beamtalk_object_class:superclass(ChildPid))
          end)]
     end}.

create_subclass_invalid_method_arity_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              %% Create a parent class
              ParentInfo = #{
                  name => 'TestArityParent',
                  module => test_class,
                  superclass => none
              },
              {ok, _ParentPid} = beamtalk_object_class:start_link('TestArityParent', ParentInfo),

              %% Try to create subclass with wrong arity method (should be 3)
              Result = beamtalk_object_class:create_subclass(
                  'TestArityParent', 'TestArityChild', #{
                      instance_methods => #{
                          'bad' => fun(X) -> X end  % Arity 1, not 3
                      }
                  }),
              ?assertMatch({error, _}, Result)
          end)]
     end}.

%%====================================================================
%% BT-344: Async cast dispatch (Future protocol) tests
%%====================================================================

async_cast_methods_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncTestClass',
                  module => test_class,
                  superclass => none,
                  instance_methods => #{
                      increment => #{arity => 0}
                  }
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncTestClass', ClassInfo),

              %% Test async methods query
              FuturePid = self(),
              gen_server:cast(ClassPid, {methods, [], FuturePid}),
              receive
                  {resolve, Methods} ->
                      ?assert(is_list(Methods)),
                      ?assert(lists:member(increment, Methods))
              after 1000 ->
                  ?assert(false)
              end
          end)]
     end}.

async_cast_superclass_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncSuperTest',
                  module => test_class,
                  superclass => 'Object'
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncSuperTest', ClassInfo),

              FuturePid = self(),
              gen_server:cast(ClassPid, {superclass, [], FuturePid}),
              receive
                  {resolve, Super} ->
                      ?assertEqual('Object', Super)
              after 1000 ->
                  ?assert(false)
              end
          end)]
     end}.

async_cast_class_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncNameTest',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncNameTest', ClassInfo),

              FuturePid = self(),
              gen_server:cast(ClassPid, {class_name, [], FuturePid}),
              receive
                  {resolve, Name} ->
                      ?assertEqual('AsyncNameTest', Name)
              after 1000 ->
                  ?assert(false)
              end
          end)]
     end}.

async_cast_module_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncModTest',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncModTest', ClassInfo),

              FuturePid = self(),
              gen_server:cast(ClassPid, {module_name, [], FuturePid}),
              receive
                  {resolve, Mod} ->
                      ?assertEqual(test_class, Mod)
              after 1000 ->
                  ?assert(false)
              end
          end)]
     end}.

async_cast_unknown_message_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncUnkTest',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncUnkTest', ClassInfo),

              FuturePid = self(),
              gen_server:cast(ClassPid, {unknownSelector, [arg1], FuturePid}),
              receive
                  {reject, {error, {unknown_class_message, unknownSelector}}} ->
                      ok
              after 1000 ->
                  ?assert(false)
              end
          end)]
     end}.

async_cast_non_tuple_ignored_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'AsyncIgnoreTest',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('AsyncIgnoreTest', ClassInfo),

              gen_server:cast(ClassPid, some_random_atom),
              timer:sleep(50),
              ?assert(is_process_alive(ClassPid))
          end)]
     end}.

%%====================================================================
%% BT-344: handle_info / terminate / code_change tests
%%====================================================================

handle_info_unknown_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'InfoTestClass',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('InfoTestClass', ClassInfo),

              ClassPid ! random_info_message,
              timer:sleep(50),
              ?assert(is_process_alive(ClassPid))
          end)]
     end}.

terminate_graceful_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [?_test(begin
              ClassInfo = #{
                  name => 'TermTestClass',
                  module => test_class,
                  superclass => none
              },
              {ok, ClassPid} = beamtalk_object_class:start_link('TermTestClass', ClassInfo),
              gen_server:stop(ClassPid),
              timer:sleep(50),
              ?assertNot(is_process_alive(ClassPid))
          end)]
     end}.
