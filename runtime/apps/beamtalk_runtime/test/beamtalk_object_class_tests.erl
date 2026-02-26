%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime

%%% @doc EUnit tests for beamtalk_object_class module.
%%%
%%% Tests class-side method dispatch, metaclass lookup, and class object behaviour.

-module(beamtalk_object_class_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
    %% Clean up ETS pending load errors table entries (BT-738)
    try
        ets:delete_all_objects(beamtalk_pending_load_errors)
    catch
        _:_ -> ok
    end,
    %% Clean up all registered class processes by getting all from pg group
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,

    %% Terminate each class process and wait for them to die
    %% gen_server will auto-unregister names when processes terminate
    lists:foreach(
        fun
            (Pid) when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        MRef = monitor(process, Pid),
                        exit(Pid, kill),
                        receive
                            {'DOWN', MRef, process, Pid, _} -> ok
                        after 1000 ->
                            %% Timeout waiting for process to die
                            ok
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
            try
                unregister(Name)
            catch
                _:_ -> ok
            end
        end,
        [
            beamtalk_class_StartLinkTestClass,
            beamtalk_class_RegistryTestClass,
            beamtalk_class_PgMembershipTestClass,
            beamtalk_class_DuplicateTestClass,
            beamtalk_class_CollisionTestClass,
            beamtalk_class_TestClassA,
            beamtalk_class_TestClassB,
            beamtalk_class_Counter,
            beamtalk_class_ProtoObject,
            beamtalk_class_LoggingCounter,
            beamtalk_class_CounterNoMethods,
            beamtalk_class_RootTestClass,
            beamtalk_class_TestParentClass,
            beamtalk_class_TestChildClass,
            beamtalk_class_TestArityParent,
            beamtalk_class_AsyncTestClass,
            beamtalk_class_AsyncSuperTest,
            beamtalk_class_AsyncNameTest,
            beamtalk_class_AsyncModTest,
            beamtalk_class_AsyncUnkTest,
            beamtalk_class_AsyncIgnoreTest,
            beamtalk_class_InfoTestClass,
            beamtalk_class_TermTestClass,
            beamtalk_class_HierRoot,
            beamtalk_class_HierMid,
            beamtalk_class_HierLeaf,
            beamtalk_class_HierOrphan,
            beamtalk_class_HierParent,
            beamtalk_class_HierChild,
            beamtalk_class_NewErrorTestClass,
            beamtalk_class_DoubleWrapTestClass,
            beamtalk_class_PrimTestInteger,
            beamtalk_class_StdlibShadowTest,
            beamtalk_class_MultiShadowInteger,
            beamtalk_class_MultiShadowHelper,
            beamtalk_class_SelfCallTestClass
        ]
    ),
    %% Clean up ETS hierarchy table entries
    try
        ets:delete_all_objects(beamtalk_class_hierarchy)
    catch
        _:_ -> ok
    end,
    %% BT-737: Clean up collision warnings table entries
    try
        ets:delete_all_objects(beamtalk_class_warnings)
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Basic Lifecycle Tests
%%====================================================================

start_link_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'RegistryTestClass',
                    module => test_class,
                    superclass => none
                },
                {ok, _Pid} = beamtalk_object_class:start_link('RegistryTestClass', ClassInfo),
                RegPid = beamtalk_class_registry:whereis_class('RegistryTestClass'),
                ?assert(is_pid(RegPid)),
                ?assertEqual(RegPid, whereis(beamtalk_class_RegistryTestClass))
            end)
        ]
    end}.

pg_group_membership_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
                {ok, Pid} =
                    case beamtalk_object_class:start_link('ProtoObject', ClassInfo) of
                        {ok, P} -> {ok, P};
                        {error, {already_started, P}} -> {ok, P}
                    end,
                ?assertEqual(none, beamtalk_object_class:superclass(Pid))
            end)
        ]
    end}.

class_name_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
                ?assertEqual('CompiledMethod', maps:get('$beamtalk_class', MethodObj)),
                ?assertEqual(increment, maps:get('__selector__', MethodObj)),
                ?assertEqual(
                    <<"increment => self.count := self.count + 1">>,
                    maps:get('__source__', MethodObj)
                )
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

%% BT-323: Tests for beamtalk_method_resolver domain service
method_resolver_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            %% Resolve via class object tuple
            ?_test(begin
                Methods = #{
                    getValue => #{arity => 0, block => fun() -> ok end}
                },
                MethodSource = #{
                    getValue => <<"getValue => ^self.value">>
                },
                ClassInfo = #{
                    name => 'ResolverTest',
                    module => resolver_test,
                    instance_methods => Methods,
                    method_source => MethodSource
                },
                {ok, Pid} = beamtalk_object_class:start_link('ResolverTest', ClassInfo),
                ClassObj = {beamtalk_object, 'ResolverTest class', resolver_test, Pid},
                MethodObj = beamtalk_method_resolver:resolve(ClassObj, getValue),
                ?assertEqual('CompiledMethod', maps:get('$beamtalk_class', MethodObj)),
                ?assertEqual(getValue, maps:get('__selector__', MethodObj))
            end),
            %% Resolve via class object tuple returns nil for missing method
            ?_test(begin
                ClassInfo = #{
                    name => 'ResolverNilTest',
                    module => resolver_nil_test,
                    instance_methods => #{}
                },
                {ok, Pid} = beamtalk_object_class:start_link('ResolverNilTest', ClassInfo),
                ClassObj = {beamtalk_object, 'ResolverNilTest class', resolver_nil_test, Pid},
                ?assertEqual(nil, beamtalk_method_resolver:resolve(ClassObj, nonexistent))
            end),
            %% Type error for non-class receiver (integer)
            ?_test(begin
                ?assertException(
                    error,
                    _,
                    beamtalk_method_resolver:resolve(42, foo)
                )
            end),
            %% Type error for instance object (not a class)
            ?_test(begin
                InstanceObj = {beamtalk_object, 'Counter', counter, self()},
                ?assertException(
                    error,
                    _,
                    beamtalk_method_resolver:resolve(InstanceObj, foo)
                )
            end),
            %% Malformed tuple with non-atom ClassTag falls through to catch-all
            ?_test(begin
                BadTuple = {beamtalk_object, 123, counter, self()},
                ?assertException(
                    error,
                    _,
                    beamtalk_method_resolver:resolve(BadTuple, foo)
                )
            end)
        ]
    end}.

instance_variables_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'Counter',
                    module => counter,
                    fields => [count, name]
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
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
                ?assertEqual('CompiledMethod', maps:get('$beamtalk_class', MethodObj)),
                ?assertEqual(newMethod, maps:get('__selector__', MethodObj)),
                ?assertEqual(Source, maps:get('__source__', MethodObj))
            end)
        ]
    end}.

replace_method_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
%% All Classes Enumeration
%%====================================================================

all_classes_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Test that all_classes returns a list
                %% More thorough testing happens in other tests
                AllClasses = beamtalk_class_registry:all_classes(),
                ?assert(is_list(AllClasses))
            end)
        ]
    end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

duplicate_registration_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
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
    Result = beamtalk_class_registry:whereis_class('NonexistentClass'),
    ?assertEqual(undefined, Result).

%%====================================================================
%% BT-344: Async cast dispatch (Future protocol) tests
%%====================================================================

async_cast_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
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
            end)
        ]
    end}.

async_cast_superclass_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
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
            end)
        ]
    end}.

async_cast_class_name_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
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
            end)
        ]
    end}.

async_cast_module_name_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
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
            end)
        ]
    end}.

async_cast_unknown_message_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'AsyncUnkTest',
                    module => test_class,
                    superclass => none
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('AsyncUnkTest', ClassInfo),

                FuturePid = self(),
                gen_server:cast(ClassPid, {unknownSelector, [arg1], FuturePid}),
                receive
                    {reject, #beamtalk_error{
                        kind = does_not_understand, selector = unknownSelector
                    }} ->
                        ok
                after 1000 ->
                    ?assert(false)
                end
            end)
        ]
    end}.

async_cast_non_tuple_ignored_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'AsyncIgnoreTest',
                    module => test_class,
                    superclass => none
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('AsyncIgnoreTest', ClassInfo),

                gen_server:cast(ClassPid, some_random_atom),
                timer:sleep(50),
                ?assert(is_process_alive(ClassPid))
            end)
        ]
    end}.

%%====================================================================
%% BT-344: handle_info / terminate / code_change tests
%%====================================================================

handle_info_unknown_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'InfoTestClass',
                    module => test_class,
                    superclass => none
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('InfoTestClass', ClassInfo),

                ClassPid ! random_info_message,
                timer:sleep(50),
                ?assert(is_process_alive(ClassPid))
            end)
        ]
    end}.

terminate_graceful_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'TermTestClass',
                    module => test_class,
                    superclass => none
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('TermTestClass', ClassInfo),
                gen_server:stop(ClassPid),
                timer:sleep(50),
                ?assertNot(is_process_alive(ClassPid))
            end)
        ]
    end}.

%%====================================================================
%% BT-510: ETS Class Hierarchy Tests
%%====================================================================

%% Test that class registration populates the hierarchy ETS table
hierarchy_ets_populated_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register root, then child with superclass
                RootInfo = #{
                    name => 'HierRoot',
                    module => hier_root,
                    superclass => none,
                    instance_methods => #{rootMethod => #{arity => 0}}
                },
                {ok, _} = beamtalk_object_class:start_link('HierRoot', RootInfo),
                %% ETS table should have an entry for root
                ?assertEqual(
                    [{'HierRoot', none}],
                    ets:lookup(beamtalk_class_hierarchy, 'HierRoot')
                ),
                %% Register child with superclass
                ChildInfo = #{
                    name => 'HierMid',
                    module => hier_mid,
                    superclass => 'HierRoot',
                    instance_methods => #{midMethod => #{arity => 0}}
                },
                {ok, _} = beamtalk_object_class:start_link('HierMid', ChildInfo),
                ?assertEqual(
                    [{'HierMid', 'HierRoot'}],
                    ets:lookup(beamtalk_class_hierarchy, 'HierMid')
                )
            end)
        ]
    end}.

%% Test inherits_from/2 uses ETS for fast hierarchy lookups
hierarchy_inherits_from_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Build 3-level hierarchy: Root -> Mid -> Leaf
                {ok, _} = beamtalk_object_class:start_link('HierRoot', #{
                    name => 'HierRoot',
                    module => hier_root,
                    superclass => none,
                    instance_methods => #{rootMethod => #{arity => 0}}
                }),
                {ok, _} = beamtalk_object_class:start_link('HierMid', #{
                    name => 'HierMid',
                    module => hier_mid,
                    superclass => 'HierRoot',
                    instance_methods => #{midMethod => #{arity => 0}}
                }),
                {ok, _} = beamtalk_object_class:start_link('HierLeaf', #{
                    name => 'HierLeaf',
                    module => hier_leaf,
                    superclass => 'HierMid',
                    instance_methods => #{leafMethod => #{arity => 0}}
                }),
                %% Direct parent
                ?assert(beamtalk_class_registry:inherits_from('HierMid', 'HierRoot')),
                %% Grandparent
                ?assert(beamtalk_class_registry:inherits_from('HierLeaf', 'HierRoot')),
                %% Self
                ?assert(beamtalk_class_registry:inherits_from('HierRoot', 'HierRoot')),
                %% Not an ancestor
                ?assertNot(beamtalk_class_registry:inherits_from('HierRoot', 'HierLeaf')),
                %% Unrelated class
                ?assertNot(beamtalk_class_registry:inherits_from('HierRoot', 'NonExistent')),
                %% none base case
                ?assertNot(beamtalk_class_registry:inherits_from(none, 'HierRoot'))
            end)
        ]
    end}.

%% ADR 0032 Phase 1: methods/1 returns local-only selectors.
%% Out-of-order registration no longer affects the methods list;
%% hierarchy walk happens at dispatch time via beamtalk_dispatch:responds_to/2.
hierarchy_methods_inherited_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, ChildPid} = beamtalk_object_class:start_link('HierChild', #{
                    name => 'HierChild',
                    module => hier_child,
                    superclass => 'HierParent',
                    instance_methods => #{childMethod => #{arity => 0}}
                }),
                %% methods/1 returns local-only selectors — never inherited
                MethodsBefore = beamtalk_object_class:methods(ChildPid),
                ?assert(lists:member(childMethod, MethodsBefore)),
                ?assertNot(lists:member(parentMethod, MethodsBefore)),

                %% Register parent
                {ok, _ParentPid} = beamtalk_object_class:start_link('HierParent', #{
                    name => 'HierParent',
                    module => hier_parent,
                    superclass => none,
                    instance_methods => #{parentMethod => #{arity => 0}}
                }),

                %% methods/1 still returns only local selectors after parent is registered
                MethodsAfter = beamtalk_object_class:methods(ChildPid),
                ?assert(lists:member(childMethod, MethodsAfter)),
                ?assertNot(lists:member(parentMethod, MethodsAfter)),

                %% Inherited method is found via responds_to (chain walk at dispatch time)
                ?assert(beamtalk_dispatch:responds_to(parentMethod, 'HierChild'))
            end)
        ]
    end}.

%% ADR 0032 Phase 1: Normal order — parent first, child local-only in methods/1.
%% Inherited method is visible via responds_to (chain walk), not methods/1.
hierarchy_methods_normal_order_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ok, _} = beamtalk_object_class:start_link('HierParent', #{
                    name => 'HierParent',
                    module => hier_parent,
                    superclass => none,
                    instance_methods => #{parentMethod => #{arity => 0}}
                }),
                {ok, ChildPid} = beamtalk_object_class:start_link('HierChild', #{
                    name => 'HierChild',
                    module => hier_child,
                    superclass => 'HierParent',
                    instance_methods => #{childMethod => #{arity => 0}}
                }),
                %% methods/1 returns local-only selectors
                Methods = beamtalk_object_class:methods(ChildPid),
                ?assert(lists:member(childMethod, Methods)),
                ?assertNot(lists:member(parentMethod, Methods)),
                %% Inherited method found via responds_to (chain walk)
                ?assert(beamtalk_dispatch:responds_to(parentMethod, 'HierChild'))
            end)
        ]
    end}.

%% BT-510: Test that ETS hierarchy records orphan relationships
hierarchy_orphan_registration_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register orphan class whose superclass doesn't exist yet
                {ok, _Pid} = beamtalk_object_class:start_link('HierOrphan', #{
                    name => 'HierOrphan',
                    module => hier_orphan,
                    superclass => 'NonExistentParent',
                    instance_methods => #{orphanMethod => #{arity => 0}}
                }),
                %% ETS records the declared relationship regardless
                ?assertEqual(
                    [{'HierOrphan', 'NonExistentParent'}],
                    ets:lookup(beamtalk_class_hierarchy, 'HierOrphan')
                ),
                %% inherits_from follows the declared chain:
                %% HierOrphan -> NonExistentParent (self-match = true)
                ?assert(beamtalk_class_registry:inherits_from('HierOrphan', 'NonExistentParent')),
                %% But NonExistentParent has no entry, so deeper queries stop
                ?assertNot(beamtalk_class_registry:inherits_from('HierOrphan', 'SomeOtherClass'))
            end)
        ]
    end}.

%%====================================================================
%% BT-525: raise/1 inside handle_call — unwrap_class_call idempotency
%%====================================================================

%% Compile a minimal test module in-memory for BT-525 tests.
%% Returns the module name atom. The module exports new/0 (returns
%% an empty map) so compute_is_constructible/2 sees it as constructible.
compile_bt525_test_module() ->
    ModName = bt525_test_mod,
    Forms = [
        {attribute, 1, module, ModName},
        {attribute, 2, export, [{new, 0}]},
        {function, 3, new, 0, [
            {clause, 3, [], [], [{map, 3, []}]}
        ]}
    ],
    {ok, ModName, Binary} = compile:forms(Forms, []),
    {module, ModName} = code:load_binary(ModName, "bt525_test_mod.erl", Binary),
    ModName.

%% Test that new: with a non-map argument raises a singly-wrapped
%% type_error via class_send (exercises raise → catch → unwrap_class_call).
%% Before BT-525 fix, raise/1 in handle_call produced an already-wrapped
%% Exception map, and unwrap_class_call called raise/1 again → double-wrap.
new_with_non_map_raises_type_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Mod = compile_bt525_test_module(),
                ClassInfo = #{
                    name => 'NewErrorTestClass',
                    module => Mod,
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('NewErrorTestClass', ClassInfo),

                %% class_send new: with integer (not a map) should raise
                %% a singly-wrapped Exception with type_error kind.
                %% The '$beamtalk_class' key uses := (match) to verify
                %% the error is a proper Exception map, not double-wrapped.
                ?assertError(
                    #{
                        '$beamtalk_class' := 'TypeError',
                        error := #beamtalk_error{
                            kind = type_error,
                            selector = 'new:'
                        }
                    },
                    beamtalk_object_class:class_send(ClassPid, 'new:', [42])
                ),

                %% Class process must survive the error
                ?assert(is_process_alive(ClassPid)),

                %% Class still responds to normal queries
                ?assertEqual(
                    'NewErrorTestClass',
                    gen_server:call(ClassPid, class_name)
                )
            end)
        ]
    end}.

%% Test that the error from new: is singly-wrapped — the inner 'error'
%% field must be a #beamtalk_error{} record, NOT another Exception map.
%% This is the specific assertion that would have caught double-wrapping.
new_with_non_map_not_double_wrapped_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Mod = compile_bt525_test_module(),
                ClassInfo = #{
                    name => 'DoubleWrapTestClass',
                    module => Mod,
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, ClassPid} = beamtalk_object_class:start_link('DoubleWrapTestClass', ClassInfo),

                %% Catch the error and inspect its structure directly
                CaughtError =
                    try
                        beamtalk_object_class:class_send(ClassPid, 'new:', [42]),
                        no_error
                    catch
                        error:E -> E
                    end,
                ?assertNotEqual(no_error, CaughtError),

                %% Must be a map with '$beamtalk_class' key (Exception object)
                ?assertMatch(#{'$beamtalk_class' := _}, CaughtError),

                %% The inner 'error' field must be a #beamtalk_error{} record,
                %% NOT another Exception map (which would indicate double-wrapping)
                #{error := Inner} = CaughtError,
                ?assert(is_record(Inner, beamtalk_error)),

                %% Verify the inner error has the expected fields
                ?assertEqual(type_error, Inner#beamtalk_error.kind),
                ?assertEqual('new:', Inner#beamtalk_error.selector)
            end)
        ]
    end}.

%%====================================================================
%% BT-737: Class Collision Warning Tests
%%====================================================================

%% Test that update_class with DIFFERENT module emits a warning in ETS.
update_class_different_module_records_warning_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure warnings table exists
                beamtalk_class_registry:ensure_class_warnings_table(),
                ets:delete_all_objects(beamtalk_class_warnings),

                %% Start class with module_a
                ClassInfo = #{
                    name => 'DuplicateTestClass',
                    module => module_a,
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start_link('DuplicateTestClass', ClassInfo),

                %% Call update_class with module_b (cross-package collision)
                NewInfo = ClassInfo#{module => module_b},
                {ok, _IVars} = beamtalk_object_class:update_class('DuplicateTestClass', NewInfo),

                %% Warning should be recorded in ETS keyed by class name
                Warnings = beamtalk_class_registry:drain_class_warnings_by_names([
                    'DuplicateTestClass'
                ]),
                ?assertEqual([{'DuplicateTestClass', module_a, module_b}], Warnings)
            end)
        ]
    end}.

%% Test that update_class with SAME module does NOT emit a warning.
update_class_same_module_no_warning_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure warnings table exists and is clean
                beamtalk_class_registry:ensure_class_warnings_table(),
                ets:delete_all_objects(beamtalk_class_warnings),

                %% Start class with same_module
                ClassInfo = #{
                    name => 'DuplicateTestClass',
                    module => same_module,
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start_link('DuplicateTestClass', ClassInfo),

                %% Call update_class with the SAME module (hot reload — no warning)
                NewInfo = ClassInfo#{instance_methods => #{greet => #{arity => 0}}},
                {ok, _IVars} = beamtalk_object_class:update_class('DuplicateTestClass', NewInfo),

                %% No warning should be recorded
                Warnings = beamtalk_class_registry:drain_class_warnings_by_names([
                    'DuplicateTestClass'
                ]),
                ?assertEqual([], Warnings)
            end)
        ]
    end}.

%% Test that new: errors from non-constructible classes (e.g. primitives)
%% also go through unwrap_class_call correctly without double-wrapping.
%% This exercises the is_constructible=false → Module:new/0 error path.
new_on_primitive_not_double_wrapped_test_() ->
    {setup,
        fun() ->
            setup(),
            %% Need stdlib loaded for primitive class modules
            case whereis(beamtalk_bootstrap) of
                undefined -> beamtalk_bootstrap:start_link();
                _ -> ok
            end,
            beamtalk_stdlib:init()
        end,
        fun teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% Integer's new/0 raises instantiation_error (sealed primitive)
                    ClassInfo = #{
                        name => 'PrimTestInteger',
                        module => 'bt@stdlib@integer',
                        superclass => none,
                        instance_methods => #{}
                    },
                    {ok, ClassPid} = beamtalk_object_class:start_link('PrimTestInteger', ClassInfo),

                    %% new on a primitive should raise an InstantiationError,
                    %% singly-wrapped through unwrap_class_call
                    CaughtError =
                        try
                            beamtalk_object_class:class_send(ClassPid, 'new', []),
                            no_error
                        catch
                            error:E -> E
                        end,
                    ?assertNotEqual(no_error, CaughtError),
                    ?assertMatch(#{'$beamtalk_class' := _}, CaughtError),
                    #{error := Inner} = CaughtError,
                    ?assert(is_record(Inner, beamtalk_error)),
                    ?assertEqual(instantiation_error, Inner#beamtalk_error.kind)
                end)
            ]
        end}.

%%====================================================================
%% BT-738: Stdlib class shadowing protection tests
%%====================================================================

%% Test that update_class returns a structured error when user code tries
%% to shadow a stdlib class (bt@stdlib@* module → non-stdlib module).
update_class_stdlib_shadowing_returns_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure the pending errors ETS table exists and is clean
                beamtalk_class_registry:record_pending_load_error('__warmup__', #beamtalk_error{}),
                ets:delete_all_objects(beamtalk_pending_load_errors),

                %% Register a class with a stdlib module (simulating a loaded stdlib class)
                ClassInfo = #{
                    name => 'StdlibShadowTest',
                    module => 'bt@stdlib@integer',
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start_link('StdlibShadowTest', ClassInfo),

                %% Attempt to update_class with a non-stdlib module (user code shadowing)
                UserInfo = ClassInfo#{module => 'bt@user_app@integer'},
                Result = beamtalk_object_class:update_class('StdlibShadowTest', UserInfo),

                %% Must return a structured stdlib_shadowing error
                ?assertMatch({error, #beamtalk_error{kind = stdlib_shadowing}}, Result),

                %% Error must be recorded in the pending errors ETS table
                Errors = beamtalk_class_registry:drain_pending_load_errors_by_names([
                    'StdlibShadowTest'
                ]),
                ?assertMatch(
                    [{'StdlibShadowTest', #beamtalk_error{kind = stdlib_shadowing}}], Errors
                )
            end)
        ]
    end}.

%% Test that update_class with a stdlib-to-stdlib reload does NOT return an error.
%% (E.g. hot-reloading the stdlib itself is allowed.)
update_class_stdlib_to_stdlib_no_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure the pending errors ETS table exists and is clean
                beamtalk_class_registry:record_pending_load_error('__warmup__', #beamtalk_error{}),
                ets:delete_all_objects(beamtalk_pending_load_errors),

                %% Register a class with a stdlib module
                ClassInfo = #{
                    name => 'StdlibShadowTest',
                    module => 'bt@stdlib@integer',
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start_link('StdlibShadowTest', ClassInfo),

                %% Update with the SAME stdlib module (hot-reload — allowed)
                NewStdlibInfo = ClassInfo#{module => 'bt@stdlib@integer'},
                Result = beamtalk_object_class:update_class('StdlibShadowTest', NewStdlibInfo),

                %% Must succeed (ok or warning, but not stdlib_shadowing error)
                ?assertNotMatch({error, #beamtalk_error{kind = stdlib_shadowing}}, Result),

                %% No pending error should be recorded
                Errors = beamtalk_class_registry:drain_pending_load_errors_by_names([
                    'StdlibShadowTest'
                ]),
                ?assertEqual([], Errors)
            end)
        ]
    end}.

%% Test that update_class with two non-stdlib modules does NOT trigger stdlib
%% shadowing protection — it only emits a cross-package collision warning (BT-737).
update_class_non_stdlib_no_shadowing_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure ETS tables are clean
                beamtalk_class_registry:ensure_class_warnings_table(),
                ets:delete_all_objects(beamtalk_class_warnings),
                beamtalk_class_registry:record_pending_load_error('__warmup__', #beamtalk_error{}),
                ets:delete_all_objects(beamtalk_pending_load_errors),

                %% Register a class with a non-stdlib module
                ClassInfo = #{
                    name => 'StdlibShadowTest',
                    module => 'bt@pkg_a@counter',
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start_link('StdlibShadowTest', ClassInfo),

                %% Update with a different non-stdlib module (cross-package collision)
                NewInfo = ClassInfo#{module => 'bt@pkg_b@counter'},
                Result = beamtalk_object_class:update_class('StdlibShadowTest', NewInfo),

                %% Must succeed (returns ok with instance vars) — stdlib shadowing NOT triggered
                ?assertNotMatch({error, #beamtalk_error{kind = stdlib_shadowing}}, Result),

                %% No pending stdlib error
                Errors = beamtalk_class_registry:drain_pending_load_errors_by_names([
                    'StdlibShadowTest'
                ]),
                ?assertEqual([], Errors)
            end)
        ]
    end}.

%%====================================================================
%% Multi-class stdlib shadowing integration tests (BT-751)
%%====================================================================

%% BT-751: Integration test for the multi-class short-circuit behaviour
%% validated at the codegen level by BT-749.
%%
%% Simulates the generated register_class/0 for a two-class module where
%% the first class (Integer) shadows a stdlib class.  The short-circuit
%% must propagate the error and prevent the second class (MyHelper) from
%% being registered.
multi_class_stdlib_shadowing_short_circuits_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Ensure ETS tables exist and are clean
                beamtalk_class_registry:record_pending_load_error('__warmup__', #beamtalk_error{}),
                ets:delete_all_objects(beamtalk_pending_load_errors),

                %% --- Pre-condition: register a fake stdlib class for Integer ---
                StdlibInfo = #{
                    name => 'MultiShadowInteger',
                    module => 'bt@stdlib@integer',
                    superclass => none,
                    instance_methods => #{}
                },
                {ok, _StdPid} = beamtalk_object_class:start_link('MultiShadowInteger', StdlibInfo),

                %% ============================================================
                %% Simulate register_class/0 for a two-class module:
                %%   Class 0: "Integer" (shadows stdlib)
                %%   Class 1: "MyHelper" (innocent bystander)
                %%
                %% Generated Core Erlang (BT-749) looks like:
                %%   _Reg0 = case start(...Integer...) of ... end
                %%   case _Reg0 of
                %%     <{'error', Err}> -> {'error', Err}   % short-circuit
                %%     <_> -> _Reg1 = case start(...MyHelper...) of ... end
                %%            in _Reg1
                %%   end
                %% ============================================================

                %% --- Class 0: start + update_class for Integer ---
                UserIntegerInfo = #{
                    name => 'MultiShadowInteger',
                    module => 'bt@user_app@integer',
                    superclass => 'Object',
                    instance_methods => #{}
                },
                Reg0 =
                    case beamtalk_object_class:start('MultiShadowInteger', UserIntegerInfo) of
                        {ok, _Pid0} ->
                            beamtalk_object_class:update_class(
                                'MultiShadowInteger', UserIntegerInfo
                            );
                        {error, {already_started, _Pid0}} ->
                            beamtalk_object_class:update_class(
                                'MultiShadowInteger', UserIntegerInfo
                            );
                        {error, Reason0} ->
                            {error, Reason0}
                    end,

                %% --- Short-circuit check (mirrors generated case expression) ---
                FinalResult =
                    case Reg0 of
                        {error, _RegErr0} ->
                            {error, _RegErr0};
                        _ ->
                            %% Class 1: start + update_class for MyHelper
                            HelperInfo = #{
                                name => 'MultiShadowHelper',
                                module => 'bt@user_app@integer',
                                superclass => 'Object',
                                instance_methods => #{}
                            },
                            case beamtalk_object_class:start('MultiShadowHelper', HelperInfo) of
                                {ok, _Pid1} ->
                                    beamtalk_object_class:update_class(
                                        'MultiShadowHelper', HelperInfo
                                    );
                                {error, {already_started, _Pid1}} ->
                                    beamtalk_object_class:update_class(
                                        'MultiShadowHelper', HelperInfo
                                    );
                                {error, Reason1} ->
                                    {error, Reason1}
                            end
                    end,

                %% === Acceptance Criteria ===

                %% 1. Loading must produce a stdlib_shadowing error
                ?assertMatch({error, #beamtalk_error{kind = stdlib_shadowing}}, FinalResult),

                %% 2. MyHelper must NOT be registered (short-circuit skipped it)
                ?assertEqual(
                    undefined,
                    beamtalk_class_registry:whereis_class('MultiShadowHelper')
                ),

                %% 3. The error must be in the pending load errors ETS table
                PendingErrors = beamtalk_class_registry:drain_pending_load_errors_by_names(
                    ['MultiShadowInteger', 'MultiShadowHelper']
                ),
                ?assertMatch(
                    [{'MultiShadowInteger', #beamtalk_error{kind = stdlib_shadowing}}],
                    PendingErrors
                ),

                %% 4. No stale ETS entry for MyHelper
                ?assertEqual(
                    [],
                    beamtalk_class_registry:drain_pending_load_errors_by_names(['MultiShadowHelper'])
                )
            end)
        ]
    end}.

%%====================================================================
%% BT-755: class method self-call (new/new: on own class)
%%====================================================================

%% @doc Verify class_send raises a clean instantiation_error when ClassPid =:= self().
%%
%% BT-755: A class method calling new: on its own class would deadlock the
%% gen_server with a {calling_self,...} OTP error.  The guard added in
%% beamtalk_class_dispatch detects this upfront and raises a structured error.
class_method_self_call_new_colon_test() ->
    %% Use self() as ClassPid — triggers the ClassPid =:= self() guard.
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = instantiation_error, selector = 'new:'}
        },
        beamtalk_class_dispatch:class_send(self(), 'new:', [#{}])
    ).

class_method_self_call_new_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = instantiation_error, selector = 'new'}
        },
        beamtalk_class_dispatch:class_send(self(), 'new', [])
    ).

class_method_self_call_spawn_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = instantiation_error, selector = spawn}
        },
        beamtalk_class_dispatch:class_send(self(), spawn, [])
    ).

class_method_self_call_spawn_with_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = instantiation_error, selector = 'spawnWith:'}
        },
        beamtalk_class_dispatch:class_send(self(), 'spawnWith:', [#{}])
    ).

%% @doc Verify the error contains the class name extracted from the registered pid name.
%%
%% BT-755: class_name_from_pid/1 strips 'beamtalk_class_' from the registered
%% name to populate the error's class field. Exercise the full error path by
%% spawning a process registered as a class and calling class_send on self().
class_method_self_call_class_name_in_error_test() ->
    TestClassName = 'SelfCallNameTestClass',
    RegName = list_to_atom("beamtalk_class_" ++ atom_to_list(TestClassName)),
    Parent = self(),
    spawn(fun() ->
        register(RegName, self()),
        try
            beamtalk_class_dispatch:class_send(self(), spawn, [])
        catch
            error:ErrorMap ->
                Parent ! {self_call_error, ErrorMap}
        end
    end),
    receive
        {self_call_error, ErrorMap} ->
            %% class_name_from_pid/1 should have recovered TestClassName from
            %% the registered process name and included it in the error record.
            ?assertMatch(
                #{
                    '$beamtalk_class' := _,
                    error := #beamtalk_error{
                        kind = instantiation_error,
                        class = TestClassName,
                        selector = spawn
                    }
                },
                ErrorMap
            )
    after 2000 ->
        ?assert(false)
    end.
