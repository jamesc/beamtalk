%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface_registry_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_interface module.

Tests the Phase 2 dispatch/3 interface for Beamtalk primitives:
- allClasses selector
- classNamed: selector
- help: selector
- help:selector: selector
- version selector
- unknown selectors (does_not_understand)
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

%% A fake Self value — Beamtalk primitives ignore Self entirely.
fake_self() ->
    {beamtalk_object, 'Beamtalk class', 'bt@stdlib@beamtalk', self()}.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    register_test_classes().

teardown(_) ->
    OldTrap = process_flag(trap_exit, true),
    cleanup_test_classes(),
    process_flag(trap_exit, OldTrap).

register_test_classes() ->
    ensure_class_started('Object', #{
        name => 'Object',
        module => beamtalk_object,
        superclass => none,
        instance_methods => #{},
        instance_variables => []
    }),
    ensure_class_started('Integer', #{
        name => 'Integer',
        module => 'bt@stdlib@integer',
        superclass => 'Object',
        instance_methods => #{
            '+' => #{arity => 1},
            '-' => #{arity => 1}
        },
        instance_variables => []
    }),
    ensure_class_started('Actor', #{
        name => 'Actor',
        module => 'bt@stdlib@actor',
        superclass => 'Object',
        instance_methods => #{},
        instance_variables => []
    }),
    %% Fresh, not `ensure_class_started/2`: a `Counter` class another suite in
    %% the same EUnit VM left running can have lost its metadata rows, which
    %% hides it from `allClasses`. This suite's `Counter` is its own fixture.
    restart_class('Counter', #{
        name => 'Counter',
        module => counter,
        superclass => 'Object',
        instance_methods => #{
            increment => #{arity => 0, '__signature__' => <<"increment">>},
            getValue => #{arity => 0, '__signature__' => <<"getValue">>}
        },
        instance_variables => [value]
    }),
    ok.

restart_class(ClassName, ClassInfo) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> ok;
        Pid -> catch gen_server:stop(Pid, normal, 5000)
    end,
    ensure_class_started(ClassName, ClassInfo).

ensure_class_started(ClassName, ClassInfo) ->
    case beamtalk_object_class:start_link(ClassName, ClassInfo) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

cleanup_test_classes() ->
    %% BT-3815: unwatch before killing so beamtalk_class_monitor cannot
    %% resurrect these classes mid-teardown.
    beamtalk_test_class_teardown:stop_pg_class_processes(),
    lists:foreach(
        fun(Name) ->
            try
                unregister(Name)
            catch
                _:_ -> ok
            end
        end,
        [
            beamtalk_class_Object,
            beamtalk_class_Integer,
            beamtalk_class_Counter,
            beamtalk_class_Actor
        ]
    ),
    ok.

%%====================================================================
%% allClasses Tests
%%====================================================================

all_classes_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns a non-empty list", fun() ->
                Result = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                ?assert(is_list(Result)),
                ?assert(length(Result) > 0)
            end},
            {"all entries are beamtalk_object tuples", fun() ->
                Classes = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                lists:foreach(
                    fun(C) -> ?assertMatch({beamtalk_object, _, _, _}, C) end,
                    Classes
                )
            end},
            {"includes registered test classes", fun() ->
                Classes = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                ClassTags = [element(2, C) || C <- Classes],
                ?assert(lists:member('Object class', ClassTags)),
                ?assert(lists:member('Counter class', ClassTags))
            end}
        ]
    end}.

%%====================================================================
%% classNamed: Tests
%%====================================================================

class_named_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"atom name returns beamtalk_object for known class", fun() ->
                Result = beamtalk_interface:dispatch(
                    'classNamed:', ['Counter'], fake_self()
                ),
                ?assertMatch({beamtalk_object, 'Counter class', _, _}, Result),
                {beamtalk_object, _, _, ClassPid} = Result,
                ?assert(is_pid(ClassPid)),
                ?assert(is_process_alive(ClassPid))
            end},
            {"binary name returns beamtalk_object for known class", fun() ->
                Result = beamtalk_interface:dispatch(
                    'classNamed:', [<<"Counter">>], fake_self()
                ),
                ?assertMatch({beamtalk_object, 'Counter class', _, _}, Result)
            end},
            {"atom name returns nil for unknown class", fun() ->
                Result = beamtalk_interface:dispatch(
                    'classNamed:', ['NoSuchClass'], fake_self()
                ),
                ?assertEqual(nil, Result)
            end},
            {"binary name returns nil for unknown class", fun() ->
                Result = beamtalk_interface:dispatch(
                    'classNamed:', [<<"NoSuchClassXYZ">>], fake_self()
                ),
                ?assertEqual(nil, Result)
            end},
            {"integer argument returns type_error", fun() ->
                Result = beamtalk_interface:dispatch('classNamed:', [42], fake_self()),
                ?assertMatch(
                    {error, #beamtalk_error{kind = type_error, class = 'Beamtalk'}}, Result
                )
            end}
        ]
    end}.

%%====================================================================
%% classNamed: Metaclass Tag Tests
%%====================================================================

class_named_metaclass_tag_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"metaclass tag returns metaclass object for known class", fun() ->
                ClassObj = beamtalk_interface:dispatch(
                    'classNamed:', ['Counter'], fake_self()
                ),
                {beamtalk_object, _, _, ClassPid} = ClassObj,
                MetaObj = beamtalk_interface:dispatch(
                    'classNamed:', ['Counter class'], fake_self()
                ),
                ?assertEqual(
                    {beamtalk_object, 'Metaclass', beamtalk_metaclass_bt, ClassPid},
                    MetaObj
                )
            end},
            {"findClass/1 resolves metaclass tag to metaclass object", fun() ->
                ClassObj = beamtalk_interface:findClass('Integer'),
                {beamtalk_object, _, _, ClassPid} = ClassObj,
                MetaObj = beamtalk_interface:findClass('Integer class'),
                ?assertEqual(
                    {beamtalk_object, 'Metaclass', beamtalk_metaclass_bt, ClassPid},
                    MetaObj
                )
            end},
            {"metaclass tag resolution matches classClass/1 shape", fun() ->
                ClassObj = beamtalk_interface:findClass('Counter'),
                MetaObj = beamtalk_interface:findClass('Counter class'),
                ?assertEqual(
                    beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    MetaObj
                )
            end},
            {"instance-side resolution unchanged for known class", fun() ->
                Result = beamtalk_interface:findClass('Counter'),
                ?assertMatch({beamtalk_object, 'Counter class', _, _}, Result)
            end},
            {"binary metaclass tag resolves to metaclass object", fun() ->
                ClassObj = beamtalk_interface:findClass('Counter'),
                {beamtalk_object, _, _, ClassPid} = ClassObj,
                MetaObj = beamtalk_interface:findClass(<<"Counter class">>),
                ?assertEqual(
                    {beamtalk_object, 'Metaclass', beamtalk_metaclass_bt, ClassPid},
                    MetaObj
                )
            end},
            {"binary metaclass tag decode does not depend on tag interning", fun() ->
                %% Decode runs on the binary, so only the base class atom must
                %% exist — the full 'Counter class' tag atom need not be interned.
                ?assert(beamtalk_class_registry:is_class_name(<<"Counter class">>)),
                ?assertEqual(
                    <<"Counter">>,
                    beamtalk_class_registry:class_display_name(<<"Counter class">>)
                )
            end},
            {"metaclass tag with unknown base class returns nil", fun() ->
                ?assertEqual(nil, beamtalk_interface:findClass('NoSuchClassXYZ class'))
            end}
        ]
    end}.

%%====================================================================
%% Transient class-process resolution
%%====================================================================

class_object_for_pid_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"resolves a live class pid to a class object tuple", fun() ->
                Pid = beamtalk_class_registry:whereis_class('Integer'),
                ?assert(is_pid(Pid)),
                ?assertMatch(
                    {beamtalk_object, _, _, Pid},
                    beamtalk_interface:class_object_for_pid('Integer', Pid)
                )
            end},
            {"returns nil (not an exit) for a dead class process", fun() ->
                %% A class object process that exited between whereis_class/1
                %% returning its pid and the module_name/1 gen_server:call must
                %% surface as nil, not propagate an `erlang_exit` to callers
                %% such as SystemNavigation classesInPackage:.
                DeadPid = spawn(fun() -> ok end),
                MRef = erlang:monitor(process, DeadPid),
                receive
                    {'DOWN', MRef, process, DeadPid, _} -> ok
                after 1000 -> error(dead_pid_setup_timeout)
                end,
                ?assertEqual(
                    nil, beamtalk_interface:class_object_for_pid('Integer', DeadPid)
                )
            end}
        ]
    end}.

%%====================================================================
%% help: Tests
%%====================================================================

help_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns a String for known class by atom", fun() ->
                {beamtalk_string, Text} = beamtalk_interface:dispatch(
                    'help:', ['Counter'], fake_self()
                ),
                ?assert(byte_size(Text) > 0)
            end},
            {"returns a String for known class by binary name", fun() ->
                Result = beamtalk_interface:dispatch(
                    'help:', [<<"Counter">>], fake_self()
                ),
                ?assertMatch({beamtalk_string, _}, Result)
            end},
            {"help output includes class name", fun() ->
                {beamtalk_string, Text} = beamtalk_interface:dispatch(
                    'help:', ['Counter'], fake_self()
                ),
                ?assertNotEqual(nomatch, binary:match(Text, <<"Counter">>))
            end},
            {"unknown class raises class_not_found", fun() ->
                try
                    beamtalk_interface:dispatch(
                        'help:', ['NoSuchClassABC'], fake_self()
                    ),
                    ?assert(false)
                catch
                    error:#{error := Err} ->
                        ?assertEqual(class_not_found, Err#beamtalk_error.kind),
                        ?assertEqual('Beamtalk', Err#beamtalk_error.class)
                end
            end},
            {"invalid argument raises type_error", fun() ->
                try
                    beamtalk_interface:dispatch('help:', [42], fake_self()),
                    ?assert(false)
                catch
                    error:#{error := Err} ->
                        ?assertEqual(type_error, Err#beamtalk_error.kind)
                end
            end}
        ]
    end}.

%%====================================================================
%% help:selector: Tests
%%====================================================================

help_selector_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns a String for known class and selector", fun() ->
                {beamtalk_string, Text} = beamtalk_interface:dispatch(
                    'help:selector:', ['Counter', increment], fake_self()
                ),
                ?assertNotEqual(nomatch, binary:match(Text, <<"increment">>))
            end},
            {"unknown selector raises does_not_understand", fun() ->
                try
                    beamtalk_interface:dispatch(
                        'help:selector:', ['Counter', noSuchMethod], fake_self()
                    ),
                    ?assert(false)
                catch
                    error:#{error := Err} ->
                        ?assertEqual(does_not_understand, Err#beamtalk_error.kind)
                end
            end},
            {"unknown class raises class_not_found", fun() ->
                try
                    beamtalk_interface:dispatch(
                        'help:selector:', ['NoSuchClass', foo], fake_self()
                    ),
                    ?assert(false)
                catch
                    error:#{error := Err} ->
                        ?assertEqual(class_not_found, Err#beamtalk_error.kind)
                end
            end}
        ]
    end}.

%%====================================================================
%% version Tests
%%====================================================================

version_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns a String", fun() ->
                Result = beamtalk_interface:dispatch(version, [], fake_self()),
                ?assertMatch({beamtalk_string, _}, Result)
            end},
            {"returns non-empty String", fun() ->
                {beamtalk_string, Text} = beamtalk_interface:dispatch(
                    version, [], fake_self()
                ),
                ?assert(byte_size(Text) > 0)
            end},
            {"matches application vsn when app is loaded", fun() ->
                _ = application:load(beamtalk_runtime),
                Result = beamtalk_interface:dispatch(version, [], fake_self()),
                case application:get_key(beamtalk_runtime, vsn) of
                    {ok, Vsn} when is_list(Vsn) ->
                        ?assertEqual({beamtalk_string, list_to_binary(Vsn)}, Result);
                    {ok, Vsn} when is_binary(Vsn) ->
                        ?assertEqual({beamtalk_string, Vsn}, Result);
                    _ ->
                        ?assertEqual({beamtalk_string, <<"unknown">>}, Result)
                end
            end}
        ]
    end}.

%%====================================================================
%% Unknown Selector Tests
%%====================================================================

unknown_selector_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"unknown selector raises does_not_understand", fun() ->
                try
                    beamtalk_interface:dispatch(
                        unknownSelector, [], fake_self()
                    ),
                    ?assert(false)
                catch
                    error:#{error := Err} ->
                        ?assertEqual(does_not_understand, Err#beamtalk_error.kind),
                        ?assertEqual('Beamtalk', Err#beamtalk_error.class),
                        ?assertEqual(unknownSelector, Err#beamtalk_error.selector)
                end
            end}
        ]
    end}.
