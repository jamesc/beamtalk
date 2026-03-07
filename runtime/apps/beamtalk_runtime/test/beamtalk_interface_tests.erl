%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_interface module.
%%%
%%% Tests the Phase 2 dispatch/3 interface for BeamtalkInterface primitives:
%%% - allClasses selector
%%% - classNamed: selector
%%% - globals selector
%%% - help: selector
%%% - help:selector: selector
%%% - version selector
%%% - unknown selectors (does_not_understand)

-module(beamtalk_interface_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

%% A fake Self value — BeamtalkInterface primitives ignore Self entirely.
fake_self() ->
    {beamtalk_object, 'BeamtalkInterface class', 'bt@stdlib@beamtalk_interface', self()}.

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
    ensure_class_started('Counter', #{
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

ensure_class_started(ClassName, ClassInfo) ->
    case beamtalk_object_class:start_link(ClassName, ClassInfo) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

cleanup_test_classes() ->
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,
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
    lists:foreach(
        fun(Name) ->
            catch unregister(Name)
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
            {"all entries are atoms", fun() ->
                Classes = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                lists:foreach(fun(C) -> ?assert(is_atom(C)) end, Classes)
            end},
            {"includes registered test classes", fun() ->
                Classes = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                ?assert(lists:member('Object', Classes)),
                ?assert(lists:member('Counter', Classes))
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
                    {error, #beamtalk_error{kind = type_error, class = 'BeamtalkInterface'}}, Result
                )
            end}
        ]
    end}.

%%====================================================================
%% globals Tests
%%====================================================================

globals_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns a map", fun() ->
                Result = beamtalk_interface:dispatch(globals, [], fake_self()),
                ?assert(is_map(Result))
            end},
            {"map is non-empty", fun() ->
                Result = beamtalk_interface:dispatch(globals, [], fake_self()),
                ?assert(map_size(Result) > 0)
            end},
            {"keys are atoms and values are beamtalk_object tuples", fun() ->
                Result = beamtalk_interface:dispatch(globals, [], fake_self()),
                maps:foreach(
                    fun(Key, Val) ->
                        ?assert(is_atom(Key)),
                        ?assertMatch({beamtalk_object, _, _, _}, Val)
                    end,
                    Result
                )
            end},
            {"includes registered classes", fun() ->
                Result = beamtalk_interface:dispatch(globals, [], fake_self()),
                ?assert(maps:is_key('Counter', Result)),
                ?assert(maps:is_key('Object', Result))
            end}
        ]
    end}.

%%====================================================================
%% help: Tests
%%====================================================================

help_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"returns binary for known class by atom", fun() ->
                Result = beamtalk_interface:dispatch(
                    'help:', ['Counter'], fake_self()
                ),
                ?assert(is_binary(Result)),
                ?assert(byte_size(Result) > 0)
            end},
            {"returns binary for known class by binary name", fun() ->
                Result = beamtalk_interface:dispatch(
                    'help:', [<<"Counter">>], fake_self()
                ),
                ?assert(is_binary(Result))
            end},
            {"help output includes class name", fun() ->
                Result = beamtalk_interface:dispatch(
                    'help:', ['Counter'], fake_self()
                ),
                ?assertNotEqual(nomatch, binary:match(Result, <<"Counter">>))
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
                        ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
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
            {"returns binary for known class and selector", fun() ->
                Result = beamtalk_interface:dispatch(
                    'help:selector:', ['Counter', increment], fake_self()
                ),
                ?assert(is_binary(Result)),
                ?assertNotEqual(nomatch, binary:match(Result, <<"increment">>))
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
            {"returns a binary", fun() ->
                Result = beamtalk_interface:dispatch(version, [], fake_self()),
                ?assert(is_binary(Result))
            end},
            {"returns non-empty binary", fun() ->
                Result = beamtalk_interface:dispatch(version, [], fake_self()),
                ?assert(byte_size(Result) > 0)
            end},
            {"matches application vsn when app is loaded", fun() ->
                _ = application:load(beamtalk_runtime),
                Result = beamtalk_interface:dispatch(version, [], fake_self()),
                case application:get_key(beamtalk_runtime, vsn) of
                    {ok, Vsn} ->
                        ?assertEqual(list_to_binary(Vsn), Result);
                    _ ->
                        ?assertEqual(<<"unknown">>, Result)
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
                        ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class),
                        ?assertEqual(unknownSelector, Err#beamtalk_error.selector)
                end
            end}
        ]
    end}.
