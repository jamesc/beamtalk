%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_class_instantiation module (BT-623).
%%%
%%% Tests instance creation logic including spawn/new protocols,
%%% constructibility checks, abstract class errors, and dynamic
%%% method validation.

-module(beamtalk_class_instantiation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    ok.

teardown(_) ->
    ok.

instantiation_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            %% handle_spawn tests
            {"spawn on abstract class returns error", fun test_spawn_abstract_class/0},
            {"spawn with too many args returns type_error", fun test_spawn_too_many_args/0},
            {"spawn with no args succeeds", fun test_spawn_no_args/0},
            {"spawn with one arg (spawnWith:) succeeds", fun test_spawn_with_arg/0},
            %% handle_new tests
            {"new compiled class delegates to module", fun test_new_compiled/0},
            {"new compiled non-constructible class", fun test_new_compiled_non_constructible/0},
            {"new compiled with map and new/1 exported", fun test_new_compiled_with_map/0},
            {"new compiled with multiple args and constructible raises",
                fun test_new_compiled_multi_args_constructible/0},
            %% ensure_is_constructible tests
            {"ensure_is_constructible returns cached value", fun test_ensure_cached/0},
            {"ensure_is_constructible computes when undefined", fun test_ensure_computes/0},
            %% compute_is_constructible tests
            {"abstract class is not constructible", fun test_compute_abstract/0},
            {"actor class (has spawn/0) is not constructible", fun test_compute_actor/0},
            %% abstract_class_error tests
            {"abstract_class_error returns structured error", fun test_abstract_error_structure/0}
        ]
    end}.

%%====================================================================
%% handle_spawn tests
%%====================================================================

test_spawn_abstract_class() ->
    Result = beamtalk_class_instantiation:handle_spawn([], 'AbstractTest', undefined, true),
    ?assertMatch(
        {error, #beamtalk_error{kind = instantiation_error, class = 'AbstractTest'}}, Result
    ).

test_spawn_too_many_args() ->
    %% Ensure Counter is loaded for spawn
    ok = ensure_counter_loaded(),
    Result = beamtalk_class_instantiation:handle_spawn(
        [arg1, arg2], 'Counter', 'bt@counter', false
    ),
    %% beamtalk_error:raise wraps the error in a tagged map
    ?assertMatch(
        {error, #{
            '$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Counter'}
        }},
        Result
    ).

test_spawn_no_args() ->
    ok = ensure_counter_loaded(),
    Result = beamtalk_class_instantiation:handle_spawn([], 'Counter', 'bt@counter', false),
    ?assertMatch({ok, #beamtalk_object{class = 'Counter'}}, Result),
    {ok, Obj} = Result,
    gen_server:stop(Obj#beamtalk_object.pid).

test_spawn_with_arg() ->
    ok = ensure_counter_loaded(),
    InitArgs = #{value => 10},
    Result = beamtalk_class_instantiation:handle_spawn([InitArgs], 'Counter', 'bt@counter', false),
    ?assertMatch({ok, #beamtalk_object{class = 'Counter'}}, Result),
    {ok, Obj} = Result,
    gen_server:stop(Obj#beamtalk_object.pid).

%%====================================================================
%% handle_new tests (dynamic classes)
%%====================================================================

%%====================================================================
%% handle_new tests (compiled classes)
%%====================================================================

test_new_compiled() ->
    %% Use Dictionary which has new/0
    code:ensure_loaded('bt@stdlib@dictionary'),
    Result = beamtalk_class_instantiation:handle_new(
        [], 'Dictionary', 'bt@stdlib@dictionary', undefined
    ),
    ?assertMatch({ok, _, _}, Result),
    {ok, Obj, _} = Result,
    cleanup_if_process(Obj).

test_new_compiled_non_constructible() ->
    %% Integer's new/0 raises instantiation_error
    code:ensure_loaded(beamtalk_integer),
    Result = beamtalk_class_instantiation:handle_new(
        [], 'Integer', beamtalk_integer, undefined
    ),
    ?assertMatch({error, _, _}, Result).

test_new_compiled_with_map() ->
    %% Dictionary supports new/1 with a map
    code:ensure_loaded('bt@stdlib@dictionary'),
    Result = beamtalk_class_instantiation:handle_new(
        [#{}], 'Dictionary', 'bt@stdlib@dictionary', undefined
    ),
    ?assertMatch({ok, _, _}, Result),
    {ok, Obj, _} = Result,
    cleanup_if_process(Obj).

test_new_compiled_multi_args_constructible() ->
    %% Compiled class that is constructible but gets multiple args → type_error
    %% Use Dictionary (constructible) with multiple args
    code:ensure_loaded('bt@stdlib@dictionary'),
    Result = beamtalk_class_instantiation:handle_new(
        [arg1, arg2], 'Dictionary', 'bt@stdlib@dictionary', true
    ),
    %% beamtalk_error:raise wraps the error
    ?assertMatch(
        {error, #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}}, _}, Result
    ).

%%====================================================================
%% ensure_is_constructible tests
%%====================================================================

test_ensure_cached() ->
    ?assertEqual(true, beamtalk_class_instantiation:ensure_is_constructible(true, some_mod, false)),
    ?assertEqual(
        false, beamtalk_class_instantiation:ensure_is_constructible(false, some_mod, false)
    ).

test_ensure_computes() ->
    %% undefined triggers computation — abstract class → false
    ?assertEqual(
        false, beamtalk_class_instantiation:ensure_is_constructible(undefined, some_mod, true)
    ).

%%====================================================================
%% compute_is_constructible tests
%%====================================================================

test_compute_abstract() ->
    ?assertEqual(false, beamtalk_class_instantiation:compute_is_constructible(any_module, true)).

test_compute_actor() ->
    %% Counter module has spawn/0 → not constructible
    ok = ensure_counter_loaded(),
    ?assertEqual(false, beamtalk_class_instantiation:compute_is_constructible('bt@counter', false)).

%%====================================================================
%% abstract_class_error tests
%%====================================================================

test_abstract_error_structure() ->
    Error = beamtalk_class_instantiation:abstract_class_error('TestClass', spawn),
    ?assertMatch(
        #beamtalk_error{
            kind = instantiation_error,
            class = 'TestClass',
            selector = spawn
        },
        Error
    ),
    ?assert(is_binary(Error#beamtalk_error.hint)).

%%====================================================================
%% Helpers
%%====================================================================

ensure_counter_loaded() ->
    case beamtalk_class_registry:whereis_class('Counter') of
        undefined ->
            case code:ensure_loaded('bt@counter') of
                {module, 'bt@counter'} ->
                    case erlang:function_exported('bt@counter', register_class, 0) of
                        true ->
                            'bt@counter':register_class(),
                            ok;
                        false ->
                            error(counter_no_register_function)
                    end;
                {error, Reason} ->
                    error({counter_module_not_found, Reason})
            end;
        _Pid ->
            ok
    end.

%% Stop process if result is a beamtalk_object with a pid
cleanup_if_process(#beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    gen_server:stop(Pid);
cleanup_if_process(_) ->
    ok.
