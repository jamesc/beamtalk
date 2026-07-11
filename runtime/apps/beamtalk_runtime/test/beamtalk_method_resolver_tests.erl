%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_method_resolver_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_method_resolver module (BT-623).

Tests method resolution from various class reference formats:
pid, atom, class object tuple, and invalid inputs.
""".

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

resolver_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"resolve with class pid returns method", fun test_resolve_with_pid/0},
            {"resolve with class pid returns nil for missing method",
                fun test_resolve_with_pid_missing/0},
            {"resolve with class name atom", fun test_resolve_with_atom/0},
            {"resolve with unknown class name raises error", fun test_resolve_unknown_class/0},
            {"resolve with class object tuple", fun test_resolve_with_class_tuple/0},
            {"resolve with instance tuple raises type_error",
                fun test_resolve_with_instance_tuple/0},
            {"resolve with non-atom/non-pid raises type_error",
                fun test_resolve_with_invalid_ref/0},
            {"resolve with integer raises type_error", fun test_resolve_with_integer/0},
            {"resolve with metaclass receiver looks up class-side methods",
                fun test_resolve_with_metaclass/0},
            {"resolve with metaclass receiver returns nil for missing class method",
                fun test_resolve_with_metaclass_missing/0},
            {"resolve finds an inherited method at the MAX_HIERARCHY_DEPTH boundary",
                fun test_resolve_at_depth_boundary_found/0},
            {"resolve returns nil for a method one level beyond MAX_HIERARCHY_DEPTH",
                fun test_resolve_beyond_depth_boundary_returns_nil/0}
        ]
    end}.

%%====================================================================
%% Test Cases
%%====================================================================

test_resolve_with_pid() ->
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    %% Integer has '+' method — CompiledMethod uses __selector__
    Result = beamtalk_method_resolver:resolve(IntegerPid, '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_with_pid_missing() ->
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    Result = beamtalk_method_resolver:resolve(IntegerPid, 'totallyFakeMethod'),
    ?assertEqual(nil, Result).

test_resolve_with_atom() ->
    Result = beamtalk_method_resolver:resolve('Integer', '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_unknown_class() ->
    %% beamtalk_error:raise wraps the error in a tagged map
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_method_resolver:resolve('NonExistentClass', '+')
    ).

test_resolve_with_class_tuple() ->
    %% Class objects have 'ClassName class' as their tag
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    ClassObj = #beamtalk_object{
        class = 'Integer class', class_mod = beamtalk_object_class, pid = IntegerPid
    },
    Result = beamtalk_method_resolver:resolve(ClassObj, '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_with_instance_tuple() ->
    %% Instance tuples (non-class tag) should raise type_error
    Obj = #beamtalk_object{class = 'Integer', class_mod = beamtalk_integer, pid = self()},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve(Obj, '+')
    ).

test_resolve_with_invalid_ref() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve("not_a_class", '+')
    ).

test_resolve_with_integer() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve(42, '+')
    ).

%% BT-2195: Metaclass receivers (class = 'Metaclass') route to the
%% class-side method dictionary instead of the instance-side one. A
%% `SystemNavigation class >> #default` lookup must return a CompiledMethod
%% map whose `__selector__` is `default` — that selector is defined on the
%% class side of SystemNavigation, not the instance side.
test_resolve_with_metaclass() ->
    NavPid = beamtalk_class_registry:whereis_class('SystemNavigation'),
    ?assertNotEqual(undefined, NavPid),
    MetaObj = #beamtalk_object{
        class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = NavPid
    },
    Result = beamtalk_method_resolver:resolve(MetaObj, 'default'),
    ?assertMatch(#{'__selector__' := 'default'}, Result).

test_resolve_with_metaclass_missing() ->
    NavPid = beamtalk_class_registry:whereis_class('SystemNavigation'),
    ?assertNotEqual(undefined, NavPid),
    MetaObj = #beamtalk_object{
        class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = NavPid
    },
    Result = beamtalk_method_resolver:resolve(
        MetaObj, 'classSideSelectorThatDoesNotExistXyzzy'
    ),
    ?assertEqual(nil, Result).

%%====================================================================
%% BT-2786: MAX_HIERARCHY_DEPTH boundary tests
%%
%% resolve_with_hierarchy/2 checks the receiver's own class "for free"
%% (outside the depth-counted walk), then walks up to ?MAX_HIERARCHY_DEPTH
%% superclasses above it — mirroring beamtalk_class_dispatch's split. These
%% tests pin that boundary: a chain of 22 classes (base + 21 superclasses)
%% has an inherited method reachable at the 21st superclass; a 23-class
%% chain's 22nd superclass is one level beyond the budget and must resolve
%% to nil instead of silently truncating the walk.
%%====================================================================

%% Starts a chain of NumClasses classes (class 1 is the base/leaf, class N
%% has superclass `none`) and installs a trivial compiled method named
%% `Selector` only on the last class in the chain. Returns {BaseClassName, Pids}.
start_superclass_chain(NumClasses, Selector) ->
    % elp:fixme W0023 intentional atom creation
    ClassNames = [
        list_to_atom("ResolverDepthTestClass" ++ integer_to_list(I))
     || I <- lists:seq(1, NumClasses)
    ],
    Pids = lists:foldl(
        fun(I, AccPids) ->
            ClassName = lists:nth(I, ClassNames),
            Super =
                if
                    I =:= NumClasses -> none;
                    true -> lists:nth(I + 1, ClassNames)
                end,
            InstanceMethods =
                case I of
                    NumClasses ->
                        #{Selector => #{arity => 0, block => fun() -> ok end}};
                    _ ->
                        #{}
                end,
            {ok, Pid} = beamtalk_object_class:start_link(ClassName, #{
                superclass => Super,
                instance_methods => InstanceMethods,
                instance_variables => []
            }),
            AccPids ++ [Pid]
        end,
        [],
        lists:seq(1, NumClasses)
    ),
    {hd(ClassNames), Pids}.

test_resolve_at_depth_boundary_found() ->
    Selector = resolverDepthBoundaryMethod,
    {BaseClassName, Pids} = start_superclass_chain(22, Selector),
    try
        BasePid = beamtalk_class_registry:whereis_class(BaseClassName),
        Result = beamtalk_method_resolver:resolve(BasePid, Selector),
        ?assertMatch(#{'__selector__' := Selector}, Result)
    after
        lists:foreach(fun gen_server:stop/1, Pids)
    end.

test_resolve_beyond_depth_boundary_returns_nil() ->
    Selector = resolverBeyondDepthMethod,
    {BaseClassName, Pids} = start_superclass_chain(23, Selector),
    try
        BasePid = beamtalk_class_registry:whereis_class(BaseClassName),
        Result = beamtalk_method_resolver:resolve(BasePid, Selector),
        ?assertEqual(nil, Result)
    after
        lists:foreach(fun gen_server:stop/1, Pids)
    end.
