%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for BT-102: Method combinations (before/after daemons).
%%%
%%% Tests the before/after method combination mechanism inspired by LFE Flavors.
%%% Verifies:
%%% - Registration and removal of before/after daemons on class objects
%%% - Before methods run in superclass -> subclass order
%%% - After methods run in subclass -> superclass order
%%% - Before methods can short-circuit (skip primary method)
%%% - After methods receive and can transform the primary result
%%% - Method combinations integrate with dispatch correctly
%%% - No overhead when no daemons are registered (fast path)

-module(beamtalk_method_combinations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Test Setup/Teardown
%%% ============================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    ok.

teardown(_) ->
    ok.

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

method_combinations_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"add and get before method", fun test_add_before_method/0},
            {"add and get after method", fun test_add_after_method/0},
            {"remove before method", fun test_remove_before_method/0},
            {"remove after method", fun test_remove_after_method/0},
            {"get empty before methods", fun test_get_empty_before/0},
            {"get empty after methods", fun test_get_empty_after/0},
            {"multiple before methods on same selector", fun test_multiple_before/0},
            {"multiple after methods on same selector", fun test_multiple_after/0},
            {"before method short-circuits dispatch", fun test_before_short_circuit/0},
            {"after method receives and transforms result", fun test_after_transforms_result/0},
            {"before/after with hierarchy ordering", fun test_hierarchy_ordering/0},
            {"dispatch fast path with no daemons", fun test_dispatch_no_daemons/0},
            {"before method error is caught and dispatch continues",
                fun test_before_method_error_resilience/0},
            {"after method error is caught and dispatch continues",
                fun test_after_method_error_resilience/0},
            {"collect_before_methods on unknown class returns empty",
                fun test_collect_before_unknown_class/0},
            {"collect_after_methods on unknown class returns empty",
                fun test_collect_after_unknown_class/0}
        ]
    end}.

%%% ============================================================================
%%% Test Cases — Registration
%%% ============================================================================

test_add_before_method() ->
    {ok, Pid} = start_test_class('BeforeTestClass1', none),
    try
        BeforeFun = fun(_Self, State) -> {ok, State} end,
        {ok, Id} = beamtalk_object_class:add_before(Pid, increment, BeforeFun),
        ?assert(is_reference(Id)),
        Daemons = beamtalk_object_class:get_before_methods(Pid, increment),
        ?assertEqual(1, length(Daemons)),
        [{RetId, RetFun}] = Daemons,
        ?assertEqual(Id, RetId),
        ?assertEqual(BeforeFun, RetFun)
    after
        gen_server:stop(Pid)
    end.

test_add_after_method() ->
    {ok, Pid} = start_test_class('AfterTestClass1', none),
    try
        AfterFun = fun(_Self, Result, State) -> {ok, Result, State} end,
        {ok, Id} = beamtalk_object_class:add_after(Pid, increment, AfterFun),
        ?assert(is_reference(Id)),
        Daemons = beamtalk_object_class:get_after_methods(Pid, increment),
        ?assertEqual(1, length(Daemons)),
        [{RetId, RetFun}] = Daemons,
        ?assertEqual(Id, RetId),
        ?assertEqual(AfterFun, RetFun)
    after
        gen_server:stop(Pid)
    end.

test_remove_before_method() ->
    {ok, Pid} = start_test_class('RemoveBeforeTestClass', none),
    try
        BeforeFun = fun(_Self, State) -> {ok, State} end,
        {ok, _Id} = beamtalk_object_class:add_before(Pid, increment, BeforeFun),
        ?assertEqual(1, length(beamtalk_object_class:get_before_methods(Pid, increment))),
        ok = beamtalk_object_class:remove_before(Pid, increment),
        ?assertEqual([], beamtalk_object_class:get_before_methods(Pid, increment))
    after
        gen_server:stop(Pid)
    end.

test_remove_after_method() ->
    {ok, Pid} = start_test_class('RemoveAfterTestClass', none),
    try
        AfterFun = fun(_Self, Result, State) -> {ok, Result, State} end,
        {ok, _Id} = beamtalk_object_class:add_after(Pid, increment, AfterFun),
        ?assertEqual(1, length(beamtalk_object_class:get_after_methods(Pid, increment))),
        ok = beamtalk_object_class:remove_after(Pid, increment),
        ?assertEqual([], beamtalk_object_class:get_after_methods(Pid, increment))
    after
        gen_server:stop(Pid)
    end.

test_get_empty_before() ->
    {ok, Pid} = start_test_class('EmptyBeforeTestClass', none),
    try
        ?assertEqual([], beamtalk_object_class:get_before_methods(Pid, noSuchSelector))
    after
        gen_server:stop(Pid)
    end.

test_get_empty_after() ->
    {ok, Pid} = start_test_class('EmptyAfterTestClass', none),
    try
        ?assertEqual([], beamtalk_object_class:get_after_methods(Pid, noSuchSelector))
    after
        gen_server:stop(Pid)
    end.

test_multiple_before() ->
    {ok, Pid} = start_test_class('MultBeforeTestClass', none),
    try
        Fun1 = fun(_Self, State) -> {ok, State} end,
        Fun2 = fun(_Self, State) -> {ok, State} end,
        {ok, Id1} = beamtalk_object_class:add_before(Pid, increment, Fun1),
        {ok, Id2} = beamtalk_object_class:add_before(Pid, increment, Fun2),
        ?assertNotEqual(Id1, Id2),
        Daemons = beamtalk_object_class:get_before_methods(Pid, increment),
        ?assertEqual(2, length(Daemons)),
        %% Order is preserved (first added = first in list)
        [{RetId1, _}, {RetId2, _}] = Daemons,
        ?assertEqual(Id1, RetId1),
        ?assertEqual(Id2, RetId2)
    after
        gen_server:stop(Pid)
    end.

test_multiple_after() ->
    {ok, Pid} = start_test_class('MultAfterTestClass', none),
    try
        Fun1 = fun(_Self, Result, State) -> {ok, Result, State} end,
        Fun2 = fun(_Self, Result, State) -> {ok, Result, State} end,
        {ok, Id1} = beamtalk_object_class:add_after(Pid, increment, Fun1),
        {ok, Id2} = beamtalk_object_class:add_after(Pid, increment, Fun2),
        ?assertNotEqual(Id1, Id2),
        Daemons = beamtalk_object_class:get_after_methods(Pid, increment),
        ?assertEqual(2, length(Daemons))
    after
        gen_server:stop(Pid)
    end.

%%% ============================================================================
%%% Test Cases — Dispatch Integration
%%% ============================================================================

test_before_short_circuit() ->
    %% Create a class with a method, add a before daemon that short-circuits.
    {ok, Pid} = start_test_class_with_dispatch('ShortCircuitClass', none),
    try
        %% Register a before daemon that always short-circuits with 999.
        ShortCircuitFun = fun(_Self, State) ->
            {short_circuit, 999, State}
        end,
        {ok, _Id} = beamtalk_object_class:add_before(Pid, getValue, ShortCircuitFun),

        %% Dispatch should return 999 instead of the real getValue result (42).
        State = #{'$beamtalk_class' => 'ShortCircuitClass', value => 42},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'ShortCircuitClass'),
        ?assertMatch({reply, 999, _}, Result)
    after
        gen_server:stop(Pid)
    end.

test_after_transforms_result() ->
    %% Create a class with a method, add an after daemon that transforms the result.
    {ok, Pid} = start_test_class_with_dispatch('AfterTransformClass', none),
    try
        %% Register an after daemon that doubles the result.
        DoubleFun = fun(_Self, Result, State) ->
            {ok, Result * 2, State}
        end,
        {ok, _Id} = beamtalk_object_class:add_after(Pid, getValue, DoubleFun),

        State = #{'$beamtalk_class' => 'AfterTransformClass', value => 21},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'AfterTransformClass'),
        %% Primary returns 21, after daemon doubles it to 42.
        ?assertMatch({reply, 42, _}, Result)
    after
        gen_server:stop(Pid)
    end.

test_hierarchy_ordering() ->
    %% Create a parent class and a child class, add before/after daemons at both levels.
    %% Verify ADR 0006 ordering: before = superclass->subclass, after = subclass->superclass.
    {ok, ParentPid} = start_test_class_with_dispatch('HierParentClass', none),
    {ok, ChildPid} = start_test_class_with_dispatch('HierChildClass', 'HierParentClass'),
    try
        %% Use a shared process dictionary to track call order.
        Self0 = self(),

        %% Before daemons — parent before should run first (superclass -> subclass).
        ParentBefore = fun(_Self, State) ->
            Self0 ! {before, parent},
            {ok, State}
        end,
        ChildBefore = fun(_Self, State) ->
            Self0 ! {before, child},
            {ok, State}
        end,
        {ok, _} = beamtalk_object_class:add_before(ParentPid, getValue, ParentBefore),
        {ok, _} = beamtalk_object_class:add_before(ChildPid, getValue, ChildBefore),

        %% After daemons — child after should run first (subclass -> superclass).
        ParentAfter = fun(_Self, Result, State) ->
            Self0 ! {after_, parent},
            {ok, Result, State}
        end,
        ChildAfter = fun(_Self, Result, State) ->
            Self0 ! {after_, child},
            {ok, Result, State}
        end,
        {ok, _} = beamtalk_object_class:add_after(ParentPid, getValue, ParentAfter),
        {ok, _} = beamtalk_object_class:add_after(ChildPid, getValue, ChildAfter),

        %% Dispatch from child class (primary method is on parent).
        State = #{'$beamtalk_class' => 'HierChildClass', value => 10},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'HierChildClass'),
        ?assertMatch({reply, 10, _}, Result),

        %% Collect messages and verify order.
        Messages = collect_messages(),
        %% Before: parent first, then child (superclass -> subclass)
        %% After: child first, then parent (subclass -> superclass)
        ?assertEqual(
            [{before, parent}, {before, child}, {after_, child}, {after_, parent}],
            Messages
        )
    after
        gen_server:stop(ChildPid),
        gen_server:stop(ParentPid)
    end.

test_dispatch_no_daemons() ->
    %% Verify the fast path: dispatch works correctly without any daemons.
    {ok, Pid} = start_test_class_with_dispatch('NoDaemonsClass', none),
    try
        State = #{'$beamtalk_class' => 'NoDaemonsClass', value => 7},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'NoDaemonsClass'),
        ?assertMatch({reply, 7, _}, Result)
    after
        gen_server:stop(Pid)
    end.

test_before_method_error_resilience() ->
    %% A before daemon that crashes should not prevent dispatch.
    {ok, Pid} = start_test_class_with_dispatch('BeforeErrorClass', none),
    try
        CrashFun = fun(_Self, _State) ->
            error(before_daemon_crash)
        end,
        {ok, _} = beamtalk_object_class:add_before(Pid, getValue, CrashFun),

        State = #{'$beamtalk_class' => 'BeforeErrorClass', value => 5},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'BeforeErrorClass'),
        %% Dispatch should still succeed despite the crashing before daemon.
        ?assertMatch({reply, 5, _}, Result)
    after
        gen_server:stop(Pid)
    end.

test_after_method_error_resilience() ->
    %% An after daemon that crashes should not prevent the result from being returned.
    {ok, Pid} = start_test_class_with_dispatch('AfterErrorClass', none),
    try
        CrashFun = fun(_Self, _Result, _State) ->
            error(after_daemon_crash)
        end,
        {ok, _} = beamtalk_object_class:add_after(Pid, getValue, CrashFun),

        State = #{'$beamtalk_class' => 'AfterErrorClass', value => 3},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(getValue, [], Self, State, 'AfterErrorClass'),
        %% Dispatch should still succeed despite the crashing after daemon.
        ?assertMatch({reply, 3, _}, Result)
    after
        gen_server:stop(Pid)
    end.

%%% ============================================================================
%%% Test Cases — Collection from Unknown Classes
%%% ============================================================================

test_collect_before_unknown_class() ->
    ?assertEqual([], beamtalk_dispatch:collect_before_methods(increment, 'NonExistentClass42')).

test_collect_after_unknown_class() ->
    ?assertEqual([], beamtalk_dispatch:collect_after_methods(increment, 'NonExistentClass42')).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @private Start a simple test class with no methods (for registration tests).
start_test_class(ClassName, Superclass) ->
    beamtalk_object_class:start_link(ClassName, #{
        superclass => Superclass,
        instance_methods => #{},
        instance_variables => []
    }).

%% @private Start a test class with a dispatch module that has a getValue method.
%%
%% Compiles a minimal dispatch module on the fly and registers the class.
start_test_class_with_dispatch(ClassName, Superclass) ->
    %% Generate a unique module name for each class to avoid collisions.
    ModName = list_to_atom("bt_test_mc_" ++ atom_to_list(ClassName)),

    %% Compile a minimal dispatch module with getValue returning State.value.
    Forms = [
        {attribute, 1, module, ModName},
        {attribute, 2, export, [{dispatch, 4}]},
        %% dispatch(getValue, [], _Self, State) -> {reply, maps:get(value, State), State};
        %% dispatch(_, _, _, State) -> {error, not_found, State}.
        {function, 3, dispatch, 4, [
            {clause, 3, [{atom, 3, getValue}, {nil, 3}, {var, 3, '_Self'}, {var, 3, 'State'}], [], [
                    {tuple, 3, [
                        {atom, 3, reply},
                        {call, 3, {remote, 3, {atom, 3, maps}, {atom, 3, get}}, [
                            {atom, 3, value}, {var, 3, 'State'}
                        ]},
                        {var, 3, 'State'}
                    ]}
                ]},
            {clause, 4, [{var, 4, '_Sel'}, {var, 4, '_Args'}, {var, 4, '_Self'}, {var, 4, 'State'}],
                [], [
                    {tuple, 4, [
                        {atom, 4, error},
                        {atom, 4, not_found},
                        {var, 4, 'State'}
                    ]}
                ]}
        ]}
    ],
    {ok, ModName, Bin} = compile:forms(Forms),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".beam", Bin),

    beamtalk_object_class:start_link(ClassName, #{
        module => ModName,
        superclass => Superclass,
        instance_methods => #{getValue => #{arity => 0}},
        instance_variables => [value]
    }).

%% @private Collect all messages from the mailbox.
collect_messages() ->
    collect_messages([]).

collect_messages(Acc) ->
    receive
        Msg -> collect_messages(Acc ++ [Msg])
    after 100 -> Acc
    end.
