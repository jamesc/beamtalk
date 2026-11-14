%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bootstrap_stub_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for the bootstrap stub modules `beamtalk_class_bt`,
`beamtalk_class_builder_bt` and `beamtalk_metaclass_bt`: `register_class/0`,
`dispatch/4` and `has_method/1`.

BT-4679 / BT-4806: the three stubs had the same tests and the same bootstrap
setup, differing only in module, class name, superclass and the selectors they
answer. This module runs them once per stub (formerly this file plus
`beamtalk_class_bt_tests`, `beamtalk_class_builder_bt_tests` and
`beamtalk_metaclass_bt_tests`). Each selector is a labelled row, so a failure
names the stub and the selector.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% {StubModule, ClassName, ExpectedSuperclass}
stubs() ->
    [
        {beamtalk_class_bt, 'Class', 'Object'},
        {beamtalk_class_builder_bt, 'ClassBuilder', 'Actor'},
        {beamtalk_metaclass_bt, 'Metaclass', 'Class'}
    ].

register_class_test_() ->
    {setup, fun setup_bootstrap/0, fun teardown_bootstrap/1, [
        {lists:flatten(io_lib:format("~p: ~s", [Class, Label])), Fun}
     || {Mod, Class, Super} <- stubs(),
        {Label, Fun} <- [
            {"register_class creates the class", fun() -> registered(Class) end},
            {"register_class is idempotent", fun() -> idempotent(Mod, Class) end},
            {"registered class has correct superclass", fun() -> superclass(Class, Super) end}
        ]
    ]}.

%% Start from a clean class registry: earlier modules that boot the real stdlib
%% leave `Class` registered under 'Behaviour', while these tests assert the
%% bootstrap-time registration (BT-4679: the original per-stub modules only passed
%% because of where they ran in the suite). Unwatched stop first so
%% beamtalk_class_monitor cannot resurrect the classes mid-teardown (BT-3815).
setup_bootstrap() ->
    beamtalk_test_class_teardown:stop_pg_class_processes(),
    beamtalk_test_boot:registry(),
    beamtalk_extensions:init(),
    case beamtalk_bootstrap:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    ok.

teardown_bootstrap(_) ->
    beamtalk_test_class_teardown:stop_pg_class_processes().

%% Bootstrap already registered the class; verify it is there.
registered(Class) ->
    Pid = beamtalk_class_registry:whereis_class(Class),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual(Class, beamtalk_object_class:class_name(Pid)).

%% Calling register_class again must not crash and the class must still be there.
idempotent(Mod, Class) ->
    ok = Mod:register_class(),
    Pid = beamtalk_class_registry:whereis_class(Class),
    ?assertNotEqual(undefined, Pid).

superclass(Class, Super) ->
    Pid = beamtalk_class_registry:whereis_class(Class),
    ?assertEqual(Super, beamtalk_object_class:superclass(Pid)).

%%% ============================================================================
%%% dispatch/4 and has_method/1 (BT-4806)
%%% ============================================================================


%% Per stub: the class name reported in errors, selectors answered by a reply
%% (`{Selector, Reply}`), selectors that must raise does_not_understand
%% (`{Selector, Args, State}`), and the selectors has_method/1 is checked on.
dispatch_stubs() ->
    [
        #{
            mod => beamtalk_class_bt,
            class => 'Class',
            replies => [],
            %% ADR 0038 Phase 1: `classBuilder` still raises DNU from dispatch/4 even though
            %% has_method/1 answers true for it (so `Class respondsTo: #classBuilder` is
            %% true); the implementation is deferred to Phase 2 (class_builder.bt).
            dnu => [
                {'classBuilder', [], #{}},
                {'classBuilder', [], #{value => 99, name => <<"test">>}},
                {'unknownMethod', [], #{}},
                {'foo:', [42], #{}},
                %% Phase 2 (class.bt) provides `name`; `superclass` is a class method here.
                {'name', [], #{}},
                {'superclass', [], #{}},
                {'anySelector', [], #{foo => bar, count => 42}}
            ],
            has_true => ['classBuilder'],
            has_false => ['unknownMethod', 'new', 'name', 'superclass', 'printString']
        },
        #{
            mod => beamtalk_class_builder_bt,
            class => 'ClassBuilder',
            replies => [],
            %% The Phase 2 ClassBuilder protocol is not answered by the Phase 1 stub.
            dnu => [
                {'anyMethod', [], #{}},
                {'name:', [<<"MyClass">>], #{}},
                {'superclass:', [self()], #{}},
                {'register', [], #{}},
                {'anything', [], #{value => 100, tag => <<"cb">>}}
            ],
            has_true => [],
            has_false => [
                'name:', 'superclass:', 'fields:', 'methods:', 'register', 'new', 'unknownSelector'
            ]
        },
        #{
            mod => beamtalk_metaclass_bt,
            class => 'Metaclass',
            replies => [
                {'isMeta', true, #{}},
                {'isClass', false, #{}},
                {'isMetaclass', true, #{}},
                %% the State map passes through unchanged
                {'isMeta', true, #{foo => bar, count => 42}}
            ],
            dnu => [
                {'unknownMethod', [], #{}},
                {'foo:', [42], #{}}
            ],
            has_true => ['isMeta', 'isClass', 'isMetaclass'],
            has_false => ['unknownMethod', 'new', 'name', 'superclass']
        }
    ].

row_label(Class, Parts) ->
    lists:flatten(io_lib:format("~p: ~s", [Class, Parts])).

dispatch_replies_test_() ->
    [
        {row_label(Class, io_lib:format("~p replies ~p", [Sel, Reply])), fun() ->
            ?assertEqual({reply, Reply, State}, Mod:dispatch(Sel, [], self(), State))
        end}
     || #{mod := Mod, class := Class, replies := Replies} <- dispatch_stubs(),
        {Sel, Reply, State} <- Replies
    ].

dispatch_unknown_selector_test_() ->
    [
        {row_label(Class, io_lib:format("~p raises does_not_understand", [Sel])), fun() ->
            {error, Error, RetState} = Mod:dispatch(Sel, Args, self(), State),
            ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
            ?assertEqual(Class, Error#beamtalk_error.class),
            ?assertEqual(Sel, Error#beamtalk_error.selector),
            ?assertEqual(State, RetState)
        end}
     || #{mod := Mod, class := Class, dnu := Dnu} <- dispatch_stubs(),
        {Sel, Args, State} <- Dnu
    ].

has_method_test_() ->
    [
        {row_label(Class, io_lib:format("has_method(~p) =:= ~p", [Sel, Expected])), fun() ->
            ?assertEqual(Expected, Mod:has_method(Sel))
        end}
     || #{mod := Mod, class := Class, has_true := True, has_false := False} <- dispatch_stubs(),
        {Sel, Expected} <- [{S, true} || S <- True] ++ [{S, false} || S <- False]
    ].
