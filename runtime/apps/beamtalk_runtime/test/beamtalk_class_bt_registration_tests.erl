%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_bt_registration_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for `register_class/0` of the bootstrap stub modules
`beamtalk_class_bt`, `beamtalk_class_builder_bt` and `beamtalk_metaclass_bt`.

BT-4679: the three stubs had the same three registration tests and the same
bootstrap setup, differing only in module, class name and superclass. This
module runs them once per stub. Dispatch and `has_method/1` behaviour differs
per stub and stays in `beamtalk_class_bt_tests`, `beamtalk_class_builder_bt_tests`
and `beamtalk_metaclass_bt_tests`.
""".

-include_lib("eunit/include/eunit.hrl").

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
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
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
