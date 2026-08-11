%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hierarchy_docs_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_hierarchy_docs (coverage improvement, BT-3087).

Covers all four public functions:

- `metaclass_method_doc/1` — pure pattern match, all four clauses including
  the previously uncovered `<<"spawnWith:">>` clause.
- `find_defining_class/2` — the root-class (no superclass) and
  unregistered-superclass terminal paths that were not reachable by the
  existing callers' integration-level tests.
- `find_defining_class_method/2` — same edge paths for the class-side walk.
- `collect_flattened_methods/2` — the root-class path that terminates on
  `superclass => none`.

The `metaclass_method_doc/1` tests run without any runtime process.
The hierarchy-walking tests start minimal class processes via
`beamtalk_object_class:start/2` in the EUnit fixture setup and stop them
in teardown.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% metaclass_method_doc/1 — pure pattern match, no runtime needed
%%% ============================================================================

metaclass_method_doc_new_test() ->
    ?assertEqual(
        {ok, <<"Create a new instance of the class.">>},
        beamtalk_hierarchy_docs:metaclass_method_doc(<<"new">>)
    ).

metaclass_method_doc_spawn_test() ->
    ?assertEqual(
        {ok, <<"Create a new actor instance. Returns an actor reference.">>},
        beamtalk_hierarchy_docs:metaclass_method_doc(<<"spawn">>)
    ).

metaclass_method_doc_spawn_with_test() ->
    ?assertEqual(
        {ok, <<"Create a new actor with initial state from a Dictionary.">>},
        beamtalk_hierarchy_docs:metaclass_method_doc(<<"spawnWith:">>)
    ).

metaclass_method_doc_unknown_test() ->
    ?assertEqual(not_found, beamtalk_hierarchy_docs:metaclass_method_doc(<<"unknownMethod">>)).

metaclass_method_doc_empty_binary_test() ->
    ?assertEqual(not_found, beamtalk_hierarchy_docs:metaclass_method_doc(<<"">>)).

%%% ============================================================================
%%% Hierarchy-walking tests — require a running class gen_server
%%% ============================================================================

%% Class names unique to this test module to avoid pg/registry collisions.
-define(ROOT_A, 'BT3087HierarchyDocsRootA').
-define(GHOST_B, 'BT3087HierarchyDocsGhostB').
-define(ROOT_C, 'BT3087HierarchyDocsRootC').
-define(GHOST_D, 'BT3087HierarchyDocsGhostD').
-define(ROOT_E, 'BT3087HierarchyDocsRootE').
-define(ROOT_F, 'BT3087HierarchyDocsRootF').
%% Superclass names that are deliberately never registered:
-define(GHOST_SUPER_1, 'BT3087HierarchyDocsNeverRegistered1').
-define(GHOST_SUPER_2, 'BT3087HierarchyDocsNeverRegistered2').

hierarchy_docs_test_() ->
    {setup, fun setup/0, fun teardown/1, fun({PidA, PidB, PidC, PidD, PidE, PidF}) ->
        [
            {"find_defining_class: root class (no superclass) returns own name", fun() ->
                ?assertEqual(
                    ?ROOT_A,
                    beamtalk_hierarchy_docs:find_defining_class(PidA, undefinedSel)
                )
            end},
            {"find_defining_class: unregistered superclass terminates with own name", fun() ->
                ?assertEqual(
                    ?GHOST_B,
                    beamtalk_hierarchy_docs:find_defining_class(PidB, undefinedSel)
                )
            end},
            {"find_defining_class_method: root class returns own name", fun() ->
                ?assertEqual(
                    ?ROOT_C,
                    beamtalk_hierarchy_docs:find_defining_class_method(
                        PidC, undefinedClassSel
                    )
                )
            end},
            {"find_defining_class_method: unregistered superclass terminates with own name",
                fun() ->
                    ?assertEqual(
                        ?GHOST_D,
                        beamtalk_hierarchy_docs:find_defining_class_method(
                            PidD, undefinedClassSel
                        )
                    )
                end},
            {"collect_flattened_methods: root class with no methods returns empty map", fun() ->
                ?assertEqual(
                    #{},
                    beamtalk_hierarchy_docs:collect_flattened_methods(?ROOT_E, PidE)
                )
            end},
            {"collect_flattened_methods: root class methods tagged with defining class", fun() ->
                Result = beamtalk_hierarchy_docs:collect_flattened_methods(?ROOT_F, PidF),
                ?assertMatch(#{fakeMethod := {?ROOT_F, _}}, Result)
            end}
        ]
    end}.

%%% ====================================================================
%%% Fixture helpers
%%% ====================================================================

setup() ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    PidA = start_class(?ROOT_A, none, #{}, #{}),
    PidB = start_class(?GHOST_B, ?GHOST_SUPER_1, #{}, #{}),
    PidC = start_class(?ROOT_C, none, #{}, #{}),
    PidD = start_class(?GHOST_D, ?GHOST_SUPER_2, #{}, #{}),
    PidE = start_class(?ROOT_E, none, #{}, #{}),
    PidF = start_class(?ROOT_F, none, #{fakeMethod => #{arity => 0}}, #{}),
    {PidA, PidB, PidC, PidD, PidE, PidF}.

teardown({PidA, PidB, PidC, PidD, PidE, PidF}) ->
    lists:foreach(fun stop_class/1, [PidA, PidB, PidC, PidD, PidE, PidF]).

start_class(Name, Superclass, InstanceMethods, ClassMethods) ->
    ClassInfo = #{
        name => Name,
        superclass => Superclass,
        module => Name,
        instance_methods => InstanceMethods,
        class_methods => ClassMethods,
        fields => []
    },
    case beamtalk_object_class:start(Name, ClassInfo) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

stop_class(Pid) ->
    try
        gen_server:stop(Pid, normal, 5000)
    catch
        _:_ -> ok
    end.
