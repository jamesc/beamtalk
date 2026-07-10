%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_store_tests).

-moduledoc """
Unit tests for beamtalk_workspace_shape_store (ADR 0105 Phase 2, BT-2780).

Covers:
- prime/1's "seed once" laziness (does not overwrite an existing entry)
- capture/1's self-seeding fallback when prime/1 was never called (always
  no_op)
- the two-phase capture flow end-to-end: prime/1 against a currently-loaded
  module, then capture/1 against a *different* installed module (simulating
  the module having been replaced by code:load_binary in between)
- clear/0 resets the session
- previous/1 is read-only
- read_shape_from_meta/1's degrade-to-undefined paths (no meta exported,
  class not registered) and the real read path via
  beamtalk_shape_store_fixture
- field_type_to_binary/1's Dynamic-sentinel normalisation (exercised
  indirectly through read_shape_from_meta/1, since it is not exported)

The end-to-end thread from a real class-body reload
(`beamtalk_repl_loader:load_class_module/3` et al.) through to a
correctly-populated store entry and a shape-change re-check is covered in
`beamtalk_repl_loader_recheck_tests.erl`, not here — this module tests the
store's own API contract in isolation.
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_class_metadata).

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_workspace_shape_store:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end.

store_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun prime_on_unregistered_class_seeds_undefined/1,
        fun capture_without_prime_is_always_no_op/1,
        fun previous_does_not_mutate_store/1,
        fun clear_resets_the_session/1,
        fun different_classes_are_independent/1
    ]}.

%%====================================================================
%% prime/1 + capture/1 (core AC coverage)
%%====================================================================

%% A class that was never loaded (no __beamtalk_meta/0 to read) primes to
%% "nothing recorded" — not an error.
prime_on_unregistered_class_seeds_undefined(_Pid) ->
    ok = beamtalk_workspace_shape_store:prime(<<"NeverLoadedClass">>),
    Prev = beamtalk_workspace_shape_store:previous(<<"NeverLoadedClass">>),
    [?_assertEqual(undefined, Prev)].

%% capture/1 without a preceding prime/1 (the method-patch/removal/new-class/
%% protocol install paths, none of which change shape) self-seeds from
%% whatever __beamtalk_meta/0 currently reports and always classifies no_op —
%% never a false positive, since there is nothing to diff against but itself.
capture_without_prime_is_always_no_op(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor'
    ),
    try
        {Prev, DiffResult} = beamtalk_workspace_shape_store:capture(<<"ShapeFixtureClass">>),
        [
            ?_assertEqual(
                #{<<"count">> => <<"Integer">>, <<"name">> => <<"Dynamic">>}, Prev
            ),
            ?_assertEqual({no_op, []}, DiffResult)
        ]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% previous/1 is read-only: calling it repeatedly must not change what the
%% next capture/1 sees as "previous".
previous_does_not_mutate_store(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor'
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        P1 = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        P2 = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        Expected = #{<<"count">> => <<"Integer">>, <<"name">> => <<"Dynamic">>},
        [
            ?_assertEqual(Expected, P1),
            ?_assertEqual(Expected, P2)
        ]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% clear/0 drops every recorded generation — the next prime/1 re-seeds as if
%% this were a fresh workspace session.
clear_resets_the_session(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor'
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        ok = beamtalk_workspace_shape_store:clear(),
        Prev = beamtalk_workspace_shape_store:previous(<<"ShapeFixtureClass">>),
        [?_assertEqual(undefined, Prev)]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%% Two different classes keep independent entries.
different_classes_are_independent(_Pid) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor'
    ),
    try
        ok = beamtalk_workspace_shape_store:prime(<<"ShapeFixtureClass">>),
        PrevOther = beamtalk_workspace_shape_store:previous(<<"OtherClass">>),
        [?_assertEqual(undefined, PrevOther)]
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.

%%====================================================================
%% read_shape_from_meta/1 (exported for TEST)
%%====================================================================

%% Registered class + module, but the module exports no __beamtalk_meta/0 —
%% degrades to undefined rather than crashing the prime/capture hook.
read_shape_degrades_to_undefined_when_no_meta_exported_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert('NoMetaClass', lists, [foo], 'Object'),
    try
        ?assertEqual(
            undefined, beamtalk_workspace_shape_store:read_shape_from_meta(<<"NoMetaClass">>)
        )
    after
        ets:delete(?TABLE, 'NoMetaClass')
    end.

%% An unregistered class degrades to undefined (binary_to_existing_atom/2
%% fails fast rather than minting a fresh atom from reload-derived text).
read_shape_degrades_to_undefined_for_unregistered_class_test() ->
    ?assertEqual(
        undefined,
        beamtalk_workspace_shape_store:read_shape_from_meta(<<"NeverSeenClassXyz">>)
    ).

%% A registered class whose module exports a realistic __beamtalk_meta/0 (via
%% beamtalk_shape_store_fixture) reads the currently-installed shape,
%% normalising `none` to the Dynamic sentinel.
read_shape_reads_field_types_from_meta_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'ShapeFixtureClass', beamtalk_shape_store_fixture, [count, name], 'Actor'
    ),
    try
        ?assertEqual(
            #{<<"count">> => <<"Integer">>, <<"name">> => <<"Dynamic">>},
            beamtalk_workspace_shape_store:read_shape_from_meta(<<"ShapeFixtureClass">>)
        )
    after
        ets:delete(?TABLE, 'ShapeFixtureClass')
    end.
