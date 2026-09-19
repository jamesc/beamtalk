%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_diff_tests).

-moduledoc "Unit tests for beamtalk_shape_diff (ADR 0105 Phase 2, ADR 0124 Section 9/B9).".

-include_lib("eunit/include/eunit.hrl").

%% A shape() value, eager kind (the common case in these tests — kind_changed
%% has its own section below).
e(TypeBin) -> {TypeBin, <<"eager">>}.

%% A shape() value, late kind.
l(TypeBin) -> {TypeBin, <<"late">>}.

identical_shapes_is_no_op_test() ->
    Shape = #{<<"count">> => e(<<"Integer">>)},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(Shape, Shape)).

both_empty_shapes_is_no_op_test() ->
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(#{}, #{})).

no_previous_generation_is_no_op_test() ->
    New = #{<<"count">> => e(<<"Integer">>)},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(undefined, New)).

unresolvable_new_generation_is_no_op_test() ->
    Old = #{<<"count">> => e(<<"Integer">>)},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(Old, undefined)).

added_field_test() ->
    Old = #{<<"count">> => e(<<"Integer">>)},
    New = #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"String">>)},
    ?assertEqual(
        {shape_change, [{added, <<"name">>}]}, beamtalk_shape_diff:diff(Old, New)
    ).

removed_field_test() ->
    Old = #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"String">>)},
    New = #{<<"count">> => e(<<"Integer">>)},
    ?assertEqual(
        {shape_change, [{removed, <<"name">>}]}, beamtalk_shape_diff:diff(Old, New)
    ).

retyped_field_test() ->
    Old = #{<<"count">> => e(<<"Integer">>)},
    New = #{<<"count">> => e(<<"String">>)},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Integer">>, <<"String">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

%% An untyped field going typed (or vice versa) is a retype — the Dynamic
%% sentinel is just another type value for comparison purposes.
untyped_to_typed_is_retyped_test() ->
    Old = #{<<"count">> => e(<<"Dynamic">>)},
    New = #{<<"count">> => e(<<"Integer">>)},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Dynamic">>, <<"Integer">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

mixed_add_remove_retype_test() ->
    Old = #{
        <<"count">> => e(<<"Integer">>),
        <<"name">> => e(<<"String">>)
    },
    New = #{
        <<"count">> => e(<<"String">>),
        <<"timeout">> => e(<<"Integer">>)
    },
    {shape_change, Changes} = beamtalk_shape_diff:diff(Old, New),
    ?assertEqual(
        lists:sort([
            {added, <<"timeout">>},
            {removed, <<"name">>},
            {retyped, <<"count">>, <<"Integer">>, <<"String">>}
        ]),
        lists:sort(Changes)
    ).

%% An unrelated field's superclass/method change never reaches diff/2 (the
%% caller only diffs flattened shape() maps), so this exercises that a field
%% present unchanged in both alongside other changes is not itself reported.
unchanged_field_alongside_others_is_not_reported_test() ->
    Old = #{<<"count">> => e(<<"Integer">>), <<"stable">> => e(<<"Boolean">>)},
    New = #{<<"count">> => e(<<"String">>), <<"stable">> => e(<<"Boolean">>)},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Integer">>, <<"String">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

%%====================================================================
%% kind_changed (ADR 0124 Section 9/B9)
%%====================================================================

%% A slot flipping eager -> late, same declared type, is a shape_change of
%% its own kind — not a retype (the DeclaredType half is unchanged) and not
%% a no_op (the moduledoc's whole point: an eager<->late flip must not be
%% invisible to diff/2).
eager_to_late_flip_is_kind_changed_test() ->
    Old = #{<<"proc">> => e(<<"String">>)},
    New = #{<<"proc">> => l(<<"String">>)},
    ?assertEqual(
        {shape_change, [{kind_changed, <<"proc">>, <<"eager">>, <<"late">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

late_to_eager_flip_is_kind_changed_test() ->
    Old = #{<<"proc">> => l(<<"String">>)},
    New = #{<<"proc">> => e(<<"String">>)},
    ?assertEqual(
        {shape_change, [{kind_changed, <<"proc">>, <<"late">>, <<"eager">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

%% A slot that both retypes AND flips kind in the same reload produces both
%% a retyped and a kind_changed entry — the two are independent axes.
retype_and_kind_flip_together_produce_both_entries_test() ->
    Old = #{<<"proc">> => e(<<"Integer">>)},
    New = #{<<"proc">> => l(<<"String">>)},
    {shape_change, Changes} = beamtalk_shape_diff:diff(Old, New),
    ?assertEqual(
        lists:sort([
            {retyped, <<"proc">>, <<"Integer">>, <<"String">>},
            {kind_changed, <<"proc">>, <<"eager">>, <<"late">>}
        ]),
        lists:sort(Changes)
    ).

%% Same kind on both sides alongside an unrelated kind flip elsewhere is not
%% itself reported (mirrors unchanged_field_alongside_others_is_not_reported_test).
unchanged_kind_alongside_a_flip_is_not_reported_test() ->
    Old = #{<<"proc">> => e(<<"String">>), <<"stable">> => e(<<"Boolean">>)},
    New = #{<<"proc">> => l(<<"String">>), <<"stable">> => e(<<"Boolean">>)},
    ?assertEqual(
        {shape_change, [{kind_changed, <<"proc">>, <<"eager">>, <<"late">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

%%====================================================================
%% field_name/1
%%====================================================================

field_name_test_() ->
    [
        ?_assertEqual(<<"count">>, beamtalk_shape_diff:field_name({added, <<"count">>})),
        ?_assertEqual(<<"count">>, beamtalk_shape_diff:field_name({removed, <<"count">>})),
        ?_assertEqual(
            <<"count">>,
            beamtalk_shape_diff:field_name({retyped, <<"count">>, <<"Integer">>, <<"String">>})
        ),
        ?_assertEqual(
            <<"proc">>,
            beamtalk_shape_diff:field_name({kind_changed, <<"proc">>, <<"eager">>, <<"late">>})
        )
    ].

%%====================================================================
%% reload_findings/4, suspended_finding/3 (ADR 0123 §4, BT-3538)
%%
%% One test per row of the findings table (BT-3538's acceptance criteria).
%%====================================================================

gen(Shape, Version, Migrations) ->
    #{
        shape => Shape,
        own_shape => Shape,
        ancestor_shape => #{},
        version => Version,
        migrations => Migrations
    }.

%% No previous generation at all — nothing to compare a version against, so
%% reload_findings/4 degrades to [] rather than manufacture a finding from
%% nothing (mirrors diff/2's own "no baseline" no_op rule).
no_previous_generation_produces_no_reload_findings_test() ->
    NewGen = gen(#{<<"count">> => e(<<"Integer">>)}, 1, #{}),
    DiffResult = {no_op, []},
    ?assertEqual(
        [], beamtalk_shape_diff:reload_findings(<<"Cart">>, undefined, NewGen, DiffResult)
    ).

%% Row 1: field removed or retyped, shapeVersion unchanged -> Warning.
dropped_without_bump_is_a_warning_test() ->
    OldShape = #{<<"discount">> => e(<<"Integer">>), <<"items">> => e(<<"Dynamic">>)},
    NewShape = #{<<"items">> => e(<<"Dynamic">>)},
    PrevGen = gen(OldShape, 2, #{}),
    NewGen = gen(NewShape, 2, #{}),
    DiffResult = beamtalk_shape_diff:diff(OldShape, NewShape),
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(<<"Cart">>, maps:get(class, Finding)),
    ?assertEqual(dropped_without_bump, maps:get(kind, Finding)),
    ?assertEqual(warning, maps:get(severity, Finding)),
    ?assertEqual([<<"discount">>], maps:get(fields, Finding)),
    ?assertEqual(2, maps:get(from_version, Finding)),
    ?assertEqual(2, maps:get(to_version, Finding)),
    ?assert(binary:match(maps:get(message, Finding), <<"discount">>) =/= nomatch),
    ?assert(binary:match(maps:get(message, Finding), <<"shapeVersion">>) =/= nomatch).

%% Row 1 also covers a retyped (not just removed) field, same severity/kind.
retyped_without_bump_is_a_warning_test() ->
    OldShape = #{<<"count">> => e(<<"Integer">>)},
    NewShape = #{<<"count">> => e(<<"String">>)},
    PrevGen = gen(OldShape, 1, #{}),
    NewGen = gen(NewShape, 1, #{}),
    DiffResult = beamtalk_shape_diff:diff(OldShape, NewShape),
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(dropped_without_bump, maps:get(kind, Finding)),
    ?assertEqual([<<"count">>], maps:get(fields, Finding)).

%% Row 1, ADR 0124 §9/B9: an eager<->late kind flip is also a
%% dropped_without_bump-eligible change — the same warning fires when the
%% version wasn't bumped, since what an absent key means on the *next*
%% reload has just changed.
kind_changed_without_bump_is_a_warning_test() ->
    OldShape = #{<<"proc">> => e(<<"String">>)},
    NewShape = #{<<"proc">> => l(<<"String">>)},
    PrevGen = gen(OldShape, 1, #{}),
    NewGen = gen(NewShape, 1, #{}),
    DiffResult = beamtalk_shape_diff:diff(OldShape, NewShape),
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(dropped_without_bump, maps:get(kind, Finding)),
    ?assertEqual([<<"proc">>], maps:get(fields, Finding)).

%% Row 2: fields only added, version unchanged -> Hint.
added_only_unchanged_version_is_a_hint_test() ->
    OldShape = #{<<"count">> => e(<<"Integer">>)},
    NewShape = #{<<"count">> => e(<<"Integer">>), <<"name">> => e(<<"Dynamic">>)},
    PrevGen = gen(OldShape, 1, #{}),
    NewGen = gen(NewShape, 1, #{}),
    DiffResult = beamtalk_shape_diff:diff(OldShape, NewShape),
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(added_only, maps:get(kind, Finding)),
    ?assertEqual(hint, maps:get(severity, Finding)),
    ?assertEqual([<<"name">>], maps:get(fields, Finding)),
    ?assertEqual(1, maps:get(from_version, Finding)),
    ?assertEqual(1, maps:get(to_version, Finding)).

%% Row 3: version bumped N -> N+1, no migrateFromVN: -> Hint.
bumped_without_migration_is_a_hint_test() ->
    Shape = #{<<"count">> => e(<<"Integer">>)},
    PrevGen = gen(Shape, 2, #{}),
    NewGen = gen(Shape, 3, #{}),
    DiffResult = {no_op, []},
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(bumped_without_migration, maps:get(kind, Finding)),
    ?assertEqual(hint, maps:get(severity, Finding)),
    ?assertEqual(2, maps:get(from_version, Finding)),
    ?assertEqual(3, maps:get(to_version, Finding)),
    ?assert(binary:match(maps:get(message, Finding), <<"migrateFromV2:">>) =/= nomatch).

%% Row 3, negative: a matching migrateFromVN: entry in the new generation's
%% shape_migrations table suppresses the finding.
bumped_with_migration_produces_no_finding_test() ->
    Shape = #{<<"count">> => e(<<"Integer">>)},
    PrevGen = gen(Shape, 2, #{}),
    NewGen = gen(Shape, 3, #{2 => 'migrateFromV2:'}),
    DiffResult = {no_op, []},
    ?assertEqual(
        [], beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult)
    ).

%% Row 4: version decreased -> Warning.
version_decreased_is_a_warning_test() ->
    Shape = #{<<"count">> => e(<<"Integer">>)},
    PrevGen = gen(Shape, 3, #{}),
    NewGen = gen(Shape, 2, #{}),
    DiffResult = {no_op, []},
    [Finding] = beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult),
    ?assertEqual(version_decreased, maps:get(kind, Finding)),
    ?assertEqual(warning, maps:get(severity, Finding)),
    ?assertEqual(3, maps:get(from_version, Finding)),
    ?assertEqual(2, maps:get(to_version, Finding)).

%% Row 5: any instance left suspended by a failed migration -> Error.
instances_suspended_is_an_error_test() ->
    Pid1 = self(),
    Outcome = #{migrated => 2, suspended => [{Pid1, some_reason}]},
    [Finding] = beamtalk_shape_diff:suspended_finding(<<"Cart">>, 2, Outcome),
    ?assertEqual(<<"Cart">>, maps:get(class, Finding)),
    ?assertEqual(instances_suspended, maps:get(kind, Finding)),
    ?assertEqual(error, maps:get(severity, Finding)),
    ?assertEqual(2, maps:get(migrated, Finding)),
    ?assertEqual(1, maps:get(suspended, Finding)),
    ?assertEqual([Pid1], maps:get(pids, Finding)),
    ?assertEqual(2, maps:get(to_version, Finding)).

%% Row 5, negative: no suspended instances -> no finding.
no_suspended_instances_produces_no_finding_test() ->
    Outcome = #{migrated => 3, suspended => []},
    ?assertEqual([], beamtalk_shape_diff:suspended_finding(<<"Cart">>, 1, Outcome)).

%% A clean reload (no field change, version unchanged) produces no findings
%% at all — the common case, exercised explicitly so a regression that makes
%% every reload noisy is caught immediately.
clean_reload_produces_no_findings_test() ->
    Shape = #{<<"count">> => e(<<"Integer">>)},
    PrevGen = gen(Shape, 1, #{}),
    NewGen = gen(Shape, 1, #{}),
    DiffResult = beamtalk_shape_diff:diff(Shape, Shape),
    ?assertEqual(
        [], beamtalk_shape_diff:reload_findings(<<"Cart">>, PrevGen, NewGen, DiffResult)
    ).
