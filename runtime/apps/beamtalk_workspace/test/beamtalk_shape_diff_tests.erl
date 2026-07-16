%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_shape_diff_tests).

-moduledoc "Unit tests for beamtalk_shape_diff (ADR 0105 Phase 2, BT-2780).".

-include_lib("eunit/include/eunit.hrl").

identical_shapes_is_no_op_test() ->
    Shape = #{<<"count">> => <<"Integer">>},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(Shape, Shape)).

both_empty_shapes_is_no_op_test() ->
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(#{}, #{})).

no_previous_generation_is_no_op_test() ->
    New = #{<<"count">> => <<"Integer">>},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(undefined, New)).

unresolvable_new_generation_is_no_op_test() ->
    Old = #{<<"count">> => <<"Integer">>},
    ?assertEqual({no_op, []}, beamtalk_shape_diff:diff(Old, undefined)).

added_field_test() ->
    Old = #{<<"count">> => <<"Integer">>},
    New = #{<<"count">> => <<"Integer">>, <<"name">> => <<"String">>},
    ?assertEqual(
        {shape_change, [{added, <<"name">>}]}, beamtalk_shape_diff:diff(Old, New)
    ).

removed_field_test() ->
    Old = #{<<"count">> => <<"Integer">>, <<"name">> => <<"String">>},
    New = #{<<"count">> => <<"Integer">>},
    ?assertEqual(
        {shape_change, [{removed, <<"name">>}]}, beamtalk_shape_diff:diff(Old, New)
    ).

retyped_field_test() ->
    Old = #{<<"count">> => <<"Integer">>},
    New = #{<<"count">> => <<"String">>},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Integer">>, <<"String">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

%% An untyped field going typed (or vice versa) is a retype — the Dynamic
%% sentinel is just another type value for comparison purposes.
untyped_to_typed_is_retyped_test() ->
    Old = #{<<"count">> => <<"Dynamic">>},
    New = #{<<"count">> => <<"Integer">>},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Dynamic">>, <<"Integer">>}]},
        beamtalk_shape_diff:diff(Old, New)
    ).

mixed_add_remove_retype_test() ->
    Old = #{
        <<"count">> => <<"Integer">>,
        <<"name">> => <<"String">>
    },
    New = #{
        <<"count">> => <<"String">>,
        <<"timeout">> => <<"Integer">>
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
%% caller only diffs field_types maps), so this exercises that a field
%% present unchanged in both alongside other changes is not itself reported.
unchanged_field_alongside_others_is_not_reported_test() ->
    Old = #{<<"count">> => <<"Integer">>, <<"stable">> => <<"Boolean">>},
    New = #{<<"count">> => <<"String">>, <<"stable">> => <<"Boolean">>},
    ?assertEqual(
        {shape_change, [{retyped, <<"count">>, <<"Integer">>, <<"String">>}]},
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
        )
    ].
