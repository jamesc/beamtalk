%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_span_tests).

-include_lib("eunit/include/eunit.hrl").

in_range_test() ->
    ?assert(beamtalk_workspace_span:in_range(<<"abcdef">>, 0, 6)),
    ?assert(beamtalk_workspace_span:in_range(<<"abcdef">>, 3, 3)),
    ?assertNot(beamtalk_workspace_span:in_range(<<"abcdef">>, 4, 7)),
    ?assertNot(beamtalk_workspace_span:in_range(<<"abcdef">>, 4, 3)),
    ?assertNot(beamtalk_workspace_span:in_range(<<"abcdef">>, -1, 3)),
    ?assertNot(beamtalk_workspace_span:in_range(<<"abcdef">>, a, 3)).

valid_span_test() ->
    ?assert(beamtalk_workspace_span:valid_span(0, 0, 0)),
    ?assertNot(beamtalk_workspace_span:valid_span(0, 1, 0)).

slice_test() ->
    ?assertEqual(<<"cd">>, beamtalk_workspace_span:slice(<<"abcdef">>, #{start => 2, 'end' => 4})),
    ?assertEqual(<<"cd">>, beamtalk_workspace_span:slice(<<"abcdef">>, {2, 4})),
    ?assertEqual(<<>>, beamtalk_workspace_span:slice(<<"abcdef">>, {3, 3})).

splice_test() ->
    ?assertEqual(<<"abXYef">>, beamtalk_workspace_span:splice(<<"abcdef">>, {2, 4}, <<"XY">>)),
    ?assertEqual(
        <<"abef">>, beamtalk_workspace_span:splice(<<"abcdef">>, #{start => 2, 'end' => 4}, <<>>)
    ),
    ?assertEqual(<<"abZcdef">>, beamtalk_workspace_span:splice(<<"abcdef">>, {2, 2}, <<"Z">>)).

matches_at_test() ->
    ?assert(beamtalk_workspace_span:matches_at(<<"abcdef">>, 2, <<"cd">>)),
    ?assertNot(beamtalk_workspace_span:matches_at(<<"abcdef">>, 2, <<"xx">>)),
    ?assertNot(beamtalk_workspace_span:matches_at(<<"abcdef">>, 5, <<"fg">>)),
    ?assertNot(beamtalk_workspace_span:matches_at(<<"abcdef">>, -1, <<"a">>)).

matches_span_test() ->
    Span = #{start => 2, 'end' => 4},
    ?assert(beamtalk_workspace_span:matches_span(<<"abcdef">>, Span, <<"cd">>)),
    ?assertNot(beamtalk_workspace_span:matches_span(<<"abcdef">>, Span, <<"cx">>)),
    ?assertNot(beamtalk_workspace_span:matches_span(<<"ab">>, Span, <<"cd">>)),
    ?assertNot(beamtalk_workspace_span:matches_span(<<"abcdef">>, undefined, <<"cd">>)).

check_span_test() ->
    ?assertEqual(ok, beamtalk_workspace_span:check_span(<<"abcdef">>, {2, 4}, <<"cd">>)),
    ?assertEqual(
        {conflict, external_edit},
        beamtalk_workspace_span:check_span(<<"abcdef">>, {2, 4}, <<"zz">>)
    ),
    ?assertEqual(
        {conflict, {span_out_of_range, 4, 9, 6}},
        beamtalk_workspace_span:check_span(<<"abcdef">>, {4, 9}, <<"ef">>)
    ).
