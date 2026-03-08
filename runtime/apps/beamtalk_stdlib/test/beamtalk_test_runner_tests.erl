%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_test_runner module.
%%%
%%% Covers path_suffix_match/2 which is the core logic for the `file`
%%% parameter of the `test` MCP tool (BT-1234).

-module(beamtalk_test_runner_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% path_suffix_match/2 tests
%%% ============================================================================

path_suffix_exact_match_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"/absolute/path/test/foo_test.bt">>
        )
    ).

path_suffix_relative_suffix_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"test/foo_test.bt">>
        )
    ).

path_suffix_filename_only_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"foo_test.bt">>
        )
    ).

path_suffix_no_match_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"bar_test.bt">>
        )
    ).

path_suffix_partial_component_no_match_test() ->
    %% "oo_test.bt" must NOT match — it is not a path-boundary-aligned suffix.
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"/absolute/path/test/foo_test.bt">>,
            <<"oo_test.bt">>
        )
    ).

path_suffix_longer_than_stored_no_match_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"test/foo.bt">>,
            <<"absolute/path/test/foo.bt">>
        )
    ).

path_suffix_empty_stored_test() ->
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"">>,
            <<"foo.bt">>
        )
    ).

path_suffix_both_empty_test() ->
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"">>,
            <<"">>
        )
    ).
