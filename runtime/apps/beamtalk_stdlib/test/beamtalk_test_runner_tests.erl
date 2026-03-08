%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

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

path_suffix_windows_backslash_stored_test() ->
    %% Stored path uses Windows-style backslashes (from std::fs::canonicalize);
    %% suffix uses forward slashes as passed by the MCP caller.
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"test/foo_test.bt">>
        )
    ).

path_suffix_windows_exact_match_test() ->
    %% Both stored and suffix use Windows-style backslashes.
    ?assert(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"C:\\Users\\project\\test\\foo_test.bt">>
        )
    ).

path_suffix_windows_boundary_no_partial_match_test() ->
    %% "oo_test.bt" must NOT match even against a Windows path.
    ?assertNot(
        beamtalk_test_runner:path_suffix_match(
            <<"C:\\Users\\project\\test\\foo_test.bt">>,
            <<"oo_test.bt">>
        )
    ).
