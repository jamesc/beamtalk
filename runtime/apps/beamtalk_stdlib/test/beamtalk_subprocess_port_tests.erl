%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_subprocess_port_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
EUnit tests for the pure utility functions in beamtalk_subprocess_port.

Covers the five side-effect-free helpers shared by beamtalk_subprocess
and beamtalk_reactive_subprocess:

  * split_lines/1       — binary line-splitter
  * flush_and_collect/2 — buffered-fragment combiner + splitter
  * bt_array_to_list/3  — Beamtalk Array / list coercion
  * ensure_binary_args/3 — argument-list binary validator
  * ensure_binary_env/3  — env-map binary validator

Port-delegation functions (open/0, spawn_child/*, kill_child/2,
write_stdin/3, close/1) require a running OS port and are not covered here.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% split_lines/1
%%% ============================================================================

split_lines_no_newline_test() ->
    %% Data with no newline — entire input is a partial line, no complete lines
    ?assertEqual({[], <<"hello">>}, beamtalk_subprocess_port:split_lines(<<"hello">>)).

split_lines_empty_binary_test() ->
    %% Empty binary — no lines, empty remainder
    ?assertEqual({[], <<>>}, beamtalk_subprocess_port:split_lines(<<>>)).

split_lines_single_newline_test() ->
    %% One complete line followed by an empty remainder
    ?assertEqual({[<<"hello">>], <<>>}, beamtalk_subprocess_port:split_lines(<<"hello\n">>)).

split_lines_multiple_newlines_test() ->
    %% Multiple complete lines
    ?assertEqual(
        {[<<"a">>, <<"b">>, <<"c">>], <<>>},
        beamtalk_subprocess_port:split_lines(<<"a\nb\nc\n">>)
    ).

split_lines_trailing_partial_test() ->
    %% Last segment without newline becomes the remainder
    ?assertEqual(
        {[<<"a">>, <<"b">>], <<"partial">>},
        beamtalk_subprocess_port:split_lines(<<"a\nb\npartial">>)
    ).

split_lines_only_newline_test() ->
    %% A bare newline — one empty line, empty remainder
    ?assertEqual({[<<>>], <<>>}, beamtalk_subprocess_port:split_lines(<<"\n">>)).

split_lines_crlf_normalised_test() ->
    %% Windows-style CR+LF: \r should be stripped from each complete line
    ?assertEqual(
        {[<<"hello">>, <<"world">>], <<>>},
        beamtalk_subprocess_port:split_lines(<<"hello\r\nworld\r\n">>)
    ).

split_lines_crlf_partial_test() ->
    %% CR in remainder is NOT stripped (it is part of the incomplete fragment)
    {Lines, Remainder} = beamtalk_subprocess_port:split_lines(<<"done\r\nstill">>),
    ?assertEqual([<<"done">>], Lines),
    ?assertEqual(<<"still">>, Remainder).

split_lines_bare_cr_not_stripped_test() ->
    %% A lone \r (no \n) is kept as-is — only trailing \r before line boundary is stripped
    ?assertEqual({[], <<"line\r">>}, beamtalk_subprocess_port:split_lines(<<"line\r">>)).

%%% ============================================================================
%%% flush_and_collect/2
%%% ============================================================================

flush_and_collect_empty_pending_test() ->
    %% No pending fragment — behaves identically to split_lines
    ?assertEqual(
        {[<<"line">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<>>, <<"line\n">>)
    ).

flush_and_collect_non_empty_pending_test() ->
    %% Pending fragment is prepended before splitting
    ?assertEqual(
        {[<<"hello world">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<"hello ">>, <<"world\n">>)
    ).

flush_and_collect_pending_completes_line_test() ->
    %% Pending + new data together form one complete line
    ?assertEqual(
        {[<<"abc">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<"ab">>, <<"c\n">>)
    ).

flush_and_collect_multiple_lines_test() ->
    %% Pending merges into first line; subsequent lines are independent
    ?assertEqual(
        {[<<"start middle">>, <<"end">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<"start ">>, <<"middle\nend\n">>)
    ).

flush_and_collect_new_remainder_test() ->
    %% Leftover after last newline becomes the new remainder
    ?assertEqual(
        {[<<"a">>], <<"b">>},
        beamtalk_subprocess_port:flush_and_collect(<<>>, <<"a\nb">>)
    ).

flush_and_collect_both_empty_test() ->
    %% Entirely empty inputs
    ?assertEqual({[], <<>>}, beamtalk_subprocess_port:flush_and_collect(<<>>, <<>>)).

%%% ============================================================================
%%% bt_array_to_list/3
%%% ============================================================================

bt_array_to_list_plain_list_test() ->
    %% Plain Erlang lists pass through unchanged
    ?assertEqual([1, 2, 3], beamtalk_subprocess_port:bt_array_to_list('C', [1, 2, 3], 'sel:')).

bt_array_to_list_empty_list_test() ->
    ?assertEqual([], beamtalk_subprocess_port:bt_array_to_list('C', [], 'sel:')).

bt_array_to_list_tagged_array_test() ->
    %% Beamtalk Array (canonical map-backed tagged map) is converted to a list
    Tagged = beamtalk_array:from_list([a, b, c]),
    ?assertEqual([a, b, c], beamtalk_subprocess_port:bt_array_to_list('C', Tagged, 'sel:')).

bt_array_to_list_empty_array_test() ->
    Tagged = beamtalk_array:from_list([]),
    ?assertEqual([], beamtalk_subprocess_port:bt_array_to_list('C', Tagged, 'sel:')).

bt_array_to_list_invalid_data_field_test() ->
    %% Tagged map with non-array 'data' field raises type_error
    Tagged = #{'$beamtalk_class' => 'Array', 'data' => not_an_array},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:bt_array_to_list('Subprocess', Tagged, 'run:')
    ).

bt_array_to_list_non_list_non_array_test() ->
    %% Any other type raises type_error
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:bt_array_to_list('Subprocess', 42, 'run:')
    ).

bt_array_to_list_atom_raises_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:bt_array_to_list('Subprocess', not_a_list, 'run:')
    ).

%%% ============================================================================
%%% ensure_binary_args/3
%%% ============================================================================

ensure_binary_args_all_binaries_test() ->
    %% All-binary list succeeds with ok
    ?assertEqual(
        ok,
        beamtalk_subprocess_port:ensure_binary_args('Subprocess', [<<"a">>, <<"b">>], 'run:')
    ).

ensure_binary_args_empty_list_test() ->
    %% Empty list is valid
    ?assertEqual(ok, beamtalk_subprocess_port:ensure_binary_args('Subprocess', [], 'run:')).

ensure_binary_args_non_binary_element_test() ->
    %% A non-binary element raises type_error
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_args('Subprocess', [<<"ok">>, 42], 'run:')
    ).

ensure_binary_args_atom_element_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_args('Subprocess', [not_a_binary], 'run:')
    ).

ensure_binary_args_mixed_first_non_binary_test() ->
    %% First element is non-binary — must also raise
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_args('Subprocess', [123, <<"ok">>], 'run:')
    ).

%%% ============================================================================
%%% ensure_binary_env/3
%%% ============================================================================

ensure_binary_env_empty_map_test() ->
    %% Empty env map is always valid
    ?assertEqual(ok, beamtalk_subprocess_port:ensure_binary_env('Subprocess', #{}, 'run:')).

ensure_binary_env_all_binary_pairs_test() ->
    %% All keys and values are binaries
    ?assertEqual(
        ok,
        beamtalk_subprocess_port:ensure_binary_env(
            'Subprocess', #{<<"PATH">> => <<"usr/bin">>, <<"HOME">> => <<"/home/user">>}, 'run:'
        )
    ).

ensure_binary_env_non_binary_key_test() ->
    %% Atom key raises type_error
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_env(
            'Subprocess', #{path => <<"/usr/bin">>}, 'run:'
        )
    ).

ensure_binary_env_non_binary_value_test() ->
    %% Integer value raises type_error
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_env(
            'Subprocess', #{<<"KEY">> => 42}, 'run:'
        )
    ).

ensure_binary_env_mixed_valid_invalid_test() ->
    %% One valid pair, one invalid pair — must still raise
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_subprocess_port:ensure_binary_env(
            'Subprocess',
            #{<<"GOOD">> => <<"val">>, <<"BAD">> => not_binary},
            'run:'
        )
    ).
