%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_json module (BT-1142).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover parse:/1, generate:/1, prettyPrint:/1, the FFI no-colon
%%% aliases (parse/1, generate/1, prettyPrint/1), and has_method/1.

-module(beamtalk_json_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% parse:/1
%%% ============================================================================

parse_integer_test() ->
    ?assertEqual(42, beamtalk_json:'parse:'(<<"42">>)).

parse_string_test() ->
    ?assertEqual(<<"hello">>, beamtalk_json:'parse:'(<<"\"hello\"">>)).

parse_array_test() ->
    ?assertEqual([1, 2, 3], beamtalk_json:'parse:'(<<"[1,2,3]">>)).

parse_null_test() ->
    ?assertEqual(nil, beamtalk_json:'parse:'(<<"null">>)).

parse_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'parse:'(not_a_binary)
    ).

parse_invalid_json_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}},
        beamtalk_json:'parse:'(<<"{invalid">>)
    ).

%%% ============================================================================
%%% generate:/1
%%% ============================================================================

generate_integer_test() ->
    ?assertEqual(<<"42">>, beamtalk_json:'generate:'(42)).

generate_null_test() ->
    ?assertEqual(<<"null">>, beamtalk_json:'generate:'(nil)).

generate_list_test() ->
    ?assertEqual(<<"[1,2,3]">>, beamtalk_json:'generate:'([1, 2, 3])).

%%% ============================================================================
%%% prettyPrint:/1
%%% ============================================================================

pretty_print_test() ->
    Result = beamtalk_json:'prettyPrint:'(42),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% FFI no-colon aliases
%%% ============================================================================

parse_alias_test() ->
    ?assertEqual(42, beamtalk_json:parse(<<"42">>)).

parse_alias_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:parse(not_a_binary)
    ).

generate_alias_test() ->
    ?assertEqual(<<"42">>, beamtalk_json:generate(42)).

pretty_print_alias_test() ->
    Result = beamtalk_json:prettyPrint(42),
    ?assert(is_binary(Result)).
