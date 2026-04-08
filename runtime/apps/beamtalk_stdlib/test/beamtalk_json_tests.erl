%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_json module (BT-1142).

Tests cover parse:/1, generate:/1, prettyPrint:/1, and the FFI no-colon
aliases (parse/1, generate/1, prettyPrint/1).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% parse:/1
%%% ============================================================================

parse_integer_test() ->
    R = beamtalk_json:'parse:'(<<"42">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42}, R).

parse_string_test() ->
    R = beamtalk_json:'parse:'(<<"\"hello\"">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := <<"hello">>}, R).

parse_array_test() ->
    R = beamtalk_json:'parse:'(<<"[1,2,3]">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := [1, 2, 3]}, R).

parse_null_test() ->
    R = beamtalk_json:'parse:'(<<"null">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := nil}, R).

parse_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_json:'parse:'(not_a_binary)
    ).

parse_invalid_json_test() ->
    R = beamtalk_json:'parse:'(<<"{invalid">>),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{'$beamtalk_class' := _, error := #beamtalk_error{kind = parse_error}}
        },
        R
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
    R = beamtalk_json:parse(<<"42">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42}, R).

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
