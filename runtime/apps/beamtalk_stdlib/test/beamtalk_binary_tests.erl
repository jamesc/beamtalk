%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_binary module (BT-1554).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests cover serialize:/1, deserialize:/1, size:/1, fromIolist:/1,
%%% and the FFI no-colon aliases.

-module(beamtalk_binary_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% serialize:/1
%%% ============================================================================

serialize_integer_test() ->
    Bin = beamtalk_binary:'serialize:'(42),
    ?assert(is_binary(Bin)).

serialize_string_test() ->
    Bin = beamtalk_binary:'serialize:'(<<"hello">>),
    ?assert(is_binary(Bin)).

serialize_list_test() ->
    Bin = beamtalk_binary:'serialize:'([1, 2, 3]),
    ?assert(is_binary(Bin)).

serialize_map_test() ->
    Bin = beamtalk_binary:'serialize:'(#{key => value}),
    ?assert(is_binary(Bin)).

serialize_atom_test() ->
    Bin = beamtalk_binary:'serialize:'(hello),
    ?assert(is_binary(Bin)).

%%% ============================================================================
%%% deserialize:/1
%%% ============================================================================

deserialize_integer_test() ->
    Bin = erlang:term_to_binary(42),
    ?assertEqual(42, beamtalk_binary:'deserialize:'(Bin)).

deserialize_string_test() ->
    Bin = erlang:term_to_binary(<<"hello">>),
    ?assertEqual(<<"hello">>, beamtalk_binary:'deserialize:'(Bin)).

deserialize_list_test() ->
    Bin = erlang:term_to_binary([1, 2, 3]),
    ?assertEqual([1, 2, 3], beamtalk_binary:'deserialize:'(Bin)).

deserialize_map_test() ->
    Bin = erlang:term_to_binary(#{key => value}),
    ?assertEqual(#{key => value}, beamtalk_binary:'deserialize:'(Bin)).

deserialize_type_error_non_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:'deserialize:'(not_a_binary)
    ).

deserialize_invalid_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:'deserialize:'(<<0, 1, 2, 3>>)
    ).

%%% ============================================================================
%%% size:/1
%%% ============================================================================

size_empty_test() ->
    ?assertEqual(0, beamtalk_binary:'size:'(<<>>)).

size_nonempty_test() ->
    ?assertEqual(5, beamtalk_binary:'size:'(<<"hello">>)).

size_bytes_test() ->
    ?assertEqual(3, beamtalk_binary:'size:'(<<1, 2, 3>>)).

size_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:'size:'(not_a_binary)
    ).

%%% ============================================================================
%%% fromIolist:/1
%%% ============================================================================

from_iolist_simple_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:'fromIolist:'([<<"hello">>])).

from_iolist_nested_test() ->
    ?assertEqual(<<"helloworld">>, beamtalk_binary:'fromIolist:'([<<"hello">>, <<"world">>])).

from_iolist_with_integers_test() ->
    ?assertEqual(<<"ABC">>, beamtalk_binary:'fromIolist:'([65, 66, 67])).

from_iolist_deeply_nested_test() ->
    ?assertEqual(<<"abc">>, beamtalk_binary:'fromIolist:'([[<<"a">>], [<<"b">>, [<<"c">>]]])).

from_iolist_empty_test() ->
    ?assertEqual(<<>>, beamtalk_binary:'fromIolist:'([])).

from_iolist_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:'fromIolist:'(not_an_iolist)
    ).

%%% ============================================================================
%%% Round-trip tests
%%% ============================================================================

round_trip_integer_test() ->
    ?assertEqual(42, beamtalk_binary:'deserialize:'(beamtalk_binary:'serialize:'(42))).

round_trip_complex_test() ->
    Term = #{<<"key">> => [1, 2, #{nested => true}]},
    ?assertEqual(Term, beamtalk_binary:'deserialize:'(beamtalk_binary:'serialize:'(Term))).

%%% ============================================================================
%%% FFI no-colon aliases
%%% ============================================================================

serialize_alias_test() ->
    Bin = beamtalk_binary:serialize(42),
    ?assert(is_binary(Bin)).

deserialize_alias_test() ->
    Bin = erlang:term_to_binary(42),
    ?assertEqual(42, beamtalk_binary:deserialize(Bin)).

deserialize_alias_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:deserialize(not_a_binary)
    ).

size_alias_test() ->
    ?assertEqual(5, beamtalk_binary:size(<<"hello">>)).

from_iolist_alias_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:fromIolist([<<"hello">>])).

%%% ============================================================================
%%% Additional edge-case and regression tests (PR: remove instance methods)
%%% ============================================================================

%% Verify serialize produces non-empty output for all common term types.
serialize_produces_nonempty_binary_test() ->
    ?assertNotEqual(<<>>, beamtalk_binary:'serialize:'(42)),
    ?assertNotEqual(<<>>, beamtalk_binary:'serialize:'(<<"hello">>)),
    ?assertNotEqual(<<>>, beamtalk_binary:'serialize:'([])),
    ?assertNotEqual(<<>>, beamtalk_binary:'serialize:'(hello)).

%% Verify deserialize round-trips an atom correctly.
round_trip_atom_test() ->
    ?assertEqual(ok, beamtalk_binary:'deserialize:'(beamtalk_binary:'serialize:'(ok))).

%% Verify deserialize round-trips a tuple correctly.
round_trip_tuple_test() ->
    Term = {ok, <<"data">>, 123},
    ?assertEqual(Term, beamtalk_binary:'deserialize:'(beamtalk_binary:'serialize:'(Term))).

%% Verify that size: correctly counts bytes for multi-byte UTF-8 characters.
size_multibyte_utf8_test() ->
    %% "é" in UTF-8 is 2 bytes (U+00E9: 0xC3 0xA9)
    ?assertEqual(2, beamtalk_binary:'size:'(<<"é"/utf8>>)).

%% size: returns byte count, not character count.
size_unicode_string_bytes_test() ->
    %% "hello" is 5 bytes, each ASCII character is 1 byte
    ?assertEqual(5, beamtalk_binary:'size:'(<<"hello">>)),
    %% empty binary is 0 bytes
    ?assertEqual(0, beamtalk_binary:'size:'(<<>>)).

%% Boundary: fromIolist: with a single binary (not wrapped in a list).
from_iolist_single_binary_test() ->
    ?assertEqual(<<"hi">>, beamtalk_binary:'fromIolist:'([<<"hi">>])).

%% Boundary: fromIolist: with mixed integers and binaries.
from_iolist_mixed_integers_and_binaries_test() ->
    %% 104 = $h, 105 = $i
    ?assertEqual(<<"Ahi">>, beamtalk_binary:'fromIolist:'([65, <<"hi">>])).

%% Regression: deserialize: rejects truncated/malformed ETF binaries.
%% ETF binaries that start with the magic byte (131) but are otherwise
%% truncated should still raise a type_error, not crash the runtime.
deserialize_truncated_etf_test() ->
    %% <<131>> alone is a valid magic byte but not a complete term — malformed.
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:'deserialize:'(<<131>>)
    ).

%% FFI alias: size/1 delegates correctly and is the same as size:/1.
size_alias_equals_colon_form_test() ->
    Bin = <<"beamtalk">>,
    ?assertEqual(beamtalk_binary:'size:'(Bin), beamtalk_binary:size(Bin)).