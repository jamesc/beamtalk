%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_binary_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_binary module (BT-1554, BT-1591).

Tests cover class methods (serialize:/1, deserialize:/1, size:/1, fromIolist:/1),
FFI no-colon aliases, and instance methods (do/2, at/2, byte_at/2, byte_size/1,
part/3, concat/2, to_bytes/1, from_bytes/1, as_string/1, as_string_unchecked/1,
print_string/1, deserialize_with_used/1).
""".

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
%%% do/2 — iterate over bytes
%%% ============================================================================

do_basic_test() ->
    Self = self(),
    beamtalk_binary:do(<<1, 2, 3>>, fun(B) -> Self ! B end),
    ?assertEqual(
        1,
        receive
            M1 -> M1
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        2,
        receive
            M2 -> M2
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        3,
        receive
            M3 -> M3
        after 100 -> timeout
        end
    ).

do_empty_test() ->
    ?assertEqual(ok, beamtalk_binary:do(<<>>, fun(_) -> error(should_not_run) end)).

do_single_byte_test() ->
    Self = self(),
    beamtalk_binary:do(<<255>>, fun(B) -> Self ! B end),
    ?assertEqual(
        255,
        receive
            M -> M
        after 100 -> timeout
        end
    ).

do_type_error_not_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:do(not_a_binary, fun(_) -> ok end)
    ).

do_type_error_not_fun_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:do(<<1>>, not_a_fun)
    ).

%%% ============================================================================
%%% at/2 — 1-based byte access
%%% ============================================================================

at_first_byte_test() ->
    ?assertEqual(104, beamtalk_binary:at(<<"hello">>, 1)).

at_last_byte_test() ->
    ?assertEqual(111, beamtalk_binary:at(<<"hello">>, 5)).

at_single_byte_test() ->
    ?assertEqual(42, beamtalk_binary:at(<<42>>, 1)).

at_index_zero_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:at(<<"hello">>, 0)
    ).

at_index_too_large_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:at(<<"hello">>, 6)
    ).

at_type_error_not_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:at(not_a_binary, 1)
    ).

at_type_error_not_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:at(<<"hello">>, <<"1">>)
    ).

at_empty_binary_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:at(<<>>, 1)
    ).

%%% ============================================================================
%%% byte_at/2 — 0-based byte access
%%% ============================================================================

byte_at_first_byte_test() ->
    ?assertEqual(104, beamtalk_binary:byte_at(<<"hello">>, 0)).

byte_at_last_byte_test() ->
    ?assertEqual(111, beamtalk_binary:byte_at(<<"hello">>, 4)).

byte_at_single_byte_test() ->
    ?assertEqual(42, beamtalk_binary:byte_at(<<42>>, 0)).

byte_at_negative_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:byte_at(<<"hello">>, -1)
    ).

byte_at_too_large_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:byte_at(<<"hello">>, 5)
    ).

byte_at_type_error_not_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:byte_at(not_a_binary, 0)
    ).

byte_at_type_error_not_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:byte_at(<<1>>, <<"0">>)
    ).

byte_at_empty_binary_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:byte_at(<<>>, 0)
    ).

%%% ============================================================================
%%% byte_size/1 — byte count
%%% ============================================================================

byte_size_empty_test() ->
    ?assertEqual(0, beamtalk_binary:byte_size(<<>>)).

byte_size_nonempty_test() ->
    ?assertEqual(5, beamtalk_binary:byte_size(<<"hello">>)).

byte_size_raw_bytes_test() ->
    ?assertEqual(3, beamtalk_binary:byte_size(<<1, 2, 3>>)).

byte_size_multibyte_utf8_test() ->
    %% UTF-8 "é" is 2 bytes
    ?assertEqual(2, beamtalk_binary:byte_size(<<"é"/utf8>>)).

byte_size_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:byte_size(not_a_binary)
    ).

%%% ============================================================================
%%% part/3 — zero-copy slice
%%% ============================================================================

part_basic_test() ->
    ?assertEqual(<<"ell">>, beamtalk_binary:part(<<"hello">>, 1, 3)).

part_from_start_test() ->
    ?assertEqual(<<"hel">>, beamtalk_binary:part(<<"hello">>, 0, 3)).

part_to_end_test() ->
    ?assertEqual(<<"lo">>, beamtalk_binary:part(<<"hello">>, 3, 2)).

part_empty_slice_test() ->
    ?assertEqual(<<>>, beamtalk_binary:part(<<"hello">>, 0, 0)).

part_full_binary_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:part(<<"hello">>, 0, 5)).

part_out_of_bounds_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_binary:part(<<"hello">>, 3, 5)
    ).

part_type_error_not_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:part(not_a_binary, 0, 1)
    ).

part_type_error_not_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:part(<<"hello">>, <<"0">>, 1)
    ).

%%% ============================================================================
%%% concat/2 — concatenation
%%% ============================================================================

concat_basic_test() ->
    ?assertEqual(<<"helloworld">>, beamtalk_binary:concat(<<"hello">>, <<"world">>)).

concat_empty_left_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:concat(<<>>, <<"hello">>)).

concat_empty_right_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:concat(<<"hello">>, <<>>)).

concat_both_empty_test() ->
    ?assertEqual(<<>>, beamtalk_binary:concat(<<>>, <<>>)).

concat_raw_bytes_test() ->
    ?assertEqual(<<1, 2, 3, 4>>, beamtalk_binary:concat(<<1, 2>>, <<3, 4>>)).

concat_type_error_left_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:concat(not_a_binary, <<"hello">>)
    ).

concat_type_error_right_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:concat(<<"hello">>, not_a_binary)
    ).

%%% ============================================================================
%%% to_bytes/1 — binary to byte list
%%% ============================================================================

to_bytes_basic_test() ->
    ?assertEqual([104, 101, 108, 108, 111], beamtalk_binary:to_bytes(<<"hello">>)).

to_bytes_empty_test() ->
    ?assertEqual([], beamtalk_binary:to_bytes(<<>>)).

to_bytes_single_byte_test() ->
    ?assertEqual([255], beamtalk_binary:to_bytes(<<255>>)).

to_bytes_raw_bytes_test() ->
    ?assertEqual([0, 127, 255], beamtalk_binary:to_bytes(<<0, 127, 255>>)).

to_bytes_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:to_bytes(not_a_binary)
    ).

%%% ============================================================================
%%% from_bytes/1 — byte list to binary
%%% ============================================================================

from_bytes_basic_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:from_bytes([104, 101, 108, 108, 111])).

from_bytes_empty_test() ->
    ?assertEqual(<<>>, beamtalk_binary:from_bytes([])).

from_bytes_single_byte_test() ->
    ?assertEqual(<<255>>, beamtalk_binary:from_bytes([255])).

from_bytes_type_error_not_list_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:from_bytes(not_a_list)
    ).

from_bytes_type_error_invalid_bytes_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:from_bytes([256])
    ).

from_bytes_round_trip_test() ->
    Bin = <<0, 42, 127, 200, 255>>,
    ?assertEqual(Bin, beamtalk_binary:from_bytes(beamtalk_binary:to_bytes(Bin))).

%%% ============================================================================
%%% as_string/1 — UTF-8 validation
%%% ============================================================================

as_string_valid_ascii_test() ->
    ?assertEqual({ok, <<"hello">>}, beamtalk_binary:as_string(<<"hello">>)).

as_string_valid_utf8_test() ->
    Bin = <<"héllo"/utf8>>,
    ?assertEqual({ok, Bin}, beamtalk_binary:as_string(Bin)).

as_string_empty_test() ->
    ?assertEqual({ok, <<>>}, beamtalk_binary:as_string(<<>>)).

as_string_invalid_utf8_test() ->
    %% 0xFF is not valid UTF-8
    Result = beamtalk_binary:as_string(<<255>>),
    ?assertMatch({error, {invalid_utf8, _}}, Result).

as_string_partial_utf8_test() ->
    %% Start of a 2-byte UTF-8 sequence without continuation
    Result = beamtalk_binary:as_string(<<192>>),
    ?assertMatch({error, {invalid_utf8, _}}, Result).

as_string_mixed_valid_invalid_test() ->
    %% Valid ASCII followed by invalid byte
    Result = beamtalk_binary:as_string(<<"hello", 255>>),
    ?assertMatch({error, {invalid_utf8, 5}}, Result).

as_string_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:as_string(not_a_binary)
    ).

%%% ============================================================================
%%% as_string_unchecked/1
%%% ============================================================================

as_string_unchecked_valid_test() ->
    ?assertEqual(<<"hello">>, beamtalk_binary:as_string_unchecked(<<"hello">>)).

as_string_unchecked_invalid_utf8_test() ->
    %% Returns the binary as-is, no validation
    Bin = <<255, 254>>,
    ?assertEqual(Bin, beamtalk_binary:as_string_unchecked(Bin)).

as_string_unchecked_empty_test() ->
    ?assertEqual(<<>>, beamtalk_binary:as_string_unchecked(<<>>)).

as_string_unchecked_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:as_string_unchecked(not_a_binary)
    ).

%%% ============================================================================
%%% print_string/1
%%% ============================================================================

print_string_valid_utf8_test() ->
    ?assertEqual(<<"\"hello\"">>, beamtalk_binary:print_string(<<"hello">>)).

print_string_empty_test() ->
    ?assertEqual(<<"\"\"">>, beamtalk_binary:print_string(<<>>)).

print_string_invalid_utf8_test() ->
    %% Non-UTF-8 bytes get hex representation
    Result = beamtalk_binary:print_string(<<255, 0, 127>>),
    ?assertEqual(<<"<<FF 00 7F>>">>, Result).

print_string_with_quotes_test() ->
    %% Embedded quotes should be escaped
    ?assertEqual(<<"\"say \\\"hi\\\"\"">>, beamtalk_binary:print_string(<<"say \"hi\"">>)).

print_string_with_backslash_test() ->
    ?assertEqual(<<"\"a\\\\b\"">>, beamtalk_binary:print_string(<<"a\\b">>)).

print_string_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:print_string(not_a_binary)
    ).

%%% ============================================================================
%%% deserialize_with_used/1
%%% ============================================================================

deserialize_with_used_basic_test() ->
    Bin = erlang:term_to_binary(42),
    {Value, Used} = beamtalk_binary:deserialize_with_used(Bin),
    ?assertEqual(42, Value),
    ?assertEqual(erlang:byte_size(Bin), Used).

deserialize_with_used_with_trailing_data_test() ->
    Bin = erlang:term_to_binary(hello),
    Extended = <<Bin/binary, "extra data">>,
    {Value, Used} = beamtalk_binary:deserialize_with_used(Extended),
    ?assertEqual(hello, Value),
    ?assertEqual(erlang:byte_size(Bin), Used).

deserialize_with_used_invalid_binary_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:deserialize_with_used(<<0, 1, 2, 3>>)
    ).

deserialize_with_used_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_binary:deserialize_with_used(not_a_binary)
    ).
