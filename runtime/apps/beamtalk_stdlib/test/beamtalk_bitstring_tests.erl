%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bitstring_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_bitstring (ADR 0141, BT-4139) and the `bitSize`
it shares with beamtalk_binary.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

bit_size_test() ->
    ?assertEqual(3, beamtalk_bitstring:bitSize(<<2:3>>)),
    ?assertEqual(16, beamtalk_binary:bitSize(<<1, 2>>)),
    ?assertEqual(16, beamtalk_binary:bitSize({beamtalk_string, <<"é"/utf8>>})).

to_bits_test() ->
    ?assertEqual([0, 1, 0], beamtalk_bitstring:toBits(<<2:3>>)),
    ?assertEqual([1, 0, 0, 0, 0, 0, 0, 0, 1], beamtalk_bitstring:toBits(<<128, 1:1>>)).

concat_crosses_to_binary_when_aligned_test() ->
    ?assertEqual(<<255>>, beamtalk_bitstring:concat(<<1:1>>, <<127:7>>)),
    ?assertEqual(<<3:2>>, beamtalk_bitstring:concat(<<1:1>>, <<1:1>>)).

concat_string_joins_bytes_test() ->
    ?assertEqual(<<2:3, "A">>, beamtalk_bitstring:concat(<<2:3>>, {beamtalk_string, <<"A">>})).

concat_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Bitstring'}},
        beamtalk_bitstring:concat(<<2:3>>, 42)
    ).

as_binary_is_unaligned_error_test() ->
    #{'isOk' := IsOk} = beamtalk_bitstring:asBinary(<<2:3>>),
    ?assertEqual(false, IsOk).

print_string_test() ->
    ?assertEqual({beamtalk_string, <<"<<2:3>>">>}, beamtalk_bitstring:printString(<<2:3>>)),
    ?assertEqual({beamtalk_string, <<"<<2:3>>">>}, beamtalk_bitstring:displayString(<<2:3>>)).

binary_concat_bitstring_is_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Binary'}},
        beamtalk_binary:concat(<<255>>, <<2:3>>)
    ).

reflection_on_a_bitstring_test() ->
    ?assertEqual([], beamtalk_reflection:field_names(<<2:3>>)),
    ?assertEqual(false, beamtalk_reflection:has_field(x, <<2:3>>)),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = immutable_value, class = 'Bitstring'}},
        beamtalk_reflection:read_field(x, <<2:3>>)
    ),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = immutable_value}},
        beamtalk_reflection:write_field(x, 1, <<2:3>>)
    ),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = immutable_value}},
        beamtalk_reflection:clear_field(x, <<2:3>>)
    ),
    ?assertEqual(<<"<<2:3>>">>, beamtalk_reflection:inspect_string(<<2:3>>)).
