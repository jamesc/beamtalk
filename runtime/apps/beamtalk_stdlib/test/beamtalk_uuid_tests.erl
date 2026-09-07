%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_uuid_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_uuid (RFC 9562).

Tests cover:
- 'v4'/0 — structure, version nibble, variant bits, uniqueness
- 'v7'/0 — structure, version nibble, monotonic ordering
- 'fromString:'/1 — canonical parse, case normalization, malformed rejection, type guard
- 'isValid:'/1 — true/false for valid/invalid strings, type guard
- 'asString'/1 — canonical lowercase output, format, length
- 'asBinary'/1 — raw 16-byte binary
- version/1 — extracts the version nibble
- 'printString'/1 — "Uuid(...)" format
- Comparison operators '<', '>', '=<', '>=' — correct ordering, type errors
- FFI shims: fromString/1, isValid/1, lt/2, gt/2, lte/2, gte/2
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% A well-known v4 UUID used as a fixed test vector throughout.
-define(KNOWN_STR, <<"550e8400-e29b-41d4-a716-446655440000">>).
-define(KNOWN_STR_UPPER, <<"550E8400-E29B-41D4-A716-446655440000">>).
-define(MIN_STR, <<"00000000-0000-7000-8000-000000000000">>).
-define(MAX_STR, <<"ffffffff-ffff-7fff-bfff-ffffffffffff">>).

%% Unwrap a successful fromString: result.
parse(Str) ->
    #{'isOk' := true, 'okValue' := Uuid} = beamtalk_uuid:'fromString:'(Str),
    Uuid.

%%% ============================================================================
%%% 'v4'/0 — structure and version
%%% ============================================================================

v4_returns_uuid_map_test() ->
    Uuid = beamtalk_uuid:'v4'(),
    ?assertMatch(#{'$beamtalk_class' := 'Uuid', bytes := <<_:128>>}, Uuid).

v4_version_nibble_is_4_test() ->
    ?assertEqual(4, beamtalk_uuid:version(beamtalk_uuid:'v4'())).

v4_variant_bits_are_rfc_9562_test() ->
    %% The character at position 20 (1-based) of the 36-char canonical string
    %% is the variant nibble; RFC 9562 requires it to be one of 8, 9, a, b.
    VariantChar = binary:at(beamtalk_uuid:'asString'(beamtalk_uuid:'v4'()), 19),
    ?assert(lists:member(VariantChar, "89ab")).

v4_uniqueness_test() ->
    A = beamtalk_uuid:'v4'(),
    B = beamtalk_uuid:'v4'(),
    ?assertNotEqual(A, B).

%%% ============================================================================
%%% 'v7'/0 — structure and ordering
%%% ============================================================================

v7_returns_uuid_map_test() ->
    Uuid = beamtalk_uuid:'v7'(),
    ?assertMatch(#{'$beamtalk_class' := 'Uuid', bytes := <<_:128>>}, Uuid).

v7_version_nibble_is_7_test() ->
    ?assertEqual(7, beamtalk_uuid:version(beamtalk_uuid:'v7'())).

v7_monotonic_ordering_test() ->
    A = beamtalk_uuid:'v7'(),
    timer:sleep(5),
    B = beamtalk_uuid:'v7'(),
    ?assert(beamtalk_uuid:'<'(A, B)),
    ?assert(beamtalk_uuid:'>'(B, A)).

%%% ============================================================================
%%% 'fromString:'/1 — happy path
%%% ============================================================================

from_string_parses_canonical_lowercase_test() ->
    Result = beamtalk_uuid:'fromString:'(?KNOWN_STR),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, Result),
    #{'okValue' := Uuid} = Result,
    ?assertMatch(#{'$beamtalk_class' := 'Uuid', bytes := <<_:128>>}, Uuid).

from_string_case_insensitive_test() ->
    Result = beamtalk_uuid:'fromString:'(?KNOWN_STR_UPPER),
    ?assertMatch(#{'isOk' := true}, Result).

from_string_canonicalizes_to_lowercase_test() ->
    Uuid = parse(?KNOWN_STR_UPPER),
    ?assertEqual(?KNOWN_STR, beamtalk_uuid:'asString'(Uuid)).

from_string_version_4_roundtrip_test() ->
    Uuid = parse(?KNOWN_STR),
    ?assertEqual(?KNOWN_STR, beamtalk_uuid:'asString'(Uuid)).

%%% ============================================================================
%%% 'fromString:'/1 — rejection of malformed inputs
%%% ============================================================================

from_string_rejects_trailing_newline_test() ->
    Result = beamtalk_uuid:'fromString:'(<<(?KNOWN_STR)/binary, "\n">>),
    ?assertMatch(#{'isOk' := false}, Result).

from_string_rejects_no_hyphens_test() ->
    Result = beamtalk_uuid:'fromString:'(<<"550e8400e29b41d4a716446655440000">>),
    ?assertMatch(#{'isOk' := false}, Result).

from_string_rejects_short_string_test() ->
    Result = beamtalk_uuid:'fromString:'(<<"550e8400-e29b-41d4-a716-44665544000">>),
    ?assertMatch(#{'isOk' := false}, Result).

from_string_rejects_invalid_hex_chars_test() ->
    Result = beamtalk_uuid:'fromString:'(<<"gggggggg-gggg-gggg-gggg-gggggggggggg">>),
    ?assertMatch(#{'isOk' := false}, Result).

from_string_rejects_empty_string_test() ->
    Result = beamtalk_uuid:'fromString:'(<<>>),
    ?assertMatch(#{'isOk' := false}, Result).

from_string_type_error_for_non_binary_test() ->
    try
        beamtalk_uuid:'fromString:'(12345),
        ?assert(false, "'fromString:' with non-binary should raise type_error")
    catch
        error:Caught ->
            ?assertMatch(
                #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
                Caught
            ),
            #{error := Inner} = Caught,
            ?assertEqual('Uuid', Inner#beamtalk_error.class),
            ?assertEqual('fromString:', Inner#beamtalk_error.selector)
    end.

%%% ============================================================================
%%% 'isValid:'/1
%%% ============================================================================

is_valid_true_for_canonical_lowercase_test() ->
    ?assert(beamtalk_uuid:'isValid:'(?KNOWN_STR)).

is_valid_true_for_canonical_uppercase_test() ->
    ?assert(beamtalk_uuid:'isValid:'(?KNOWN_STR_UPPER)).

is_valid_false_for_malformed_test() ->
    ?assertNot(beamtalk_uuid:'isValid:'(<<"not-a-uuid">>)),
    ?assertNot(beamtalk_uuid:'isValid:'(<<(?KNOWN_STR)/binary, "\n">>)),
    ?assertNot(beamtalk_uuid:'isValid:'(<<>>)).

is_valid_type_error_for_non_binary_test() ->
    try
        beamtalk_uuid:'isValid:'(42),
        ?assert(false, "'isValid:' with non-binary should raise type_error")
    catch
        error:Caught ->
            ?assertMatch(
                #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
                Caught
            ),
            #{error := Inner} = Caught,
            ?assertEqual('Uuid', Inner#beamtalk_error.class),
            ?assertEqual('isValid:', Inner#beamtalk_error.selector)
    end.

%%% ============================================================================
%%% 'asString'/1 — format
%%% ============================================================================

as_string_length_is_36_test() ->
    Uuid = parse(?KNOWN_STR),
    ?assertEqual(36, byte_size(beamtalk_uuid:'asString'(Uuid))).

as_string_canonical_format_test() ->
    Uuid = parse(?KNOWN_STR),
    S = beamtalk_uuid:'asString'(Uuid),
    %% 8-4-4-4-12 structure with hyphens at positions 9, 14, 19, 24 (1-based).
    ?assertEqual($-, binary:at(S, 8)),
    ?assertEqual($-, binary:at(S, 13)),
    ?assertEqual($-, binary:at(S, 18)),
    ?assertEqual($-, binary:at(S, 23)).

as_string_all_lowercase_test() ->
    %% Parse an uppercase input and verify the output is lowercase.
    Uuid = parse(?KNOWN_STR_UPPER),
    S = beamtalk_uuid:'asString'(Uuid),
    ?assertEqual(?KNOWN_STR, S).

%%% ============================================================================
%%% 'asBinary'/1 — raw bytes
%%% ============================================================================

as_binary_is_16_bytes_test() ->
    Uuid = parse(?KNOWN_STR),
    ?assertEqual(16, byte_size(beamtalk_uuid:'asBinary'(Uuid))).

as_binary_roundtrip_test() ->
    %% Bytes are the canonical representation; asString(make) == original.
    Uuid = parse(?KNOWN_STR),
    Bytes = beamtalk_uuid:'asBinary'(Uuid),
    ?assert(is_binary(Bytes)),
    ?assertEqual(16, byte_size(Bytes)).

%%% ============================================================================
%%% version/1
%%% ============================================================================

version_of_known_v4_uuid_is_4_test() ->
    ?assertEqual(4, beamtalk_uuid:version(parse(?KNOWN_STR))).

version_of_v7_uuid_is_7_test() ->
    ?assertEqual(7, beamtalk_uuid:version(beamtalk_uuid:'v7'())).

%%% ============================================================================
%%% 'printString'/1
%%% ============================================================================

print_string_format_test() ->
    Uuid = parse(?KNOWN_STR),
    ?assertEqual(
        <<"Uuid(550e8400-e29b-41d4-a716-446655440000)">>,
        beamtalk_uuid:'printString'(Uuid)
    ).

%%% ============================================================================
%%% Comparison operators
%%% ============================================================================

lt_orders_correctly_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:'<'(Lo, Hi)),
    ?assertNot(beamtalk_uuid:'<'(Hi, Lo)),
    ?assertNot(beamtalk_uuid:'<'(Lo, Lo)).

gt_orders_correctly_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:'>'(Hi, Lo)),
    ?assertNot(beamtalk_uuid:'>'(Lo, Hi)),
    ?assertNot(beamtalk_uuid:'>'(Lo, Lo)).

lte_orders_correctly_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:'=<'(Lo, Hi)),
    ?assert(beamtalk_uuid:'=<'(Lo, Lo)),
    ?assertNot(beamtalk_uuid:'=<'(Hi, Lo)).

gte_orders_correctly_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:'>='(Hi, Lo)),
    ?assert(beamtalk_uuid:'>='(Lo, Lo)),
    ?assertNot(beamtalk_uuid:'>='(Lo, Hi)).

comparison_type_error_for_non_uuid_test() ->
    Uuid = parse(?KNOWN_STR),
    lists:foreach(
        fun({Fn, Args}) ->
            try
                erlang:apply(beamtalk_uuid, Fn, Args),
                ?assert(false, "expected type_error")
            catch
                error:Caught ->
                    ?assertMatch(
                        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
                        Caught
                    )
            end
        end,
        [{'<', [Uuid, 5]}, {'>', [Uuid, 5]}, {'=<', [Uuid, 5]}, {'>=', [Uuid, 5]}]
    ).

%%% ============================================================================
%%% FFI shims — delegate to canonical colon-suffixed functions
%%% ============================================================================

from_string_shim_returns_ok_result_test() ->
    Result = beamtalk_uuid:fromString(?KNOWN_STR),
    ?assertMatch(#{'isOk' := true, 'okValue' := #{'$beamtalk_class' := 'Uuid'}}, Result).

from_string_shim_returns_error_for_invalid_test() ->
    Result = beamtalk_uuid:fromString(<<"bad">>),
    ?assertMatch(#{'isOk' := false}, Result).

is_valid_shim_delegates_test() ->
    ?assert(beamtalk_uuid:isValid(?KNOWN_STR)),
    ?assertNot(beamtalk_uuid:isValid(<<"bad">>)).

lt_shim_delegates_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:lt(Lo, Hi)),
    ?assertNot(beamtalk_uuid:lt(Hi, Lo)).

gt_shim_delegates_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:gt(Hi, Lo)),
    ?assertNot(beamtalk_uuid:gt(Lo, Hi)).

lte_shim_delegates_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:lte(Lo, Hi)),
    ?assert(beamtalk_uuid:lte(Lo, Lo)).

gte_shim_delegates_test() ->
    Lo = parse(?MIN_STR),
    Hi = parse(?MAX_STR),
    ?assert(beamtalk_uuid:gte(Hi, Lo)),
    ?assert(beamtalk_uuid:gte(Lo, Lo)).
