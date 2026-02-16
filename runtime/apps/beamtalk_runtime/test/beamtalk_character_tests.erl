%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_character module (BT-622).
%%
%% Tests edge cases and error paths in the character runtime module
%% that can't be easily reached from Beamtalk stdlib tests:
%% - Negative codepoints
%% - Surrogate range codepoints (0xD800-0xDFFF)
%% - Out-of-range codepoints (> 0x10FFFF)
%% - value/1 error path
-module(beamtalk_character_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% is_letter/1 Tests
%%% ============================================================================

is_letter_basic_test() ->
    ?assert(beamtalk_character:is_letter($A)),
    ?assert(beamtalk_character:is_letter($z)),
    ?assertNot(beamtalk_character:is_letter($0)),
    ?assertNot(beamtalk_character:is_letter($!)).

is_letter_negative_test() ->
    ?assertNot(beamtalk_character:is_letter(-1)),
    ?assertNot(beamtalk_character:is_letter(-100)).

is_letter_out_of_range_test() ->
    ?assertNot(beamtalk_character:is_letter(16#110000)),
    ?assertNot(beamtalk_character:is_letter(16#FFFFFF)).

is_letter_surrogate_test() ->
    ?assertNot(beamtalk_character:is_letter(16#D800)),
    ?assertNot(beamtalk_character:is_letter(16#DFFF)).

%%% ============================================================================
%%% is_digit/1 Tests
%%% ============================================================================

is_digit_basic_test() ->
    ?assert(beamtalk_character:is_digit($0)),
    ?assert(beamtalk_character:is_digit($9)),
    ?assertNot(beamtalk_character:is_digit($A)).

is_digit_negative_test() ->
    ?assertNot(beamtalk_character:is_digit(-1)).

is_digit_out_of_range_test() ->
    ?assertNot(beamtalk_character:is_digit(16#110000)).

%%% ============================================================================
%%% is_uppercase/1 Tests
%%% ============================================================================

is_uppercase_basic_test() ->
    ?assert(beamtalk_character:is_uppercase($A)),
    ?assert(beamtalk_character:is_uppercase($Z)),
    ?assertNot(beamtalk_character:is_uppercase($a)).

is_uppercase_negative_test() ->
    ?assertNot(beamtalk_character:is_uppercase(-1)).

is_uppercase_out_of_range_test() ->
    ?assertNot(beamtalk_character:is_uppercase(16#110000)).

%%% ============================================================================
%%% is_lowercase/1 Tests
%%% ============================================================================

is_lowercase_basic_test() ->
    ?assert(beamtalk_character:is_lowercase($a)),
    ?assert(beamtalk_character:is_lowercase($z)),
    ?assertNot(beamtalk_character:is_lowercase($A)).

is_lowercase_negative_test() ->
    ?assertNot(beamtalk_character:is_lowercase(-1)).

is_lowercase_out_of_range_test() ->
    ?assertNot(beamtalk_character:is_lowercase(16#110000)).

%%% ============================================================================
%%% is_whitespace/1 Tests
%%% ============================================================================

is_whitespace_basic_test() ->
    ?assert(beamtalk_character:is_whitespace($\t)),
    ?assert(beamtalk_character:is_whitespace($\n)),
    ?assert(beamtalk_character:is_whitespace($\r)),
    ?assert(beamtalk_character:is_whitespace(16#0B)),  % vertical tab
    ?assert(beamtalk_character:is_whitespace(16#0C)),  % form feed
    ?assert(beamtalk_character:is_whitespace($\ )),
    ?assertNot(beamtalk_character:is_whitespace($A)).

is_whitespace_negative_test() ->
    ?assertNot(beamtalk_character:is_whitespace(-1)).

is_whitespace_out_of_range_test() ->
    ?assertNot(beamtalk_character:is_whitespace(16#110000)).

%%% ============================================================================
%%% to_uppercase/1 Tests
%%% ============================================================================

to_uppercase_basic_test() ->
    ?assertEqual($A, beamtalk_character:to_uppercase($a)),
    ?assertEqual($Z, beamtalk_character:to_uppercase($z)),
    ?assertEqual($A, beamtalk_character:to_uppercase($A)).

to_uppercase_non_letter_test() ->
    ?assertEqual($0, beamtalk_character:to_uppercase($0)),
    ?assertEqual($!, beamtalk_character:to_uppercase($!)).

to_uppercase_negative_test() ->
    ?assertEqual(-1, beamtalk_character:to_uppercase(-1)).

to_uppercase_out_of_range_test() ->
    ?assertEqual(16#110000, beamtalk_character:to_uppercase(16#110000)).

%%% ============================================================================
%%% to_lowercase/1 Tests
%%% ============================================================================

to_lowercase_basic_test() ->
    ?assertEqual($a, beamtalk_character:to_lowercase($A)),
    ?assertEqual($z, beamtalk_character:to_lowercase($Z)),
    ?assertEqual($a, beamtalk_character:to_lowercase($a)).

to_lowercase_non_letter_test() ->
    ?assertEqual($0, beamtalk_character:to_lowercase($0)),
    ?assertEqual($!, beamtalk_character:to_lowercase($!)).

to_lowercase_negative_test() ->
    ?assertEqual(-1, beamtalk_character:to_lowercase(-1)).

to_lowercase_out_of_range_test() ->
    ?assertEqual(16#110000, beamtalk_character:to_lowercase(16#110000)).

%%% ============================================================================
%%% as_string/1 Tests
%%% ============================================================================

as_string_basic_test() ->
    ?assertEqual(<<"A">>, beamtalk_character:as_string($A)),
    ?assertEqual(<<"z">>, beamtalk_character:as_string($z)),
    ?assertEqual(<<"0">>, beamtalk_character:as_string($0)).

as_string_unicode_test() ->
    %% Multi-byte Unicode character
    ?assertEqual(<<"é"/utf8>>, beamtalk_character:as_string(16#E9)).

%%% ============================================================================
%%% print_string/1 Tests
%%% ============================================================================

print_string_basic_test() ->
    ?assertEqual(<<"$A">>, beamtalk_character:print_string($A)),
    ?assertEqual(<<"$z">>, beamtalk_character:print_string($z)),
    ?assertEqual(<<"$0">>, beamtalk_character:print_string($0)).

%%% ============================================================================
%%% as_string/1 and print_string/1 Edge Cases
%%% ============================================================================

%% Negative and out-of-range codepoints raise function_clause
%% (guards reject them — no matching clause exists)
as_string_negative_test() ->
    ?assertError(function_clause, beamtalk_character:as_string(-1)).

as_string_out_of_range_test() ->
    ?assertError(function_clause, beamtalk_character:as_string(16#110000)).

print_string_negative_test() ->
    ?assertError(function_clause, beamtalk_character:print_string(-1)).

print_string_out_of_range_test() ->
    ?assertError(function_clause, beamtalk_character:print_string(16#110000)).

%%% ============================================================================
%%% value/1 Tests
%%% ============================================================================

value_valid_test() ->
    ?assertEqual($A, beamtalk_character:value($A)),
    ?assertEqual(0, beamtalk_character:value(0)),
    ?assertEqual(16#D7FF, beamtalk_character:value(16#D7FF)),
    ?assertEqual(16#E000, beamtalk_character:value(16#E000)),
    ?assertEqual(16#10FFFF, beamtalk_character:value(16#10FFFF)).

value_surrogate_error_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Character', selector = 'value:'}},
                 beamtalk_character:value(16#D800)),
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Character', selector = 'value:'}},
                 beamtalk_character:value(16#DFFF)).

value_negative_error_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Character', selector = 'value:'}},
                 beamtalk_character:value(-1)).

value_out_of_range_error_test() ->
    ?assertError(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Character', selector = 'value:'}},
                 beamtalk_character:value(16#110000)).
