%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_string_ops_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% at/2
%%% ============================================================================

at_first_char_test() ->
    ?assertEqual(<<"h">>, beamtalk_string_ops:at(<<"hello">>, 1)).

at_last_char_test() ->
    ?assertEqual(<<"o">>, beamtalk_string_ops:at(<<"hello">>, 5)).

at_middle_char_test() ->
    ?assertEqual(<<"l">>, beamtalk_string_ops:at(<<"hello">>, 3)).

at_utf8_grapheme_test() ->
    ?assertEqual(<<"é"/utf8>>, beamtalk_string_ops:at(<<"café"/utf8>>, 4)).

at_emoji_test() ->
    ?assertEqual(<<"👋"/utf8>>, beamtalk_string_ops:at(<<"hi👋"/utf8>>, 3)).

at_out_of_bounds_test() ->
    ?assertError(_, beamtalk_string_ops:at(<<"hi">>, 10)).

at_zero_index_test() ->
    ?assertError(_, beamtalk_string_ops:at(<<"hi">>, 0)).

at_negative_index_test() ->
    ?assertError(_, beamtalk_string_ops:at(<<"hi">>, -1)).

%%% ============================================================================
%%% capitalize/1
%%% ============================================================================

capitalize_lowercase_test() ->
    ?assertEqual(<<"Hello">>, beamtalk_string_ops:capitalize(<<"hello">>)).

capitalize_already_upper_test() ->
    ?assertEqual(<<"Hello">>, beamtalk_string_ops:capitalize(<<"Hello">>)).

capitalize_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string_ops:capitalize(<<>>)).

capitalize_single_char_test() ->
    ?assertEqual(<<"A">>, beamtalk_string_ops:capitalize(<<"a">>)).

capitalize_utf8_test() ->
    ?assertEqual(<<"Über"/utf8>>, beamtalk_string_ops:capitalize(<<"über"/utf8>>)).

%%% ============================================================================
%%% reverse/1
%%% ============================================================================

reverse_simple_test() ->
    ?assertEqual(<<"olleh">>, beamtalk_string_ops:reverse(<<"hello">>)).

reverse_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string_ops:reverse(<<>>)).

reverse_single_char_test() ->
    ?assertEqual(<<"a">>, beamtalk_string_ops:reverse(<<"a">>)).

reverse_utf8_test() ->
    ?assertEqual(<<"éfac"/utf8>>, beamtalk_string_ops:reverse(<<"café"/utf8>>)).

%%% ============================================================================
%%% includes/2
%%% ============================================================================

includes_found_test() ->
    ?assertEqual(true, beamtalk_string_ops:includes(<<"hello">>, <<"ell">>)).

includes_not_found_test() ->
    ?assertEqual(false, beamtalk_string_ops:includes(<<"hello">>, <<"xyz">>)).

includes_empty_substr_test() ->
    ?assertEqual(true, beamtalk_string_ops:includes(<<"hello">>, <<>>)).

includes_full_match_test() ->
    ?assertEqual(true, beamtalk_string_ops:includes(<<"hello">>, <<"hello">>)).

%%% ============================================================================
%%% starts_with/2
%%% ============================================================================

starts_with_true_test() ->
    ?assertEqual(true, beamtalk_string_ops:starts_with(<<"hello">>, <<"hel">>)).

starts_with_false_test() ->
    ?assertEqual(false, beamtalk_string_ops:starts_with(<<"hello">>, <<"world">>)).

starts_with_empty_test() ->
    ?assertEqual(true, beamtalk_string_ops:starts_with(<<"hello">>, <<>>)).

starts_with_full_test() ->
    ?assertEqual(true, beamtalk_string_ops:starts_with(<<"hello">>, <<"hello">>)).

starts_with_longer_prefix_test() ->
    ?assertEqual(false, beamtalk_string_ops:starts_with(<<"hi">>, <<"hello">>)).

%%% ============================================================================
%%% ends_with/2
%%% ============================================================================

ends_with_true_test() ->
    ?assertEqual(true, beamtalk_string_ops:ends_with(<<"hello">>, <<"llo">>)).

ends_with_false_test() ->
    ?assertEqual(false, beamtalk_string_ops:ends_with(<<"hello">>, <<"world">>)).

ends_with_empty_test() ->
    ?assertEqual(true, beamtalk_string_ops:ends_with(<<"hello">>, <<>>)).

ends_with_full_test() ->
    ?assertEqual(true, beamtalk_string_ops:ends_with(<<"hello">>, <<"hello">>)).

ends_with_longer_suffix_test() ->
    ?assertEqual(false, beamtalk_string_ops:ends_with(<<"hi">>, <<"hello">>)).

%%% ============================================================================
%%% index_of/2
%%% ============================================================================

index_of_found_test() ->
    ?assertEqual(1, beamtalk_string_ops:index_of(<<"hello">>, <<"h">>)).

index_of_middle_test() ->
    ?assertEqual(3, beamtalk_string_ops:index_of(<<"hello">>, <<"ll">>)).

index_of_not_found_test() ->
    ?assertEqual(nil, beamtalk_string_ops:index_of(<<"hello">>, <<"xyz">>)).

index_of_empty_substr_test() ->
    ?assertEqual(nil, beamtalk_string_ops:index_of(<<"hello">>, <<>>)).

index_of_utf8_test() ->
    %% "café" - 'f' is at grapheme position 3
    ?assertEqual(3, beamtalk_string_ops:index_of(<<"café"/utf8>>, <<"f">>)).

%%% ============================================================================
%%% split_on/2
%%% ============================================================================

split_on_comma_test() ->
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>],
                 beamtalk_string_ops:split_on(<<"a,b,c">>, <<",">>)).

split_on_no_match_test() ->
    ?assertEqual([<<"hello">>],
                 beamtalk_string_ops:split_on(<<"hello">>, <<",">>)).

split_on_multi_char_test() ->
    ?assertEqual([<<"a">>, <<"b">>],
                 beamtalk_string_ops:split_on(<<"a::b">>, <<"::">>)).

split_on_empty_pattern_test() ->
    ?assertEqual([<<"hello">>],
                 beamtalk_string_ops:split_on(<<"hello">>, <<>>)).

%%% ============================================================================
%%% repeat/2
%%% ============================================================================

repeat_test() ->
    ?assertEqual(<<"abcabcabc">>, beamtalk_string_ops:repeat(<<"abc">>, 3)).

repeat_zero_test() ->
    ?assertEqual(<<>>, beamtalk_string_ops:repeat(<<"abc">>, 0)).

repeat_one_test() ->
    ?assertEqual(<<"abc">>, beamtalk_string_ops:repeat(<<"abc">>, 1)).

%%% ============================================================================
%%% as_list/1
%%% ============================================================================

as_list_simple_test() ->
    ?assertEqual([<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>],
                 beamtalk_string_ops:as_list(<<"hello">>)).

as_list_empty_test() ->
    ?assertEqual([], beamtalk_string_ops:as_list(<<>>)).

as_list_utf8_test() ->
    ?assertEqual([<<"c">>, <<"a">>, <<"f">>, <<"é"/utf8>>],
                 beamtalk_string_ops:as_list(<<"café"/utf8>>)).

%%% ============================================================================
%%% each/2
%%% ============================================================================

each_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_string_ops:each(<<"hi">>, fun(_) -> ok end)).

each_calls_block_test() ->
    Ref = make_ref(),
    Self = self(),
    beamtalk_string_ops:each(<<"ab">>, fun(G) -> Self ! {Ref, G} end),
    ?assertEqual(<<"a">>, receive {Ref, M1} -> M1 after 100 -> timeout end),
    ?assertEqual(<<"b">>, receive {Ref, M2} -> M2 after 100 -> timeout end).

%%% ============================================================================
%%% collect/2
%%% ============================================================================

collect_uppercase_test() ->
    ?assertEqual([<<"H">>, <<"I">>],
                 beamtalk_string_ops:collect(<<"hi">>, fun string:uppercase/1)).

collect_empty_test() ->
    ?assertEqual([], beamtalk_string_ops:collect(<<>>, fun(X) -> X end)).

%%% ============================================================================
%%% select/2
%%% ============================================================================

select_vowels_test() ->
    Vowels = [<<"a">>, <<"e">>, <<"i">>, <<"o">>, <<"u">>],
    ?assertEqual(<<"eoo">>,
                 beamtalk_string_ops:select(<<"hello world">>,
                    fun(G) -> lists:member(G, Vowels) end)).

select_all_test() ->
    ?assertEqual(<<"hi">>,
                 beamtalk_string_ops:select(<<"hi">>, fun(_) -> true end)).

select_none_test() ->
    ?assertEqual(<<>>,
                 beamtalk_string_ops:select(<<"hi">>, fun(_) -> false end)).
