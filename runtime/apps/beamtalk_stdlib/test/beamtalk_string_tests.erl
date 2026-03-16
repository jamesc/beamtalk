%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

%%% @doc EUnit tests for beamtalk_string module.
%%%
%%% Tests string operations: indexing, slicing, searching, casing, and trimming.

-module(beamtalk_string_tests).
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% at/2
%%% ============================================================================

at_first_char_test() ->
    ?assertEqual(<<"h">>, beamtalk_string:at(<<"hello">>, 1)).

at_last_char_test() ->
    ?assertEqual(<<"o">>, beamtalk_string:at(<<"hello">>, 5)).

at_middle_char_test() ->
    ?assertEqual(<<"l">>, beamtalk_string:at(<<"hello">>, 3)).

at_utf8_grapheme_test() ->
    ?assertEqual(<<"é"/utf8>>, beamtalk_string:at(<<"café"/utf8>>, 4)).

at_emoji_test() ->
    ?assertEqual(<<"👋"/utf8>>, beamtalk_string:at(<<"hi👋"/utf8>>, 3)).

at_out_of_bounds_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = index_out_of_bounds, class = 'String', selector = 'at:'}
        },
        beamtalk_string:at(<<"hi">>, 10)
    ).

at_zero_index_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = index_out_of_bounds, class = 'String', selector = 'at:'}
        },
        beamtalk_string:at(<<"hi">>, 0)
    ).

at_negative_index_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = index_out_of_bounds, class = 'String', selector = 'at:'}
        },
        beamtalk_string:at(<<"hi">>, -1)
    ).

%%% ============================================================================
%%% capitalize/1
%%% ============================================================================

capitalize_lowercase_test() ->
    ?assertEqual(<<"Hello">>, beamtalk_string:capitalize(<<"hello">>)).

capitalize_already_upper_test() ->
    ?assertEqual(<<"Hello">>, beamtalk_string:capitalize(<<"Hello">>)).

capitalize_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string:capitalize(<<>>)).

capitalize_single_char_test() ->
    ?assertEqual(<<"A">>, beamtalk_string:capitalize(<<"a">>)).

capitalize_utf8_test() ->
    ?assertEqual(<<"Über"/utf8>>, beamtalk_string:capitalize(<<"über"/utf8>>)).

%%% ============================================================================
%%% reverse/1
%%% ============================================================================

reverse_simple_test() ->
    ?assertEqual(<<"olleh">>, beamtalk_string:reverse(<<"hello">>)).

reverse_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string:reverse(<<>>)).

reverse_single_char_test() ->
    ?assertEqual(<<"a">>, beamtalk_string:reverse(<<"a">>)).

reverse_utf8_test() ->
    ?assertEqual(<<"éfac"/utf8>>, beamtalk_string:reverse(<<"café"/utf8>>)).

%%% ============================================================================
%%% includes/2
%%% ============================================================================

includes_found_test() ->
    ?assertEqual(true, beamtalk_string:includes(<<"hello">>, <<"ell">>)).

includes_not_found_test() ->
    ?assertEqual(false, beamtalk_string:includes(<<"hello">>, <<"xyz">>)).

includes_empty_substr_test() ->
    ?assertEqual(true, beamtalk_string:includes(<<"hello">>, <<>>)).

includes_full_match_test() ->
    ?assertEqual(true, beamtalk_string:includes(<<"hello">>, <<"hello">>)).

%%% ============================================================================
%%% starts_with/2
%%% ============================================================================

starts_with_true_test() ->
    ?assertEqual(true, beamtalk_string:starts_with(<<"hello">>, <<"hel">>)).

starts_with_false_test() ->
    ?assertEqual(false, beamtalk_string:starts_with(<<"hello">>, <<"world">>)).

starts_with_empty_test() ->
    ?assertEqual(true, beamtalk_string:starts_with(<<"hello">>, <<>>)).

starts_with_full_test() ->
    ?assertEqual(true, beamtalk_string:starts_with(<<"hello">>, <<"hello">>)).

starts_with_longer_prefix_test() ->
    ?assertEqual(false, beamtalk_string:starts_with(<<"hi">>, <<"hello">>)).

%%% ============================================================================
%%% ends_with/2
%%% ============================================================================

ends_with_true_test() ->
    ?assertEqual(true, beamtalk_string:ends_with(<<"hello">>, <<"llo">>)).

ends_with_false_test() ->
    ?assertEqual(false, beamtalk_string:ends_with(<<"hello">>, <<"world">>)).

ends_with_empty_test() ->
    ?assertEqual(true, beamtalk_string:ends_with(<<"hello">>, <<>>)).

ends_with_full_test() ->
    ?assertEqual(true, beamtalk_string:ends_with(<<"hello">>, <<"hello">>)).

ends_with_longer_suffix_test() ->
    ?assertEqual(false, beamtalk_string:ends_with(<<"hi">>, <<"hello">>)).

%%% ============================================================================
%%% index_of/2
%%% ============================================================================

index_of_found_test() ->
    ?assertEqual(1, beamtalk_string:index_of(<<"hello">>, <<"h">>)).

index_of_middle_test() ->
    ?assertEqual(3, beamtalk_string:index_of(<<"hello">>, <<"ll">>)).

index_of_not_found_test() ->
    ?assertEqual(nil, beamtalk_string:index_of(<<"hello">>, <<"xyz">>)).

index_of_empty_substr_test() ->
    ?assertEqual(nil, beamtalk_string:index_of(<<"hello">>, <<>>)).

index_of_utf8_test() ->
    %% "café" - 'f' is at grapheme position 3
    ?assertEqual(3, beamtalk_string:index_of(<<"café"/utf8>>, <<"f">>)).

%%% ============================================================================
%%% split_on/2
%%% ============================================================================

split_on_comma_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"c">>],
        beamtalk_string:split_on(<<"a,b,c">>, <<",">>)
    ).

split_on_no_match_test() ->
    ?assertEqual(
        [<<"hello">>],
        beamtalk_string:split_on(<<"hello">>, <<",">>)
    ).

split_on_multi_char_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>],
        beamtalk_string:split_on(<<"a::b">>, <<"::">>)
    ).

split_on_empty_pattern_test() ->
    ?assertEqual(
        [<<"hello">>],
        beamtalk_string:split_on(<<"hello">>, <<>>)
    ).

%%% ============================================================================
%%% repeat/2
%%% ============================================================================

repeat_test() ->
    ?assertEqual(<<"abcabcabc">>, beamtalk_string:repeat(<<"abc">>, 3)).

repeat_zero_test() ->
    ?assertEqual(<<>>, beamtalk_string:repeat(<<"abc">>, 0)).

repeat_one_test() ->
    ?assertEqual(<<"abc">>, beamtalk_string:repeat(<<"abc">>, 1)).

%%% ============================================================================
%%% as_list/1
%%% ============================================================================

as_list_simple_test() ->
    ?assertEqual(
        [<<"h">>, <<"e">>, <<"l">>, <<"l">>, <<"o">>],
        beamtalk_string:as_list(<<"hello">>)
    ).

as_list_empty_test() ->
    ?assertEqual([], beamtalk_string:as_list(<<>>)).

as_list_utf8_test() ->
    ?assertEqual(
        [<<"c">>, <<"a">>, <<"f">>, <<"é"/utf8>>],
        beamtalk_string:as_list(<<"café"/utf8>>)
    ).

%%% ============================================================================
%%% each/2
%%% ============================================================================

each_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_string:each(<<"hi">>, fun(_) -> ok end)).

each_calls_block_test() ->
    Ref = make_ref(),
    Self = self(),
    beamtalk_string:each(<<"ab">>, fun(G) -> Self ! {Ref, G} end),
    ?assertEqual(
        <<"a">>,
        receive
            {Ref, M1} -> M1
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        <<"b">>,
        receive
            {Ref, M2} -> M2
        after 100 -> timeout
        end
    ).

%%% ============================================================================
%%% collect/2
%%% ============================================================================

collect_uppercase_test() ->
    ?assertEqual(
        <<"HI">>,
        beamtalk_string:collect(
            <<"hi">>,
            fun(G) -> unicode:characters_to_binary(string:uppercase(G)) end
        )
    ).

collect_identity_test() ->
    ?assertEqual(<<"abc">>, beamtalk_string:collect(<<"abc">>, fun(X) -> X end)).

collect_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string:collect(<<>>, fun(X) -> X end)).

%%% ============================================================================
%%% select/2
%%% ============================================================================

select_vowels_test() ->
    Vowels = [<<"a">>, <<"e">>, <<"i">>, <<"o">>, <<"u">>],
    ?assertEqual(
        <<"eoo">>,
        beamtalk_string:select(
            <<"hello world">>,
            fun(G) -> lists:member(G, Vowels) end
        )
    ).

select_all_test() ->
    ?assertEqual(
        <<"hi">>,
        beamtalk_string:select(<<"hi">>, fun(_) -> true end)
    ).

select_none_test() ->
    ?assertEqual(
        <<>>,
        beamtalk_string:select(<<"hi">>, fun(_) -> false end)
    ).

%%% ============================================================================
%%% reject/2
%%% ============================================================================

reject_vowels_test() ->
    Vowels = [<<"a">>, <<"e">>, <<"i">>, <<"o">>, <<"u">>],
    ?assertEqual(
        <<"hll wrld">>,
        beamtalk_string:reject(
            <<"hello world">>,
            fun(G) -> lists:member(G, Vowels) end
        )
    ).

reject_none_test() ->
    ?assertEqual(
        <<"hi">>,
        beamtalk_string:reject(<<"hi">>, fun(_) -> false end)
    ).

reject_all_test() ->
    ?assertEqual(
        <<>>,
        beamtalk_string:reject(<<"hi">>, fun(_) -> true end)
    ).

reject_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string:reject(<<>>, fun(_) -> true end)).

%%% ============================================================================
%%% lines/1
%%% ============================================================================

lines_newline_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"c">>],
        beamtalk_string:lines(<<"a\nb\nc">>)
    ).

lines_crlf_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>],
        beamtalk_string:lines(<<"a\r\nb">>)
    ).

lines_mixed_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"c">>],
        beamtalk_string:lines(<<"a\r\nb\nc">>)
    ).

lines_single_test() ->
    ?assertEqual(
        [<<"hello">>],
        beamtalk_string:lines(<<"hello">>)
    ).

lines_empty_test() ->
    ?assertEqual(
        [<<>>],
        beamtalk_string:lines(<<>>)
    ).

%%% ============================================================================
%%% words/1
%%% ============================================================================

words_basic_test() ->
    ?assertEqual(
        [<<"hello">>, <<"world">>],
        beamtalk_string:words(<<"hello world">>)
    ).

words_extra_spaces_test() ->
    ?assertEqual(
        [<<"hello">>, <<"world">>],
        beamtalk_string:words(<<"  hello   world  ">>)
    ).

words_tabs_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>],
        beamtalk_string:words(<<"a\tb">>)
    ).

words_single_test() ->
    ?assertEqual(
        [<<"hello">>],
        beamtalk_string:words(<<"hello">>)
    ).

words_empty_test() ->
    ?assertEqual(
        [],
        beamtalk_string:words(<<>>)
    ).

%%% ============================================================================
%%% take/2
%%% ============================================================================

take_basic_test() ->
    ?assertEqual(<<"hel">>, beamtalk_string:take(<<"hello">>, 3)).

take_all_test() ->
    ?assertEqual(<<"hello">>, beamtalk_string:take(<<"hello">>, 10)).

take_zero_test() ->
    ?assertEqual(<<>>, beamtalk_string:take(<<"hello">>, 0)).

take_negative_test() ->
    ?assertEqual(<<>>, beamtalk_string:take(<<"hello">>, -1)).

take_utf8_test() ->
    Str = unicode:characters_to_binary("héllo"),
    ?assertEqual(
        unicode:characters_to_binary("hé"),
        beamtalk_string:take(Str, 2)
    ).

%%% ============================================================================
%%% drop/2
%%% ============================================================================

drop_basic_test() ->
    ?assertEqual(<<"llo">>, beamtalk_string:drop(<<"hello">>, 2)).

drop_all_test() ->
    ?assertEqual(<<>>, beamtalk_string:drop(<<"hello">>, 10)).

drop_zero_test() ->
    ?assertEqual(<<"hello">>, beamtalk_string:drop(<<"hello">>, 0)).

drop_negative_test() ->
    ?assertEqual(<<"hello">>, beamtalk_string:drop(<<"hello">>, -1)).

drop_utf8_test() ->
    Str = unicode:characters_to_binary("héllo"),
    ?assertEqual(<<"llo">>, beamtalk_string:drop(Str, 2)).

%%% ============================================================================
%%% is_blank/1
%%% ============================================================================

is_blank_empty_test() ->
    ?assertEqual(true, beamtalk_string:is_blank(<<>>)).

is_blank_spaces_test() ->
    ?assertEqual(true, beamtalk_string:is_blank(<<"   ">>)).

is_blank_tabs_test() ->
    ?assertEqual(true, beamtalk_string:is_blank(<<"\t\n">>)).

is_blank_content_test() ->
    ?assertEqual(false, beamtalk_string:is_blank(<<"hello">>)).

is_blank_mixed_test() ->
    ?assertEqual(false, beamtalk_string:is_blank(<<"  hi  ">>)).

%%% ============================================================================
%%% is_digit/1
%%% ============================================================================

is_digit_all_digits_test() ->
    ?assertEqual(true, beamtalk_string:is_digit(<<"12345">>)).

is_digit_mixed_test() ->
    ?assertEqual(false, beamtalk_string:is_digit(<<"12a">>)).

is_digit_empty_test() ->
    ?assertEqual(false, beamtalk_string:is_digit(<<>>)).

is_digit_alpha_test() ->
    ?assertEqual(false, beamtalk_string:is_digit(<<"abc">>)).

%%% ============================================================================
%%% is_alpha/1
%%% ============================================================================

is_alpha_lowercase_test() ->
    ?assertEqual(true, beamtalk_string:is_alpha(<<"hello">>)).

is_alpha_uppercase_test() ->
    ?assertEqual(true, beamtalk_string:is_alpha(<<"HELLO">>)).

is_alpha_mixed_case_test() ->
    ?assertEqual(true, beamtalk_string:is_alpha(<<"Hello">>)).

is_alpha_with_digits_test() ->
    ?assertEqual(false, beamtalk_string:is_alpha(<<"hello1">>)).

is_alpha_empty_test() ->
    ?assertEqual(false, beamtalk_string:is_alpha(<<>>)).

%%% ============================================================================
%%% from_code_point/1
%%% ============================================================================

from_code_point_ascii_test() ->
    ?assertEqual(<<"A">>, beamtalk_string:from_code_point(65)).

from_code_point_lowercase_test() ->
    ?assertEqual(<<"z">>, beamtalk_string:from_code_point(122)).

from_code_point_multibyte_test() ->
    ?assertEqual(<<"€"/utf8>>, beamtalk_string:from_code_point(8364)).

from_code_point_emoji_test() ->
    ?assertEqual(<<"😀"/utf8>>, beamtalk_string:from_code_point(128512)).

from_code_point_zero_test() ->
    ?assertEqual(<<0>>, beamtalk_string:from_code_point(0)).

from_code_point_negative_raises_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'String', selector = 'fromCodePoint:'
            }
        },
        beamtalk_string:from_code_point(-1)
    ).

%%% ============================================================================
%%% from_code_points/1
%%% ============================================================================

from_code_points_ascii_list_test() ->
    ?assertEqual(<<"Hi">>, beamtalk_string:from_code_points([72, 105])).

from_code_points_braces_test() ->
    ?assertEqual(<<"{}">>, beamtalk_string:from_code_points([123, 125])).

from_code_points_empty_test() ->
    ?assertEqual(<<>>, beamtalk_string:from_code_points([])).

from_code_points_multibyte_test() ->
    ?assertEqual(<<"€£"/utf8>>, beamtalk_string:from_code_points([8364, 163])).

from_code_point_non_integer_raises_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'String', selector = 'fromCodePoint:'
            }
        },
        beamtalk_string:from_code_point(<<"not_an_integer">>)
    ).

from_code_points_non_list_raises_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'String', selector = 'fromCodePoints:'
            }
        },
        beamtalk_string:from_code_points(not_a_list)
    ).

from_code_points_non_integer_elements_raises_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = type_error, class = 'String', selector = 'fromCodePoints:'
            }
        },
        beamtalk_string:from_code_points([65, <<"not_an_int">>, 97])
    ).
