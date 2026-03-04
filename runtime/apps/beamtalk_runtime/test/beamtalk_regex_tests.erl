%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_regex module (BT-1088).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests constructors, instance methods, string helper functions,
%%% has_method/1, and error paths.

-module(beamtalk_regex_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

make_regex(Pattern) ->
    beamtalk_regex:'from:'(Pattern).

%%% ============================================================================
%%% from:/1 — Constructor
%%% ============================================================================

from_valid_test() ->
    R = make_regex(<<"[0-9]+">>),
    ?assertEqual('Regex', maps:get('$beamtalk_class', R)),
    ?assertEqual(<<"[0-9]+">>, maps:get(source, R)),
    ?assert(maps:is_key(compiled, R)).

from_empty_pattern_test() ->
    R = make_regex(<<"">>),
    ?assertEqual(<<"">>, maps:get(source, R)).

from_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        make_regex(<<"[unclosed">>)
    ).

from_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:'from:'(not_a_string)
    ).

%%% ============================================================================
%%% from:options:/2 — Constructor with options
%%% ============================================================================

from_options_caseless_test() ->
    R = beamtalk_regex:'from:options:'(<<"hello">>, [caseless]),
    ?assertEqual(<<"hello">>, maps:get(source, R)).

from_options_multiline_test() ->
    R = beamtalk_regex:'from:options:'(<<"^foo">>, [multiline]),
    ?assert(maps:is_key(compiled, R)).

from_options_unknown_option_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:'from:options:'(<<"foo">>, [unknown_opt])
    ).

from_options_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:'from:options:'(42, [caseless])
    ).

%%% ============================================================================
%%% source/1 and printString/1
%%% ============================================================================

source_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual(<<"\\d+">>, beamtalk_regex:source(R)).

print_string_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual(<<"Regex([a-z]+)">>, beamtalk_regex:'printString'(R)).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_known_test() ->
    ?assert(beamtalk_regex:has_method(source)),
    ?assert(beamtalk_regex:has_method('printString')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_regex:has_method(foobar)),
    ?assertNot(beamtalk_regex:has_method(new)).

%%% ============================================================================
%%% matches_regex/2 — String + Pattern or Compiled Regex
%%% ============================================================================

matches_regex_string_pattern_true_test() ->
    ?assert(beamtalk_regex:matches_regex(<<"hello123">>, <<"[0-9]+">>)).

matches_regex_string_pattern_false_test() ->
    ?assertNot(beamtalk_regex:matches_regex(<<"hello">>, <<"[0-9]+">>)).

matches_regex_compiled_true_test() ->
    R = make_regex(<<"\\d+">>),
    ?assert(beamtalk_regex:matches_regex(<<"abc123">>, R)).

matches_regex_compiled_false_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertNot(beamtalk_regex:matches_regex(<<"abc">>, R)).

matches_regex_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:matches_regex(<<"hello">>, <<"[bad">>)
    ).

matches_regex_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:matches_regex(42, <<"[0-9]+">>)
    ).

%%% ============================================================================
%%% matches_regex_options/3
%%% ============================================================================

matches_regex_options_caseless_test() ->
    ?assert(beamtalk_regex:matches_regex_options(<<"HELLO">>, <<"hello">>, [caseless])).

matches_regex_options_no_match_test() ->
    ?assertNot(beamtalk_regex:matches_regex_options(<<"world">>, <<"hello">>, [caseless])).

matches_regex_options_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:matches_regex_options(42, <<"hello">>, [caseless])
    ).

%%% ============================================================================
%%% first_match/2
%%% ============================================================================

first_match_string_pattern_found_test() ->
    ?assertEqual(<<"123">>, beamtalk_regex:first_match(<<"abc123def">>, <<"\\d+">>)).

first_match_string_pattern_not_found_test() ->
    ?assertEqual(nil, beamtalk_regex:first_match(<<"abc">>, <<"\\d+">>)).

first_match_compiled_found_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual(<<"abc">>, beamtalk_regex:first_match(<<"abc123">>, R)).

first_match_compiled_not_found_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual(nil, beamtalk_regex:first_match(<<"123">>, R)).

first_match_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:first_match(<<"hello">>, <<"[bad">>)
    ).

first_match_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:first_match(42, <<"\\d+">>)
    ).

%%% ============================================================================
%%% all_matches/2
%%% ============================================================================

all_matches_string_pattern_test() ->
    ?assertEqual(
        [<<"1">>, <<"2">>, <<"3">>],
        beamtalk_regex:all_matches(<<"a1b2c3">>, <<"\\d">>)
    ).

all_matches_no_match_test() ->
    ?assertEqual([], beamtalk_regex:all_matches(<<"abc">>, <<"\\d">>)).

all_matches_compiled_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual([<<"123">>, <<"456">>], beamtalk_regex:all_matches(<<"123abc456">>, R)).

all_matches_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:all_matches(<<"hello">>, <<"[bad">>)
    ).

all_matches_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:all_matches(42, <<"\\d+">>)
    ).

%%% ============================================================================
%%% replace_regex/3 — first match
%%% ============================================================================

replace_regex_string_pattern_test() ->
    ?assertEqual(
        <<"hXllo">>,
        beamtalk_regex:replace_regex(<<"hello">>, <<"e">>, <<"X">>)
    ).

replace_regex_compiled_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual(<<"abc_def">>, beamtalk_regex:replace_regex(<<"abc123def">>, R, <<"_">>)).

replace_regex_no_match_test() ->
    ?assertEqual(<<"hello">>, beamtalk_regex:replace_regex(<<"hello">>, <<"\\d+">>, <<"X">>)).

replace_regex_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:replace_regex(<<"hello">>, <<"[bad">>, <<"X">>)
    ).

replace_regex_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:replace_regex(42, <<"e">>, <<"X">>)
    ).

%%% ============================================================================
%%% replace_all_regex/3 — all matches
%%% ============================================================================

replace_all_regex_string_pattern_test() ->
    ?assertEqual(
        <<"XaX">>,
        beamtalk_regex:replace_all_regex(<<"1a2">>, <<"\\d">>, <<"X">>)
    ).

replace_all_regex_compiled_test() ->
    R = make_regex(<<"[aeiou]">>),
    ?assertEqual(
        <<"h_ll_ w_rld">>, beamtalk_regex:replace_all_regex(<<"hello world">>, R, <<"_">>)
    ).

replace_all_regex_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:replace_all_regex(<<"hello">>, <<"[bad">>, <<"X">>)
    ).

replace_all_regex_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:replace_all_regex(42, <<"e">>, <<"X">>)
    ).

%%% ============================================================================
%%% split_regex/2
%%% ============================================================================

split_regex_string_pattern_test() ->
    ?assertEqual(
        [<<"a">>, <<"b">>, <<"c">>],
        beamtalk_regex:split_regex(<<"a,b,c">>, <<",">>)
    ).

split_regex_compiled_test() ->
    R = make_regex(<<"\\s+">>),
    ?assertEqual(
        [<<"hello">>, <<"world">>],
        beamtalk_regex:split_regex(<<"hello   world">>, R)
    ).

split_regex_no_match_test() ->
    %% No delimiters → single element
    Result = beamtalk_regex:split_regex(<<"hello">>, <<",">>),
    ?assertEqual([<<"hello">>], Result).

split_regex_invalid_pattern_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = regex_error}},
        beamtalk_regex:split_regex(<<"hello">>, <<"[bad">>)
    ).

split_regex_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_regex:split_regex(42, <<",">>)
    ).
