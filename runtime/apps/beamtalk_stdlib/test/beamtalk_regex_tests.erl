%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_regex_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_regex module.

Tests constructors, instance methods, string helper functions,
and error paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

make_regex(Pattern) ->
    R = beamtalk_regex:'from:'(Pattern),
    maps:get('okValue', R).

%%% ============================================================================
%%% from:/1 — Constructor
%%% ============================================================================

from_valid_test() ->
    R = beamtalk_regex:'from:'(<<"[0-9]+">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    Regex = maps:get('okValue', R),
    ?assertEqual('Regex', maps:get('$beamtalk_class', Regex)),
    ?assertEqual({beamtalk_string, <<"[0-9]+">>}, maps:get(source, Regex)),
    ?assert(maps:is_key(compiled, Regex)).

from_empty_pattern_test() ->
    R = beamtalk_regex:'from:'(<<"">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    ?assertEqual({beamtalk_string, <<"">>}, maps:get(source, maps:get('okValue', R))).

from_invalid_pattern_test() ->
    R = beamtalk_regex:'from:'(<<"[unclosed">>),
    ?assertBtResultObjError(regex_error, R).

from_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:'from:'(not_a_string)).

%%% ============================================================================
%%% from:options:/2 — Constructor with options
%%% ============================================================================

from_options_caseless_test() ->
    R = beamtalk_regex:'from:options:'(<<"hello">>, [caseless]),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    ?assertEqual({beamtalk_string, <<"hello">>}, maps:get(source, maps:get('okValue', R))).

from_options_multiline_test() ->
    R = beamtalk_regex:'from:options:'(<<"^foo">>, [multiline]),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    ?assert(maps:is_key(compiled, maps:get('okValue', R))).

from_options_unknown_option_test() ->
    ?assertBtError(regex_error, beamtalk_regex:'from:options:'(<<"foo">>, [unknown_opt])).

from_options_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:'from:options:'(42, [caseless])).

%%% ============================================================================
%%% RegexOption conformance (BT-4381)
%%%
%%% The option set lives in three places: the `RegexOption` type alias in
%%% stdlib/src/regex.bt, `beamtalk_regex:valid_options/0` (which drives
%%% `translate_option/1` and its hint), and the `regex_option()` Erlang type.
%%% These tests fail if any one of them changes alone.
%%% ============================================================================

regex_option_alias_matches_runtime_test() ->
    ?assertEqual(
        lists:sort(beamtalk_regex:valid_options()),
        lists:sort(regex_option_alias_members())
    ).

regex_option_erlang_type_matches_runtime_test() ->
    ?assertEqual(
        lists:sort(beamtalk_regex:valid_options()),
        lists:sort(regex_option_type_members())
    ).

every_valid_option_compiles_test() ->
    lists:foreach(
        fun(Opt) ->
            R = beamtalk_regex:'from:options:'(<<"a">>, [Opt]),
            ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R)
        end,
        beamtalk_regex:valid_options()
    ).

unknown_option_hint_lists_valid_options_test() ->
    try beamtalk_regex:'from:options:'(<<"a">>, [bogus]) of
        _ -> ?assert(false)
    catch
        error:#{error := #beamtalk_error{kind = regex_error, hint = {beamtalk_string, Hint}}} ->
            lists:foreach(
                fun(Opt) ->
                    ?assertNotEqual(nomatch, binary:match(Hint, atom_to_binary(Opt)))
                end,
                beamtalk_regex:valid_options()
            )
    end.

%% Members of `type RegexOption = #a | #b | ...` as recorded in the stdlib
%% app's `type_aliases` env (generated from stdlib/src/regex.bt).
regex_option_alias_members() ->
    _ = application:load(beamtalk_stdlib),
    {ok, Aliases} = application:get_env(beamtalk_stdlib, type_aliases),
    [Expansion] = [E || #{name := 'RegexOption', expansion := E} <- Aliases],
    [
        list_to_atom(string:trim(Member, leading, "#"))
     || Member <- string:lexemes(unicode:characters_to_list(Expansion), "| ")
    ].

%% Members of the `-type regex_option() :: a | b | ...` union, read from the
%% compiled module's abstract code. Locates the .beam on the code path rather
%% than via `code:which/1`, which answers `cover_compiled` under `--cover`.
regex_option_type_members() ->
    Beam = code:where_is_file("beamtalk_regex.beam"),
    ?assert(is_list(Beam)),
    {ok, {beamtalk_regex, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Beam, [abstract_code]),
    [Members] = [
        Ms
     || {attribute, _, type, {regex_option, {type, _, union, Ms}, []}} <- Forms
    ],
    [Atom || {atom, _, Atom} <- Members].

%%% ============================================================================
%%% FFI no-colon aliases
%%% ============================================================================

from_alias_test() ->
    R = beamtalk_regex:from(<<"[0-9]+">>),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    ?assertEqual('Regex', maps:get('$beamtalk_class', maps:get('okValue', R))).

from_alias_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:from(not_a_string)).

from_two_alias_test() ->
    R = beamtalk_regex:from(<<"[a-z]+">>, [caseless]),
    ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true}, R),
    ?assertEqual('Regex', maps:get('$beamtalk_class', maps:get('okValue', R))).

from_two_alias_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:from(42, [caseless])).

%%% ============================================================================
%%% source/1 and printString/1
%%% ============================================================================

source_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual({beamtalk_string, <<"\\d+">>}, beamtalk_regex:source(R)).

print_string_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual({beamtalk_string, <<"Regex([a-z]+)">>}, beamtalk_regex:'printString'(R)).

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
    ?assertBtError(regex_error, beamtalk_regex:matches_regex(<<"hello">>, <<"[bad">>)).

matches_regex_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:matches_regex(42, <<"[0-9]+">>)).

%%% ============================================================================
%%% matches_regex_options/3
%%% ============================================================================

matches_regex_options_caseless_test() ->
    ?assert(beamtalk_regex:matches_regex_options(<<"HELLO">>, <<"hello">>, [caseless])).

matches_regex_options_no_match_test() ->
    ?assertNot(beamtalk_regex:matches_regex_options(<<"world">>, <<"hello">>, [caseless])).

matches_regex_options_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:matches_regex_options(42, <<"hello">>, [caseless])).

%%% ============================================================================
%%% first_match/2
%%% ============================================================================

first_match_string_pattern_found_test() ->
    ?assertEqual(
        {beamtalk_string, <<"123">>}, beamtalk_regex:first_match(<<"abc123def">>, <<"\\d+">>)
    ).

first_match_string_pattern_not_found_test() ->
    ?assertEqual(nil, beamtalk_regex:first_match(<<"abc">>, <<"\\d+">>)).

first_match_compiled_found_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual({beamtalk_string, <<"abc">>}, beamtalk_regex:first_match(<<"abc123">>, R)).

first_match_compiled_not_found_test() ->
    R = make_regex(<<"[a-z]+">>),
    ?assertEqual(nil, beamtalk_regex:first_match(<<"123">>, R)).

first_match_invalid_pattern_test() ->
    ?assertBtError(regex_error, beamtalk_regex:first_match(<<"hello">>, <<"[bad">>)).

first_match_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:first_match(42, <<"\\d+">>)).

%%% ============================================================================
%%% all_matches/2
%%% ============================================================================

all_matches_string_pattern_test() ->
    ?assertEqual(
        [{beamtalk_string, <<"1">>}, {beamtalk_string, <<"2">>}, {beamtalk_string, <<"3">>}],
        beamtalk_regex:all_matches(<<"a1b2c3">>, <<"\\d">>)
    ).

all_matches_no_match_test() ->
    ?assertEqual([], beamtalk_regex:all_matches(<<"abc">>, <<"\\d">>)).

all_matches_compiled_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual(
        [{beamtalk_string, <<"123">>}, {beamtalk_string, <<"456">>}],
        beamtalk_regex:all_matches(<<"123abc456">>, R)
    ).

all_matches_invalid_pattern_test() ->
    ?assertBtError(regex_error, beamtalk_regex:all_matches(<<"hello">>, <<"[bad">>)).

all_matches_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:all_matches(42, <<"\\d+">>)).

%%% ============================================================================
%%% replace_regex/3 — first match
%%% ============================================================================

replace_regex_string_pattern_test() ->
    ?assertEqual(
        {beamtalk_string, <<"hXllo">>},
        beamtalk_regex:replace_regex(<<"hello">>, <<"e">>, <<"X">>)
    ).

replace_regex_compiled_test() ->
    R = make_regex(<<"\\d+">>),
    ?assertEqual(
        {beamtalk_string, <<"abc_def">>}, beamtalk_regex:replace_regex(<<"abc123def">>, R, <<"_">>)
    ).

replace_regex_no_match_test() ->
    ?assertEqual(
        {beamtalk_string, <<"hello">>},
        beamtalk_regex:replace_regex(<<"hello">>, <<"\\d+">>, <<"X">>)
    ).

replace_regex_invalid_pattern_test() ->
    ?assertBtError(regex_error, beamtalk_regex:replace_regex(<<"hello">>, <<"[bad">>, <<"X">>)).

replace_regex_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:replace_regex(42, <<"e">>, <<"X">>)).

%%% ============================================================================
%%% replace_all_regex/3 — all matches
%%% ============================================================================

replace_all_regex_string_pattern_test() ->
    ?assertEqual(
        {beamtalk_string, <<"XaX">>},
        beamtalk_regex:replace_all_regex(<<"1a2">>, <<"\\d">>, <<"X">>)
    ).

replace_all_regex_compiled_test() ->
    R = make_regex(<<"[aeiou]">>),
    ?assertEqual(
        {beamtalk_string, <<"h_ll_ w_rld">>},
        beamtalk_regex:replace_all_regex(<<"hello world">>, R, <<"_">>)
    ).

replace_all_regex_invalid_pattern_test() ->
    ?assertBtError(regex_error, beamtalk_regex:replace_all_regex(<<"hello">>, <<"[bad">>, <<"X">>)).

replace_all_regex_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:replace_all_regex(42, <<"e">>, <<"X">>)).

%%% ============================================================================
%%% split_regex/2
%%% ============================================================================

split_regex_string_pattern_test() ->
    ?assertEqual(
        [{beamtalk_string, <<"a">>}, {beamtalk_string, <<"b">>}, {beamtalk_string, <<"c">>}],
        beamtalk_regex:split_regex(<<"a,b,c">>, <<",">>)
    ).

split_regex_compiled_test() ->
    R = make_regex(<<"\\s+">>),
    ?assertEqual(
        [{beamtalk_string, <<"hello">>}, {beamtalk_string, <<"world">>}],
        beamtalk_regex:split_regex(<<"hello   world">>, R)
    ).

split_regex_no_match_test() ->
    %% No delimiters → single element
    Result = beamtalk_regex:split_regex(<<"hello">>, <<",">>),
    ?assertEqual([{beamtalk_string, <<"hello">>}], Result).

split_regex_invalid_pattern_test() ->
    ?assertBtError(regex_error, beamtalk_regex:split_regex(<<"hello">>, <<"[bad">>)).

split_regex_type_error_test() ->
    ?assertBtError(type_error, beamtalk_regex:split_regex(42, <<",">>)).
