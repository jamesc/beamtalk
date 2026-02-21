%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Character runtime support for primitives that cannot be expressed
%% as direct Erlang BIF calls.
%%
%% DDD Context: Runtime — Standard Library
%%
%% Character values are integers (Unicode codepoints) at the BEAM level.
%% This module provides predicate and conversion helpers for the Character class.
-module(beamtalk_character).

-export([
    is_letter/1,
    is_digit/1,
    is_uppercase/1,
    is_lowercase/1,
    is_whitespace/1,
    to_uppercase/1,
    to_lowercase/1,
    as_string/1,
    print_string/1,
    value/1
]).

%% @doc Test if codepoint is a Unicode letter (category L).
-spec is_letter(integer()) -> boolean().
is_letter(CP) when is_integer(CP), CP >= 0 ->
    unicode_match(CP, <<"\\p{L}">>);
is_letter(CP) when is_integer(CP) -> false.

%% @doc Test if codepoint is a Unicode digit (category Nd).
-spec is_digit(integer()) -> boolean().
is_digit(CP) when is_integer(CP), CP >= 0 ->
    unicode_match(CP, <<"\\p{Nd}">>);
is_digit(CP) when is_integer(CP) -> false.

%% @doc Test if codepoint is uppercase (category Lu).
-spec is_uppercase(integer()) -> boolean().
is_uppercase(CP) when is_integer(CP), CP >= 0 ->
    unicode_match(CP, <<"\\p{Lu}">>);
is_uppercase(CP) when is_integer(CP) -> false.

%% @doc Test if codepoint is lowercase (category Ll).
-spec is_lowercase(integer()) -> boolean().
is_lowercase(CP) when is_integer(CP), CP >= 0 ->
    unicode_match(CP, <<"\\p{Ll}">>);
is_lowercase(CP) when is_integer(CP) -> false.

%% @doc Test if codepoint is Unicode whitespace (category Z or control whitespace).
-spec is_whitespace(integer()) -> boolean().
is_whitespace(CP) when is_integer(CP), CP >= 0 ->
    unicode_match(CP, <<"\\p{Z}">>) orelse
        CP =:= $\t orelse CP =:= $\n orelse CP =:= $\r orelse
        CP =:= 16#0B orelse CP =:= 16#0C;
is_whitespace(CP) when is_integer(CP) -> false.

%% @private Match a codepoint against a Unicode property regex.
%% Returns false for values outside the Unicode scalar range.
-spec unicode_match(integer(), binary()) -> boolean().
unicode_match(CP, Pattern) when
    is_integer(CP),
    CP >= 0,
    CP =< 16#10FFFF,
    not (CP >= 16#D800 andalso CP =< 16#DFFF)
->
    Str = unicode:characters_to_binary([CP]),
    match =:= re:run(Str, Pattern, [unicode, {capture, none}]);
unicode_match(_CP, _Pattern) ->
    false.

%% @doc Convert codepoint to uppercase.
%% For multi-codepoint expansions (e.g., ß → SS), returns the first codepoint.
-spec to_uppercase(integer()) -> integer().
to_uppercase(CP) when is_integer(CP), CP >= 0, CP =< 16#10FFFF ->
    Str = unicode:characters_to_binary([CP]),
    Upper = string:uppercase(Str),
    case unicode:characters_to_list(Upper) of
        [UpperCP | _] -> UpperCP;
        _ -> CP
    end;
to_uppercase(CP) when is_integer(CP) -> CP.

%% @doc Convert codepoint to lowercase.
%% For multi-codepoint expansions, returns the first codepoint.
-spec to_lowercase(integer()) -> integer().
to_lowercase(CP) when is_integer(CP), CP >= 0, CP =< 16#10FFFF ->
    Str = unicode:characters_to_binary([CP]),
    Lower = string:lowercase(Str),
    case unicode:characters_to_list(Lower) of
        [LowerCP | _] -> LowerCP;
        _ -> CP
    end;
to_lowercase(CP) when is_integer(CP) -> CP.

%% @doc Convert codepoint to a single-character string.
-spec as_string(integer()) -> binary().
as_string(CP) when is_integer(CP), CP >= 0, CP =< 16#10FFFF ->
    unicode:characters_to_binary([CP]).

%% @doc Display representation of a character ($a format).
-spec print_string(integer()) -> binary().
print_string(CP) when is_integer(CP), CP >= 0, CP =< 16#10FFFF ->
    CharStr = unicode:characters_to_binary([CP]),
    <<"$", CharStr/binary>>.

%% @doc Create a Character from a codepoint integer (factory method).
%% Validates that the argument is a valid Unicode scalar value
%% (non-negative, <= 16#10FFFF, and not in the surrogate range).
-spec value(integer()) -> integer().
value(CP) when is_integer(CP), CP >= 0, CP =< 16#D7FF -> CP;
value(CP) when is_integer(CP), CP >= 16#E000, CP =< 16#10FFFF -> CP;
value(CP) ->
    Error0 = beamtalk_error:new(type_error, 'Character'),
    Error1 = beamtalk_error:with_selector(Error0, 'value:'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<
            "Argument must be a valid Unicode scalar value "
            "(non-negative, <= 0x10FFFF, and not a surrogate codepoint)"
        >>
    ),
    Error3 = beamtalk_error:with_details(Error2, #{got => CP}),
    beamtalk_error:raise(Error3).
