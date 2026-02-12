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
-spec unicode_match(integer(), binary()) -> boolean().
unicode_match(CP, Pattern) ->
    Str = unicode:characters_to_binary([CP]),
    match =:= re:run(Str, Pattern, [unicode, {capture, none}]).

%% @doc Convert codepoint to uppercase.
-spec to_uppercase(integer()) -> integer().
to_uppercase(CP) when is_integer(CP) ->
    Str = unicode:characters_to_binary([CP]),
    Upper = string:uppercase(Str),
    [UpperCP | _] = string:to_graphemes(Upper),
    case UpperCP of
        I when is_integer(I) -> I;
        _ -> CP
    end.

%% @doc Convert codepoint to lowercase.
-spec to_lowercase(integer()) -> integer().
to_lowercase(CP) when is_integer(CP) ->
    Str = unicode:characters_to_binary([CP]),
    Lower = string:lowercase(Str),
    [LowerCP | _] = string:to_graphemes(Lower),
    case LowerCP of
        I when is_integer(I) -> I;
        _ -> CP
    end.

%% @doc Convert codepoint to a single-character string.
-spec as_string(integer()) -> binary().
as_string(CP) when is_integer(CP), CP >= 0 ->
    unicode:characters_to_binary([CP]).

%% @doc Display representation of a character ($a format).
-spec print_string(integer()) -> binary().
print_string(CP) when is_integer(CP), CP >= 0 ->
    CharStr = unicode:characters_to_binary([CP]),
    <<"$", CharStr/binary>>.

%% @doc Create a Character from a codepoint integer (factory method).
%% Validates that the argument is a non-negative integer.
-spec value(integer()) -> integer().
value(CP) when is_integer(CP), CP >= 0 -> CP;
value(CP) ->
    Error0 = beamtalk_error:new(type_error, 'Character'),
    Error1 = beamtalk_error:with_selector(Error0, 'value:'),
    Error2 = beamtalk_error:with_hint(Error1,
        <<"Argument must be a non-negative integer (Unicode codepoint)">>),
    Error3 = beamtalk_error:with_details(Error2, #{got => CP}),
    beamtalk_error:raise(Error3).
