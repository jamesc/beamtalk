%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc String helper operations for compiled stdlib.
%%%
%%% **DDD Context:** Runtime Context — Domain Service
%%%
%%% This module provides implementations for String primitive methods
%%% that require more than simple Erlang BIF calls. Called by compiled
%%% stdlib String module via `@primitive` codegen.
%%%
%%% All operations are grapheme-aware and handle UTF-8 correctly.

-module(beamtalk_string_ops).
-export([
    at/2,
    capitalize/1,
    reverse/1,
    includes/2,
    starts_with/2,
    ends_with/2,
    index_of/2,
    split_on/2,
    repeat/2,
    as_list/1,
    each/2,
    collect/2,
    select/2,
    lines/1,
    words/1,
    take/2,
    drop/2,
    is_blank/1,
    is_digit/1,
    is_alpha/1
]).

%% @doc 1-based grapheme access. Returns the grapheme at the given index.
-spec at(binary(), integer()) -> binary().
at(Str, Idx) when is_binary(Str), is_integer(Idx), Idx >= 1 ->
    case get_nth_grapheme(Str, Idx) of
        {ok, Grapheme} -> Grapheme;
        error ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'String'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Index is beyond string length">>),
            beamtalk_error:raise(Error2)
    end;
at(Str, Idx) when is_binary(Str), is_integer(Idx) ->
    Error0 = beamtalk_error:new(index_out_of_bounds, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Index must be >= 1 (1-based indexing)">>),
    beamtalk_error:raise(Error2).

%% @doc Capitalize the first grapheme, keep rest unchanged.
-spec capitalize(binary()) -> binary().
capitalize(<<>>) -> <<>>;
capitalize(Str) when is_binary(Str) ->
    case string:next_grapheme(Str) of
        [First | Rest] ->
            Upper = unicode:characters_to_binary(
                        string:uppercase(unicode:characters_to_binary([First]))),
            RestBin = unicode:characters_to_binary(Rest),
            <<Upper/binary, RestBin/binary>>;
        [] -> <<>>
    end.

%% @doc Grapheme-aware string reverse.
-spec reverse(binary()) -> binary().
reverse(Str) when is_binary(Str) ->
    unicode:characters_to_binary(string:reverse(Str)).

%% @doc Check if string contains substring.
-spec includes(binary(), binary()) -> boolean().
includes(_Str, <<>>) -> true;
includes(Str, Sub) when is_binary(Str), is_binary(Sub) ->
    binary:match(Str, Sub) =/= nomatch.

%% @doc Check if string starts with prefix.
-spec starts_with(binary(), binary()) -> boolean().
starts_with(_Str, <<>>) -> true;
starts_with(Str, Prefix) when is_binary(Str), is_binary(Prefix) ->
    PrefixSize = byte_size(Prefix),
    case Str of
        <<Prefix:PrefixSize/binary, _/binary>> -> true;
        _ -> false
    end.

%% @doc Check if string ends with suffix.
-spec ends_with(binary(), binary()) -> boolean().
ends_with(_Str, <<>>) -> true;
ends_with(Str, Suffix) when is_binary(Str), is_binary(Suffix) ->
    SuffixSize = byte_size(Suffix),
    StringSize = byte_size(Str),
    case SuffixSize =< StringSize of
        true ->
            PrefixSize = StringSize - SuffixSize,
            case Str of
                <<_:PrefixSize/binary, Suffix:SuffixSize/binary>> -> true;
                _ -> false
            end;
        false -> false
    end.

%% @doc Find first occurrence of substring, return 1-based grapheme index or nil.
-spec index_of(binary(), binary()) -> integer() | nil.
index_of(_Str, <<>>) -> nil;
index_of(Str, Sub) when is_binary(Str), is_binary(Sub) ->
    StrGs = string:to_graphemes(Str),
    SubGs = string:to_graphemes(Sub),
    index_of_graphemes(StrGs, SubGs, 1).

%% @doc Split string by pattern (global split).
-spec split_on(binary(), binary()) -> [binary()].
split_on(Str, <<>>) when is_binary(Str) -> [Str];
split_on(Str, Pattern) when is_binary(Str), is_binary(Pattern) ->
    binary:split(Str, Pattern, [global]).

%% @doc Repeat string N times.
-spec repeat(binary(), integer()) -> binary().
repeat(Str, N) when is_binary(Str), is_integer(N), N >= 0 ->
    binary:copy(Str, N).

%% @doc Convert string to list of grapheme binaries.
-spec as_list(binary()) -> [binary()].
as_list(Str) when is_binary(Str) ->
    [unicode:characters_to_binary([G]) || G <- string:to_graphemes(Str)].

%% @doc Iterate over graphemes, applying block to each. Returns nil.
-spec each(binary(), function()) -> nil.
each(Str, Block) when is_binary(Str), is_function(Block, 1) ->
    lists:foreach(Block, as_list(Str)),
    nil.

%% @doc Map over graphemes, applying block to each. Returns list of results.
-spec collect(binary(), function()) -> list().
collect(Str, Block) when is_binary(Str), is_function(Block, 1) ->
    lists:map(Block, as_list(Str)).

%% @doc Filter graphemes by block predicate. Returns new string.
-spec select(binary(), function()) -> binary().
select(Str, Block) when is_binary(Str), is_function(Block, 1) ->
    Graphemes = as_list(Str),
    Selected = lists:filter(Block, Graphemes),
    iolist_to_binary(Selected).

%% @doc Split string by newlines.
-spec lines(binary()) -> [binary()].
lines(Str) when is_binary(Str) ->
    binary:split(Str, [<<"\n">>, <<"\r\n">>], [global]).

%% @doc Split string by whitespace, filtering empty segments.
-spec words(binary()) -> [binary()].
words(Str) when is_binary(Str) ->
    Parts = binary:split(string:trim(Str), [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>], [global]),
    [P || P <- Parts, P =/= <<>>].

%% @doc First N graphemes.
-spec take(binary(), integer()) -> binary().
take(_Str, N) when is_integer(N), N =< 0 -> <<>>;
take(Str, N) when is_binary(Str), is_integer(N) ->
    Graphemes = as_list(Str),
    Taken = lists:sublist(Graphemes, N),
    iolist_to_binary(Taken).

%% @doc Skip first N graphemes.
-spec drop(binary(), integer()) -> binary().
drop(Str, N) when is_integer(N), N =< 0, is_binary(Str) -> Str;
drop(Str, N) when is_binary(Str), is_integer(N) ->
    Graphemes = as_list(Str),
    Dropped = lists:nthtail(min(N, length(Graphemes)), Graphemes),
    iolist_to_binary(Dropped).

%% @doc Test if string is empty or only whitespace.
-spec is_blank(binary()) -> boolean().
is_blank(Str) when is_binary(Str) ->
    string:trim(Str) =:= <<>>.

%% @doc Test if all characters are digits.
-spec is_digit(binary()) -> boolean().
is_digit(<<>>) -> false;
is_digit(Str) when is_binary(Str) ->
    lists:all(fun(G) ->
        case G of
            <<C>> when C >= $0, C =< $9 -> true;
            _ -> false
        end
    end, as_list(Str)).

%% @doc Test if all characters are alphabetic.
-spec is_alpha(binary()) -> boolean().
is_alpha(<<>>) -> false;
is_alpha(Str) when is_binary(Str) ->
    lists:all(fun(G) ->
        case G of
            <<C>> when C >= $a, C =< $z -> true;
            <<C>> when C >= $A, C =< $Z -> true;
            _ -> false
        end
    end, as_list(Str)).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Grapheme-aware substring search.
-spec index_of_graphemes([string:grapheme_cluster()], [string:grapheme_cluster()], integer()) ->
          integer() | nil.
index_of_graphemes([], _SubGs, _Idx) ->
    nil;
index_of_graphemes(StrGs, SubGs, Idx) ->
    case lists:prefix(SubGs, StrGs) of
        true -> Idx;
        false -> index_of_graphemes(tl(StrGs), SubGs, Idx + 1)
    end.

%% @private
%% @doc Return the Nth grapheme (1-based) from a UTF-8 binary.
-spec get_nth_grapheme(binary(), pos_integer()) -> {ok, binary()} | error.
get_nth_grapheme(Str, N) ->
    get_nth_grapheme(Str, N, 1).

get_nth_grapheme(<<>>, _N, _Current) ->
    error;
get_nth_grapheme(Str, N, Current) when Current =:= N ->
    case string:next_grapheme(Str) of
        [Grapheme | _] -> {ok, unicode:characters_to_binary([Grapheme])};
        {error, _} -> error;
        [] -> error
    end;
get_nth_grapheme(Str, N, Current) ->
    case string:next_grapheme(Str) of
        [_Grapheme | Rest] -> get_nth_grapheme(Rest, N, Current + 1);
        {error, _} -> error;
        [] -> error
    end.
