%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_string).

%%% **DDD Context:** Object System Context

-moduledoc """
String helper operations for compiled stdlib.

This module provides implementations for String primitive methods
that require more than simple Erlang BIF calls. Called by compiled
stdlib String module via `@primitive` codegen.

All operations are grapheme-aware and handle UTF-8 correctly.

Representation (BT-4170): a `String` is the tagged term
`{beamtalk_string, Bin}` — see `beamtalk_primitive:string_from_binary/1`,
the single authority for constructing it. Every function here matches
that tag in its head and wraps text results through that constructor.
Receivers also accept a bare binary (pre-flip Erlang callers stay valid —
see the `BT-4173` clauses); anything else raises `type_error` via
`raise_not_string/2` or, for the unary accessors that forward a bare binary,
`beamtalk_text:unwrap_or_raise/4`. Bare-binary join elements (`join_element/2`)
raise via `raise_not_string/2`.
""".
-type t() :: {beamtalk_string, binary()}.
-define(NOT_STRING_HINT, <<"Expected a String">>).

-export_type([t/0]).
-export([
    at/2,
    first/1,
    last/1,
    capitalize/1,
    ascii_lowercase/1,
    reverse/1,
    includes/2,
    starts_with/2,
    ends_with/2,
    index_of/2,
    split/2,
    split_on/2,
    repeat/2,
    as_list/1,
    as_charlist/1,
    join/1,
    join/2,
    each/2,
    collect/2,
    select/2,
    reject/2,
    lines/1,
    words/1,
    take/2,
    drop/2,
    is_blank/1,
    is_digit/1,
    is_alpha/1,
    from_code_point/1,
    from_code_points/1,
    from_iolist/1
]).
-export([parse_integer/2]).

%% Instance methods — URL encoding (explicit `(Erlang beamtalk_string) sel:
%% self` FFI dispatch, not @primitive — see docs/beamtalk-native-erlang.md
%% "The naming rule": function name is the first keyword with its colon
%% removed, no case conversion, so these are camelCase not snake_case).
-export([urlEncoded/1, urlDecoded/1]).

-doc """
1-based grapheme access. Returns the grapheme at the given index.

Mirrors `beamtalk_list:at/2` — indexing an empty String is
`empty_collection`, any other out-of-range index is `index_out_of_bounds`. An
index below 1 is malformed whether or not the String is empty, so that clause
is reached first (it is listed last, but `Idx >= 1` excludes it above).
""".
-spec at(t() | binary(), integer()) -> t().
at({beamtalk_string, <<>>}, Idx) when is_integer(Idx), Idx >= 1 ->
    raise_empty('at:');
at({beamtalk_string, Str}, Idx) when is_binary(Str), is_integer(Idx), Idx >= 1 ->
    case get_nth_grapheme(Str, Idx) of
        {ok, Grapheme} ->
            beamtalk_primitive:string_from_binary(Grapheme);
        error ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    index_out_of_bounds, 'String', 'at:', <<"Index is beyond string length">>
                )
            )
    end;
at({beamtalk_string, Str}, Idx) when is_binary(Str), is_integer(Idx) ->
    beamtalk_error:raise(
        beamtalk_error:new(
            index_out_of_bounds, 'String', 'at:', <<"Index must be >= 1 (1-based indexing)">>
        )
    );
%% BT-4173: a bare binary spells the same text (pre-flip calls stay valid).
at(Bin, Idx) when is_binary(Bin) -> at({beamtalk_string, Bin}, Idx);
at(Str, _Idx) ->
    raise_not_string('at:', Str).

-doc """
First grapheme cluster, as a String.

Raises `empty_collection` on `""` — the same kind `List first` raises,
so `on:do:` can handle both with one clause.
""".
-spec first(t() | binary()) -> t().
first({beamtalk_string, <<>>}) ->
    raise_empty('first');
first({beamtalk_string, Str}) when is_binary(Str) ->
    case string:next_grapheme(Str) of
        [Grapheme | _Rest] ->
            beamtalk_primitive:string_from_binary(unicode:characters_to_binary([Grapheme]));
        [] ->
            raise_empty('first')
    end;
first(Bin) ->
    first(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'first', ?NOT_STRING_HINT)}
    ).

-doc """
Last grapheme cluster, as a String.

Raises `empty_collection` on `""` (see `first/1`).
""".
-spec last(t() | binary()) -> t().
last({beamtalk_string, <<>>}) ->
    raise_empty('last');
last({beamtalk_string, Str}) when is_binary(Str) ->
    %% `string:slice/3` is grapheme-aware, so this is the final grapheme cluster
    %% — not the final byte or code point. Preferred over reversing the string:
    %% both are O(n) to traverse, but `string:reverse/1` also allocates a full
    %% reversed copy just to read one grapheme off the front.
    case string:slice(Str, string:length(Str) - 1, 1) of
        <<>> -> raise_empty('last');
        Grapheme -> beamtalk_primitive:string_from_binary(unicode:characters_to_binary(Grapheme))
    end;
last(Bin) ->
    last({beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'last', ?NOT_STRING_HINT)}).

-doc "Raise `empty_collection` for an accessor called on an empty String.".
-spec raise_empty(atom()) -> no_return().
raise_empty(Selector) ->
    Hint = <<"String is empty; guard with `isEmpty` before indexing">>,
    beamtalk_error:raise(beamtalk_error:new(empty_collection, 'String', Selector, Hint)).

-doc """
Raise `type_error` for a non-`String` receiver or argument (BT-4170).

A bare binary gets the downcast hint (`asString`); anything else gets
the plain expectation. Used as the catch-all clause of the functions that do
not forward a bare binary through `beamtalk_text:unwrap_or_raise/4`, so a `Binary` (or any other value) reaching a
`String`-only operation fails with an actionable error instead of a
bare `function_clause`.
""".
-spec raise_not_string(atom(), term()) -> no_return().
raise_not_string(Selector, X) when is_binary(X) ->
    beamtalk_error:raise_type_error(
        'String',
        Selector,
        <<"Expected a String; bare binaries are Binary; convert with asString">>
    );
raise_not_string(Selector, _) ->
    beamtalk_error:raise_type_error('String', Selector, ?NOT_STRING_HINT).

-doc """
Unwrap one join element to raw bytes (BT-4170).

Shared by `join/1` and the `collect:`/`select:`/`reject:`/`take:`/`drop:`
reassembly paths: every element must be a `String`. Anything else —
including a bare binary — raises naming `Selector`, so a `collect:`
block answering bytes fails here with an actionable error instead of
smuggling unchecked bytes into a `String`.
""".
-spec join_element(term(), atom()) -> binary().
join_element({beamtalk_string, Bin}, _Selector) when is_binary(Bin) ->
    Bin;
join_element(E, Selector) ->
    raise_not_string(Selector, E).

-doc """
ASCII-only lowercase: only `A`-`Z` fold; every other byte passes through.

UTF-8 multi-byte sequences consist solely of bytes >= 16#80, so non-ASCII
characters are untouched. Single O(n) pass.
""".
-spec ascii_lowercase(t() | binary()) -> t().
ascii_lowercase({beamtalk_string, Str}) when is_binary(Str) ->
    beamtalk_primitive:string_from_binary(<<
        <<
            (if
                C >= $A, C =< $Z -> C + 32;
                true -> C
            end)
        >>
     || <<C>> <= Str
    >>);
ascii_lowercase(Bin) ->
    ascii_lowercase(
        {beamtalk_string,
            beamtalk_text:unwrap_or_raise(Bin, 'String', 'asciiLowercase', ?NOT_STRING_HINT)}
    ).

-doc "Capitalize the first grapheme, keep rest unchanged.".
-spec capitalize(t() | binary()) -> t().
capitalize({beamtalk_string, <<>>}) ->
    beamtalk_primitive:string_from_binary(<<>>);
capitalize({beamtalk_string, Str}) when is_binary(Str) ->
    case string:next_grapheme(Str) of
        [First | Rest] ->
            Upper = unicode:characters_to_binary(
                string:uppercase(unicode:characters_to_binary([First]))
            ),
            RestBin = unicode:characters_to_binary(Rest),
            beamtalk_primitive:string_from_binary(<<Upper/binary, RestBin/binary>>);
        [] ->
            beamtalk_primitive:string_from_binary(<<>>)
    end;
capitalize(Bin) ->
    capitalize(
        {beamtalk_string,
            beamtalk_text:unwrap_or_raise(Bin, 'String', 'capitalize', ?NOT_STRING_HINT)}
    ).

-doc "Grapheme-aware string reverse.".
-spec reverse(t() | binary()) -> t().
reverse({beamtalk_string, Str}) when is_binary(Str) ->
    beamtalk_primitive:string_from_binary(unicode:characters_to_binary(string:reverse(Str)));
reverse(Bin) ->
    reverse(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'reverse', ?NOT_STRING_HINT)}
    ).

-doc "Check if string contains substring.".
-spec includes(t() | binary(), t() | binary()) -> boolean().
includes(_Str, {beamtalk_string, <<>>}) ->
    true;
includes({beamtalk_string, Str}, {beamtalk_string, Sub}) when
    is_binary(Str), is_binary(Sub)
->
    binary:match(Str, Sub) =/= nomatch;
includes(S, Sub) when is_binary(S), is_binary(Sub) ->
    includes({beamtalk_string, S}, {beamtalk_string, Sub});
includes({beamtalk_string, _} = S, Sub) when is_binary(Sub) -> includes(S, {beamtalk_string, Sub});
includes(S, {beamtalk_string, _} = Sub) when is_binary(S) -> includes({beamtalk_string, S}, Sub);
includes(Str, _) ->
    raise_not_string('includes:', Str).

-doc "Check if string starts with prefix.".
-spec starts_with(t() | binary(), t() | binary()) -> boolean().
starts_with(_Str, {beamtalk_string, <<>>}) ->
    true;
starts_with({beamtalk_string, Str}, {beamtalk_string, Prefix}) when
    is_binary(Str), is_binary(Prefix)
->
    PrefixSize = byte_size(Prefix),
    case Str of
        <<Prefix:PrefixSize/binary, _/binary>> -> true;
        _ -> false
    end;
starts_with(S, Sub) when is_binary(S), is_binary(Sub) ->
    starts_with({beamtalk_string, S}, {beamtalk_string, Sub});
starts_with({beamtalk_string, _} = S, Sub) when is_binary(Sub) ->
    starts_with(S, {beamtalk_string, Sub});
starts_with(S, {beamtalk_string, _} = Sub) when is_binary(S) ->
    starts_with({beamtalk_string, S}, Sub);
starts_with(Str, _) ->
    raise_not_string('startsWith:', Str).

-doc "Check if string ends with suffix.".
-spec ends_with(t() | binary(), t() | binary()) -> boolean().
ends_with(_Str, {beamtalk_string, <<>>}) ->
    true;
ends_with({beamtalk_string, Str}, {beamtalk_string, Suffix}) when
    is_binary(Str), is_binary(Suffix)
->
    SuffixSize = byte_size(Suffix),
    StringSize = byte_size(Str),
    case SuffixSize =< StringSize of
        true ->
            PrefixSize = StringSize - SuffixSize,
            case Str of
                <<_:PrefixSize/binary, Suffix:SuffixSize/binary>> -> true;
                _ -> false
            end;
        false ->
            false
    end;
ends_with(S, Sub) when is_binary(S), is_binary(Sub) ->
    ends_with({beamtalk_string, S}, {beamtalk_string, Sub});
ends_with({beamtalk_string, _} = S, Sub) when is_binary(Sub) ->
    ends_with(S, {beamtalk_string, Sub});
ends_with(S, {beamtalk_string, _} = Sub) when is_binary(S) -> ends_with({beamtalk_string, S}, Sub);
ends_with(Str, _) ->
    raise_not_string('endsWith:', Str).

-doc "Find first occurrence of substring, return 1-based grapheme index or nil.".
-spec index_of(t() | binary(), t() | binary()) -> integer() | 'nil'.
index_of(_Str, {beamtalk_string, <<>>}) ->
    nil;
index_of({beamtalk_string, Str}, {beamtalk_string, Sub}) when
    is_binary(Str), is_binary(Sub)
->
    case byte_per_grapheme(Str) andalso byte_per_grapheme(Sub) of
        true ->
            %% BT-4761: every byte is its own grapheme, so the byte offset is the
            %% grapheme index and no grapheme lists are built.
            case binary:match(Str, Sub) of
                nomatch -> nil;
                {Pos, _Len} -> Pos + 1
            end;
        false ->
            StrGs = string:to_graphemes(Str),
            SubGs = string:to_graphemes(Sub),
            index_of_graphemes(StrGs, SubGs, 1)
    end;
index_of(S, Sub) when is_binary(S), is_binary(Sub) ->
    index_of({beamtalk_string, S}, {beamtalk_string, Sub});
index_of({beamtalk_string, _} = S, Sub) when is_binary(Sub) -> index_of(S, {beamtalk_string, Sub});
index_of(S, {beamtalk_string, _} = Sub) when is_binary(S) -> index_of({beamtalk_string, S}, Sub);
index_of(Str, _) ->
    raise_not_string('indexOf:', Str).

-doc "Global `binary:split/3`, wrapping each part as a String (backs `split:`).".
-spec split(t() | binary(), t() | binary()) -> [t()].
split({beamtalk_string, Str}, {beamtalk_string, Sep}) when
    is_binary(Str), is_binary(Sep)
->
    [beamtalk_primitive:string_from_binary(P) || P <- binary:split(Str, Sep, [global])];
split(S, Sub) when is_binary(S), is_binary(Sub) ->
    split({beamtalk_string, S}, {beamtalk_string, Sub});
split({beamtalk_string, _} = S, Sub) when is_binary(Sub) -> split(S, {beamtalk_string, Sub});
split(S, {beamtalk_string, _} = Sub) when is_binary(S) -> split({beamtalk_string, S}, Sub);
split(Str, _) ->
    raise_not_string('split:', Str).

-doc "Split string by pattern (global split).".
-spec split_on(t() | binary(), t() | binary()) -> [t()].
split_on({beamtalk_string, Str}, {beamtalk_string, <<>>}) when is_binary(Str) ->
    [beamtalk_primitive:string_from_binary(Str)];
split_on({beamtalk_string, Str}, {beamtalk_string, Pattern}) when
    is_binary(Str), is_binary(Pattern)
->
    [beamtalk_primitive:string_from_binary(P) || P <- binary:split(Str, Pattern, [global])];
split_on(S, Sub) when is_binary(S), is_binary(Sub) ->
    split_on({beamtalk_string, S}, {beamtalk_string, Sub});
split_on({beamtalk_string, _} = S, Sub) when is_binary(Sub) -> split_on(S, {beamtalk_string, Sub});
split_on(S, {beamtalk_string, _} = Sub) when is_binary(S) -> split_on({beamtalk_string, S}, Sub);
split_on(Str, _) ->
    raise_not_string('splitOn:', Str).

-doc "Repeat string N times.".
-spec repeat(t() | binary(), integer()) -> t().
repeat({beamtalk_string, Str}, N) when is_binary(Str), is_integer(N), N >= 0 ->
    beamtalk_primitive:string_from_binary(binary:copy(Str, N));
repeat(Bin, N) when is_binary(Bin) -> repeat({beamtalk_string, Bin}, N);
repeat(Str, _) ->
    raise_not_string('repeat:', Str).

-doc "Convert string to a list of grapheme Strings.".
-spec as_list(t() | binary()) -> [t()].
as_list({beamtalk_string, Str}) when is_binary(Str) ->
    [
        beamtalk_primitive:string_from_binary(unicode:characters_to_binary([G]))
     || G <- string:to_graphemes(Str)
    ];
as_list(Bin) ->
    as_list(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'asList', ?NOT_STRING_HINT)}
    ).

-doc """
Convert a String to an Erlang charlist of Unicode code points (BT-4380).

Uses `unicode:characters_to_list/2` — the OTP charlist convention, and the
same conversion the FFI proxy applies when it retries a call with a
`String` argument coerced to a charlist (`beamtalk_erlang_proxy`'s
`coerce_arg/1`), so `"café" asCharlist` names the same file as passing
`"café"` straight to `file:`/`filename:`. A `String` built with
`asStringUnchecked` from bytes that are not valid UTF-8 has no code-point
reading and raises a `type_error` rather than answering a partial list.

A bare binary is accepted too, for symmetry with this module's other
`t() | binary()` entry points. Beamtalk never sends `asCharlist` to a
`Binary` (BT-4380: raw bytes have no charlist reading), so this clause is
reachable only from a direct Erlang call; it reads valid UTF-8 the same way,
and its error names `Binary`, not `String` (BT-4382).
""".
-spec as_charlist(t() | binary()) -> [char()].
as_charlist({beamtalk_string, Str}) when is_binary(Str) ->
    utf8_code_points(
        Str, 'String', <<"String is not valid UTF-8; use toBytes for the raw bytes">>
    );
as_charlist(Bin) when is_binary(Bin) ->
    utf8_code_points(
        Bin,
        'Binary',
        <<"Binary is not valid UTF-8 and has no charlist reading; use toBytes for its bytes">>
    );
as_charlist(Str) ->
    raise_not_string('asCharlist', Str).

-spec utf8_code_points(binary(), atom(), binary()) -> [char()].
utf8_code_points(Bin, Class, Hint) ->
    case unicode:characters_to_list(Bin, utf8) of
        Chars when is_list(Chars) -> Chars;
        _ -> beamtalk_error:raise_type_error(Class, asCharlist, Hint)
    end.

-doc """
Join a list of grapheme Strings into a single string.

Used by `String class withAll:` to reconstruct a String from a
list of grapheme elements (e.g., the result of `select:`).

BT-4170: every element must be a `String` — a raw binary element is
`Binary`, not text, and joining it unchecked could smuggle invalid
UTF-8 into the result. Use `asString` on such elements first.
""".
-spec join([t() | binary()]) -> t().
join(List) ->
    join(List, 'withAll:').

-doc """
Join a list of `String`s, naming `Selector` in element errors (BT-4170).

Backs `String withAll:` (via `join/1`) and `List join`/`join:` — one
strict-join implementation, precise attribution per caller.
""".
-spec join(term(), atom()) -> t().
join(List, Selector) when is_list(List) ->
    beamtalk_primitive:string_from_binary(
        iolist_to_binary([join_element(E, Selector) || E <- List])
    );
join(_, Selector) ->
    beamtalk_error:raise_type_error(
        'String', Selector, <<"Expected a List of Strings">>
    ).

-doc "Iterate over graphemes, applying block to each. Returns nil.".
-spec each(t() | binary(), function()) -> 'nil'.
each({beamtalk_string, _} = Str, Block) when is_function(Block, 1) ->
    lists:foreach(Block, as_list(Str)),
    nil;
each(Bin, Block) when is_binary(Bin) -> each({beamtalk_string, Bin}, Block);
each(Str, _) ->
    raise_not_string('do:', Str).

-doc "Map over graphemes, applying block to each. Returns new string.".
-spec collect(t() | binary(), function()) -> t().
collect({beamtalk_string, _} = Str, Block) when is_function(Block, 1) ->
    Mapped = [Block(C) || C <- as_list(Str)],
    beamtalk_primitive:string_from_binary(
        iolist_to_binary([join_element(E, 'collect:') || E <- Mapped])
    );
collect(Bin, Block) when is_binary(Bin) -> collect({beamtalk_string, Bin}, Block);
collect(Str, _) ->
    raise_not_string('collect:', Str).

-doc "Filter graphemes by block predicate. Returns new string.".
-spec select(t() | binary(), function()) -> t().
select({beamtalk_string, _} = Str, Block) when is_function(Block, 1) ->
    Graphemes = as_list(Str),
    Selected = lists:filter(Block, Graphemes),
    beamtalk_primitive:string_from_binary(
        iolist_to_binary([join_element(E, 'select:') || E <- Selected])
    );
select(Bin, Block) when is_binary(Bin) -> select({beamtalk_string, Bin}, Block);
select(Str, _) ->
    raise_not_string('select:', Str).

-doc "Reject graphemes for which block returns true. Returns new string.".
-spec reject(t() | binary(), function()) -> t().
reject({beamtalk_string, _} = Str, Block) when is_function(Block, 1) ->
    Graphemes = as_list(Str),
    Kept = lists:filter(fun(G) -> not Block(G) end, Graphemes),
    beamtalk_primitive:string_from_binary(
        iolist_to_binary([join_element(E, 'reject:') || E <- Kept])
    );
reject(Bin, Block) when is_binary(Bin) -> reject({beamtalk_string, Bin}, Block);
reject(Str, _) ->
    raise_not_string('reject:', Str).

-doc "Split string by newlines.".
-spec lines(t() | binary()) -> [t()].
lines({beamtalk_string, Str}) when is_binary(Str) ->
    [
        beamtalk_primitive:string_from_binary(P)
     || P <- binary:split(Str, [<<"\n">>, <<"\r\n">>], [global])
    ];
lines(Bin) ->
    lines(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'lines', ?NOT_STRING_HINT)}
    ).

-doc "Split string by whitespace, filtering empty segments.".
-spec words(t() | binary()) -> [t()].
words({beamtalk_string, Str}) when is_binary(Str) ->
    Parts = binary:split(string:trim(Str), [<<" ">>, <<"\t">>, <<"\n">>, <<"\r">>], [global]),
    [beamtalk_primitive:string_from_binary(P) || P <- Parts, P =/= <<>>];
words(Bin) ->
    words(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'words', ?NOT_STRING_HINT)}
    ).

-doc "First N graphemes.".
-spec take(t() | binary(), integer()) -> t().
take(_Str, N) when is_integer(N), N =< 0 ->
    beamtalk_primitive:string_from_binary(<<>>);
take({beamtalk_string, Bin} = Str, N) when is_integer(N) ->
    case byte_per_grapheme(Bin) of
        true ->
            %% BT-4761: byte slice, no grapheme list.
            beamtalk_primitive:string_from_binary(binary:part(Bin, 0, min(N, byte_size(Bin))));
        false ->
            Graphemes = as_list(Str),
            Taken = lists:sublist(Graphemes, N),
            beamtalk_primitive:string_from_binary(
                iolist_to_binary([join_element(E, 'take:') || E <- Taken])
            )
    end;
take(Bin, N) when is_binary(Bin) -> take({beamtalk_string, Bin}, N);
take(Str, _) ->
    raise_not_string('take:', Str).

-doc "Skip first N graphemes.".
-spec drop(t() | binary(), integer()) -> t().
drop({beamtalk_string, _} = Str, N) when is_integer(N), N =< 0 -> Str;
drop({beamtalk_string, Bin} = Str, N) when is_integer(N) ->
    case byte_per_grapheme(Bin) of
        true ->
            %% BT-4761: byte slice, no grapheme list.
            Skip = min(N, byte_size(Bin)),
            beamtalk_primitive:string_from_binary(binary:part(Bin, Skip, byte_size(Bin) - Skip));
        false ->
            Graphemes = as_list(Str),
            Dropped = lists:nthtail(min(N, length(Graphemes)), Graphemes),
            beamtalk_primitive:string_from_binary(
                iolist_to_binary([join_element(E, 'drop:') || E <- Dropped])
            )
    end;
drop(Bin, N) when is_binary(Bin) -> drop({beamtalk_string, Bin}, N);
drop(Str, _) ->
    raise_not_string('drop:', Str).

-doc "Test if string is empty or only whitespace.".
-spec is_blank(t() | binary()) -> boolean().
is_blank({beamtalk_string, Str}) when is_binary(Str) ->
    string:trim(Str) =:= <<>>;
is_blank(Bin) ->
    is_blank(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'isBlank', ?NOT_STRING_HINT)}
    ).

-doc "Test if all characters are digits.".
-spec is_digit(t() | binary()) -> boolean().
is_digit({beamtalk_string, <<>>}) ->
    false;
is_digit({beamtalk_string, _} = Str) ->
    lists:all(
        fun(G) ->
            case G of
                {beamtalk_string, <<C>>} when C >= $0, C =< $9 -> true;
                _ -> false
            end
        end,
        as_list(Str)
    );
is_digit(Bin) ->
    is_digit(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'isDigit', ?NOT_STRING_HINT)}
    ).

-doc "Test if all characters are alphabetic.".
-spec is_alpha(t() | binary()) -> boolean().
is_alpha({beamtalk_string, <<>>}) ->
    false;
is_alpha({beamtalk_string, _} = Str) ->
    lists:all(
        fun(G) ->
            case G of
                {beamtalk_string, <<C>>} when C >= $a, C =< $z -> true;
                {beamtalk_string, <<C>>} when C >= $A, C =< $Z -> true;
                _ -> false
            end
        end,
        as_list(Str)
    );
is_alpha(Bin) ->
    is_alpha(
        {beamtalk_string, beamtalk_text:unwrap_or_raise(Bin, 'String', 'isAlpha', ?NOT_STRING_HINT)}
    ).

-doc """
Parse text as an integer in the given radix (BT-4544).

Backs `Integer parse:radix:` — the fallible, `Result`-based counterpart to
`String>>asInteger`/`asIntegerRadix:`. Accepts a `String` or a bare `Binary`
(both spell the same bytes); anything else raises `type_error`.

Radix must be between 2 and 36 (`invalid_argument` otherwise — a caller bug,
not a parse failure). Digits above 9 are case-insensitive and a leading
`-`/`+` sign is accepted (`binary_to_integer/2` semantics). Empty input and
bad digits answer `Result error:` with a `parse_error` carrying a hint
(`empty integer`, `not a base-N integer`).
""".
-spec parse_integer(t() | binary(), integer()) -> beamtalk_result:t().
parse_integer({beamtalk_string, Bin}, Base) when is_binary(Bin) ->
    parse_integer(Bin, Base);
parse_integer(Bin, Base) when is_binary(Bin), is_integer(Base) ->
    case Base >= 2 andalso Base =< 36 of
        false ->
            Error0 = beamtalk_error:new(invalid_argument, 'Integer'),
            Error1 = beamtalk_error:with_selector(Error0, 'parse:radix:'),
            Error2 = beamtalk_error:with_details(Error1, #{got => Base}),
            Error3 = beamtalk_error:with_hint(
                Error2, <<"Radix must be between 2 and 36">>
            ),
            beamtalk_error:raise(Error3);
        true when byte_size(Bin) =:= 0 ->
            parse_radix_error(<<"empty integer">>);
        true ->
            try binary_to_integer(Bin, Base) of
                Int ->
                    beamtalk_result:from_tagged_tuple({ok, Int})
            catch
                error:badarg ->
                    parse_radix_error(
                        <<"not a base-", (integer_to_binary(Base))/binary, " integer">>
                    )
            end
    end;
parse_integer(_Text, Base) when not is_integer(Base) ->
    beamtalk_error:raise_type_error(
        'Integer', 'parse:radix:', <<"Radix must be an Integer between 2 and 36">>
    );
parse_integer(_Text, _Base) ->
    beamtalk_error:raise_type_error(
        'Integer', 'parse:radix:', <<"Text must be a String or Binary">>
    ).

-doc "Wrap `Hint` in the `parse_error` `Result error:` `parse:radix:` answers.".
-spec parse_radix_error(binary()) -> beamtalk_result:t().
parse_radix_error(Hint) ->
    Error0 = beamtalk_error:new(parse_error, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, 'parse:radix:'),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_result:from_tagged_tuple({error, Error2}).

-doc "Create a single-character UTF-8 String from a Unicode code point.".
-spec from_code_point(integer()) -> t().
from_code_point(CodePoint) when is_integer(CodePoint), CodePoint >= 0 ->
    case unicode:characters_to_binary([CodePoint]) of
        Bin when is_binary(Bin) -> beamtalk_primitive:string_from_binary(Bin);
        _ ->
            beamtalk_error:raise_type_error(
                'String', 'fromCodePoint:', <<"Invalid Unicode code point">>
            )
    end;
from_code_point(CodePoint) when is_integer(CodePoint) ->
    beamtalk_error:raise_type_error(
        'String', 'fromCodePoint:', <<"Code point must be a non-negative integer">>
    );
from_code_point(_) ->
    beamtalk_error:raise_type_error(
        'String', 'fromCodePoint:', <<"Code point must be an Integer">>
    ).

-doc "Create a UTF-8 String from a list of Unicode code points.".
-spec from_code_points([integer()]) -> t().
from_code_points(List) when is_list(List) ->
    case lists:all(fun(X) -> is_integer(X) andalso X >= 0 end, List) of
        true ->
            case unicode:characters_to_binary(List) of
                Bin when is_binary(Bin) -> beamtalk_primitive:string_from_binary(Bin);
                _ ->
                    beamtalk_error:raise_type_error(
                        'String', 'fromCodePoints:', <<"Invalid Unicode code points in list">>
                    )
            end;
        false ->
            beamtalk_error:raise_type_error(
                'String',
                'fromCodePoints:',
                <<"List must contain only non-negative integer code points">>
            )
    end;
from_code_points(_) ->
    beamtalk_error:raise_type_error(
        'String', 'fromCodePoints:', <<"Expected a List of Integer code points">>
    ).

-doc """
Coerce an Erlang iolist or charlist to a UTF-8 String.

Strategy: try iolist_to_binary/1 first to preserve byte-oriented iodata,
then validate/normalise the result as UTF-8. Only fall back to treating the
original list as a Unicode charlist of code points when the byte-oriented
path fails — this correctly handles both UTF-8 byte iolists such as
[195, 169] and Unicode codepoint charlists such as [233], both yielding "é".

BT-4170: tagged `String` elements unwrap before assembly (a raw binary
element is bytes — the assembled result is validated as UTF-8, so the
wrap stays sound). A top-level `String` passes through; a top-level
bare binary is validated, not trusted.
""".
-spec from_iolist(iolist() | binary() | t() | binary()) -> t().
from_iolist({beamtalk_string, _} = Str) ->
    Str;
from_iolist(X) when is_binary(X) ->
    case beamtalk_binary:check_utf8(X) of
        {ok, Valid} ->
            beamtalk_primitive:string_from_binary(Valid);
        {error, _} ->
            beamtalk_error:raise_type_error(
                'String', 'fromIolist:', <<"Binary is not valid UTF-8">>
            )
    end;
from_iolist(X) when is_list(X) ->
    Flat = beamtalk_primitive:deep_unwrap_strings(X),
    case
        try
            {ok, iolist_to_binary(Flat)}
        catch
            _:_ -> error
        end
    of
        {ok, Bytes} when is_binary(Bytes) ->
            %% Assemble bytes first; then validate/normalise as UTF-8.
            case
                try
                    {ok, unicode:characters_to_binary(Bytes)}
                catch
                    _:_ -> error
                end
            of
                {ok, UtfBin} when is_binary(UtfBin) ->
                    beamtalk_primitive:string_from_binary(UtfBin);
                _ ->
                    %% Byte assembly succeeded but the bytes are not valid UTF-8.
                    %% Fall back: interpret the original list as a charlist of code points.
                    case
                        try
                            {ok, unicode:characters_to_binary(Flat)}
                        catch
                            _:_ -> error
                        end
                    of
                        {ok, UtfBin2} when is_binary(UtfBin2) ->
                            beamtalk_primitive:string_from_binary(UtfBin2);
                        _ ->
                            beamtalk_error:raise_type_error(
                                'String', 'fromIolist:', <<"Could not convert iolist to String">>
                            )
                    end
            end;
        _ ->
            %% iolist_to_binary/1 failed — try treating the list as a charlist directly.
            case
                try
                    {ok, unicode:characters_to_binary(Flat)}
                catch
                    _:_ -> error
                end
            of
                {ok, UtfBin} when is_binary(UtfBin) ->
                    beamtalk_primitive:string_from_binary(UtfBin);
                _ ->
                    beamtalk_error:raise_type_error(
                        'String', 'fromIolist:', <<"Could not convert iolist to String">>
                    )
            end
    end;
from_iolist(_) ->
    beamtalk_error:raise_type_error(
        'String', 'fromIolist:', <<"Expected an iolist (List) or String">>
    ).

-doc """
Percent-encode characters outside the URL-safe unreserved set (RFC 3986 §2.3:
letters, digits, `-`, `.`, `_`, `~`) via `uri_string:quote/1`.
""".
-spec urlEncoded(t() | binary()) -> t().
urlEncoded({beamtalk_string, Str}) when is_binary(Str) ->
    %% `uri_string:quote/1` output is ASCII by construction, so the wrap
    %% is sound without re-validating.
    beamtalk_primitive:string_from_binary(uri_string:quote(Str));
urlEncoded(Bin) ->
    urlEncoded(
        {beamtalk_string,
            beamtalk_text:unwrap_or_raise(Bin, 'String', 'urlEncoded', ?NOT_STRING_HINT)}
    ).

-doc """
Decode a percent-encoded string via `uri_string:unquote/1`.

Raises a structured `parse_error` when a `%` is followed by two characters
that aren't both valid hex digits (e.g. `%ZZ`, `%G0`) — `uri_string:unquote/1`
throws `{error, invalid_percent_encoding, Input}` in that case rather than
returning a value. A truncated escape at the end of the string (`%2`, or a
bare trailing `%`) is left unchanged rather than rejected — that leniency
comes from `uri_string:unquote/1` itself, not this wrapper.

BT-4170: `%XX` escapes decode to arbitrary bytes, so the result is only
a `String` when it is valid UTF-8 — anything else raises `parse_error`
(this is a text boundary: bytes become text here or not at all).
""".
-spec urlDecoded(t() | binary()) -> t().
urlDecoded({beamtalk_string, Str}) when is_binary(Str) ->
    try
        Decoded = uri_string:unquote(Str),
        case beamtalk_binary:check_utf8(Decoded) of
            {ok, Valid} ->
                beamtalk_primitive:string_from_binary(Valid);
            {error, _} ->
                raise_url_decode_error(
                    <<"Decoded bytes are not valid UTF-8; urlDecoded answers a String">>
                )
        end
    catch
        throw:{error, _Reason, _Input} ->
            raise_url_decode_error(
                <<"Malformed percent-encoding; a '%' must be followed by two hex digits">>
            )
    end;
urlDecoded(Bin) ->
    urlDecoded(
        {beamtalk_string,
            beamtalk_text:unwrap_or_raise(Bin, 'String', 'urlDecoded', ?NOT_STRING_HINT)}
    ).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc "Raise a structured parse_error for a malformed percent-encoded string.".
-spec raise_url_decode_error(binary()) -> no_return().
raise_url_decode_error(Hint) ->
    Error0 = beamtalk_error:new(parse_error, 'String'),
    Error1 = beamtalk_error:with_selector(Error0, 'urlDecoded'),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).

-doc """
True when every byte of the binary is a whole grapheme cluster on its own (BT-4761).

That holds for pure ASCII with no carriage return: UTF-8 multi-byte text can
carry combining marks / ZWJ sequences, and `\r\n` is a single grapheme. Such
binaries take the byte-level fast paths; anything else uses the grapheme walk.
Single O(n) pass, no allocation.
""".
-spec byte_per_grapheme(binary()) -> boolean().
byte_per_grapheme(<<C, Rest/binary>>) when C < 128, C =/= $\r -> byte_per_grapheme(Rest);
byte_per_grapheme(<<>>) -> true;
byte_per_grapheme(_) -> false.

-doc "Grapheme-aware substring search.".
-spec index_of_graphemes([string:grapheme_cluster()], [string:grapheme_cluster()], integer()) ->
    integer() | 'nil'.
index_of_graphemes([], _SubGs, _Idx) ->
    nil;
index_of_graphemes(StrGs, SubGs, Idx) ->
    case lists:prefix(SubGs, StrGs) of
        true -> Idx;
        false -> index_of_graphemes(tl(StrGs), SubGs, Idx + 1)
    end.

-doc "Return the Nth grapheme (1-based) from a UTF-8 binary.".
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
