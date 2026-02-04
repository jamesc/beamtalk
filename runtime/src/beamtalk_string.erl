%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc String primitive class implementation.
%%%
%%% This module provides method dispatch for Erlang binaries (strings), mapping them
%%% to the Beamtalk `String` class. Supports string operations, reflection,
%%% and extension methods via the extension registry.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'String'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `size`   | []   | Byte size of string |
%%% | `length` | []   | Character count (grapheme count) |
%%% | `isEmpty` | []  | True if empty string |
%%% | `uppercase` | [] | Convert to uppercase |
%%% | `lowercase` | [] | Convert to lowercase |
%%% | `trim`   | []   | Remove leading/trailing whitespace |
%%% | `++`     | [Str] | Concatenate strings |
%%% | `concat:` | [Str] | Concatenate (keyword form) |
%%% | `at:`    | [Idx] | Character at index (1-based) |
%%% | `includes:` | [Substr] | Check if contains substring |
%%% | `startsWith:` | [Prefix] | Check if string starts with prefix |
%%% | `endsWith:` | [Suffix] | Check if string ends with suffix |
%%% | `indexOf:` | [Substr] | Find first occurrence (1-based index or nil) |
%%% | `replace:with:` | [Old, New] | Replace all occurrences |
%%% | `substring:to:` | [Start, End] | Extract substring (1-based, inclusive) |
%%% | `split:` | [Delim] | Split by delimiter |
%%% | `asInteger` | [] | Parse as integer (error if invalid) |
%%% | `instVarNames` | [] | Returns `[]` (no instance variables) |
%%% | `instVarAt` | [Name] | Returns `nil` (no fields) |
%%% | `instVarAt:put:` | [Name, Value] | Error: immutable primitive |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Reflection
%%% beamtalk_string:dispatch('class', [], <<"hello">>). % => 'String'
%%%
%%% %% Size operations
%%% beamtalk_string:dispatch('size', [], <<"hello">>).  % => 5
%%% beamtalk_string:dispatch('isEmpty', [], <<>>).       % => true
%%%
%%% %% Case conversion
%%% beamtalk_string:dispatch('uppercase', [], <<"hello">>). % => <<"HELLO">>
%%%
%%% %% Concatenation
%%% beamtalk_string:dispatch('++', [<<" world">>], <<"hello">>). % => <<"hello world">>
%%%
%%% %% Access
%%% beamtalk_string:dispatch('at:', [1], <<"hello">>). % => <<"h">>
%%%
%%% %% Search
%%% beamtalk_string:dispatch('includes:', [<<"ell">>], <<"hello">>). % => true
%%%
%%% %% Split
%%% beamtalk_string:dispatch('split:', [<<",">>], <<"a,b,c">>). % => [<<"a">>, <<"b">>, <<"c">>]
%%%
%%% %% Conversion
%%% beamtalk_string:dispatch('asInteger', [], <<"42">>). % => 42
%%%
%%% %% Check if method exists
%%% beamtalk_string:has_method('size').  % => true
%%% beamtalk_string:has_method('foo').   % => false (checks extensions too)
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_string).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to a string value.
%%
%% Tries builtin methods first, then falls back to the extension registry
%% for user-defined methods. Raises does_not_understand error if method not found.
%%
%% Examples:
%% ```
%% dispatch('size', [], <<"hello">>)    % => 5
%% dispatch('class', [], <<"hello">>)   % => 'String'
%% dispatch('unknown', [], <<"hello">>) % => error({does_not_understand, ...})
%% ```
-spec dispatch(atom(), list(), binary()) -> term().
dispatch(Selector, Args, Value) ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if a string responds to the given selector.
%%
%% Checks both builtin methods and the extension registry.
%%
%% Examples:
%% ```
%% has_method('size')  % => true
%% has_method('foo')   % => false (unless registered as extension)
%% ```
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    %% Check if builtin exists
    is_builtin(Selector) orelse beamtalk_extensions:has('String', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArgs:') -> true;
is_builtin('size') -> true;
is_builtin('length') -> true;
is_builtin('isEmpty') -> true;
is_builtin('uppercase') -> true;
is_builtin('lowercase') -> true;
is_builtin('trim') -> true;
is_builtin('++') -> true;
is_builtin('concat:') -> true;
is_builtin('at:') -> true;
is_builtin('includes:') -> true;
is_builtin('startsWith:') -> true;
is_builtin('endsWith:') -> true;
is_builtin('indexOf:') -> true;
is_builtin('replace:with:') -> true;
is_builtin('substring:to:') -> true;
is_builtin('split:') -> true;
is_builtin('asInteger') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin string methods.
%%
%% Returns {ok, Result} if method exists, not_found otherwise.
-spec builtin_dispatch(atom(), list(), binary()) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'String'};
builtin_dispatch('respondsTo', [Selector], _X) when is_atom(Selector) -> 
    {ok, has_method(Selector)};

%% Dynamic message send
builtin_dispatch('perform', [TargetSelector], X) when is_atom(TargetSelector) ->
    builtin_dispatch(TargetSelector, [], X);
builtin_dispatch('perform:withArgs:', [TargetSelector, ArgList], X) 
  when is_atom(TargetSelector), is_list(ArgList) ->
    builtin_dispatch(TargetSelector, ArgList, X);
builtin_dispatch('perform:withArgs:', [_TargetSelector, ArgList], _X)
  when not is_list(ArgList) ->
    %% Type error: ArgList must be a list (consistent with actor behavior)
    error({type_error, list, ArgList});


%% Size operations
builtin_dispatch('size', [], X) -> {ok, byte_size(X)};
builtin_dispatch('length', [], X) -> {ok, string:length(X)};  % Grapheme count
builtin_dispatch('isEmpty', [], X) -> {ok, byte_size(X) =:= 0};

%% Case conversion
builtin_dispatch('uppercase', [], X) -> {ok, string:uppercase(X)};
builtin_dispatch('lowercase', [], X) -> {ok, string:lowercase(X)};

%% Trimming
builtin_dispatch('trim', [], X) -> {ok, string:trim(X, both)};

%% Concatenation
builtin_dispatch('++', [Y], X) when is_binary(Y) -> {ok, <<X/binary, Y/binary>>};
builtin_dispatch('concat:', [Y], X) when is_binary(Y) -> {ok, <<X/binary, Y/binary>>};

%% Access
builtin_dispatch('at:', [Idx], X) when is_integer(Idx), Idx >= 1 ->
    %% 1-based indexing with UTF-8 grapheme support
    case string:next_grapheme(X) of
        [] when Idx =:= 1 andalso X =:= <<>> ->
            not_found;  % Empty string
        _ ->
            case get_nth_grapheme(X, Idx) of
                {ok, Grapheme} -> {ok, Grapheme};
                error -> not_found
            end
    end;

%% Search
builtin_dispatch('includes:', [Substr], X) when is_binary(Substr) ->
    case Substr of
        <<>> -> {ok, true};  % Empty substring is always found
        _ -> {ok, binary:match(X, Substr) =/= nomatch}
    end;

builtin_dispatch('startsWith:', [Prefix], X) when is_binary(Prefix) ->
    PrefixSize = byte_size(Prefix),
    case X of
        <<Prefix:PrefixSize/binary, _Rest/binary>> -> {ok, true};
        _ when Prefix =:= <<>> -> {ok, true};  % Empty prefix always matches
        _ -> {ok, false}
    end;

builtin_dispatch('endsWith:', [Suffix], X) when is_binary(Suffix) ->
    SuffixSize = byte_size(Suffix),
    StringSize = byte_size(X),
    case SuffixSize =< StringSize of
        true ->
            PrefixSize = StringSize - SuffixSize,
            case X of
                <<_Prefix:PrefixSize/binary, Suffix:SuffixSize/binary>> -> {ok, true};
                _ -> {ok, false}
            end;
        false when Suffix =:= <<>> -> {ok, true};  % Empty suffix always matches
        false -> {ok, false}
    end;

builtin_dispatch('indexOf:', [Substr], X) when is_binary(Substr) ->
    case Substr of
        <<>> -> {ok, nil};  % Empty substring has no position
        _ ->
            case binary:match(X, Substr) of
                nomatch -> {ok, nil};
                {BytePos, _Length} ->
                    %% Convert byte position to grapheme position (1-based)
                    <<Before:BytePos/binary, _Rest/binary>> = X,
                    GraphemePos = string:length(Before) + 1,
                    {ok, GraphemePos}
            end
    end;

builtin_dispatch('replace:with:', [Old, New], X) when is_binary(Old), is_binary(New) ->
    case Old of
        <<>> -> not_found;  % Cannot replace empty string
        _ -> {ok, binary:replace(X, Old, New, [global])}
    end;

builtin_dispatch('substring:to:', [Start, End], X) when is_integer(Start), is_integer(End), Start >= 1 ->
    Length = string:length(X),
    case Start =< Length of
        true ->
            case End < Start of
                true -> {ok, <<>>};  % Invalid range returns empty string
                false ->
                    ActualEnd = min(End, Length),
                    SubLength = ActualEnd - Start + 1,
                    case SubLength > 0 of
                        true ->
                            case get_substring_graphemes(X, Start, SubLength) of
                                {ok, Substring} -> {ok, Substring};
                                error -> not_found
                            end;
                        false -> {ok, <<>>}
                    end
            end;
        false -> not_found  % Start beyond string length
    end;

%% Split
builtin_dispatch('split:', [Delim], X) when is_binary(Delim) ->
    case Delim of
        <<>> -> not_found;  % Empty delimiter is invalid
        _ -> {ok, binary:split(X, Delim, [global])}
    end;

%% Conversion
builtin_dispatch('asInteger', [], X) ->
    try
        {ok, binary_to_integer(X)}
    catch
        error:badarg -> not_found  % Will trigger does_not_understand
    end;

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _X) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _X) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _Value], _X) -> 
    error(immutable_primitive_error('String', Name));

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @private
%% @doc Construct immutable_primitive error for String.
-spec immutable_primitive_error(atom(), term()) -> term().
immutable_primitive_error(Class, FieldName) ->
    Error0 = beamtalk_error:new(immutable_primitive, Class),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Strings are immutable. Use assignment (x := newValue) instead.">>),
    beamtalk_error:with_details(Error2, #{field => FieldName}).

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), binary()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('String', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            error({does_not_understand, 'String', Selector, length(Args)})
    end.

%% @doc Helper to get the Nth grapheme from a string.
-spec get_nth_grapheme(binary(), pos_integer()) -> {ok, binary()} | error.
get_nth_grapheme(Str, N) ->
    get_nth_grapheme(Str, N, 1).

get_nth_grapheme(<<>>, _N, _Current) ->
    error;
get_nth_grapheme(Str, N, Current) when Current =:= N ->
    case string:next_grapheme(Str) of
        [Grapheme | _Rest] -> {ok, unicode:characters_to_binary([Grapheme])};
        [] -> error
    end;
get_nth_grapheme(Str, N, Current) ->
    case string:next_grapheme(Str) of
        [_Grapheme | Rest] -> get_nth_grapheme(Rest, N, Current + 1);
        [] -> error
    end.

%% @doc Helper to extract a substring by grapheme range.
-spec get_substring_graphemes(binary(), pos_integer(), pos_integer()) -> {ok, binary()} | error.
get_substring_graphemes(Str, Start, Length) ->
    %% Skip to Start position
    SkipCount = Start - 1,
    case skip_graphemes(Str, SkipCount) of
        {ok, Rest} ->
            %% Take Length graphemes
            take_graphemes(Rest, Length);
        error -> error
    end.

skip_graphemes(Str, 0) -> {ok, Str};
skip_graphemes(<<>>, _N) -> error;
skip_graphemes(Str, N) ->
    case string:next_grapheme(Str) of
        [_Grapheme | Rest] -> skip_graphemes(Rest, N - 1);
        [] -> error
    end.

take_graphemes(Str, Length) ->
    take_graphemes(Str, Length, []).

take_graphemes(_Str, 0, Acc) ->
    {ok, unicode:characters_to_binary(lists:reverse(Acc))};
take_graphemes(<<>>, _Length, Acc) ->
    {ok, unicode:characters_to_binary(lists:reverse(Acc))};
take_graphemes(Str, Length, Acc) ->
    case string:next_grapheme(Str) of
        [Grapheme | Rest] ->
            take_graphemes(Rest, Length - 1, [Grapheme | Acc]);
        [] ->
            {ok, unicode:characters_to_binary(lists:reverse(Acc))}
    end.
