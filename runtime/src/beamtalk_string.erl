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
%%% | `size`   | []   | Byte size of string |
%%% | `length` | []   | Byte size (alias for size) |
%%% | `isEmpty` | []  | True if empty string |
%%% | `uppercase` | [] | Convert to uppercase |
%%% | `lowercase` | [] | Convert to lowercase |
%%% | `trim`   | []   | Remove leading/trailing whitespace |
%%% | `++`     | [Str] | Concatenate strings |
%%% | `concat:` | [Str] | Concatenate (keyword form) |
%%% | `at:`    | [Idx] | Character at index (1-based) |
%%% | `includes:` | [Substr] | Check if contains substring |
%%% | `split:` | [Delim] | Split by delimiter |
%%% | `asInteger` | [] | Parse as integer (error if invalid) |
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
is_builtin('split:') -> true;
is_builtin('asInteger') -> true;
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

%% Size operations
builtin_dispatch('size', [], X) -> {ok, byte_size(X)};
builtin_dispatch('length', [], X) -> {ok, byte_size(X)};
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
builtin_dispatch('at:', [Idx], X) when is_integer(Idx), Idx >= 1, Idx =< byte_size(X) ->
    %% 1-based indexing
    ByteIdx = Idx - 1,
    <<_:ByteIdx/binary, Char:8, _/binary>> = X,
    {ok, <<Char:8>>};

%% Search
builtin_dispatch('includes:', [Substr], X) when is_binary(Substr) ->
    case Substr of
        <<>> -> {ok, true};  % Empty substring is always found
        _ -> {ok, binary:match(X, Substr) =/= nomatch}
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

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), binary()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('String', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            error({does_not_understand, 'String', Selector, length(Args)})
    end.
