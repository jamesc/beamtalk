%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Association class implementation (key-value pairs).
%%%
%%% This module provides method dispatch for Association instances, which are
%%% tagged Erlang tuples of the form {association, Key, Value}. Associations
%%% are created with the `->` binary message on Object.
%%% Example: `#name -> 'James'` creates `{association, name, <<"James">>}`.
%%%
%%% ## Builtin Methods
%%%
%%% | Selector | Args | Description |
%%% |----------|------|-------------|
%%% | `class`  | []   | Returns `'Association'` |
%%% | `respondsTo` | [Sel] | Returns true if responds to selector |
%%% | `key`    | []   | Returns the key (first element) |
%%% | `value`  | []   | Returns the value (second element) |
%%% | `asString` | [] | Format as `#key -> value` |
%%% | `instVarNames` | [] | Returns `[]` (no instance variables) |
%%% | `instVarAt` | [Name] | Returns `nil` (no fields) |
%%% | `instVarAt:put:` | [Name, Value] | Error: immutable primitive |
%%%
%%% ## Usage Examples
%%%
%%% ```erlang
%%% %% Create association (via Object dispatch)
%%% Assoc = {association, name, <<"James">>}.
%%%
%%% %% Access
%%% beamtalk_association:dispatch('key', [], Assoc). % => name
%%% beamtalk_association:dispatch('value', [], Assoc). % => <<"James">>
%%%
%%% %% String representation
%%% beamtalk_association:dispatch('asString', [], Assoc). % => <<"#name -> James">>
%%% ```

-module(beamtalk_association).
-export([dispatch/3, has_method/1]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Dispatch a message to an Association value.
-spec dispatch(atom(), list(), tuple()) -> term().
dispatch(Selector, Args, Value) when is_tuple(Value), tuple_size(Value) =:= 3,
                                       element(1, Value) =:= association ->
    case builtin_dispatch(Selector, Args, Value) of
        {ok, Result} -> Result;
        not_found -> does_not_understand(Selector, Args, Value)
    end.

%% @doc Check if an Association responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(Selector) ->
    is_builtin(Selector) orelse beamtalk_extensions:has('Association', Selector).

%% @doc Check if a selector is a builtin method.
-spec is_builtin(atom()) -> boolean().
is_builtin('class') -> true;
is_builtin('respondsTo') -> true;
is_builtin('perform') -> true;
is_builtin('perform:withArguments:') -> true;
is_builtin('key') -> true;
is_builtin('value') -> true;
is_builtin('asString') -> true;
is_builtin('instVarNames') -> true;
is_builtin('instVarAt') -> true;
is_builtin('instVarAt:put:') -> true;
is_builtin(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Dispatch to builtin Association methods.
-spec builtin_dispatch(atom(), list(), tuple()) -> {ok, term()} | not_found.

%% Reflection
builtin_dispatch('class', [], _X) -> {ok, 'Association'};
builtin_dispatch('respondsTo', [Selector], _X) when is_atom(Selector) -> 
    {ok, has_method(Selector)};

%% Dynamic message send
builtin_dispatch('perform', [TargetSelector], X) when is_atom(TargetSelector) ->
    builtin_dispatch(TargetSelector, [], X);
builtin_dispatch('perform:withArguments:', [TargetSelector, ArgList], X) 
  when is_atom(TargetSelector), is_list(ArgList) ->
    builtin_dispatch(TargetSelector, ArgList, X);
builtin_dispatch('perform:withArguments:', [_TargetSelector, ArgList], _X)
  when not is_list(ArgList) ->
    %% Type error: ArgList must be a list
    Error0 = beamtalk_error:new(type_error, 'Association'),
    Error = beamtalk_error:with_selector(Error0, 'perform:withArguments:'),
    error(Error);

%% Access
builtin_dispatch('key', [], {association, Key, _Value}) -> {ok, Key};
builtin_dispatch('value', [], {association, _Key, Value}) -> {ok, Value};

%% Conversion
builtin_dispatch('asString', [], {association, Key, Value}) ->
    %% Format as "#key -> value"
    KeyStr = format_element(Key),
    ValueStr = format_element(Value),
    Result = iolist_to_binary([KeyStr, <<" -> ">>, ValueStr]),
    {ok, Result};

%% Instance variable reflection (BT-164)
%% Primitives are immutable and have no instance variables
builtin_dispatch('instVarNames', [], _Assoc) -> 
    {ok, []};
builtin_dispatch('instVarAt', [_Name], _Assoc) -> 
    {ok, nil};
builtin_dispatch('instVarAt:put:', [Name, _Value], _Assoc) -> 
    error(immutable_primitive_error('Association', Name));

%% Not a builtin method
builtin_dispatch(_, _, _) -> not_found.

%% @private
%% @doc Construct immutable_primitive error for Association.
-spec immutable_primitive_error(atom(), term()) -> beamtalk_error:error().
immutable_primitive_error(Class, FieldName) ->
    Error0 = beamtalk_error:new(immutable_primitive, Class),
    Error1 = beamtalk_error:with_selector(Error0, 'instVarAt:put:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Associations are immutable. Create a new association instead.">>),
    beamtalk_error:with_details(Error2, #{field => FieldName}).

%% @doc Format a single element for string representation.
-spec format_element(term()) -> binary().
format_element(X) when is_atom(X) -> 
    %% Symbols get # prefix
    beamtalk_primitive:print_string(X);
format_element(X) when is_binary(X) -> X;
format_element(X) when is_integer(X) -> integer_to_binary(X);
format_element(X) when is_float(X) -> float_to_binary(X);
format_element(_X) -> <<"<term>">>.

%% @doc Handle doesNotUnderstand by checking extension registry.
-spec does_not_understand(atom(), list(), tuple()) -> term().
does_not_understand(Selector, Args, Value) ->
    case beamtalk_extensions:lookup('Association', Selector) of
        {ok, Fun, _Owner} -> 
            Fun(Args, Value);
        not_found -> 
            Error0 = beamtalk_error:new(does_not_understand, 'Association'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
            error(Error)
    end.
