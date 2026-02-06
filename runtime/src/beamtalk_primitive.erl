%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive type dispatch and reflection.
%%%
%%% This module provides uniform dispatch and class identity for primitive types
%%% (integers, strings, etc.) that are not actors. It enables reflection operations
%%% like `42 class` to return `'Integer'`.
%%%
%%% ## Type Mappings
%%%
%%% | Erlang Type | Beamtalk Class |
%%% |-------------|----------------|
%%% | integer()   | Integer        |
%%% | float()     | Float          |
%%% | binary()    | String         |
%%% | atom()      | Symbol         |
%%% | true/false  | Boolean        |
%%% | nil         | UndefinedObject|
%%% | function()  | Block          |
%%% | list()      | Array          |
%%% | map()       | Dictionary     |
%%% | tuple()     | Tuple          |
%%% | pid()       | Pid            |
%%% | port()      | Port           |
%%% | reference() | Reference      |
%%%
%%% ## Usage
%%%
%%% ```erlang
%%% %% Get class of any value
%%% beamtalk_primitive:class_of(42).          % => 'Integer'
%%% beamtalk_primitive:class_of(<<"hello">>). % => 'String'
%%%
%%% %% Send message to primitive or actor
%%% beamtalk_primitive:send(42, '+', [8]).    % => 50
%%%
%%% %% Check if value responds to selector
%%% beamtalk_primitive:responds_to(42, '+').  % => true
%%% ```
%%%
%%% See: docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_primitive).
-export([class_of/1, send/3, responds_to/2]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Determine the Beamtalk class of any value.
%%
%% Returns the class atom for primitives, or extracts the class field from
%% beamtalk_object records for actors.
%%
%% Examples:
%% ```
%% class_of(42)              % => 'Integer'
%% class_of(3.14)            % => 'Float'
%% class_of(<<"hello">>)     % => 'String'
%% class_of(true)            % => 'Boolean'
%% class_of(nil)             % => 'UndefinedObject'
%% class_of(fun() -> ok end) % => 'Block'
%% class_of('symbol')        % => 'Symbol'
%% class_of([1,2,3])         % => 'Array'
%% class_of(#{a => 1})       % => 'Dictionary'
%% class_of({1,2,3})         % => 'Tuple'
%% class_of(self())          % => 'Pid'
%% ```
-spec class_of(term()) -> atom().
class_of(X) when is_integer(X) -> 'Integer';
class_of(X) when is_float(X) -> 'Float';
class_of(X) when is_binary(X) -> 'String';
class_of(true) -> 'Boolean';
class_of(false) -> 'Boolean';
class_of(nil) -> 'UndefinedObject';
class_of(X) when is_function(X) -> 'Block';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'Array';
class_of(X) when is_map(X) -> 'Dictionary';
class_of(X) when is_tuple(X), tuple_size(X) >= 2, element(1, X) =:= beamtalk_object ->
    element(2, X);  % Extract class field from #beamtalk_object{}
class_of(X) when is_tuple(X) -> 'Tuple';
class_of(X) when is_pid(X) -> 'Pid';
class_of(X) when is_port(X) -> 'Port';
class_of(X) when is_reference(X) -> 'Reference';
class_of(_) -> 'Object'.

%% @doc Send a message to any value (actor or primitive).
%%
%% Provides uniform dispatch across actors (via gen_server) and primitives
%% (via static dispatch to class modules like beamtalk_integer).
%%
%% Examples:
%% ```
%% send(42, '+', [8])           % => 50
%% send(<<"hi">>, '++', [<<"!">]]) % => <<"hi!">>
%% send(ActorObj, 'increment', []) % => Future or result
%% ```
%%
%% Note: Primitive class modules (beamtalk_integer, beamtalk_string, etc.)
%% are implemented in separate issues (BT-166, BT-167, etc.).
-spec send(term(), atom(), list()) -> term().
send(#beamtalk_object{pid = Pid}, Selector, Args) ->
    %% Actor: use gen_server
    gen_server:call(Pid, {Selector, Args});
send(X, Selector, Args) when is_integer(X) ->
    %% Primitive: static dispatch to class module
    beamtalk_integer:dispatch(Selector, Args, X);
send(X, Selector, Args) when is_binary(X) ->
    beamtalk_string:dispatch(Selector, Args, X);
send(X, Selector, Args) when X =:= true; X =:= false ->
    beamtalk_boolean:dispatch(Selector, Args, X);
send(nil, Selector, Args) ->
    beamtalk_nil:dispatch(Selector, Args, nil);
send(X, Selector, Args) when is_function(X) ->
    beamtalk_block:dispatch(Selector, Args, X);
send(X, Selector, Args) when is_tuple(X) ->
    %% Check if it's a beamtalk_object (should not happen - covered by clause above)
    %% but handle tuples that might not match the record pattern
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% This is a beamtalk_object that didn't match the record pattern
            %% Extract pid from the known record structure
            Pid = element(4, X),
            gen_server:call(Pid, {Selector, Args});
        false ->
            %% Regular tuple - dispatch to tuple class
            beamtalk_tuple:dispatch(Selector, Args, X)
    end;
send(X, Selector, Args) when is_float(X) ->
    beamtalk_float:dispatch(Selector, Args, X);
send(X, Selector, Args) ->
    %% Other primitives: dispatch to generic handler
    Class = class_of(X),
    error({not_implemented, {Class, Selector, Args}}).

%% @doc Check if a value responds to a given selector.
%%
%% For actors, delegates to the module's has_method/1 function.
%% For primitives, checks both built-in methods and extension registry
%% via dedicated class modules (beamtalk_integer, beamtalk_string, etc.).
%%
%% Examples:
%% ```
%% responds_to(ActorObj, 'class')     % => true
%% responds_to(42, '+')               % => true
%% responds_to(<<"hi">>, 'size')      % => true
%% responds_to(3.14, '+')             % => true
%% responds_to(42, 'unknownMsg')      % => false
%% ```
-spec responds_to(term(), atom()) -> boolean().
responds_to(#beamtalk_object{class_mod = Mod}, Selector) ->
    %% Actor: check if module exports has_method/1
    erlang:function_exported(Mod, has_method, 1) andalso Mod:has_method(Selector);
responds_to(X, Selector) when is_integer(X) ->
    beamtalk_integer:has_method(Selector);
responds_to(X, Selector) when is_binary(X) ->
    beamtalk_string:has_method(Selector);
responds_to(X, Selector) when X =:= true; X =:= false ->
    beamtalk_boolean:has_method(Selector);
responds_to(nil, Selector) ->
    beamtalk_nil:has_method(Selector);
responds_to(X, Selector) when is_function(X) ->
    beamtalk_block:has_method(Selector);
responds_to(X, Selector) when is_tuple(X) ->
    %% Check if it's a beamtalk_object (should not happen - covered by clause above)
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% Actor object that didn't match record pattern
            Mod = element(3, X),  % class_mod field
            erlang:function_exported(Mod, has_method, 1) andalso Mod:has_method(Selector);
        false ->
            %% Regular tuple
            beamtalk_tuple:has_method(Selector)
    end;
responds_to(X, Selector) when is_float(X) ->
    beamtalk_float:has_method(Selector);
responds_to(_, _) ->
    %% Other primitives: no methods yet
    false.
