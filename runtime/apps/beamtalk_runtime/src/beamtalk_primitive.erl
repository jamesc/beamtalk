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
-export([class_of/1, send/3, responds_to/2, class_name_to_module/1]).

-include("beamtalk.hrl").

%% Compiled stdlib modules (beamtalk_integer, beamtalk_string, etc.) are
%% generated from Core Erlang, not .erl source. Dialyzer can't resolve them
%% if stdlib hasn't been built yet.
-dialyzer({nowarn_function, [send/3, responds_to/2]}).

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
class_of(true) -> 'True';
class_of(false) -> 'False';
class_of(nil) -> 'UndefinedObject';
class_of(X) when is_function(X) -> 'Block';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'Array';
class_of(X) when is_map(X) ->
    case maps:find('__class__', X) of
        {ok, Class} when is_atom(Class) -> Class;
        _ -> 'Dictionary'
    end;
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
send(X, Selector, Args) when X =:= true ->
    %% BT-340: True dispatches to compiled beamtalk_true module
    beamtalk_true:dispatch(Selector, Args, X);
send(X, Selector, Args) when X =:= false ->
    %% BT-340: False dispatches to compiled beamtalk_false module
    beamtalk_false:dispatch(Selector, Args, X);
send(nil, Selector, Args) ->
    beamtalk_undefined_object:dispatch(Selector, Args, nil);
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
send(X, Selector, Args) when is_map(X) ->
    %% Check for tagged maps (CompiledMethod, value type instances, plain maps)
    case maps:find('__class__', X) of
        {ok, 'CompiledMethod'} ->
            beamtalk_compiled_method:dispatch(Selector, Args, X);
        {ok, Class} when is_atom(Class) ->
            %% Value type instance - route to class module (BT-354)
            value_type_send(X, Class, Selector, Args);
        _ ->
            %% Plain map (Dictionary)
            beamtalk_map:dispatch(Selector, Args, X)
    end;
send(X, Selector, Args) when is_list(X) ->
    %% List/Array dispatch
    beamtalk_list:dispatch(Selector, Args, X);
send(X, Selector, _Args) ->
    %% Other primitives: dispatch to generic handler
    Class = class_of(X),
    Error0 = beamtalk_error:new(does_not_understand, Class),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Primitive type does not support this message">>),
    error(Error2).

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
responds_to(X, Selector) when X =:= true ->
    beamtalk_true:has_method(Selector);
responds_to(X, Selector) when X =:= false ->
    beamtalk_false:has_method(Selector);
responds_to(nil, Selector) ->
    beamtalk_undefined_object:has_method(Selector);
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
responds_to(X, Selector) when is_map(X) ->
    case maps:find('__class__', X) of
        {ok, 'CompiledMethod'} ->
            beamtalk_compiled_method:has_method(Selector);
        {ok, Class} when is_atom(Class) ->
            %% Value type instance - check class module exports (BT-354)
            value_type_responds_to(Class, Selector);
        _ -> beamtalk_map:has_method(Selector)
    end;
responds_to(X, Selector) when is_list(X) ->
    beamtalk_list:has_method(Selector);
responds_to(_, _) ->
    %% Other primitives: no methods yet
    false.

%%% ============================================================================
%%% Internal: Value Type Dispatch (BT-354)
%%% ============================================================================

%% @doc Send a message to a value type instance.
%%
%% Value type modules generate pure functions (e.g., point:getX/1(Self)).
%% Falls back to beamtalk_object base methods for inherited protocol.
-spec value_type_send(map(), atom(), atom(), list()) -> term().
value_type_send(Self, Class, Selector, Args) ->
    Module = class_name_to_module(Class),
    Arity = length(Args) + 1,  % +1 for Self parameter
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Selector, Arity) of
        true ->
            erlang:apply(Module, Selector, [Self | Args]);
        false ->
            %% Fall back to Object base methods (class, printString, etc.)
            case beamtalk_object:has_method(Selector) of
                true ->
                    case beamtalk_object:dispatch(Selector, Args, Self, Self) of
                        {reply, Result, _State} -> Result;
                        {error, Error, _State} -> error(Error)
                    end;
                false ->
                    Error0 = beamtalk_error:new(does_not_understand, Class),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    error(Error1)
            end
    end.

%% @doc Check if a value type responds to a selector.
-spec value_type_responds_to(atom(), atom()) -> boolean().
value_type_responds_to(Class, Selector) ->
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    Exports = case erlang:function_exported(Module, module_info, 1) of
        true -> Module:module_info(exports);
        false -> []
    end,
    lists:any(fun({Name, _Arity}) -> Name =:= Selector end, Exports)
        orelse beamtalk_object:has_method(Selector).

%% @doc Convert a CamelCase class name atom to a snake_case module name atom.
%%
%% Matches the Rust `to_module_name` function in codegen/core_erlang/util.rs.
%% Examples: 'Point' → 'point', 'MyCounter' → 'my_counter'
%%
%% Uses list_to_existing_atom/1 to avoid atom table exhaustion from untrusted
%% class names — the module atom must already exist if the module is loaded.
-spec class_name_to_module(atom()) -> atom().
class_name_to_module(Class) when is_atom(Class) ->
    SnakeCase = camel_to_snake(atom_to_list(Class)),
    try list_to_existing_atom(SnakeCase)
    catch error:badarg ->
        %% Module atom doesn't exist — cannot be a loaded module.
        %% Return a non-existent atom safely; callers use code:ensure_loaded
        %% and function_exported which will return false, triggering proper
        %% does_not_understand error handling.
        list_to_atom(SnakeCase)
    end.

%% @private CamelCase string to snake_case string conversion.
-spec camel_to_snake(string()) -> string().
camel_to_snake(Str) ->
    camel_to_snake(Str, false, []).

camel_to_snake([], _PrevWasLower, Acc) ->
    lists:reverse(Acc);
camel_to_snake([H|T], PrevWasLower, Acc) when H >= $A, H =< $Z ->
    Lower = H + 32,
    case PrevWasLower of
        true -> camel_to_snake(T, false, [Lower, $_ | Acc]);
        false -> camel_to_snake(T, false, [Lower | Acc])
    end;
camel_to_snake([H|T], _PrevWasLower, Acc) ->
    camel_to_snake(T, (H >= $a andalso H =< $z), [H | Acc]).
