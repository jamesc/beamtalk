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
%%% | list()      | List           |
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
-export([class_of/1, class_of_object/1, class_of_object_by_name/1, send/3, responds_to/2, class_name_to_module/1, print_string/1]).

-include("beamtalk.hrl").

%% Compiled stdlib modules (bt@stdlib@integer, bt@stdlib@string, etc.) are
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
%% class_of([1,2,3])         % => 'List'
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
class_of(X) when is_list(X) -> 'List';
class_of(X) when is_map(X) ->
    beamtalk_tagged_map:class_of(X, 'Dictionary');
class_of(X) when is_tuple(X), tuple_size(X) >= 2, element(1, X) =:= beamtalk_object ->
    element(2, X);  % Extract class field from #beamtalk_object{}
class_of(X) when is_tuple(X) -> 'Tuple';
class_of(X) when is_pid(X) -> 'Pid';
class_of(X) when is_port(X) -> 'Port';
class_of(X) when is_reference(X) -> 'Reference';
class_of(_) -> 'Object'.

%% @doc Return the class of any value as a first-class class object (BT-412).
%%
%% Unlike class_of/1 which returns an atom, this returns a #beamtalk_object{}
%% tuple representing the class as a first-class object that can receive messages.
%% Uses whereis_class to look up the class process pid.
%%
%% Examples:
%% ```
%% class_of_object(42)           % => {beamtalk_object, 'Integer class', ..., Pid}
%% class_of_object(<<"hello">>)  % => {beamtalk_object, 'String class', ..., Pid}
%% ```
-spec class_of_object(term()) -> tuple() | atom().
class_of_object('Metaclass') ->
    %% BT-412: Metaclass tower is terminal
    'Metaclass';
class_of_object(#beamtalk_object{class = ClassName}) ->
    %% BT-412: class of a class object → 'Metaclass' sentinel (terminal)
    case beamtalk_object_class:is_class_name(ClassName) of
        true -> 'Metaclass';
        false -> class_of_object_inner(ClassName)
    end;
class_of_object(X) ->
    ClassName = class_of(X),
    class_of_object_inner(ClassName).

%% @private Helper to construct class object from class name.
class_of_object_inner(ClassName) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            %% Fallback for classes without a registered process
            ClassName;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_object_class:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end.

%% @doc Return a class object given a class name atom (BT-412).
%%
%% Used by dispatch when the class name is already known (e.g., from state tag).
-spec class_of_object_by_name(atom()) -> tuple() | atom().
class_of_object_by_name(ClassName) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined -> ClassName;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_object_class:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end.

%% @doc Return the printString representation of any value.
%%
%% Handles all BEAM types uniformly — used by the printString intrinsic.
-spec print_string(term()) -> binary().
print_string(X) when is_integer(X) ->
    erlang:integer_to_binary(X);
print_string(X) when is_float(X) ->
    erlang:float_to_binary(X, [short]);
print_string(X) when is_binary(X) ->
    X;
print_string(true) ->
    <<"true">>;
print_string(false) ->
    <<"false">>;
print_string(nil) ->
    <<"nil">>;
print_string('Metaclass') ->
    <<"Metaclass">>;
print_string(X) when is_atom(X) ->
    iolist_to_binary([<<"#">>, erlang:atom_to_binary(X, utf8)]);
print_string(X) when is_list(X) ->
    iolist_to_binary([<<"#(">>, lists:join(<<", ">>, [print_string(E) || E <- X]), <<")">>]);
print_string(#beamtalk_object{class = ClassName}) ->
    %% BT-412: Class objects display as their class name (e.g., "Integer")
    case beamtalk_object_class:is_class_name(ClassName) of
        true ->
            beamtalk_object_class:class_display_name(ClassName);
        false ->
            iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8)])
    end;
print_string(X) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'Exception' ->
            %% BT-338: Format exceptions nicely
            beamtalk_exception_handler:dispatch('printString', [], X);
        'Association' ->
            %% BT-335: Format associations as "key -> value"
            beamtalk_association:format_string(X);
        'Set' ->
            %% BT-73: Format sets as "Set(element1, element2, ...)"
            Elements = maps:get(elements, X, []),
            ElemStrs = [print_string(E) || E <- Elements],
            iolist_to_binary([<<"Set(">>, lists:join(<<", ">>, ElemStrs), <<")">>]);
        'CompiledMethod' ->
            %% BT-457: Delegate to ops module for "a CompiledMethod(selector)" format
            beamtalk_compiled_method_ops:dispatch('printString', [], X);
        _ ->
            ClassName = beamtalk_tagged_map:class_of(X, 'Dictionary'),
            iolist_to_binary([<<"a ">>, erlang:atom_to_binary(ClassName, utf8)])
    end;
print_string(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

%% @doc Send a message to any value (actor or primitive).
%%
%% Provides uniform dispatch across actors (via gen_server) and primitives
%% (via static dispatch to class modules like bt@stdlib@integer).
%%
%% Examples:
%% ```
%% send(42, '+', [8])           % => 50
%% send(<<"hi">>, '++', [<<"!">]]) % => <<"hi!">>
%% send(ActorObj, 'increment', []) % => Future or result
%% ```
%%
%% Note: Primitive class modules (bt@stdlib@integer, bt@stdlib@string, etc.)
%% use the ADR 0016 naming convention.
-spec send(term(), atom(), list()) -> term().
send(#beamtalk_object{pid = Pid}, Selector, Args) ->
    %% Actor: use gen_server
    gen_server:call(Pid, {Selector, Args});
send(X, Selector, Args) when is_integer(X) ->
    %% Primitive: static dispatch to class module (ADR 0016)
    'bt@stdlib@integer':dispatch(Selector, Args, X);
send(X, Selector, Args) when is_binary(X) ->
    'bt@stdlib@string':dispatch(Selector, Args, X);
send(X, Selector, Args) when X =:= true ->
    'bt@stdlib@true':dispatch(Selector, Args, X);
send(X, Selector, Args) when X =:= false ->
    'bt@stdlib@false':dispatch(Selector, Args, X);
send(nil, Selector, Args) ->
    'bt@stdlib@undefined_object':dispatch(Selector, Args, nil);
send(X, Selector, Args) when is_atom(X) ->
    'bt@stdlib@symbol':dispatch(Selector, Args, X);
send(X, Selector, Args) when is_function(X) ->
    'bt@stdlib@block':dispatch(Selector, Args, X);
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
            'bt@stdlib@tuple':dispatch(Selector, Args, X)
    end;
send(X, Selector, Args) when is_float(X) ->
    'bt@stdlib@float':dispatch(Selector, Args, X);
send(X, Selector, Args) when is_map(X) ->
    %% Check for tagged maps (CompiledMethod, value type instances, plain maps)
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' ->
            'bt@stdlib@compiled_method':dispatch(Selector, Args, X);
        'Exception' ->
            %% BT-338: Exception value type - direct dispatch
            beamtalk_exception_handler:dispatch(Selector, Args, X);
        'Association' ->
            %% BT-335: Association value type - direct dispatch (ADR 0016)
            'bt@stdlib@association':dispatch(Selector, Args, X);
        'Set' ->
            %% BT-73: Set value type - dispatch to compiled stdlib
            'bt@stdlib@set':dispatch(Selector, Args, X);
        undefined ->
            %% Plain map (Dictionary) — BT-418: compiled stdlib dispatch
            'bt@stdlib@dictionary':dispatch(Selector, Args, X);
        Class ->
            %% Value type instance - route to class module (BT-354)
            value_type_send(X, Class, Selector, Args)
    end;
send(X, Selector, Args) when is_list(X) ->
    %% List/Array dispatch
    'bt@stdlib@list':dispatch(Selector, Args, X);
send(X, Selector, _Args) ->
    %% Other primitives: dispatch to generic handler
    Class = class_of(X),
    Error0 = beamtalk_error:new(does_not_understand, Class),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Primitive type does not support this message">>),
    beamtalk_error:raise(Error2).

%% @doc Check if a value responds to a given selector.
%%
%% For actors, delegates to the module's has_method/1 function.
%% For primitives, checks both built-in methods and extension registry
%% via dedicated class modules (bt@stdlib@integer, bt@stdlib@string, etc.).
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
    'bt@stdlib@integer':has_method(Selector);
responds_to(X, Selector) when is_binary(X) ->
    'bt@stdlib@string':has_method(Selector);
responds_to(X, Selector) when X =:= true ->
    'bt@stdlib@true':has_method(Selector);
responds_to(X, Selector) when X =:= false ->
    'bt@stdlib@false':has_method(Selector);
responds_to(nil, Selector) ->
    'bt@stdlib@undefined_object':has_method(Selector);
responds_to(X, Selector) when is_atom(X) ->
    'bt@stdlib@symbol':has_method(Selector);
responds_to(X, Selector) when is_function(X) ->
    'bt@stdlib@block':has_method(Selector);
responds_to(X, Selector) when is_tuple(X) ->
    %% Check if it's a beamtalk_object (should not happen - covered by clause above)
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% Actor object that didn't match record pattern
            Mod = element(3, X),  % class_mod field
            erlang:function_exported(Mod, has_method, 1) andalso Mod:has_method(Selector);
        false ->
            %% Regular tuple
            'bt@stdlib@tuple':has_method(Selector)
    end;
responds_to(X, Selector) when is_float(X) ->
    'bt@stdlib@float':has_method(Selector);
responds_to(X, Selector) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' ->
            'bt@stdlib@compiled_method':has_method(Selector);
        'Exception' ->
            %% BT-338: Exception value type
            beamtalk_exception_handler:has_method(Selector);
        'Association' ->
            %% BT-335: Association value type (ADR 0016)
            'bt@stdlib@association':has_method(Selector);
        'Set' ->
            %% BT-73: Set value type
            'bt@stdlib@set':has_method(Selector);
        undefined ->
            'bt@stdlib@dictionary':has_method(Selector);
        Class ->
            %% Value type instance - check class module exports (BT-354)
            value_type_responds_to(Class, Selector)
    end;
responds_to(X, Selector) when is_list(X) ->
    'bt@stdlib@list':has_method(Selector);
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
    %% BT-359: Catch mutating/ivar methods on value types early
    case is_ivar_method(Selector) of
        {true, Hint} ->
            IvarErr0 = beamtalk_error:new(immutable_value, Class),
            IvarErr1 = beamtalk_error:with_selector(IvarErr0, Selector),
            IvarErr2 = beamtalk_error:with_hint(IvarErr1, Hint),
            beamtalk_error:raise(IvarErr2);
        false ->
            ok
    end,
    Module = class_name_to_module(Class),
    Arity = length(Args) + 1,  % +1 for Self parameter
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Selector, Arity) of
        true ->
            erlang:apply(Module, Selector, [Self | Args]);
        false ->
            %% Fall back to Object base methods (class, printString, etc.)
            case beamtalk_object_ops:has_method(Selector) of
                true ->
                    case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                        {reply, Result, _State} -> Result;
                        {error, Error, _State} -> beamtalk_error:raise(Error)
                    end;
                false ->
                    Error0 = beamtalk_error:new(does_not_understand, Class),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    beamtalk_error:raise(Error1)
            end
    end.

%% @private Check if a selector is an instance variable method that should
%% raise immutable_value on value types (BT-359).
-spec is_ivar_method(atom()) -> {true, binary()} | false.
is_ivar_method('instVarAt:put:') ->
    {true, <<"Value types are immutable. Use a method that returns a new instance instead.">>};
is_ivar_method('instVarAt:') ->
    {true, <<"Value types have no instance variables">>};
is_ivar_method(_) ->
    false.

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
        orelse beamtalk_object_ops:has_method(Selector).

%% @doc Convert a CamelCase class name atom to a module name atom.
%%
%% ADR 0016: User-defined value types use bt@ prefix.
%% Matches the Rust codegen convention in build.rs.
%%
%% Uses list_to_existing_atom/1 first to avoid atom table exhaustion.
%% Falls back to list_to_atom/1 because Class is already a known atom
%% (from class registration) — the new atom merely adds the bt@ prefix.
%% Callers (value_type_send/responds_to) check function_exported which
%% returns false for non-existent modules, triggering does_not_understand.
-spec class_name_to_module(atom()) -> atom().
class_name_to_module(Class) when is_atom(Class) ->
    SnakeCase = camel_to_snake(atom_to_list(Class)),
    %% ADR 0016: User code modules use bt@ prefix
    ModName = "bt@" ++ SnakeCase,
    try list_to_existing_atom(ModName)
    catch error:badarg ->
        %% Module atom doesn't exist yet. Since Class is already a known
        %% atom, creating bt@{snake} is bounded by the number of registered
        %% classes, not by arbitrary user input.
        list_to_atom(ModName)
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
