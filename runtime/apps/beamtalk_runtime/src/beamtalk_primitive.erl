%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive type dispatch and reflection.
%%%
%%% **DDD Context:** Object System
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
    case beamtalk_class_registry:is_class_name(ClassName) of
        true -> 'Metaclass';
        false -> class_of_object_inner(ClassName)
    end;
class_of_object(X) ->
    ClassName = class_of(X),
    class_of_object_inner(ClassName).

%% @private Helper to construct class object from class name.
class_of_object_inner(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            %% Fallback for classes without a registered process
            ClassName;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end.

%% @doc Return a class object given a class name atom (BT-412).
%%
%% Used by dispatch when the class name is already known (e.g., from state tag).
-spec class_of_object_by_name(atom()) -> tuple() | atom().
class_of_object_by_name(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> ClassName;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
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
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            beamtalk_class_registry:class_display_name(ClassName);
        false ->
            iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8)])
    end;
print_string(X) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'Association' ->
            %% BT-335: Format associations as "key -> value"
            beamtalk_association:format_string(X);
        'Set' ->
            %% BT-73: Format sets as "Set(element1, element2, ...)"
            Elements = maps:get(elements, X, []),
            ElemStrs = [print_string(E) || E <- Elements],
            iolist_to_binary([<<"Set(">>, lists:join(<<", ">>, ElemStrs), <<")">>]);
        'Stream' ->
            %% BT-511: Format streams with pipeline description
            beamtalk_stream:print_string(X);
        'CompiledMethod' ->
            %% BT-457: Delegate to ops module for "a CompiledMethod(selector)" format
            beamtalk_compiled_method_ops:dispatch('printString', [], X);
        'ErlangModule' ->
            %% BT-676: Format as #ErlangModule<module_name>
            beamtalk_erlang_proxy:dispatch('printString', [], X);
        Class ->
            case beamtalk_exception_handler:is_exception_class(Class) of
                true ->
                    %% BT-338/BT-452: Format exception hierarchy objects
                    beamtalk_exception_handler:dispatch('printString', [], X);
                false when Class =:= undefined ->
                    %% BT-535: Plain maps (Dictionary) use #{key => value} format
                    beamtalk_map_ops:print_string(X);
                false ->
                    iolist_to_binary([<<"a ">>, erlang:atom_to_binary(Class, utf8)])
            end
    end;
print_string(#beamtalk_error{} = Error) ->
    %% BT-536: Format error records using their format function
    iolist_to_binary(beamtalk_error:format(Error));
print_string(X) when is_tuple(X) ->
    %% BT-536: Format Erlang tuples as {el1, el2, ...}
    Elements = tuple_to_list(X),
    iolist_to_binary([<<"{">>, lists:join(<<", ">>, [print_string(E) || E <- Elements]), <<"}">>]);
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
    dispatch_via_module(X, Selector, Args);
send(X, Selector, Args) when is_binary(X) ->
    dispatch_via_module(X, Selector, Args);
send(X, Selector, Args) when X =:= true ->
    dispatch_via_module(X, Selector, Args);
send(X, Selector, Args) when X =:= false ->
    dispatch_via_module(X, Selector, Args);
send(nil, Selector, Args) ->
    dispatch_via_module(nil, Selector, Args);
send(X, Selector, Args) when is_atom(X) ->
    dispatch_via_module(X, Selector, Args);
send(X, Selector, Args) when is_function(X) ->
    dispatch_via_module(X, Selector, Args);
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
            dispatch_via_module(X, Selector, Args)
    end;
send(X, Selector, Args) when is_float(X) ->
    dispatch_via_module(X, Selector, Args);
send(X, Selector, Args) when is_map(X) ->
    %% Check for tagged maps (CompiledMethod, value type instances, plain maps)
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' ->
            dispatch_via_module(X, Selector, Args);
        'Association' ->
            %% BT-335: Association value type - direct dispatch (ADR 0016)
            dispatch_via_module(X, Selector, Args);
        'Set' ->
            %% BT-73: Set value type - dispatch to compiled stdlib
            dispatch_via_module(X, Selector, Args);
        'Stream' ->
            %% BT-511: Stream value type - dispatch to compiled stdlib
            dispatch_via_module(X, Selector, Args);
        'FileHandle' ->
            %% BT-513: FileHandle - dispatch to beamtalk_file runtime
            case Selector of
                'lines' ->
                    beamtalk_file:handle_lines(X);
                _ ->
                    %% Fall back to Object protocol (class, printString, etc.)
                    case beamtalk_object_ops:has_method(Selector) of
                        true ->
                            case beamtalk_object_ops:dispatch(Selector, Args, X, X) of
                                {reply, Result, _State} -> Result;
                                {error, Error, _State} -> beamtalk_error:raise(Error)
                            end;
                        false ->
                            Error0 = beamtalk_error:new(does_not_understand, 'FileHandle'),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            beamtalk_error:raise(Error1)
                    end
            end;
        'TestCase' ->
            %% BT-438: TestCase value type - dispatch to compiled stdlib
            dispatch_via_module(X, Selector, Args);
        'StackFrame' ->
            %% BT-107: StackFrame value type - dispatch to runtime
            beamtalk_stack_frame:dispatch(Selector, Args, X);
        'ErlangModule' ->
            %% BT-676: ErlangModule proxy - dispatch to Erlang module
            beamtalk_erlang_proxy:dispatch(Selector, Args, X);
        'Erlang' ->
            %% BT-676: Erlang class-side proxy - construct ErlangModule on message
            erlang_class_dispatch(X, Selector, Args);
        undefined ->
            %% Plain map (Dictionary) — BT-418: compiled stdlib dispatch
            dispatch_via_module(X, Selector, Args);
        Class ->
            case beamtalk_exception_handler:is_exception_class(Class) of
                true ->
                    %% BT-338/BT-452: Exception hierarchy - direct dispatch
                    beamtalk_exception_handler:dispatch(Selector, Args, X);
                false ->
                    %% Value type instance - route to class module (BT-354)
                    value_type_send(X, Class, Selector, Args)
            end
    end;
send(X, Selector, Args) when is_list(X) ->
    %% List/Array dispatch
    dispatch_via_module(X, Selector, Args);
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
    responds_via_module(X, Selector);
responds_to(X, Selector) when is_binary(X) ->
    responds_via_module(X, Selector);
responds_to(X, Selector) when X =:= true ->
    responds_via_module(X, Selector);
responds_to(X, Selector) when X =:= false ->
    responds_via_module(X, Selector);
responds_to(nil, Selector) ->
    responds_via_module(nil, Selector);
responds_to(X, Selector) when is_atom(X) ->
    responds_via_module(X, Selector);
responds_to(X, Selector) when is_function(X) ->
    responds_via_module(X, Selector);
responds_to(X, Selector) when is_tuple(X) ->
    %% Check if it's a beamtalk_object (should not happen - covered by clause above)
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% Actor object that didn't match record pattern
            Mod = element(3, X),  % class_mod field
            erlang:function_exported(Mod, has_method, 1) andalso Mod:has_method(Selector);
        false ->
            %% Regular tuple
            responds_via_module(X, Selector)
    end;
responds_to(X, Selector) when is_float(X) ->
    responds_via_module(X, Selector);
responds_to(X, Selector) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' ->
            responds_via_module(X, Selector);
        'Association' ->
            %% BT-335: Association value type (ADR 0016)
            responds_via_module(X, Selector);
        'Set' ->
            %% BT-73: Set value type
            responds_via_module(X, Selector);
        'Stream' ->
            %% BT-511: Stream value type
            responds_via_module(X, Selector);
        'StackFrame' ->
            %% BT-107: StackFrame value type
            beamtalk_stack_frame:has_method(Selector);
        'ErlangModule' ->
            %% BT-676: ErlangModule proxy — always responds (forwards to Erlang)
            Selector =:= 'class' orelse
                Selector =:= 'printString' orelse
                beamtalk_object_ops:has_method(Selector) orelse
                true;
        'Erlang' ->
            %% BT-676: Erlang class-side proxy — always responds (creates module proxies)
            true;
        'FileHandle' ->
            %% BT-513: FileHandle value type
            beamtalk_file:handle_has_method(Selector) orelse
                beamtalk_object_ops:has_method(Selector);
        undefined ->
            responds_via_module(X, Selector);
        Class ->
            case beamtalk_exception_handler:is_exception_class(Class) of
                true ->
                    %% BT-338/BT-452: Exception hierarchy
                    beamtalk_exception_handler:has_method(Selector);
                false ->
                    %% Value type instance - check class module exports (BT-354)
                    value_type_responds_to(Class, Selector)
            end
    end;
responds_to(X, Selector) when is_list(X) ->
    responds_via_module(X, Selector);
responds_to(_, _) ->
    %% Other primitives: no methods yet
    false.

%% @private
%% @doc Dispatch a value using its mapped stdlib module.
-spec dispatch_via_module(term(), atom(), list()) -> term().
dispatch_via_module(X, Selector, Args) ->
    case module_for_value(X) of
        undefined ->
            Class = class_of(X),
            Error = beamtalk_error:new(
                does_not_understand,
                Class,
                Selector,
                <<"Primitive type does not support this message">>),
            beamtalk_error:raise(Error);
        Mod ->
            Mod:dispatch(Selector, Args, X)
    end.

%% @private
%% @doc Check method support using the mapped stdlib module.
-spec responds_via_module(term(), atom()) -> boolean().
responds_via_module(X, Selector) ->
    case module_for_value(X) of
        undefined -> false;
        Mod -> Mod:has_method(Selector)
    end.

%% @private
%% @doc Map a runtime value to its stdlib dispatch module.
-spec module_for_value(term()) -> atom() | undefined.
module_for_value(X) when is_integer(X) -> 'bt@stdlib@integer';
module_for_value(X) when is_binary(X) -> 'bt@stdlib@string';
module_for_value(true) -> 'bt@stdlib@true';
module_for_value(false) -> 'bt@stdlib@false';
module_for_value(nil) -> 'bt@stdlib@undefined_object';
module_for_value(X) when is_atom(X) -> 'bt@stdlib@symbol';
module_for_value(X) when is_function(X) -> 'bt@stdlib@block';
module_for_value(X) when is_tuple(X) -> 'bt@stdlib@tuple';
module_for_value(X) when is_float(X) -> 'bt@stdlib@float';
module_for_value(X) when is_list(X) -> 'bt@stdlib@list';
module_for_value(X) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' -> 'bt@stdlib@compiled_method';
        'Association' -> 'bt@stdlib@association';
        'Set' -> 'bt@stdlib@set';
        'Stream' -> 'bt@stdlib@stream';
        'TestCase' -> 'bt@stdlib@test_case';
        undefined -> 'bt@stdlib@dictionary';
        _ -> undefined
    end;
module_for_value(_) ->
    undefined.

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
    code:ensure_loaded(Module),
    %% BT-439: Use dispatch/3 for proper superclass delegation.
    %% Previously called Module:Selector(Self, Args) directly, which
    %% missed inherited methods (e.g., TestCase assertion methods).
    case erlang:function_exported(Module, dispatch, 3) of
        true ->
            Module:dispatch(Selector, Args, Self);
        false ->
            %% Fallback for modules without dispatch/3
            Arity = length(Args) + 1,
            case erlang:function_exported(Module, Selector, Arity) of
                true ->
                    erlang:apply(Module, Selector, [Self | Args]);
                false ->
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
            end
    end.

%% @doc Check if a selector is an instance variable method that should
%% raise immutable_value on value types (BT-359).
-spec is_ivar_method(atom()) -> {true, binary()} | false.
is_ivar_method('instVarAt:put:') ->
    {true, <<"Value types are immutable. Use a method that returns a new instance instead.">>};
is_ivar_method('instVarAt:') ->
    {true, <<"Value types have no instance variables">>};
is_ivar_method(_) ->
    false.

%% @doc Dispatch messages to the Erlang class-side proxy (BT-676).
%%
%% The Erlang proxy is a tagged map #{$beamtalk_class => 'Erlang'}.
%% Unary messages construct an ErlangModule proxy for the named module.
%% E.g., `Erlang lists` → #{$beamtalk_class => 'ErlangModule', module => lists}
-spec erlang_class_dispatch(map(), atom(), list()) -> term().
erlang_class_dispatch(_Self, 'class', _Args) ->
    'Erlang';
erlang_class_dispatch(_Self, 'printString', _Args) ->
    <<"Erlang">>;
erlang_class_dispatch(Self, Selector, Args) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                {reply, Result, _State} -> Result;
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            %% Unary message = module name → return ErlangModule proxy
            beamtalk_erlang_proxy:new(Selector)
    end.

%% @doc Check if a value type responds to a selector.
-spec value_type_responds_to(atom(), atom()) -> boolean().
value_type_responds_to(Class, Selector) ->
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, has_method, 1) of
        true ->
            Module:has_method(Selector);
        false ->
            Exports = case erlang:function_exported(Module, module_info, 1) of
                true -> Module:module_info(exports);
                false -> []
            end,
            lists:any(fun({Name, _Arity}) -> Name =:= Selector end, Exports)
                orelse beamtalk_object_ops:has_method(Selector)
    end.

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

%% @doc CamelCase string to snake_case string conversion.
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
