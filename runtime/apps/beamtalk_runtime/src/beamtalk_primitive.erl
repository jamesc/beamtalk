%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive type dispatch and reflection.
%%%
%%% **DDD Context:** Object System
%%%
%%% Provides uniform dispatch and class identity for primitive types (integers,
%%% strings, etc.) and tagged maps. Enables reflection like `42 class` → `'Integer'`.
%%%
%%% @see docs/internal/design-self-as-object.md Section 3.3

-module(beamtalk_primitive).
-export([
    class_of/1,
    class_of_object/1,
    class_of_object_by_name/1,
    send/3,
    responds_to/2,
    class_name_to_module/1,
    print_string/1,
    display_string/1
]).

-include("beamtalk.hrl").

%% Compiled stdlib modules are generated from Core Erlang, not .erl source.
-dialyzer({nowarn_function, [send/3, responds_to/2]}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Determine the Beamtalk class of any value.
-spec class_of(term()) -> atom().
class_of(X) when is_integer(X) -> 'Integer';
class_of(X) when is_float(X) -> 'Float';
class_of(X) when is_binary(X) -> 'String';
class_of(true) ->
    'True';
class_of(false) ->
    'False';
class_of(nil) ->
    'UndefinedObject';
class_of(X) when is_function(X) -> 'Block';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'List';
class_of(X) when is_map(X) ->
    beamtalk_tagged_map:class_of(X, 'Dictionary');
class_of({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before type inspection.
    class_of(beamtalk_future:await(Future));
class_of(X) when is_tuple(X), tuple_size(X) >= 2, element(1, X) =:= beamtalk_object ->
    % Extract class field from #beamtalk_object{}
    element(2, X);
class_of(X) when is_tuple(X) -> 'Tuple';
class_of(X) when is_pid(X) -> 'Pid';
class_of(X) when is_port(X) -> 'Port';
class_of(X) when is_reference(X) -> 'Reference';
class_of(_) ->
    'Object'.

%% @doc Return the class of any value as a first-class class object (BT-412).
%%
%% ADR 0036: For class objects (tagged with " class" suffix) returns a real
%% metaclass object instead of the sentinel atom 'Metaclass'. For metaclass
%% objects (tagged with 'Metaclass'), returns the same struct (idempotent) to
%% enable the self-grounding invariant: `Metaclass class class == Metaclass class`.
-spec class_of_object(term()) -> #beamtalk_object{} | atom().
class_of_object({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before class object inspection.
    class_of_object(beamtalk_future:await(Future));
class_of_object(#beamtalk_object{class = 'Metaclass', class_mod = ClassMod, pid = Pid}) ->
    %% ADR 0036 self-grounding: class of a metaclass object is itself (idempotent).
    %% This ensures `Metaclass class class == Metaclass class` holds via structural
    %% equality: both produce #beamtalk_object{class='Metaclass', pid=MetaclassPid}.
    #beamtalk_object{class = 'Metaclass', class_mod = ClassMod, pid = Pid};
class_of_object(#beamtalk_object{class = ClassName, pid = Pid}) ->
    %% ADR 0036: class of a class object → real metaclass object (wraps same pid).
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid};
        false ->
            class_of_object_inner(ClassName)
    end;
class_of_object(X) ->
    ClassName = class_of(X),
    class_of_object_inner(ClassName).

%% @private Helper to construct class object from class name.
class_of_object_inner(ClassName) ->
    class_of_object_by_name(ClassName).

%% @doc Return a class object given a class name atom (BT-412).
-spec class_of_object_by_name(atom()) -> tuple() | atom().
class_of_object_by_name(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            ClassName;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end.

%% @doc Return the printString representation of any value.
%%
%% Strings are quoted (developer representation), symbols use the `#` prefix.
-spec print_string(term()) -> binary().
print_string(X) when is_integer(X) -> erlang:integer_to_binary(X);
print_string(X) when is_float(X) -> erlang:float_to_binary(X, [short]);
print_string(X) when is_binary(X) ->
    Escaped = binary:replace(X, <<"\"">>, <<"\"\"">>, [global]),
    iolist_to_binary([$", Escaped, $"]);
print_string(true) ->
    <<"true">>;
print_string(false) ->
    <<"false">>;
print_string(nil) ->
    <<"nil">>;
print_string(X) when is_atom(X) ->
    iolist_to_binary([<<"#">>, erlang:atom_to_binary(X, utf8)]);
print_string(X) when is_list(X) ->
    iolist_to_binary([<<"#(">>, lists:join(<<", ">>, [print_string(E) || E <- X]), <<")">>]);
print_string({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before string conversion.
    print_string(beamtalk_future:await(Future));
print_string(#beamtalk_object{class = 'Metaclass', pid = Pid}) ->
    %% ADR 0036: Metaclass objects display as "ClassName class" (e.g. "Integer class").
    ClassName = beamtalk_object_class:class_name(Pid),
    iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
print_string(#beamtalk_object{class = ClassName}) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true -> beamtalk_class_registry:class_display_name(ClassName);
        false -> iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8)])
    end;
print_string(X) when is_map(X) -> print_string_map(X);
print_string(#beamtalk_error{} = Error) ->
    iolist_to_binary(beamtalk_error:format(Error));
print_string(X) when is_tuple(X) ->
    Elements = tuple_to_list(X),
    iolist_to_binary([<<"{">>, lists:join(<<", ">>, [print_string(E) || E <- Elements]), <<"}">>]);
print_string(X) when is_pid(X) -> beamtalk_opaque_ops:pid_to_string(X);
print_string(X) when is_port(X) -> beamtalk_opaque_ops:port_to_string(X);
print_string(X) when is_reference(X) -> beamtalk_opaque_ops:ref_to_string(X);
print_string(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

%% @private Format tagged maps for display.
-spec print_string_map(map()) -> binary().
print_string_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'Association' ->
            beamtalk_association:format_string(X);
        'Set' ->
            ElemStrs = [print_string(E) || E <- maps:get(elements, X, [])],
            iolist_to_binary([<<"Set(">>, lists:join(<<", ">>, ElemStrs), <<")">>]);
        'Array' ->
            beamtalk_array_ops:print_string(X);
        'Stream' ->
            beamtalk_stream:print_string(X);
        'CompiledMethod' ->
            beamtalk_compiled_method_ops:dispatch('printString', [], X);
        'ErlangModule' ->
            beamtalk_erlang_proxy:dispatch('printString', [], X);
        undefined ->
            beamtalk_map_ops:print_string(X);
        Class ->
            case beamtalk_exception_handler:is_exception_class(Class) of
                true ->
                    beamtalk_exception_handler:dispatch('printString', [], X);
                false ->
                    iolist_to_binary([<<"a ">>, erlang:atom_to_binary(Class, utf8)])
            end
    end.

%% @doc Return the displayString representation of any value.
%%
%% Strings are returned as-is (no surrounding quotes), symbols without
%% the `#` prefix. This is the user-facing representation suitable for
%% `Transcript show:` and string interpolation.
-spec display_string(term()) -> binary().
display_string(X) when is_integer(X) -> erlang:integer_to_binary(X);
display_string(X) when is_float(X) -> erlang:float_to_binary(X, [short]);
display_string(X) when is_binary(X) ->
    case unicode:characters_to_binary(X) of
        Utf8 when is_binary(Utf8) -> Utf8;
        _ -> iolist_to_binary(io_lib:format("~p", [X]))
    end;
display_string(true) ->
    <<"true">>;
display_string(false) ->
    <<"false">>;
display_string(nil) ->
    <<"nil">>;
display_string(X) when is_atom(X) -> erlang:atom_to_binary(X, utf8);
display_string(X) when is_list(X) ->
    iolist_to_binary([<<"#(">>, lists:join(<<", ">>, [display_string(E) || E <- X]), <<")">>]);
display_string({beamtalk_future, _} = Future) ->
    display_string(beamtalk_future:await(Future));
display_string(#beamtalk_object{class = 'Metaclass', pid = Pid}) ->
    ClassName = beamtalk_object_class:class_name(Pid),
    iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
display_string(#beamtalk_object{class = ClassName}) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true -> beamtalk_class_registry:class_display_name(ClassName);
        false -> iolist_to_binary([<<"a ">>, atom_to_binary(ClassName, utf8)])
    end;
display_string(X) when is_map(X) ->
    beamtalk_tagged_map:format_for_display(X);
display_string(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

%% @doc Send a message to any value (actor or primitive).
-spec send(term(), atom(), list()) -> term().
send({beamtalk_future, _} = Future, Selector, Args) ->
    %% BT-840: Auto-await tagged futures before dispatching.
    send(beamtalk_future:await(Future), Selector, Args);
send(#beamtalk_object{class = 'Metaclass', pid = Pid} = Self, Selector, Args) ->
    %% ADR 0036: Route metaclass objects through the Metaclass dispatch chain.
    %% Must be matched before the generic #beamtalk_object{} clause below.
    beamtalk_class_dispatch:metaclass_send(Pid, Selector, Args, Self);
send(#beamtalk_object{pid = Pid}, Selector, Args) ->
    gen_server:call(Pid, {Selector, Args});
send(X, Selector, Args) when is_tuple(X) ->
    %% Handle tuples that might be beamtalk_objects not matching the record pattern
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            Pid = element(4, X),
            gen_server:call(Pid, {Selector, Args});
        false ->
            dispatch_via_module(X, Selector, Args)
    end;
send(X, Selector, Args) when is_map(X) ->
    send_map(X, Selector, Args);
send(X, Selector, Args) when is_pid(X) ->
    send_pid(X, Selector, Args);
send(X, Selector, Args) ->
    %% All other primitives: route through module_for_value/1
    dispatch_via_module(X, Selector, Args).

%% @private Tagged map dispatch — routes through module_for_value/1 for all
%% tagged-map types, falling back to send_tagged_map_fallback/4 if unregistered.
-spec send_map(map(), atom(), list()) -> term().
send_map(X, Selector, Args) ->
    case module_for_value(X) of
        undefined ->
            Class = beamtalk_tagged_map:class_of(X),
            send_tagged_map_fallback(X, Class, Selector, Args);
        Mod ->
            Mod:dispatch(Selector, Args, X)
    end.

%% @private Dispatch messages to a pid value.
%%
%% Routes Future-specific selectors (BT-813) directly to beamtalk_future.
%% All other selectors fall through to the Pid stdlib module.
-spec send_pid(pid(), atom(), list()) -> term().
send_pid(X, await, _Args) ->
    beamtalk_future:await(X);
send_pid(X, awaitForever, _Args) ->
    beamtalk_future:await_forever(X);
send_pid(X, 'await:', [Timeout]) ->
    beamtalk_future:await(X, Timeout);
send_pid(X, 'whenResolved:', [Block]) ->
    beamtalk_future:when_resolved(X, Block);
send_pid(X, 'whenRejected:', [Block]) ->
    beamtalk_future:when_rejected(X, Block);
send_pid(X, Selector, Args) ->
    dispatch_via_module(X, Selector, Args).

%% @private True if the selector belongs to the Future protocol.
%%
%% Used by both send_pid/3 and responds_to/2 to recognise Future-specific
%% messages on bare pids.
-spec is_future_selector(atom()) -> boolean().
is_future_selector(await) -> true;
is_future_selector(awaitForever) -> true;
is_future_selector('await:') -> true;
is_future_selector('whenResolved:') -> true;
is_future_selector('whenRejected:') -> true;
is_future_selector(_) -> false.

%% @private Fallback for tagged maps without a compiled stdlib module.
-spec send_tagged_map_fallback(map(), atom(), atom(), list()) -> term().
send_tagged_map_fallback(X, Class, Selector, Args) ->
    case beamtalk_exception_handler:is_exception_class(Class) of
        true ->
            beamtalk_exception_handler:dispatch(Selector, Args, X);
        false ->
            value_type_send(X, Class, Selector, Args)
    end.

%% @doc Check if a value responds to a given selector.
%%
%% ADR 0032 Phase 3: For actor instances (#beamtalk_object{}), delegate to
%% beamtalk_dispatch:responds_to/2 which walks the full class hierarchy.
%% The previous approach (Mod:has_method/1) only checked locally-defined methods,
%% causing inherited methods (e.g. 'class' from ProtoObject) to return false.
-spec responds_to(term(), atom()) -> boolean().
responds_to({beamtalk_future, _} = Future, Selector) ->
    %% BT-840: Auto-await tagged futures before protocol checking.
    responds_to(beamtalk_future:await(Future), Selector);
responds_to(#beamtalk_object{class = Tag} = Obj, Selector) ->
    %% BT-776: Class objects (e.g., Counter as a value) are instances of 'Class'.
    %% Walk the Class → Behaviour → Object hierarchy instead of checking class_mod.
    case beamtalk_class_registry:is_class_object(Obj) of
        true ->
            beamtalk_dispatch:responds_to(Selector, 'Class');
        false ->
            ClassName = class_name_from_tag(Tag),
            beamtalk_dispatch:responds_to(Selector, ClassName)
    end;
responds_to(X, Selector) when is_tuple(X) ->
    %% Handle tuples that might be beamtalk_objects not matching the record pattern
    case tuple_size(X) >= 4 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% BT-776: Class objects walk Class hierarchy.
            case beamtalk_class_registry:is_class_object(X) of
                true ->
                    beamtalk_dispatch:responds_to(Selector, 'Class');
                false ->
                    Tag = element(2, X),
                    ClassName = class_name_from_tag(Tag),
                    beamtalk_dispatch:responds_to(Selector, ClassName)
            end;
        false ->
            responds_via_module(X, Selector)
    end;
responds_to(X, Selector) when is_map(X) ->
    responds_to_map(X, Selector);
responds_to(X, Selector) when is_pid(X) ->
    %% BT-813: Future-specific selectors are handled by send_pid/3.
    is_future_selector(Selector) orelse responds_via_module(X, Selector);
responds_to(X, Selector) ->
    %% All other primitives: route through module_for_value/1
    responds_via_module(X, Selector).

%% @private Tagged map responds_to — routes through module_for_value/1 for all
%% tagged-map types, falling back to exception/value-type checks if unregistered.
-spec responds_to_map(map(), atom()) -> boolean().
responds_to_map(X, Selector) ->
    case module_for_value(X) of
        undefined ->
            Class = beamtalk_tagged_map:class_of(X),
            case beamtalk_exception_handler:is_exception_class(Class) of
                true -> beamtalk_exception_handler:has_method(Selector);
                false -> value_type_responds_to(Class, Selector)
            end;
        Mod ->
            Mod:has_method(Selector)
    end.

%% @private Dispatch a value using its mapped stdlib module.
-spec dispatch_via_module(term(), atom(), list()) -> term().
dispatch_via_module(X, Selector, Args) ->
    case module_for_value(X) of
        undefined ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    does_not_understand,
                    class_of(X),
                    Selector,
                    <<"Primitive type does not support this message">>
                )
            );
        Mod ->
            Mod:dispatch(Selector, Args, X)
    end.

%% @private Check method support using the mapped stdlib module.
-spec responds_via_module(term(), atom()) -> boolean().
responds_via_module(X, Selector) ->
    case module_for_value(X) of
        undefined -> false;
        Mod -> Mod:has_method(Selector)
    end.

%% @private Map a runtime value to its stdlib dispatch module.
-spec module_for_value(term()) -> atom() | undefined.
module_for_value(X) when is_integer(X) -> 'bt@stdlib@integer';
module_for_value(X) when is_binary(X) -> 'bt@stdlib@string';
module_for_value(true) ->
    'bt@stdlib@true';
module_for_value(false) ->
    'bt@stdlib@false';
module_for_value(nil) ->
    'bt@stdlib@undefined_object';
module_for_value(X) when is_atom(X) -> 'bt@stdlib@symbol';
module_for_value(X) when is_function(X) -> 'bt@stdlib@block';
module_for_value(X) when is_tuple(X) -> 'bt@stdlib@tuple';
module_for_value(X) when is_float(X) -> 'bt@stdlib@float';
module_for_value(X) when is_list(X) -> 'bt@stdlib@list';
module_for_value(X) when is_pid(X) -> 'bt@stdlib@pid';
module_for_value(X) when is_port(X) -> 'bt@stdlib@port';
module_for_value(X) when is_reference(X) -> 'bt@stdlib@reference';
module_for_value(X) when is_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'CompiledMethod' -> 'bt@stdlib@compiled_method';
        'Association' -> 'bt@stdlib@association';
        'Set' -> 'bt@stdlib@set';
        'Stream' -> 'bt@stdlib@stream';
        'Random' -> 'bt@stdlib@random';
        'TestCase' -> 'bt@stdlib@test_case';
        'Regex' -> 'bt@stdlib@regex';
        'DateTime' -> 'bt@stdlib@date_time';
        'TestResult' -> 'bt@stdlib@test_result';
        'StackFrame' -> beamtalk_stack_frame;
        'ErlangModule' -> beamtalk_erlang_proxy;
        'FileHandle' -> beamtalk_file_handle;
        'Erlang' -> beamtalk_erlang_class;
        undefined -> 'bt@stdlib@dictionary';
        _ -> undefined
    end;
module_for_value(_) ->
    undefined.

%% @private Try dispatching via beamtalk_object_ops (base Object protocol).
%% Returns {ok, Result} if handled, false if not.
-spec try_object_ops(atom(), list(), term()) -> {ok, term()} | false.
try_object_ops(Selector, Args, Self) ->
    case beamtalk_object_ops:has_method(Selector) of
        true ->
            case beamtalk_object_ops:dispatch(Selector, Args, Self, Self) of
                {reply, Result, _State} -> {ok, Result};
                {error, Error, _State} -> beamtalk_error:raise(Error)
            end;
        false ->
            false
    end.

%% @private Send a message to a value type instance (BT-354).
-spec value_type_send(map(), atom(), atom(), list()) -> term().
value_type_send(Self, Class, Selector, Args) ->
    case is_ivar_method(Selector) of
        {true, Hint} ->
            beamtalk_error:raise(beamtalk_error:new(immutable_value, Class, Selector, Hint));
        false ->
            ok
    end,
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, dispatch, 3) of
        true ->
            Module:dispatch(Selector, Args, Self);
        false ->
            Arity = length(Args) + 1,
            case erlang:function_exported(Module, Selector, Arity) of
                true ->
                    erlang:apply(Module, Selector, [Self | Args]);
                false ->
                    case try_object_ops(Selector, Args, Self) of
                        {ok, Result} ->
                            Result;
                        false ->
                            beamtalk_error:raise(
                                beamtalk_error:new(
                                    does_not_understand,
                                    Class,
                                    Selector,
                                    <<"Value type does not understand this message">>
                                )
                            )
                    end
            end
    end.

%% @private Check if a selector is an ivar method (BT-359).
-spec is_ivar_method(atom()) -> {true, binary()} | false.
is_ivar_method('fieldAt:put:') ->
    {true, <<"Value types are immutable. Use a method that returns a new instance instead.">>};
is_ivar_method('fieldAt:') ->
    {true, <<"Value types have no fields">>};
is_ivar_method(_) ->
    false.

%% @private Check if a value type responds to a selector (BT-354).
-spec value_type_responds_to(atom(), atom()) -> boolean().
value_type_responds_to(Class, Selector) ->
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, has_method, 1) of
        true ->
            Module:has_method(Selector);
        false ->
            Exports =
                case erlang:function_exported(Module, module_info, 1) of
                    true -> Module:module_info(exports);
                    false -> []
                end,
            lists:any(fun({Name, _Arity}) -> Name =:= Selector end, Exports) orelse
                beamtalk_object_ops:has_method(Selector)
    end.

%% @private Convert a CamelCase class name atom to a module name atom (ADR 0016).
%%
%% First tries the static naming convention (bt@{snake_case}).
%% If that module is not loaded, falls back to the class registry to
%% resolve package-qualified module names (e.g. bt@{package}@{snake_case}).
%% BT-760: This fallback enables `beamtalk test` to dispatch on package classes.
-spec class_name_to_module(atom()) -> atom().
class_name_to_module(Class) when is_atom(Class) ->
    StaticModule = static_class_module_name(Class),
    case code:is_loaded(StaticModule) of
        {file, _} ->
            StaticModule;
        false ->
            %% Module not yet loaded — try loading it
            case code:ensure_loaded(StaticModule) of
                {module, _} ->
                    StaticModule;
                {error, _} ->
                    %% BT-760: Fall back to class registry for package-qualified modules
                    case beamtalk_class_registry:whereis_class(Class) of
                        undefined -> StaticModule;
                        ClassPid -> beamtalk_object_class:module_name(ClassPid)
                    end
            end
    end.

%% @private Static module name from class name (bt@{snake_case}).
-spec static_class_module_name(atom()) -> atom().
static_class_module_name(Class) ->
    SnakeCase = camel_to_snake(atom_to_list(Class)),
    ModName = "bt@" ++ SnakeCase,
    try
        list_to_existing_atom(ModName)
    catch
        error:badarg ->
            list_to_atom(ModName)
    end.

%% @private CamelCase string to snake_case string conversion.
-spec camel_to_snake(string()) -> string().
camel_to_snake(Str) ->
    camel_to_snake(Str, false, []).

camel_to_snake([], _PrevWasLower, Acc) ->
    lists:reverse(Acc);
camel_to_snake([H | T], PrevWasLower, Acc) when H >= $A, H =< $Z ->
    Lower = H + 32,
    case PrevWasLower of
        true -> camel_to_snake(T, false, [Lower, $_ | Acc]);
        false -> camel_to_snake(T, false, [Lower | Acc])
    end;
camel_to_snake([H | T], _PrevWasLower, Acc) ->
    camel_to_snake(T, (H >= $a andalso H =< $z), [H | Acc]).

%% @private Extract the class name atom from a class tag or class object tag.
%%
%% Handles both plain instance tags (e.g. 'Counter') and class object tags
%% (e.g. 'Counter class') — in both cases returns the class name atom 'Counter'.
%% Used by responds_to/2 to delegate to beamtalk_dispatch:responds_to/2.
-spec class_name_from_tag(atom()) -> atom() | undefined.
class_name_from_tag(Tag) ->
    Bin = beamtalk_class_registry:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> undefined
    end.
