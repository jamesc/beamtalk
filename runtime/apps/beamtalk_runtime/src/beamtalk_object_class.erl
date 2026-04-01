%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-class gen_server for Beamtalk class objects.
%%%
%%% **DDD Context:** Object System Context
%%%
%% Each class in Beamtalk is a first-class object backed by a gen_server
%% process. Follows Smalltalk philosophy where classes are messageable objects.
%%
%% Class method dispatch is handled by beamtalk_class_dispatch.
%% Instance creation (new/spawn) by beamtalk_class_instantiation.
%% Method lookup walks the class chain directly via has_method/2 + superclass/1.
%%
%% ## Registration
%%
%% Classes register via Erlang's built-in registry (beamtalk_class_Counter)
%% and join the `beamtalk_classes` pg group for enumeration.
-module(beamtalk_object_class).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start/2,
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    methods/1,
    superclass/1,
    method/2,
    has_method/2,
    put_method/3,
    put_method/4,
    instance_variables/1,
    is_sealed/1,
    is_abstract/1,
    is_internal/1,
    is_constructible/1,
    class_name/1,
    module_name/1,
    class_send/3,
    local_call/3,
    set_class_var/3,
    update_class/2,
    local_class_methods/1,
    local_class_methods_map/1,
    local_instance_methods/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type class_name() :: atom().
-type selector() :: atom().
%% ADR 0068: Runtime representation of types in method_info metadata.
%% Atoms for concrete types, tagged tuples for type parameters and generic types.
-type meta_type_repr() ::
    atom()
    | {type_param, Name :: atom(), Index :: integer()}
    | {generic, Base :: atom(), Params :: [meta_type_repr()]}.
-type method_info() :: #{
    arity => non_neg_integer(),
    block => fun(),
    is_sealed => boolean()
}.

-record(class_state, {
    name :: class_name(),
    module :: atom(),
    superclass :: class_name() | none,
    is_sealed = false :: boolean(),
    is_abstract = false :: boolean(),
    is_constructible = undefined :: boolean() | undefined,
    instance_methods = #{} :: #{selector() => method_info()},
    class_methods = #{} :: #{selector() => method_info()},
    fields = [] :: [atom()],
    class_state = #{} :: map(),
    method_source = #{} :: #{selector() => binary()},
    %% BT-988: Method display signatures for :help command
    method_signatures = #{} :: #{selector() => binary()},
    %% BT-990: Class-side method display signatures for :help command
    class_method_signatures = #{} :: #{selector() => binary()},
    %% BT-1002 / ADR 0045: Machine-readable return-type map for chain resolution.
    %% ADR 0068: Values may be atoms (concrete types) or tagged tuples:
    %%   {type_param, Name :: atom(), Index :: integer()}
    %%   {generic, Base :: atom(), Params :: [meta_type_repr()]}
    method_return_types = #{} :: #{selector() => meta_type_repr()},
    class_method_return_types = #{} :: #{selector() => meta_type_repr()},
    %% ADR 0033: Runtime-embedded documentation
    doc = none :: binary() | none,
    method_docs = #{} :: #{selector() => binary()},
    %% BT-1634: Class method doc comments
    class_method_docs = #{} :: #{selector() => binary()},
    %% ADR 0071 Phase 5: Class visibility (internal vs public)
    is_internal = false :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a class process (unlinked) for on_load registration.
-spec start(class_name(), map()) -> {ok, pid()} | {error, term()}.
start(ClassName, ClassInfo) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    gen_server:start({local, RegName}, ?MODULE, {ClassName, ClassInfo}, []).

%% @doc Start a class process with full options.
-spec start_link(class_name(), map()) -> {ok, pid()} | {error, term()}.
start_link(ClassName, ClassInfo) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    gen_server:start_link({local, RegName}, ?MODULE, {ClassName, ClassInfo}, []).

%% @doc Start a class process with minimal info (for testing).
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(ClassInfo) ->
    ClassName = maps:get(name, ClassInfo),
    start_link(ClassName, ClassInfo).

%% @doc Update an existing class process with new metadata after redefinition.
-spec update_class(class_name(), map()) -> {ok, [atom()]} | {error, term()}.
update_class(ClassName, ClassInfo) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        Pid ->
            gen_server:call(Pid, {update_class, ClassInfo})
    end.

%% @doc Set a class variable on a class by name.
-spec set_class_var(class_name(), atom(), term()) -> term().
set_class_var(ClassName, Name, Value) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            error(Error0);
        Pid ->
            gen_server:call(Pid, {set_class_var, Name, Value})
    end.

%% @doc Create a new instance of this class.
-spec new(pid()) -> {ok, #beamtalk_object{}} | {error, term()}.
new(ClassPid) ->
    new(ClassPid, []).

%% @doc Create a new instance with initialization arguments.
-spec new(pid(), list()) -> {ok, #beamtalk_object{}} | {error, term()}.
new(ClassPid, Args) ->
    gen_server:call(ClassPid, {new, Args}).

%% @doc Get all method selectors.
-spec methods(pid()) -> [selector()].
methods(ClassPid) ->
    gen_server:call(ClassPid, methods).

%% @doc Get local class-side method selectors (not inherited).
-spec local_class_methods(pid()) -> [selector()].
local_class_methods(ClassPid) ->
    maps:keys(local_class_methods_map(ClassPid)).

%% @doc Get the full local class-side methods map (selector => info).
-spec local_class_methods_map(pid()) -> #{selector() => map()}.
local_class_methods_map(ClassPid) ->
    gen_server:call(ClassPid, get_local_class_methods).

%% @doc Get local instance-side method selectors (not inherited).
-spec local_instance_methods(pid()) -> [selector()].
local_instance_methods(ClassPid) ->
    {ok, InstanceMethods} = gen_server:call(ClassPid, get_instance_methods),
    maps:keys(InstanceMethods).

%% @doc Get the superclass name.
-spec superclass(pid()) -> class_name() | none.
superclass(ClassPid) ->
    gen_server:call(ClassPid, superclass).

%% @doc Get the class name.
%%
%% BT-893: If called from within the class gen_server itself (self-call), read
%% from the process dictionary instead of gen_server:call to avoid calling_self.
-spec class_name(pid()) -> class_name().
class_name(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_name);
class_name(ClassPid) ->
    gen_server:call(ClassPid, class_name).

%% @doc Get the module name.
%%
%% BT-893: If called from within the class gen_server itself (self-call), read
%% from the process dictionary instead of gen_server:call to avoid calling_self.
-spec module_name(pid()) -> atom().
module_name(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_module);
module_name(ClassPid) ->
    gen_server:call(ClassPid, module_name).

%% @doc Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).
%%
%% Delegates to beamtalk_class_dispatch (BT-704).
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(ClassPid, Selector, Args) ->
    beamtalk_class_dispatch:class_send(ClassPid, Selector, Args).

%% @doc Execute a class method in the caller's process (BT-1664).
%%
%% Resolves the target module from the class object, then calls
%% Module:class_<Selector>(nil, #{}, Args) directly — bypassing the class
%% object's gen_server. The caller takes responsibility for knowing the
%% method does not mutate class state (nil is passed for ClassSelf).
%%
%% Raises beamtalk_error if the receiver is not a class object, or if the
%% class does not define the requested method.
-spec local_call(term(), atom(), list()) -> term().
local_call(Receiver = #beamtalk_object{class_mod = Module}, Selector, Args) when
    is_atom(Selector), is_list(Args)
->
    case beamtalk_class_registry:is_class_object(Receiver) of
        true ->
            FunName = beamtalk_class_dispatch:class_method_fun_name(Selector),
            code:ensure_loaded(Module),
            case erlang:function_exported(Module, FunName, length(Args) + 2) of
                true ->
                    case erlang:apply(Module, FunName, [nil, #{} | Args]) of
                        {class_var_result, Value, _NewClassVars} ->
                            %% Discard class var mutations — local_call does not
                            %% update the class object's state.
                            Value;
                        Result ->
                            Result
                    end;
                false ->
                    ClassName = Receiver#beamtalk_object.class,
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Class does not understand this message">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        false ->
            ClassName = Receiver#beamtalk_object.class,
            Error = beamtalk_error:new(
                type_error,
                ClassName,
                'performLocally:withArguments:',
                <<"Receiver is not a class object">>
            ),
            beamtalk_error:raise(Error)
    end;
local_call(Receiver, _Selector, _Args) ->
    ClassName =
        case Receiver of
            #beamtalk_object{class = C} -> C;
            _ -> erlang
        end,
    Error = beamtalk_error:new(
        type_error,
        ClassName,
        'performLocally:withArguments:',
        <<"Receiver is not a class object">>
    ),
    beamtalk_error:raise(Error).

%% @doc Get a compiled method object.
%% Delegates to beamtalk_method_resolver.
-spec method(pid() | class_name() | tuple(), selector()) -> compiled_method() | 'nil'.
method(ClassRef, Selector) ->
    beamtalk_method_resolver:resolve(ClassRef, Selector).

%% @doc Check if a class is internal (ADR 0071).
-spec is_internal(pid()) -> boolean().
is_internal(ClassPid) ->
    gen_server:call(ClassPid, is_internal).

%% @doc Check if a class has a method (does not walk hierarchy).
%% First checks the gen_server state (instance_methods map for compiled methods),
%% then falls back to the module's has_method/1. The fallback is needed for
%% abstract/value-type classes (e.g., Behaviour) where @primitive methods appear
%% in the compiled module but may not be tracked in the gen_server instance_methods.
-spec has_method(pid(), selector()) -> boolean().
has_method(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            case module_name(ClassPid) of
                undefined ->
                    false;
                ModuleName ->
                    case erlang:function_exported(ModuleName, has_method, 1) of
                        true ->
                            try
                                ModuleName:has_method(Selector)
                            catch
                                _:_ -> false
                            end;
                        false ->
                            false
                    end
            end;
        _MethodInfo ->
            true
    end.

%% @doc Replace a method with a new function (hot patching).
-spec put_method(pid(), selector(), fun()) -> ok.
put_method(ClassPid, Selector, Fun) ->
    put_method(ClassPid, Selector, Fun, <<"">>).

%% @doc Replace a method with source.
-spec put_method(pid(), selector(), fun(), binary()) -> ok.
put_method(ClassPid, Selector, Fun, Source) ->
    gen_server:call(ClassPid, {put_method, Selector, Fun, Source}).

%% @doc Get instance variable names.
-spec instance_variables(pid()) -> [atom()].
instance_variables(ClassPid) ->
    gen_server:call(ClassPid, instance_variables).

%% @doc Check if a class is sealed (cannot be subclassed).
-spec is_sealed(pid()) -> boolean().
is_sealed(ClassPid) ->
    gen_server:call(ClassPid, is_sealed).

%% @doc Check if a class is abstract (cannot be instantiated).
-spec is_abstract(pid()) -> boolean().
is_abstract(ClassPid) ->
    gen_server:call(ClassPid, is_abstract).

%% @doc Check if a class can be instantiated via new/new:.
%% BT-474: Returns false for actors (use spawn) and sealed primitives.
-spec is_constructible(pid()) -> boolean().
is_constructible(ClassPid) ->
    gen_server:call(ClassPid, is_constructible).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ClassName, ClassInfo}) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    ok = pg:join(beamtalk_classes, self()),

    Superclass = maps:get(superclass, ClassInfo, none),
    Module = maps:get(module, ClassInfo, ClassName),

    %% ADR 0050 Phase 5: Use meta from ClassInfo when available (set by the compiler
    %% in BuilderState.meta). During on_load, erlang:function_exported/3 returns false,
    %% making Module:'__beamtalk_meta'() unavailable via read_meta. The meta map literal
    %% in BuilderState provides the same data without requiring a function call.
    %% Falls back to read_meta for hot reload (apply_class_info) and dynamic classes.
    Meta =
        case maps:get(meta, ClassInfo, undefined) of
            undefined -> read_meta(Module);
            M when is_map(M) -> M
        end,
    IsAbstract = maps:get(is_abstract, Meta, maps:get(is_abstract, ClassInfo, false)),
    %% ADR 0071 Phase 5: Read visibility from meta (the codegen emits 'visibility' => public|internal).
    %% Also support 'is_internal' boolean for direct ClassInfo usage.
    IsInternal =
        case maps:get(visibility, Meta, maps:get(visibility, ClassInfo, public)) of
            internal -> true;
            _ -> maps:get(is_internal, Meta, maps:get(is_internal, ClassInfo, false))
        end,
    {InstanceMethods, MethodReturnTypes} = meta_to_methods(
        maps:get(method_info, Meta, undefined),
        maps:get(instance_methods, ClassInfo, #{})
    ),
    {ClassMethods, ClassMethodReturnTypes} = meta_to_methods(
        maps:get(class_method_info, Meta, undefined),
        maps:get(class_methods, ClassInfo, #{})
    ),

    beamtalk_class_hierarchy_table:insert(ClassName, Superclass),
    %% BT-1285: Store module name in ETS for deadlock-free lookup during supervisor init.
    beamtalk_class_module_table:insert(ClassName, Module),

    %% BT-893: Store class metadata in process dictionary so class_send can
    %% bypass gen_server for self-calls (new/spawn from within class methods).
    put(beamtalk_class_name, ClassName),
    put(beamtalk_class_module, Module),
    put(beamtalk_class_is_abstract, IsAbstract),

    %% BT-1768: Record pid→classname mapping for crash recovery.
    %% This entry survives process death, allowing class_send to identify
    %% which class a dead pid belonged to and attempt auto-restart.
    beamtalk_class_registry:record_class_pid(self(), ClassName),

    %% ADR 0050 Phase 3: Notify compiler server of this class registration.
    %% Cast is fire-and-forget — silently dropped if the compiler server is not running.
    %% Use Meta map availability (not function_exported, which returns false during on_load).
    %% BT-1732: Wrap in try/catch so an undef crash (e.g., beamtalk_compiler_server
    %% not on code path) doesn't kill the class process during on_load.
    case Meta of
        CompilerMeta when is_map(CompilerMeta), map_size(CompilerMeta) > 0 ->
            try
                beamtalk_compiler_server:register_class(ClassName, CompilerMeta)
            catch
                error:undef ->
                    ?LOG_WARNING(#{
                        event => register_class_undef,
                        class => ClassName,
                        reason => "beamtalk_compiler_server:register_class/2 not available",
                        domain => [beamtalk, runtime]
                    }),
                    ok
            end;
        _ ->
            ok
    end,

    %% BT-877: is_constructible may be set by the compiler via ClassBuilder.
    %% If not set (undefined), it will be computed lazily on first new call.
    %% ADR 0032 Phase 1: No flattened method tables — dispatch walks the chain directly.
    State = #class_state{
        name = ClassName,
        module = Module,
        superclass = Superclass,
        is_sealed = maps:get(is_sealed, Meta, maps:get(is_sealed, ClassInfo, false)),
        is_abstract = IsAbstract,
        is_constructible = maps:get(is_constructible, ClassInfo, undefined),
        instance_methods = InstanceMethods,
        class_methods = ClassMethods,
        fields = maps:get(fields, Meta, maps:get(fields, ClassInfo, [])),
        class_state = maps:get(class_state, ClassInfo, #{}),
        method_source = maps:get(method_source, ClassInfo, #{}),
        method_signatures = maps:get(method_signatures, ClassInfo, #{}),
        class_method_signatures = maps:get(class_method_signatures, ClassInfo, #{}),
        %% Merge: ClassInfo explicit values (from old format) are lower priority than meta.
        method_return_types = maps:merge(
            maps:get(method_return_types, ClassInfo, #{}),
            MethodReturnTypes
        ),
        class_method_return_types = maps:merge(
            maps:get(class_method_return_types, ClassInfo, #{}),
            ClassMethodReturnTypes
        ),
        doc = maps:get(doc, ClassInfo, none),
        method_docs = maps:get(method_docs, ClassInfo, #{}),
        class_method_docs = maps:get(class_method_docs, ClassInfo, #{}),
        is_internal = IsInternal
    },
    {ok, State}.

handle_call(
    {spawn, Args},
    _From,
    #class_state{name = ClassName, module = Module, is_abstract = IsAbstract} = State
) ->
    {reply, beamtalk_class_instantiation:handle_spawn(Args, ClassName, Module, IsAbstract), State};
handle_call({new, Args}, _From, #class_state{name = ClassName, is_abstract = true} = State) ->
    Selector =
        case Args of
            [] -> 'new';
            _ -> 'new:'
        end,
    {reply, {error, beamtalk_class_instantiation:abstract_class_error(ClassName, Selector)}, State};
handle_call(
    {new, Args},
    _From,
    #class_state{
        name = ClassName,
        module = Module,
        is_constructible = IsConstructible0,
        class_methods = ClassMethods,
        class_state = ClassVars
    } = State
) ->
    %% When Args is non-empty and the class (or any ancestor) has a user-defined
    %% `class new:` method, route the `new:` call through dispatch instead of the
    %% generic field-initialisation constructor.
    %% This lets `List new: #(1,2,3)` call `List class new:` rather than the
    %% `Object.new:` dict-initialiser (which would error on a non-map argument).
    %%
    %% Note: the `classMethods` metadata includes auto-generated `new:` for all
    %% classes, so `is_map_key/2` alone cannot distinguish user-defined from
    %% auto-generated. We check `erlang:function_exported/3` on each module in the
    %% superclass chain, which is true only when the BEAM module contains an
    %% explicit `class_new:` function — correctly supporting inherited constructors.
    case Args =/= [] andalso has_class_new_in_chain(ClassName, Module) of
        true ->
            case
                beamtalk_class_dispatch:handle_class_method_call(
                    'new:', Args, ClassName, Module, ClassMethods, ClassVars
                )
            of
                {reply, Result, NewClassVars} ->
                    {reply, Result, State#class_state{class_state = NewClassVars}};
                {error, not_found} ->
                    Err = beamtalk_error:new(does_not_understand, ClassName, 'new:'),
                    {reply, {error, Err}, State}
            end;
        false ->
            case
                beamtalk_class_instantiation:handle_new(
                    Args,
                    ClassName,
                    Module,
                    IsConstructible0
                )
            of
                {ok, Result, IsConstructible} ->
                    {reply, {ok, Result}, State#class_state{is_constructible = IsConstructible}};
                {error, Error, IsConstructible} ->
                    {reply, {error, Error}, State#class_state{is_constructible = IsConstructible}}
            end
    end;
%% ADR 0032 Phase 1: Returns local (non-inherited) method selectors only.
%% Full hierarchy walk is done by Behaviour.methods in Phase 2.
handle_call(methods, _From, #class_state{instance_methods = Methods} = State) ->
    {reply, maps:keys(Methods), State};
handle_call(superclass, _From, #class_state{superclass = Super} = State) ->
    {reply, Super, State};
handle_call(class_name, _From, #class_state{name = Name} = State) ->
    {reply, Name, State};
handle_call(module_name, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};
handle_call(
    {method, Selector},
    _From,
    #class_state{
        instance_methods = Methods,
        method_source = Source,
        method_signatures = Signatures,
        method_docs = MethodDocs
    } = State
) ->
    Result =
        case maps:find(Selector, Methods) of
            {ok, MethodInfo} ->
                #{
                    '$beamtalk_class' => 'CompiledMethod',
                    '__selector__' => Selector,
                    '__source__' => maps:get(Selector, Source, <<"">>),
                    '__signature__' => maps:get(Selector, Signatures, nil),
                    '__method_info__' => MethodInfo,
                    '__doc__' => maps:get(Selector, MethodDocs, nil)
                };
            error ->
                nil
        end,
    {reply, Result, State};
%% BT-1002 / ADR 0045: Local return-type lookup (no chain walk — that is done in the registry).
handle_call(
    {get_method_return_type, Selector},
    _From,
    #class_state{method_return_types = MRT} = State
) ->
    case maps:find(Selector, MRT) of
        {ok, Type} -> {reply, {ok, Type}, State};
        error -> {reply, not_found, State}
    end;
handle_call(
    {get_class_method_return_type, Selector},
    _From,
    #class_state{class_method_return_types = CMRT} = State
) ->
    case maps:find(Selector, CMRT) of
        {ok, Type} -> {reply, {ok, Type}, State};
        error -> {reply, not_found, State}
    end;
%% BT-990: Return CompiledMethod-like map for class-side methods.
%% Walks superclass chain for inherited class methods (mirrors dispatch behaviour).
handle_call(
    {class_method, Selector},
    _From,
    #class_state{
        superclass = Superclass,
        class_methods = ClassMethods,
        class_method_signatures = ClassMethodSigs,
        class_method_docs = ClassMethodDocs
    } = State
) ->
    Result =
        case maps:find(Selector, ClassMethods) of
            {ok, MethodInfo} ->
                #{
                    '$beamtalk_class' => 'CompiledMethod',
                    '__selector__' => Selector,
                    '__source__' => <<"">>,
                    '__signature__' => maps:get(Selector, ClassMethodSigs, nil),
                    '__method_info__' => MethodInfo,
                    '__doc__' => maps:get(Selector, ClassMethodDocs, nil)
                };
            error ->
                %% Not found locally — walk superclass chain
                find_inherited_class_method(Selector, Superclass)
        end,
    {reply, Result, State};
handle_call({put_method, Selector, Fun, Source}, _From, State) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    MethodInfo = #{block => Fun, arity => Arity},
    %% ADR 0032 Phase 1: No flattened table to rebuild or invalidate.
    %% Dispatch finds the new method via chain walk on the next call.
    %% BT-988: Remove stale display signature — dynamically-defined methods
    %% have no AST, so the fallback to selector atom is correct.
    %% BT-1002: Also clear stale return-type entry for the same reason.
    NewState = State#class_state{
        instance_methods = maps:put(Selector, MethodInfo, State#class_state.instance_methods),
        method_source = maps:put(Selector, Source, State#class_state.method_source),
        method_signatures = maps:remove(Selector, State#class_state.method_signatures),
        method_return_types = maps:remove(Selector, State#class_state.method_return_types)
    },
    %% ADR 0050 Phase 3: Notify compiler server of hot-patch.
    %% __beamtalk_meta/0 is stale after hot-patching, so synthesize metadata
    %% from the live class_state record instead.  Return types are cleared for
    %% hot-patched methods — the compiler treats them as dynamic.
    notify_hot_patch(NewState),
    {reply, ok, NewState};
%% BT-572: Update class metadata after redefinition (hot reload).
%% BT-737/BT-738: Validation (shadowing + collision) delegated to beamtalk_class_registry.
%% ADR 0032 Phase 1: No flattened tables to rebuild or invalidate.
handle_call({update_class, ClassInfo}, _From, #class_state{name = ClassName} = State) ->
    OldModule = State#class_state.module,
    case beamtalk_class_registry:validate_class_update(ClassName, OldModule, ClassInfo) of
        {error, Error} ->
            {reply, {error, Error}, State};
        ok ->
            NewState = apply_class_info(State, ClassInfo),
            %% ADR 0050 Phase 3: Notify compiler server of class redefinition.
            %% The new module is already loaded, so __beamtalk_meta/0 is fresh.
            NewModule = NewState#class_state.module,
            case erlang:function_exported(NewModule, '__beamtalk_meta', 0) of
                true ->
                    try NewModule:'__beamtalk_meta'() of
                        Meta when is_map(Meta) ->
                            beamtalk_compiler_server:register_class(ClassName, Meta);
                        _ ->
                            ok
                    catch
                        _:_ -> ok
                    end;
                false ->
                    ok
            end,
            {reply, {ok, NewState#class_state.fields}, NewState}
    end;
handle_call(instance_variables, _From, #class_state{fields = IVars} = State) ->
    {reply, IVars, State};
handle_call(is_sealed, _From, #class_state{is_sealed = Sealed} = State) ->
    {reply, Sealed, State};
handle_call(is_internal, _From, #class_state{is_internal = Internal} = State) ->
    {reply, Internal, State};
handle_call(is_abstract, _From, #class_state{is_abstract = Abstract} = State) ->
    {reply, Abstract, State};
handle_call(
    is_constructible,
    _From,
    #class_state{
        is_constructible = IsConstructible0,
        module = Module,
        is_abstract = IsAbstract
    } = State
) ->
    IsConstructible = beamtalk_class_instantiation:ensure_is_constructible(
        IsConstructible0, Module, IsAbstract
    ),
    {reply, IsConstructible, State#class_state{is_constructible = IsConstructible}};
%% ADR 0032 Phase 1: Returns local methods for chain walk queries.
handle_call(get_local_class_methods, _From, #class_state{class_methods = ClassMethods} = State) ->
    {reply, ClassMethods, State};
handle_call(get_instance_methods, _From, #class_state{instance_methods = InstanceMethods} = State) ->
    {reply, {ok, InstanceMethods}, State};
%% ADR 0033: Runtime-embedded documentation — class doc accessors.
handle_call(get_doc, _From, #class_state{doc = Doc} = State) ->
    {reply, Doc, State};
handle_call({set_doc, DocBinary}, _From, State) ->
    {reply, ok, State#class_state{doc = DocBinary}};
handle_call({set_method_doc, Selector, DocBinary}, _From, State) ->
    NewMethodDocs = maps:put(Selector, DocBinary, State#class_state.method_docs),
    {reply, ok, State#class_state{method_docs = NewMethodDocs}};
%% BT-411/BT-412/BT-440: Class method dispatch (BT-704).
%% ADR 0036 Phase 2 (BT-823): Also handles metaclass_method_call with identical logic.
%% ADR 0032 Phase 1: Passes local class_methods; dispatch walks superclass chain.
handle_call(
    {MethodCallType, Selector, Args},
    From,
    #class_state{
        class_methods = ClassMethods,
        instance_methods = InstanceMethods,
        name = ClassName,
        module = Module,
        class_state = ClassVars
    } = State
) when MethodCallType =:= class_method_call; MethodCallType =:= metaclass_method_call ->
    case
        beamtalk_class_dispatch:handle_class_method_call(
            Selector, Args, ClassName, Module, ClassMethods, ClassVars
        )
    of
        {reply, Result, NewClassVars} ->
            {reply, Result, State#class_state{class_state = NewClassVars}};
        test_spawn ->
            beamtalk_test_case:spawn_test_execution(
                Selector, Args, ClassName, Module, InstanceMethods, From
            ),
            {noreply, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;
handle_call({initialize, _Args}, _From, #class_state{} = State) ->
    {reply, {ok, nil}, State};
handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};
handle_call({get_class_var, Name}, _From, #class_state{class_state = ClassVars} = State) ->
    {reply, maps:get(Name, ClassVars, nil), State};
handle_call({set_class_var, Name, Value}, _From, #class_state{class_state = ClassVars} = State) ->
    {reply, Value, State#class_state{class_state = maps:put(Name, Value, ClassVars)}}.

%% ADR 0032 Phase 1: Passes instance_methods (local only) instead of flattened table.
handle_cast(
    Msg,
    #class_state{
        instance_methods = InstanceMethods,
        name = ClassName,
        superclass = Superclass,
        module = Module
    } = State
) ->
    beamtalk_class_dispatch:handle_async_dispatch(
        Msg, ClassName, InstanceMethods, Superclass, Module
    ),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #class_state{name = ClassName}) ->
    %% BT-785: Clean up ETS hierarchy entry and pg group membership on shutdown.
    %% This runs when removeFromSystem stops the gen_server (gen_server:stop/1),
    %% ensuring the class is fully removed from the runtime registries.
    %% Wrapped in catch/try to be safe during node shutdown when ETS/pg may be gone.
    _ = (catch beamtalk_class_hierarchy_table:delete(ClassName)),
    _ = (catch beamtalk_class_module_table:delete(ClassName)),
    %% BT-1768: Clean up pid reverse index. Only cleaned on graceful shutdown —
    %% on crash, the entry intentionally survives for auto-restart recovery.
    _ = (catch ets:delete(beamtalk_class_pids, self())),
    _ = (catch pg:leave(beamtalk_classes, self())),
    ok.

%% ADR 0032 Phase 1: No flattened tables to rebuild after hot reload.
code_change(OldVsn, State, Extra) ->
    {ok, NewState} = beamtalk_hot_reload:code_change(OldVsn, State, Extra),
    {ok, NewState#class_state{is_constructible = undefined}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Walk superclass chain to find an inherited class method.
%% Returns a CompiledMethod-like map or nil if not found.
%% Each hop is a gen_server:call to the superclass process, which handles its
%% own {class_method, _} lookup.  The self() guard prevents deadlock if a
%% class lists itself as its own superclass; the 5 s timeout caps any deeper
%% cycle that might slip through.
-spec find_inherited_class_method(atom(), atom() | none) -> map() | nil.
find_inherited_class_method(_Selector, none) ->
    nil;
find_inherited_class_method(Selector, SuperName) ->
    case beamtalk_class_registry:whereis_class(SuperName) of
        undefined ->
            nil;
        SuperPid when SuperPid =:= self() ->
            %% Guard against self-call which would deadlock
            nil;
        SuperPid ->
            try
                gen_server:call(SuperPid, {class_method, Selector}, 5000)
            catch
                exit:{timeout, _} -> nil;
                exit:{noproc, _} -> nil
            end
    end.

%% @private Check whether `class_new:/3` is exported anywhere in the superclass chain.
%%
%% Walks the ETS hierarchy table from `ClassName` upward.  Returns `true` as soon as
%% a module exporting `class_new:'/3` is found, `false` if none is found in the chain.
%% This correctly supports inherited `class new:` constructors: a subclass that does
%% not override `class new:` will still route through the parent's implementation.
%%
%% We use `function_exported/3` rather than checking the `classMethods` metadata map
%% because the metadata includes auto-generated `new:` entries for all classes,
%% making it impossible to distinguish user-defined from auto-generated with a map
%% key check alone.
-spec has_class_new_in_chain(class_name(), atom()) -> boolean().
has_class_new_in_chain(ClassName, Module) ->
    has_class_new_in_chain(ClassName, Module, 0).

-spec has_class_new_in_chain(class_name(), atom(), non_neg_integer()) -> boolean().
has_class_new_in_chain(_ClassName, _Module, Depth) when Depth > 50 ->
    false;
has_class_new_in_chain(ClassName, Module, Depth) ->
    case erlang:function_exported(Module, 'class_new:', 3) of
        true ->
            true;
        false ->
            SuperName =
                case beamtalk_class_hierarchy_table:lookup(ClassName) of
                    {ok, S} -> S;
                    not_found -> none
                end,
            case SuperName of
                none ->
                    false;
                _ ->
                    case beamtalk_class_registry:whereis_class(SuperName) of
                        undefined ->
                            false;
                        SuperPid when SuperPid =:= self() ->
                            false;
                        SuperPid ->
                            try
                                SuperModule = module_name(SuperPid),
                                has_class_new_in_chain(SuperName, SuperModule, Depth + 1)
                            catch
                                _:_ -> false
                            end
                    end
            end
    end.

%% @private Read __beamtalk_meta/0 from a compiled module.
%%
%% Returns the meta map or #{} if the function is not exported or fails.
%% ADR 0050 Phase 5: Used by init/1 and apply_class_info/2 to read static metadata.
-spec read_meta(atom()) -> map().
read_meta(Module) ->
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true ->
            try Module:'__beamtalk_meta'() of
                M when is_map(M) -> M;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.

%% @private Convert a method_info map from __beamtalk_meta/0 to the internal format.
%%
%% Returns {MethodMap, ReturnTypes} where:
%%   MethodMap: selector => #{arity => N, is_sealed => bool, ...}
%%   ReturnTypes: selector => atom (only for non-none return types)
%%
%% Falls back to FallbackMethods when MetaMethodInfo is undefined (dynamic classes
%% without __beamtalk_meta/0 pass method closures via ClassInfo instead).
%%
%% The full method info map is preserved (including is_sealed, param_types, etc.)
%% because runtime tooling (e.g. :help) reads these keys from __method_info__.
-spec meta_to_methods(map() | undefined, map()) -> {map(), map()}.
meta_to_methods(undefined, FallbackMethods) ->
    %% No meta available: return fallback methods with empty return types.
    %% Callers that need to preserve explicit return_types must merge them separately.
    {FallbackMethods, #{}};
meta_to_methods(MetaMethodInfo, _FallbackMethods) when is_map(MetaMethodInfo) ->
    ReturnTypes = maps:filtermap(
        fun
            (_Sel, #{return_type := RT}) when RT =/= none -> {true, RT};
            (_, _) -> false
        end,
        MetaMethodInfo
    ),
    {MetaMethodInfo, ReturnTypes}.

%% @private Notify the compiler server of a hot-patch.
%%
%% ADR 0050 Phase 3: `__beamtalk_meta/0` is stale after hot-patching (it reflects
%% compile-time metadata only).  This function synthesizes a minimal metadata map
%% from the current live `class_state` and casts it to the compiler server.
%% The compiler server replaces the class entry wholesale.
%% Fire-and-forget — silently dropped if the server is not running.
-spec notify_hot_patch(#class_state{}) -> ok.
notify_hot_patch(#class_state{
    name = ClassName,
    module = Module,
    superclass = Superclass,
    is_sealed = IsSealed,
    is_abstract = IsAbstract,
    instance_methods = InstanceMethods,
    class_methods = ClassMethods,
    fields = Fields,
    method_return_types = MethodReturnTypes,
    class_method_return_types = ClassMethodReturnTypes
}) ->
    %% Build selector→arity maps from the live instance_methods and class_methods.
    %% param_types uses atom `none` (not empty list) per ADR 0050 format.
    MethodInfo = maps:map(
        fun(Sel, #{arity := Arity}) ->
            %% Return types are cleared for hot-patched selectors — the compiler
            %% will treat them as dynamic (none).
            #{
                arity => Arity,
                return_type => maps:get(Sel, MethodReturnTypes, none),
                param_types => none
            }
        end,
        InstanceMethods
    ),
    ClassMethodInfo = maps:map(
        fun(Sel, #{arity := Arity}) ->
            #{
                arity => Arity,
                return_type => maps:get(Sel, ClassMethodReturnTypes, none),
                param_types => none
            }
        end,
        ClassMethods
    ),
    %% Normalize superclass: class_state uses `none` for root classes, but
    %% __beamtalk_meta/0 uses `nil`.  Keep the cache consistent with the meta format.
    SuperclassOut =
        case Superclass of
            none -> nil;
            S -> S
        end,
    Meta = #{
        class => ClassName,
        superclass => SuperclassOut,
        module => Module,
        meta_version => 2,
        is_sealed => IsSealed,
        is_abstract => IsAbstract,
        fields => Fields,
        method_info => MethodInfo,
        class_method_info => ClassMethodInfo
    },
    %% Guard: the compiler app may not be present in all deployments.
    %% beamtalk_runtime does not formally depend on beamtalk_compiler.
    case erlang:function_exported(beamtalk_compiler_server, register_class, 2) of
        true -> beamtalk_compiler_server:register_class(ClassName, Meta);
        false -> ok
    end.

%% @private Apply ClassInfo map to existing State, returning updated State.
%% ADR 0032 Phase 1: No flattened tables to rebuild.
%% ADR 0050 Phase 5: Static metadata read from __beamtalk_meta/0 on the new module.
-spec apply_class_info(#class_state{}, map()) -> #class_state{}.
apply_class_info(State, ClassInfo) ->
    %% BT-893: Keep process dictionary in sync with state for self-instantiation.
    NewModule = maps:get(module, ClassInfo, State#class_state.module),
    put(beamtalk_class_module, NewModule),

    %% ADR 0050 Phase 5: Prefer meta from ClassInfo when available (same reason as init/1:
    %% erlang:function_exported/3 returns false during on_load, so read_meta/1 returns #{}).
    %% Falls back to read_meta for explicit hot-reload calls made outside on_load.
    Meta =
        case maps:get(meta, ClassInfo, undefined) of
            undefined -> read_meta(NewModule);
            M when is_map(M) -> M
        end,
    NewIsAbstract = maps:get(
        is_abstract,
        Meta,
        maps:get(is_abstract, ClassInfo, State#class_state.is_abstract)
    ),
    put(beamtalk_class_is_abstract, NewIsAbstract),

    %% ADR 0071 Phase 5: Recompute is_internal on reload/redefinition.
    %% Same logic as init/1 — keeps visibility in sync after hot-reload.
    NewIsInternal =
        case maps:get(visibility, Meta, maps:get(visibility, ClassInfo, public)) of
            internal ->
                true;
            _ ->
                maps:get(
                    is_internal,
                    Meta,
                    maps:get(is_internal, ClassInfo, State#class_state.is_internal)
                )
        end,

    {NewInstanceMethods, NewMethodReturnTypes} = meta_to_methods(
        maps:get(method_info, Meta, undefined),
        maps:get(instance_methods, ClassInfo, State#class_state.instance_methods)
    ),
    {NewClassMethods, NewClassMethodReturnTypes} = meta_to_methods(
        maps:get(class_method_info, Meta, undefined),
        maps:get(class_methods, ClassInfo, State#class_state.class_methods)
    ),

    %% ADR 0057: Fix stale superclass left by bootstrap stubs. Bootstrap stubs register
    %% abstract stdlib classes with placeholder superclasses (e.g. 'Class' → 'Object').
    %% When the compiled stdlib module loads and calls update_class, Meta now carries the
    %% correct superclass from __beamtalk_meta/0. We update both the gen_server field and
    %% the ETS hierarchy table so all hierarchy traversal code sees the right value.
    %% Dynamic classes (no compiled module) have Meta = #{} so maps:find returns error
    %% and their existing superclass is preserved unchanged.
    NewSuperclass =
        case maps:find(superclass, Meta) of
            %% key absent — keep existing
            error -> State#class_state.superclass;
            %% root class (codegen emits 'nil')
            {ok, nil} -> none;
            %% corrected value from compiled module
            {ok, S} -> S
        end,
    beamtalk_class_hierarchy_table:insert(State#class_state.name, NewSuperclass),
    %% BT-1285: Keep module ETS table in sync when module changes on hot-reload.
    beamtalk_class_module_table:insert(State#class_state.name, NewModule),

    State#class_state{
        module = NewModule,
        superclass = NewSuperclass,
        instance_methods = NewInstanceMethods,
        class_methods = NewClassMethods,
        fields = maps:get(
            fields,
            Meta,
            maps:get(fields, ClassInfo, State#class_state.fields)
        ),
        is_abstract = NewIsAbstract,
        is_sealed = maps:get(
            is_sealed,
            Meta,
            maps:get(is_sealed, ClassInfo, State#class_state.is_sealed)
        ),
        method_source = maps:get(method_source, ClassInfo, State#class_state.method_source),
        method_signatures = maps:get(
            method_signatures, ClassInfo, State#class_state.method_signatures
        ),
        class_method_signatures = maps:get(
            class_method_signatures, ClassInfo, State#class_state.class_method_signatures
        ),
        method_return_types = maps:merge(
            maps:get(method_return_types, ClassInfo, State#class_state.method_return_types),
            NewMethodReturnTypes
        ),
        class_method_return_types = maps:merge(
            maps:get(
                class_method_return_types, ClassInfo, State#class_state.class_method_return_types
            ),
            NewClassMethodReturnTypes
        ),
        is_constructible = maps:get(is_constructible, ClassInfo, undefined),
        doc = maps:get(doc, ClassInfo, State#class_state.doc),
        method_docs = maps:get(method_docs, ClassInfo, State#class_state.method_docs),
        class_method_docs = maps:get(
            class_method_docs, ClassInfo, State#class_state.class_method_docs
        ),
        is_internal = NewIsInternal
    }.
