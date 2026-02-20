%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-class gen_server for Beamtalk class objects.
%%%
%%% **DDD Context:** Object System
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
    is_constructible/1,
    add_before/3,
    add_after/3,
    class_name/1,
    module_name/1,
    create_subclass/3,
    class_send/3,
    set_class_var/3,
    update_class/2
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
    instance_variables = [] :: [atom()],
    class_variables = #{} :: map(),
    method_source = #{} :: #{selector() => binary()},
    before_methods = #{} :: #{selector() => [fun()]},
    after_methods = #{} :: #{selector() => [fun()]},
    dynamic_methods = #{} :: #{selector() => fun()}
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
        Pid -> gen_server:call(Pid, {set_class_var, Name, Value})
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

%% @doc Get the superclass name.
-spec superclass(pid()) -> class_name() | none.
superclass(ClassPid) ->
    gen_server:call(ClassPid, superclass).

%% @doc Get the class name.
-spec class_name(pid()) -> class_name().
class_name(ClassPid) ->
    gen_server:call(ClassPid, class_name).

%% @doc Get the module name.
-spec module_name(pid()) -> atom().
module_name(ClassPid) ->
    gen_server:call(ClassPid, module_name).

%% @doc Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).
%%
%% Delegates to beamtalk_class_dispatch (BT-704).
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(ClassPid, Selector, Args) ->
    beamtalk_class_dispatch:class_send(ClassPid, Selector, Args).

%% @doc Get a compiled method object.
%% Delegates to beamtalk_method_resolver.
-spec method(pid() | class_name() | tuple(), selector()) -> compiled_method() | nil.
method(ClassRef, Selector) ->
    beamtalk_method_resolver:resolve(ClassRef, Selector).

%% @doc Check if a class has a method (does not walk hierarchy).
%% First checks class metadata, then falls back to module's has_method/1.
-spec has_method(pid(), selector()) -> boolean().
has_method(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            case module_name(ClassPid) of
                undefined -> false;
                ModuleName ->
                    case erlang:function_exported(ModuleName, has_method, 1) of
                        true ->
                            try ModuleName:has_method(Selector)
                            catch _:_ -> false
                            end;
                        false -> false
                    end
            end;
        _MethodInfo -> true
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

%% @doc Add a before daemon (Flavors pattern).
-spec add_before(pid(), selector(), fun()) -> ok.
add_before(ClassPid, Selector, Fun) ->
    gen_server:call(ClassPid, {add_before, Selector, Fun}).

%% @doc Add an after daemon (Flavors pattern).
-spec add_after(pid(), selector(), fun()) -> ok.
add_after(ClassPid, Selector, Fun) ->
    gen_server:call(ClassPid, {add_after, Selector, Fun}).

%% @doc Create a dynamic subclass at runtime.
%% Delegates to beamtalk_class_instantiation (BT-704).
-spec create_subclass(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
create_subclass(SuperclassName, ClassName, ClassSpec) ->
    beamtalk_class_instantiation:create_subclass(SuperclassName, ClassName, ClassSpec).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ClassName, ClassInfo}) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    ok = pg:join(beamtalk_classes, self()),
    
    Superclass = maps:get(superclass, ClassInfo, none),
    Module = maps:get(module, ClassInfo, ClassName),
    IsAbstract = maps:get(is_abstract, ClassInfo, false),
    InstanceMethods = maps:get(instance_methods, ClassInfo, #{}),
    ClassMethods = maps:get(class_methods, ClassInfo, #{}),
    
    ets:insert(beamtalk_class_hierarchy, {ClassName, Superclass}),

    %% BT-474: is_constructible starts undefined — computed lazily on first new call
    %% ADR 0032 Phase 1: No flattened method tables — dispatch walks the chain directly.
    State = #class_state{
        name = ClassName,
        module = Module,
        superclass = Superclass,
        is_sealed = maps:get(is_sealed, ClassInfo, false),
        is_abstract = IsAbstract,
        instance_methods = InstanceMethods,
        class_methods = ClassMethods,
        instance_variables = maps:get(instance_variables, ClassInfo, []),
        class_variables = maps:get(class_variables, ClassInfo, #{}),
        method_source = maps:get(method_source, ClassInfo, #{}),
        before_methods = maps:get(before_methods, ClassInfo, #{}),
        after_methods = maps:get(after_methods, ClassInfo, #{}),
        dynamic_methods = maps:get(dynamic_methods, ClassInfo, #{})
    },
    {ok, State}.

handle_call({spawn, Args}, _From, #class_state{
    name = ClassName,
    module = Module,
    is_abstract = IsAbstract
} = State) ->
    Result = beamtalk_class_instantiation:handle_spawn(Args, ClassName, Module, IsAbstract),
    {reply, Result, State};

handle_call({new, Args}, _From, #class_state{name = ClassName, is_abstract = true} = State) ->
    Selector = case Args of [] -> 'new'; _ -> 'new:' end,
    {reply, {error, beamtalk_class_instantiation:abstract_class_error(ClassName, Selector)}, State};

handle_call({new, Args}, _From, #class_state{
    name = ClassName,
    module = Module,
    dynamic_methods = DynamicMethods,
    instance_variables = InstanceVars,
    is_constructible = IsConstructible0
} = State) ->
    case beamtalk_class_instantiation:handle_new(
            Args, ClassName, Module, DynamicMethods, InstanceVars, IsConstructible0,
            self()) of
        {ok, Result, IsConstructible} ->
            {reply, {ok, Result}, State#class_state{is_constructible = IsConstructible}};
        {error, Error, IsConstructible} ->
            {reply, {error, Error}, State#class_state{is_constructible = IsConstructible}}
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

handle_call({method, Selector}, _From, #class_state{
    instance_methods = Methods,
    method_source = Source
} = State) ->
    case maps:find(Selector, Methods) of
        {ok, MethodInfo} ->
            Src = maps:get(Selector, Source, <<"">>),
            MethodObj = #{
                '$beamtalk_class' => 'CompiledMethod',
                '__selector__' => Selector,
                '__source__' => Src,
                '__method_info__' => MethodInfo
            },
            {reply, MethodObj, State};
        error ->
            {reply, nil, State}
    end;

handle_call({put_method, Selector, Fun, Source}, _From, State) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    MethodInfo = #{block => Fun, arity => Arity},
    NewMethods = maps:put(Selector, MethodInfo, State#class_state.instance_methods),
    NewSource = maps:put(Selector, Source, State#class_state.method_source),
    %% ADR 0032 Phase 1: No flattened table to rebuild or invalidate.
    %% Dispatch finds the new method via chain walk on the next call.
    NewState = State#class_state{instance_methods = NewMethods, method_source = NewSource},
    notify_instances(State#class_state.name, NewMethods),
    {reply, ok, NewState};

%% BT-572: Update class metadata after redefinition (hot reload).
%% BT-737: Emits a collision warning when a class is redefined from a different module.
%% Returns {ok, InstanceVars} with the new instance variable list for state migration.
%% ADR 0032 Phase 1: No flattened tables to rebuild or invalidate.
handle_call({update_class, ClassInfo}, _From, #class_state{name = ClassName} = State) ->
    %% BT-737: Detect cross-package class redefinition (different BEAM module).
    %% Same-module reload (hot reload of the same file) does NOT produce a warning.
    OldModule = State#class_state.module,
    NewModule = maps:get(module, ClassInfo, OldModule),
    case OldModule =:= NewModule of
        false ->
            ?LOG_WARNING("Class redefined from different module", #{
                class => ClassName,
                old_module => OldModule,
                new_module => NewModule
            }),
            beamtalk_class_registry:record_class_collision_warning(
                ClassName, OldModule, NewModule);
        true ->
            ok
    end,
    NewInstanceMethods = maps:get(instance_methods, ClassInfo, State#class_state.instance_methods),
    NewClassMethods = maps:get(class_methods, ClassInfo, State#class_state.class_methods),
    NewIVars = maps:get(instance_variables, ClassInfo, State#class_state.instance_variables),
    NewState = State#class_state{
        module = maps:get(module, ClassInfo, State#class_state.module),
        instance_methods = NewInstanceMethods, class_methods = NewClassMethods,
        instance_variables = NewIVars,
        method_source = maps:get(method_source, ClassInfo, State#class_state.method_source),
        is_constructible = undefined},
    {reply, {ok, NewIVars}, NewState};

handle_call(instance_variables, _From, #class_state{instance_variables = IVars} = State) ->
    {reply, IVars, State};

handle_call(is_sealed, _From, #class_state{is_sealed = Sealed} = State) ->
    {reply, Sealed, State};

handle_call(is_abstract, _From, #class_state{is_abstract = Abstract} = State) ->
    {reply, Abstract, State};

handle_call(is_constructible, _From, #class_state{
    is_constructible = IsConstructible0,
    module = Module,
    is_abstract = IsAbstract
} = State) ->
    IsConstructible = beamtalk_class_instantiation:ensure_is_constructible(
        IsConstructible0, Module, IsAbstract),
    {reply, IsConstructible, State#class_state{is_constructible = IsConstructible}};

handle_call({add_before, Selector, Fun}, _From, State) ->
    Befores = maps:get(Selector, State#class_state.before_methods, []),
    NewBefores = maps:put(Selector, [Fun | Befores], State#class_state.before_methods),
    {reply, ok, State#class_state{before_methods = NewBefores}};

handle_call({add_after, Selector, Fun}, _From, State) ->
    Afters = maps:get(Selector, State#class_state.after_methods, []),
    NewAfters = maps:put(Selector, Afters ++ [Fun], State#class_state.after_methods),
    {reply, ok, State#class_state{after_methods = NewAfters}};

%% ADR 0032 Phase 1: Returns local class_methods for chain walk queries.
handle_call(get_local_class_methods, _From, #class_state{class_methods = ClassMethods} = State) ->
    {reply, ClassMethods, State};

%% ADR 0032 Phase 1: Returns local instance_methods for chain walk queries.
handle_call(get_instance_methods, _From, #class_state{instance_methods = InstanceMethods} = State) ->
    {reply, {ok, InstanceMethods}, State};

%% BT-411/BT-412/BT-440: Class method dispatch.
%% Delegates to beamtalk_class_dispatch (BT-704).
%% ADR 0032 Phase 1: Passes local class_methods (no flattened table).
%% beamtalk_class_dispatch walks the superclass chain if not found locally.
handle_call({class_method_call, Selector, Args}, From,
            #class_state{class_methods = ClassMethods,
                         instance_methods = InstanceMethods,
                         name = ClassName, module = Module,
                         class_variables = ClassVars} = State) ->
    case beamtalk_class_dispatch:handle_class_method_call(
            Selector, Args, ClassName, Module, ClassMethods, ClassVars) of
        {reply, Result, NewClassVars} ->
            {reply, Result, State#class_state{class_variables = NewClassVars}};
        test_spawn ->
            beamtalk_test_case:spawn_test_execution(
                Selector, Args, ClassName, Module, InstanceMethods, From),
            {noreply, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({initialize, _Args}, _From, #class_state{} = State) ->
    {reply, {ok, nil}, State};

handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};

handle_call({get_class_var, Name}, _From, #class_state{class_variables = ClassVars} = State) ->
    {reply, maps:get(Name, ClassVars, nil), State};

handle_call({set_class_var, Name, Value}, _From, #class_state{class_variables = ClassVars} = State) ->
    {reply, Value, State#class_state{class_variables = maps:put(Name, Value, ClassVars)}}.

%% ADR 0032 Phase 1: Passes instance_methods (local only) instead of flattened table.
handle_cast(Msg, #class_state{instance_methods = InstanceMethods,
                              name = ClassName, superclass = Superclass,
                              module = Module} = State) ->
    beamtalk_class_dispatch:handle_async_dispatch(Msg, ClassName, InstanceMethods, Superclass, Module),
    {noreply, State}.

%% ADR 0032 Phase 1: {rebuild_flattened, _} messages are no longer sent
%% (invalidate_subclass_flattened_tables removed from beamtalk_class_registry).
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% ADR 0032 Phase 1: No flattened tables to rebuild after hot reload.
code_change(OldVsn, State, Extra) ->
    {ok, NewState} = beamtalk_hot_reload:code_change(OldVsn, State, Extra),
    {ok, NewState#class_state{is_constructible = undefined}}.

%%====================================================================
%% Internal functions
%%====================================================================

notify_instances(_ClassName, _NewMethods) ->
    ok.
