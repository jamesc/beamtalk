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
%% Flattened method table management by beamtalk_class_hierarchy.
%% Instance creation (new/spawn) by beamtalk_class_instantiation.
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
    dynamic_methods = #{} :: #{selector() => fun()},  % For dynamic classes: actual closures
    flattened_methods = #{} :: #{selector() => {class_name(), method_info()}},  % ADR 0006 Phase 2: cached method table including inherited
    flattened_class_methods = #{} :: #{selector() => {class_name(), method_info()}}  % BT-411: cached class method table including inherited
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
%%
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
    FlattenedMethods = beamtalk_class_hierarchy:build_flattened_methods(ClassName, Superclass, InstanceMethods),
    FlattenedClassMethods = beamtalk_class_hierarchy:build_flattened_methods(ClassName, Superclass, ClassMethods, get_flattened_class_methods),
    
    %% BT-474: is_constructible starts undefined — computed lazily on first new call
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
        dynamic_methods = maps:get(dynamic_methods, ClassInfo, #{}),
        flattened_methods = FlattenedMethods,
        flattened_class_methods = FlattenedClassMethods
    },
    %% Notify subclasses to rebuild flattened tables (out-of-order registration)
    beamtalk_class_registry:invalidate_subclass_flattened_tables(ClassName),
    {ok, State}.

%% BT-246: Actor spawn via dynamic class dispatch.
%% Routes spawn/spawnWith: through class_send → {spawn, Args} protocol.
%% Separate from {new, Args} to avoid confusion with value type new.
handle_call({spawn, Args}, _From, #class_state{
    name = ClassName,
    module = Module,
    is_abstract = IsAbstract
} = State) ->
    Result = beamtalk_class_instantiation:handle_spawn(Args, ClassName, Module, IsAbstract),
    {reply, Result, State};

handle_call({new, Args}, _From, #class_state{
    name = ClassName,
    is_abstract = true
} = State) ->
    %% BT-105: Abstract classes cannot be instantiated
    Selector = case Args of
        [] -> 'new';
        _ -> 'new:'
    end,
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

handle_call(methods, _From, #class_state{flattened_methods = Flattened} = State) ->
    %% ADR 0006 Phase 2: Return all methods (local + inherited) from flattened table
    {reply, maps:keys(Flattened), State};

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
    NewFlattened = beamtalk_class_hierarchy:build_flattened_methods(
        State#class_state.name, State#class_state.superclass, NewMethods),
    NewState = State#class_state{
        instance_methods = NewMethods, method_source = NewSource,
        flattened_methods = NewFlattened},
    notify_instances(State#class_state.name, NewMethods),
    beamtalk_class_registry:invalidate_subclass_flattened_tables(State#class_state.name),
    {reply, ok, NewState};

%% BT-572: Update class metadata after redefinition (hot reload).
%% Returns {ok, InstanceVars} with the new instance variable list for state migration.
handle_call({update_class, ClassInfo}, _From, #class_state{name = ClassName} = State) ->
    NewInstanceMethods = maps:get(instance_methods, ClassInfo, State#class_state.instance_methods),
    NewClassMethods = maps:get(class_methods, ClassInfo, State#class_state.class_methods),
    NewFlattened = beamtalk_class_hierarchy:build_flattened_methods(
        ClassName, State#class_state.superclass, NewInstanceMethods),
    NewFlattenedClass = beamtalk_class_hierarchy:build_flattened_methods(
        ClassName, State#class_state.superclass, NewClassMethods, get_flattened_class_methods),
    NewIVars = maps:get(instance_variables, ClassInfo, State#class_state.instance_variables),
    NewState = State#class_state{
        module = maps:get(module, ClassInfo, State#class_state.module),
        instance_methods = NewInstanceMethods, class_methods = NewClassMethods,
        instance_variables = NewIVars,
        method_source = maps:get(method_source, ClassInfo, State#class_state.method_source),
        flattened_methods = NewFlattened, flattened_class_methods = NewFlattenedClass,
        is_constructible = undefined},
    beamtalk_class_registry:invalidate_subclass_flattened_tables(ClassName),
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

%% ADR 0006 Phase 2: Query for flattened methods (used by build_flattened_methods)
handle_call(get_flattened_methods, _From, #class_state{flattened_methods = Flattened} = State) ->
    {reply, {ok, Flattened}, State};

%% ADR 0006 Phase 2: Fast single-selector lookup in flattened table.
%% Returns just the result to avoid copying the entire map to the caller.
handle_call({lookup_flattened, Selector}, _From, #class_state{flattened_methods = Flattened} = State) ->
    Result = case maps:find(Selector, Flattened) of
        {ok, {DefiningClass, MethodInfo}} -> {ok, DefiningClass, MethodInfo};
        error -> not_found
    end,
    {reply, Result, State};

%% BT-411/BT-412/BT-440: Class method dispatch.
%% Delegates to beamtalk_class_dispatch (BT-704).
handle_call({class_method_call, Selector, Args}, From,
            #class_state{flattened_class_methods = FlatClassMethods,
                         flattened_methods = FlatMethods,
                         name = ClassName, module = Module,
                         class_variables = ClassVars} = State) ->
    case beamtalk_class_dispatch:handle_class_method_call(
            Selector, Args, ClassName, Module, FlatClassMethods, ClassVars) of
        {reply, Result, NewClassVars} ->
            {reply, Result, State#class_state{class_variables = NewClassVars}};
        {test_spawn, DefiningModule} ->
            beamtalk_test_case:spawn_test_execution(
                Selector, Args, ClassName, DefiningModule, FlatMethods, From),
            {noreply, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

%% BT-411: Actor initialize protocol — should not reach class process.
handle_call({initialize, _Args}, _From, #class_state{} = State) ->
    {reply, {ok, nil}, State};

%% BT-411: Query for flattened class methods (for inheritance)
handle_call(get_flattened_class_methods, _From, #class_state{flattened_class_methods = Flattened} = State) ->
    {reply, {ok, Flattened}, State};

%% Internal query used by reflective method lookup to get behavior module
handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};

%% BT-412: Get a class variable value
handle_call({get_class_var, Name}, _From,
            #class_state{class_variables = ClassVars} = State) ->
    Value = maps:get(Name, ClassVars, nil),
    {reply, Value, State};

%% BT-412: Set a class variable value
handle_call({set_class_var, Name, Value}, _From,
            #class_state{class_variables = ClassVars} = State) ->
    NewClassVars = maps:put(Name, Value, ClassVars),
    NewState = State#class_state{class_variables = NewClassVars},
    {reply, Value, NewState}.

handle_cast(Msg, #class_state{flattened_methods = Flattened,
                              name = ClassName, superclass = Superclass,
                              module = Module} = State) ->
    beamtalk_class_dispatch:handle_async_dispatch(Msg, ClassName, Flattened, Superclass, Module),
    {noreply, State}.

handle_info({rebuild_flattened, ChangedClass}, #class_state{superclass = Superclass} = State) ->
    try beamtalk_class_registry:inherits_from(Superclass, ChangedClass) of
        true ->
            {noreply, rebuild_all_flattened_tables(State)};
        false ->
            {noreply, State}
    catch
        _:_ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, NewState} = beamtalk_hot_reload:code_change(OldVsn, State, Extra),
    FinalState = rebuild_all_flattened_tables(NewState),
    FinalState2 = FinalState#class_state{is_constructible = undefined},
    beamtalk_class_registry:invalidate_subclass_flattened_tables(FinalState2#class_state.name),
    {ok, FinalState2}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Rebuild both flattened tables. Delegates to beamtalk_class_hierarchy.
-spec rebuild_all_flattened_tables(#class_state{}) -> #class_state{}.
rebuild_all_flattened_tables(#class_state{
    name = Name,
    superclass = Superclass,
    instance_methods = InstanceMethods,
    class_methods = ClassMethods,
    flattened_methods = OldFlattened
} = State) ->
    {NewFlattened, NewFlattenedClass} = beamtalk_class_hierarchy:rebuild_all_flattened_tables(
        Name, Superclass, InstanceMethods, ClassMethods, OldFlattened),
    State#class_state{
        flattened_methods = NewFlattened,
        flattened_class_methods = NewFlattenedClass
    }.

notify_instances(_ClassName, _NewMethods) ->
    ok.
