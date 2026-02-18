%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-class gen_server for Beamtalk class objects.
%%%
%%% **DDD Context:** Object System
%%
%% Each class in Beamtalk is a first-class object - a process that holds
%% metadata and responds to messages. This follows Smalltalk's philosophy
%% where classes are messageable objects supporting introspection and
%% metaprogramming.
%%
%% BEAM processes are cheap (~2KB each). Per-class processes give us true
%% Smalltalk semantics:
%% - Classes can receive messages: `Counter class methods`
%% - Classes can be stored in variables and passed as arguments
%% - Classes can be hot-patched at runtime
%% - Each class has isolated state
%%
%% ## Name Registration
%%
%% Classes register with Erlang's built-in registry:
%% ```
%% register(beamtalk_class_Counter, self())
%% ```
%%
%% Lookup is simple:
%% ```
%% whereis(beamtalk_class_Counter)  %=> <pid>
%% ```
%%
%% ## Process Group Enumeration
%%
%% All class processes join the OTP `pg` group `beamtalk_classes`:
%% ```
%% pg:get_members(beamtalk_classes)  %=> [<pid>, <pid>, ...]
%% ```
%%
%% This replaces the old global registry (`beamtalk_classes.erl`).
%%
%% ## ClassInfo Structure
%%
%% ```
%% #{
%%   name => atom(),                           % class name (e.g., 'Counter')
%%   module => atom(),                         % compiled BEAM module
%%   superclass => atom() | none,              % parent class name
%%   instance_methods => #{selector() => method_info()},
%%   class_methods => #{selector() => method_info()},
%%   instance_variables => [atom()],
%%   class_variables => map(),
%%   method_source => #{selector() => binary()},
%%   before_methods => #{selector() => [fun()]},  % Flavors-style
%%   after_methods => #{selector() => [fun()]}    % Flavors-style
%% }
%% ```
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
%%
%% Uses gen_server:start (no link) so the class process survives after
%% the on_load caller exits. Class processes are long-lived singletons
%% that must persist independently of whoever loaded the module.
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
%%
%% Called when a class is reloaded (via :load or inline REPL).
%% Updates instance methods, class methods, instance variables, method source,
%% and rebuilds flattened method tables. Returns the list of instance variable
%% names from the new definition (for state migration during hot reload).
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
%%
%% Delegates to beamtalk_method_resolver for the actual resolution logic.
%% Accepts a class process pid, a class name atom, or a class object tuple.
%% Returns a CompiledMethod map or nil if the method is not found.
-spec method(pid() | class_name() | tuple(), selector()) -> compiled_method() | nil.
method(ClassRef, Selector) ->
    beamtalk_method_resolver:resolve(ClassRef, Selector).

%% @doc Check if a class has a method (does not walk hierarchy).
%%
%% Returns true if the method is defined in this class, false otherwise.
%% This is used by beamtalk_dispatch for hierarchy walking.
%%
%% ## Implementation
%%
%% First checks the class metadata (instance_methods map), then falls back
%% to the module's has_method/1 function if available. This handles both
%% explicitly registered methods and inlined reflection methods.
-spec has_method(pid(), selector()) -> boolean().
has_method(ClassPid, Selector) ->
    %% First check the class metadata
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            %% Not in metadata - check if module has_method/1 function exists
            case module_name(ClassPid) of
                undefined ->
                    false;
                ModuleName ->
                    %% Check if module exports has_method/1
                    case erlang:function_exported(ModuleName, has_method, 1) of
                        true ->
                            %% Call the module's has_method/1
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
%%
%% BT-474: Domain query replacing the try-new/0 probe. Returns false for
%% actors (use spawn) and non-instantiable primitives (Integer, String, etc.).
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
%% This is the Phase 1 implementation using interpreter-based dynamic classes.
%% Methods are stored as closures and dispatch via apply/2.
%%
%% Arguments:
%% - SuperclassName: Atom name of the superclass (e.g., 'Actor', 'Object')
%% - ClassName: Atom name of the new class (e.g., 'MyClass')
%% - ClassSpec: Map containing:
%%   - instance_variables: List of field names [atom()]
%%   - instance_methods: Map of {Selector => Fun} where Fun is arity 3:
%%                       fun(Self, Args, State) -> {reply, Result, NewState}
%%
%% Returns: {ok, ClassPid} | {error, Reason}
%%
%% Example:
%% ```erlang
%% beamtalk_class:create_subclass('Actor', 'MyClass', #{
%%     instance_variables => [count],
%%     instance_methods => #{
%%         increment => fun(Self, [], State) ->
%%             Count = maps:get(count, State, 0),
%%             {reply, Count + 1, maps:put(count, Count + 1, State)}
%%         end
%%     }
%% })
%% ```
-spec create_subclass(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
create_subclass(SuperclassName, ClassName, ClassSpec) ->
    %% Verify superclass exists
    case beamtalk_class_registry:whereis_class(SuperclassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, SuperclassName),
            Error = beamtalk_error:with_hint(Error0, <<"Superclass must be registered before creating subclass">>),
            {error, Error};
        _SuperclassPid ->
            %% Extract fields from ClassSpec
            InstanceVars = maps:get(instance_variables, ClassSpec, []),
            InstanceMethods = maps:get(instance_methods, ClassSpec, #{}),
            
            %% Validate and convert methods
            try beamtalk_class_instantiation:convert_methods_to_info(InstanceMethods) of
                MethodInfo ->
                    %% Build ClassInfo compatible with beamtalk_class
                    ClassInfo = #{
                        name => ClassName,
                        module => beamtalk_dynamic_object,  % All dynamic classes use this behavior
                        superclass => SuperclassName,
                        instance_methods => MethodInfo,
                        instance_variables => InstanceVars,
                        class_methods => #{},
                        %% Store the actual closures in a custom field
                        dynamic_methods => InstanceMethods
                    },
                    
                    %% Register as a class process
                    case start_link(ClassName, ClassInfo) of
                        {ok, ClassPid} ->
                            {ok, ClassPid};
                        {error, {already_started, _Pid}} ->
                            %% Class already exists - return error for consistency
                            Error0 = beamtalk_error:new(class_already_exists, ClassName),
                            {error, Error0};
                        Error ->
                            Error
                    end
            catch
                error:ErrorReason ->
                    {error, ErrorReason}
            end
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ClassName, ClassInfo}) ->
    %% Ensure pg is started (needed for class registry)
    beamtalk_class_registry:ensure_pg_started(),
    
    %% BT-510: Ensure hierarchy ETS table exists and register this class
    beamtalk_class_registry:ensure_hierarchy_table(),
    
    %% Name registration is handled by gen_server:start_link({local, Name}, ...)
    %% Join pg group for all_classes enumeration
    ok = pg:join(beamtalk_classes, self()),
    
    %% Extract class info
    Superclass = maps:get(superclass, ClassInfo, none),
    Module = maps:get(module, ClassInfo, ClassName),
    IsAbstract = maps:get(is_abstract, ClassInfo, false),
    InstanceMethods = maps:get(instance_methods, ClassInfo, #{}),
    ClassMethods = maps:get(class_methods, ClassInfo, #{}),
    
    %% BT-510: Record hierarchy in ETS (immediately visible to all processes)
    ets:insert(beamtalk_class_hierarchy, {ClassName, Superclass}),
    
    %% Build flattened method tables (ADR 0006 Phase 2)
    FlattenedMethods = beamtalk_class_hierarchy:build_flattened_methods(ClassName, Superclass, InstanceMethods),
    FlattenedClassMethods = beamtalk_class_hierarchy:build_flattened_methods(ClassName, Superclass, ClassMethods, get_flattened_class_methods),
    
    %% Build state
    %% BT-474: is_constructible starts as undefined — computed lazily on first
    %% {new, Args} call. Can't probe Module:new() during on_load init because
    %% the module isn't fully available yet.
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
    %% ADR 0006 Phase 2: Notify existing subclasses to rebuild their flattened
    %% tables. Handles out-of-order registration (e.g., Counter registered
    %% before Actor) — subclasses that had incomplete tables now pick up our methods.
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
    
    %% ADR 0006 Phase 2: Rebuild flattened methods when method added/changed
    NewFlattened = beamtalk_class_hierarchy:build_flattened_methods(
        State#class_state.name,
        State#class_state.superclass,
        NewMethods
    ),
    
    NewState = State#class_state{
        instance_methods = NewMethods,
        method_source = NewSource,
        flattened_methods = NewFlattened
    },
    
    %% Notify running instances to pick up new method table
    notify_instances(State#class_state.name, NewMethods),
    
    %% ADR 0006 Phase 2: Notify subclasses to rebuild their flattened tables
    beamtalk_class_registry:invalidate_subclass_flattened_tables(State#class_state.name),
    
    {reply, ok, NewState};

%% BT-572: Update class metadata after redefinition (hot reload).
%% Returns {ok, InstanceVars} with the new instance variable list for state migration.
handle_call({update_class, ClassInfo}, _From, #class_state{name = ClassName} = State) ->
    NewInstanceMethods = maps:get(instance_methods, ClassInfo, State#class_state.instance_methods),
    NewClassMethods = maps:get(class_methods, ClassInfo, State#class_state.class_methods),
    NewInstanceVariables = maps:get(instance_variables, ClassInfo, State#class_state.instance_variables),
    NewMethodSource = maps:get(method_source, ClassInfo, State#class_state.method_source),
    NewModule = maps:get(module, ClassInfo, State#class_state.module),

    %% Rebuild flattened method tables with new methods
    NewFlattened = beamtalk_class_hierarchy:build_flattened_methods(
        ClassName, State#class_state.superclass, NewInstanceMethods),
    NewFlattenedClass = beamtalk_class_hierarchy:build_flattened_methods(
        ClassName, State#class_state.superclass, NewClassMethods, get_flattened_class_methods),

    NewState = State#class_state{
        module = NewModule,
        instance_methods = NewInstanceMethods,
        class_methods = NewClassMethods,
        instance_variables = NewInstanceVariables,
        method_source = NewMethodSource,
        flattened_methods = NewFlattened,
        flattened_class_methods = NewFlattenedClass,
        is_constructible = undefined
    },

    %% Notify subclasses to rebuild their flattened tables
    beamtalk_class_registry:invalidate_subclass_flattened_tables(ClassName),

    {reply, {ok, NewInstanceVariables}, NewState};

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
            spawn(fun() ->
                try
                    Result = beamtalk_test_case:execute_tests(
                        Selector, Args, ClassName, DefiningModule, FlatMethods),
                    gen_server:reply(From, {ok, Result})
                catch
                    C:E ->
                        ?LOG_ERROR("Test execution ~p:~p failed: ~p:~p",
                                     [ClassName, Selector, C, E]),
                        gen_server:reply(From, {error, E})
                end
            end),
            {noreply, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

%% BT-411: Actor initialize protocol.
%% Called as first message after spawn to run user-defined initialization.
handle_call({initialize, _Args}, _From, #class_state{} = State) ->
    %% Initialize is dispatched as an instance method on the just-spawned actor,
    %% not on the class process. This handler should not be reached — initialize
    %% is called directly on the actor pid by spawn codegen.
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

handle_cast(Msg, #class_state{flattened_methods = Flattened} = State) ->
    %% Handle async message dispatch (Future protocol)
    case Msg of
        {Selector, Args, FuturePid} ->
            %% Async message with Future - dispatch and resolve/reject the future
            %% Match the same selectors as handle_call
            case {Selector, Args} of
                {methods, []} ->
                    %% ADR 0006 Phase 2: Return all methods (local + inherited)
                    Result = maps:keys(Flattened),
                    FuturePid ! {resolve, Result},
                    {noreply, State};
                {superclass, []} ->
                    Result = State#class_state.superclass,
                    FuturePid ! {resolve, Result},
                    {noreply, State};
                {class_name, []} ->
                    Result = State#class_state.name,
                    FuturePid ! {resolve, Result},
                    {noreply, State};
                {module_name, []} ->
                    Result = State#class_state.module,
                    FuturePid ! {resolve, Result},
                    {noreply, State};
                {{method, _MethodSelector}, _} ->
                    %% For method lookup, just reject - this is complex
                    Error = beamtalk_error:new(type_error, State#class_state.name),
                    FuturePid ! {reject, Error},
                    {noreply, State};
                _ ->
                    %% Unknown message
                    Error = beamtalk_error:new(does_not_understand, State#class_state.name, Selector),
                    FuturePid ! {reject, Error},
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end.

handle_info({rebuild_flattened, ChangedClass}, #class_state{
    superclass = Superclass
} = State) ->
    %% ADR 0006 Phase 2: A parent class changed — rebuild if we inherit from it.
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
    %% First let hot_reload modify state (may update instance_methods)
    {ok, NewState} = beamtalk_hot_reload:code_change(OldVsn, State, Extra),
    %% ADR 0006 Phase 2: Rebuild flattened methods after hot reload
    FinalState = rebuild_all_flattened_tables(NewState),
    %% BT-474: Reset is_constructible cache — module exports may have changed
    FinalState2 = FinalState#class_state{is_constructible = undefined},
    %% Invalidate subclass tables in case hot_reload modified our methods
    beamtalk_class_registry:invalidate_subclass_flattened_tables(FinalState2#class_state.name),
    {ok, FinalState2}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc Rebuild both instance and class flattened method tables from State.
%%
%% Returns updated State with new flattened_methods and flattened_class_methods.
%% Delegates to beamtalk_class_hierarchy (BT-704).
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
    %% TODO: Once beamtalk_object_instances is updated to work with per-class processes,
    %% this will broadcast method table updates to running instances.
    %% For now, this is a no-op.
    ok.
