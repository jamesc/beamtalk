%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-class gen_server for Beamtalk class objects.
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
    super_dispatch/3,
    class_name/1,
    module_name/1,
    create_subclass/3,
    class_send/3,
    set_class_var/3
]).

%% Deprecated re-exports — callers should migrate to beamtalk_class_registry (BT-576)
-export([
    whereis_class/1,
    all_classes/0,
    is_class_object/1,
    is_class_name/1,
    class_display_name/1,
    class_object_tag/1,
    inherits_from/2,
    ensure_hierarchy_table/0
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

%% @doc Look up a class by name.
%% @deprecated Use {@link beamtalk_class_registry:whereis_class/1} instead (BT-576).
-spec whereis_class(class_name()) -> pid() | undefined.
whereis_class(ClassName) ->
    beamtalk_class_registry:whereis_class(ClassName).

%% @doc Get all class processes.
%% @deprecated Use {@link beamtalk_class_registry:all_classes/0} instead (BT-576).
-spec all_classes() -> [pid()].
all_classes() ->
    beamtalk_class_registry:all_classes().

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

%% @doc Check if a value is a class object (BT-246).
%% @deprecated Use {@link beamtalk_class_registry:is_class_object/1} instead (BT-576).
-spec is_class_object(term()) -> boolean().
is_class_object(Value) ->
    beamtalk_class_registry:is_class_object(Value).

%% @doc Check if an atom class name represents a class object (ends with " class").
%% @deprecated Use {@link beamtalk_class_registry:is_class_name/1} instead (BT-576).
-spec is_class_name(atom()) -> boolean().
is_class_name(ClassName) ->
    beamtalk_class_registry:is_class_name(ClassName).

%% @doc Strip " class" suffix from a class object name to get the display name.
%% @deprecated Use {@link beamtalk_class_registry:class_display_name/1} instead (BT-576).
-spec class_display_name(atom()) -> binary().
class_display_name(ClassName) ->
    beamtalk_class_registry:class_display_name(ClassName).

%% @doc Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).
%%
%% Dispatches messages to the class gen_server, translating the Beamtalk
%% message protocol ({Selector, Args}) to the class process message format.
%% Unwraps {ok, Value} / {error, Error} results for seamless integration.
-spec class_send(pid(), atom(), list()) -> term().
class_send(ClassPid, 'new', []) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, []}));
class_send(ClassPid, 'new:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {new, [Map]}));
class_send(ClassPid, spawn, []) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, []}));
class_send(ClassPid, 'spawnWith:', [Map]) ->
    unwrap_class_call(gen_server:call(ClassPid, {spawn, [Map]}));
class_send(ClassPid, methods, []) ->
    gen_server:call(ClassPid, methods);
class_send(ClassPid, superclass, []) ->
    case gen_server:call(ClassPid, superclass) of
        none -> nil;  % Beamtalk nil, not Erlang 'none'
        Super -> Super
    end;
class_send(ClassPid, class_name, []) ->
    gen_server:call(ClassPid, class_name);
class_send(ClassPid, module_name, []) ->
    gen_server:call(ClassPid, module_name);
class_send(ClassPid, 'printString', []) ->
    %% BT-477: Class objects return their display name as a string.
    %% e.g., Integer printString → <<"Integer">>, Counter printString → <<"Counter">>
    %% Enables Object >> printString => 'a ' ++ self class printString
    ClassName = gen_server:call(ClassPid, class_name),
    atom_to_binary(ClassName, utf8);
class_send(_ClassPid, class, []) ->
    %% BT-412: Metaclass terminal — returns 'Metaclass' sentinel atom.
    %% The metaclass tower terminates here (no infinite regression).
    'Metaclass';
class_send(ClassPid, Selector, Args) ->
    %% BT-411: Try user-defined class methods before raising does_not_understand
    %% BT-440: Test execution may take a long time; use longer timeout.
    Timeout = case is_test_execution_selector(Selector) of
        true -> 300000;  % 5 minutes for test suites
        false -> 5000    % default gen_server timeout
    end,
    case gen_server:call(ClassPid, {class_method_call, Selector, Args}, Timeout) of
        {ok, Result} -> Result;
        {error, not_found} ->
            ClassName = gen_server:call(ClassPid, class_name),
            Error0 = beamtalk_error:new(does_not_understand, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(Error1, <<"Class does not understand this message">>),
            beamtalk_error:raise(Error2);
        Other -> unwrap_class_call(Other)
    end.

%% @doc Convert a class name atom to a class object tag (BT-246).
%% @deprecated Use {@link beamtalk_class_registry:class_object_tag/1} instead (BT-576).
-spec class_object_tag(atom()) -> atom().
class_object_tag(ClassName) ->
    beamtalk_class_registry:class_object_tag(ClassName).

%% @doc Check if a class inherits from a given ancestor.
%% @deprecated Use {@link beamtalk_class_registry:inherits_from/2} instead (BT-576).
-spec inherits_from(class_name() | none, class_name()) -> boolean().
inherits_from(ClassName, Ancestor) ->
    beamtalk_class_registry:inherits_from(ClassName, Ancestor).

%% @doc Ensure the class hierarchy ETS table exists.
%% @deprecated Use {@link beamtalk_class_registry:ensure_hierarchy_table/0} instead (BT-576).
-spec ensure_hierarchy_table() -> ok.
ensure_hierarchy_table() ->
    beamtalk_class_registry:ensure_hierarchy_table().

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

%% @doc Super dispatch - invoke a method from the superclass chain.
%%
%% @deprecated Use {@link beamtalk_dispatch:super/5} instead (ADR 0006).
%% This function is retained for backward compatibility with existing compiled
%% modules. New codegen uses beamtalk_dispatch:super/5 directly.
%%
%% Returns `{reply, Result, NewState}` to match the actor dispatch protocol.
-spec super_dispatch(map(), selector(), list()) -> {reply, term(), map()} | {error, term()}.
super_dispatch(State, Selector, Args) ->
    %% Extract the current class from state
    case beamtalk_tagged_map:class_of(State) of
        undefined ->
            Error0 = beamtalk_error:new(internal_error, unknown),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            ClassKey = beamtalk_tagged_map:class_key(),
            Hint = iolist_to_binary(io_lib:format("State map missing '~p' field", [ClassKey])),
            Error = beamtalk_error:with_hint(Error1, Hint),
            {error, Error};
        CurrentClass ->
            %% Look up the current class process
            case beamtalk_class_registry:whereis_class(CurrentClass) of
                undefined ->
                    Error0 = beamtalk_error:new(class_not_found, CurrentClass),
                    Error = beamtalk_error:with_selector(Error0, Selector),
                    {error, Error};
                ClassPid ->
                    %% Get the superclass
                    case superclass(ClassPid) of
                        none ->
                            Error0 = beamtalk_error:new(no_superclass, CurrentClass),
                            Error = beamtalk_error:with_selector(Error0, Selector),
                            {error, Error};
                        Superclass ->
                            %% Find and invoke in superclass chain
                            find_and_invoke_super_method(Superclass, Selector, Args, State)
                    end
            end
    end.

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
    FlattenedMethods = build_flattened_methods(ClassName, Superclass, InstanceMethods),
    FlattenedClassMethods = build_flattened_methods(ClassName, Superclass, ClassMethods, get_flattened_class_methods),
    
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
    NewFlattened = build_flattened_methods(
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

%% BT-411: User-defined class method dispatch.
%% BT-412: Passes class variables to method and handles updates.
%% Looks up selector in flattened_class_methods and calls the module function.
%%
%% BT-440: For runAll and run: (test execution), we spawn test execution in
%% a separate process using {noreply, State} to avoid gen_server deadlock,
%% since test execution needs to call back into the class system.
handle_call({class_method_call, Selector, Args}, From,
            #class_state{flattened_class_methods = FlatClassMethods,
                         flattened_methods = FlatMethods,
                         name = ClassName, module = Module,
                         class_variables = ClassVars} = State) ->
    case maps:find(Selector, FlatClassMethods) of
        {ok, {DefiningClass, _MethodInfo}} ->
            %% Resolve the module for the defining class (may differ for inherited methods)
            DefiningModule = case DefiningClass of
                ClassName -> Module;
                _ ->
                    case beamtalk_class_registry:whereis_class(DefiningClass) of
                        undefined -> Module;
                        DefPid -> gen_server:call(DefPid, get_module, 5000)
                    end
            end,
            %% BT-440: For test execution (runAll, run:) inherited from TestCase,
            %% spawn in separate process to avoid gen_server deadlock when
            %% test execution calls class_send.
            case is_test_execution_selector(Selector) andalso
                 DefiningClass =:= 'TestCase' of
                true ->
                    spawn(fun() ->
                        try
                            Result = beamtalk_test_case:execute_tests(
                                Selector, Args, ClassName, Module, FlatMethods),
                            gen_server:reply(From, {ok, Result})
                        catch
                            C:E ->
                                ?LOG_ERROR("Test execution ~p:~p failed: ~p:~p",
                                             [ClassName, Selector, C, E]),
                                gen_server:reply(From, {error, E})
                        end
                    end),
                    {noreply, State};
                false ->
                    %% Build class self object for `self` reference in class methods
                    ClassSelf = {beamtalk_object, beamtalk_class_registry:class_object_tag(ClassName), DefiningModule, self()},
                    %% Class method function name: class_<selector>
                    FunName = class_method_fun_name(Selector),
                    %% BT-412: Pass class variables; handle {Result, NewClassVars} returns
                    try erlang:apply(DefiningModule, FunName, [ClassSelf, ClassVars | Args]) of
                        {class_var_result, Result, NewClassVars} ->
                            NewState = State#class_state{class_variables = NewClassVars},
                            {reply, {ok, Result}, NewState};
                        Result ->
                            {reply, {ok, Result}, State}
                    catch
                        Class:Error ->
                            ?LOG_ERROR("Class method ~p:~p failed: ~p:~p",
                                         [ClassName, Selector, Class, Error]),
                            {reply, {error, Error}, State}
                    end
            end;
        error ->
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

%% Internal query for super_dispatch - get the class's behavior module
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
                    Error0 = beamtalk_error:new(does_not_understand, State#class_state.name),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    FuturePid ! {reject, Error1},
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
%% @doc Unwrap a class gen_server call result for use in class_send.
%%
%% Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
%% Handles both raw #beamtalk_error{} records and already-wrapped Exception
%% maps (from raise/1 inside handle_call). Uses ensure_wrapped/1 for
%% idempotent wrapping (BT-525).
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) -> Value;
unwrap_class_call({error, Error}) ->
    Wrapped = beamtalk_exception_handler:ensure_wrapped(Error),
    error(Wrapped).

%% @private
%% @doc Rebuild both instance and class flattened method tables from State.
%%
%% Returns updated State with new flattened_methods and flattened_class_methods.
%% DRYs the repeated rebuild in code_change and handle_info({rebuild_flattened,...}).
-spec rebuild_all_flattened_tables(#class_state{}) -> #class_state{}.
rebuild_all_flattened_tables(#class_state{
    name = Name,
    superclass = Superclass,
    instance_methods = InstanceMethods,
    class_methods = ClassMethods
} = State) ->
    NewFlattened = build_flattened_methods(Name, Superclass, InstanceMethods),
    NewFlattenedClass = build_flattened_methods(Name, Superclass, ClassMethods, get_flattened_class_methods),
    State#class_state{
        flattened_methods = NewFlattened,
        flattened_class_methods = NewFlattenedClass
    }.

notify_instances(_ClassName, _NewMethods) ->
    %% TODO: Once beamtalk_object_instances is updated to work with per-class processes,
    %% this will broadcast method table updates to running instances.
    %% For now, this is a no-op.
    ok.

%% @private
%% Find and invoke a method in the superclass chain.
%% Recursively walks up the inheritance hierarchy until the method is found.
%% Returns {reply, Result, NewState} matching the actor dispatch protocol.
-spec find_and_invoke_super_method(class_name(), selector(), list(), map()) ->
    {reply, term(), map()} | {error, term()}.
find_and_invoke_super_method(ClassName, Selector, Args, State) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            Error = beamtalk_error:with_selector(Error0, Selector),
            {error, Error};
        ClassPid ->
            %% Get class info via API calls
            case get_class_method_info(ClassPid, Selector) of
                {ok, Module, MethodInfo} ->
                    %% Found the method - invoke it (returns {reply, Result, NewState})
                    invoke_super_method(Module, Selector, MethodInfo, Args, State);
                method_not_found ->
                    %% Method not found in this class, try superclass
                    case superclass(ClassPid) of
                        none ->
                            %% Reached root, method not found
                            StateClassName = beamtalk_tagged_map:class_of(State, ClassName),
                            Error0 = beamtalk_error:new(does_not_understand, StateClassName),
                            Error = beamtalk_error:with_selector(Error0, Selector),
                            {error, Error};
                        Superclass ->
                            %% Recursively search in superclass
                            find_and_invoke_super_method(Superclass, Selector, Args, State)
                    end
            end
    end.

%% @private
%% Get method info for a selector from a class process.
-spec get_class_method_info(pid(), selector()) ->
    {ok, atom(), method_info()} | method_not_found.
get_class_method_info(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            method_not_found;
        MethodObj ->
            %% Extract module from class state (need to add a query for this)
            %% For now, we'll need to get it via another call
            Module = gen_server:call(ClassPid, get_module),
            MethodInfo = maps:get('__method_info__', MethodObj),
            {ok, Module, MethodInfo}
    end.

%% @private
%% Invoke a method found in the superclass.
%% Returns {reply, Result, NewState} matching the actor dispatch protocol.
-spec invoke_super_method(atom(), selector(), method_info(), list(), map()) ->
    {reply, term(), map()}.
invoke_super_method(Module, Selector, MethodInfo, Args, State) ->
    case maps:find(block, MethodInfo) of
        {ok, Block} ->
            %% Runtime block available (from live development) - call it
            %% Blocks don't have state, so return state unchanged
            Result = apply(Block, Args),
            {reply, Result, State};
        error ->
            %% No runtime block, must call the compiled module
            %% Construct Self object reference for dispatch/4 (BT-161)
            Self = beamtalk_actor:make_self(State),
            %% Call the module's dispatch function with the selector
            %% This returns {reply, Result, NewState} - pass through as-is
            Module:dispatch(Selector, Args, Self, State)
    end.

%% @private
%% @doc Build flattened method table by walking hierarchy and merging methods.
%%
%% ADR 0006 Phase 2: Pre-compute all methods (local + inherited) at class
%% registration time for O(1) lookup. Child methods override parent methods.
%%
%% Returns a map of {Selector => {DefiningClass, MethodInfo}} where DefiningClass
%% is the class that actually defines the method.
%%
%% ## Algorithm
%%
%% 1. Start with empty accumulator
%% 2. Walk from current class up to root (none)
%% 3. At each level, add methods from that class (if not already present)
%% 4. Child methods naturally override parent (first-seen wins)
%%
%% ## Example
%%
%% ```
%% Counter (defines: increment, getValue)
%%   → Actor (defines: spawn)
%%     → Object (defines: class, respondsTo:)
%%
%% Flattened table for Counter:
%% #{
%%   increment => {'Counter', #{arity => 0, ...}},
%%   getValue => {'Counter', #{arity => 0, ...}},
%%   spawn => {'Actor', #{arity => 1, ...}},
%%   class => {'Object', #{arity => 0, ...}},
%%   respondsTo: => {'Object', #{arity => 1, ...}}
%% }
%% ```
-spec build_flattened_methods(class_name(), class_name() | none, #{selector() => method_info()}) ->
    #{selector() => {class_name(), method_info()}}.
build_flattened_methods(CurrentClass, Superclass, LocalMethods) ->
    build_flattened_methods(CurrentClass, Superclass, LocalMethods, get_flattened_methods).

%% @doc Build flattened method table with configurable superclass query message.
%% Used for both instance methods (get_flattened_methods) and class methods
%% (get_flattened_class_methods).
-spec build_flattened_methods(class_name(), class_name() | none, #{selector() => method_info()}, atom()) ->
    #{selector() => {class_name(), method_info()}}.
build_flattened_methods(CurrentClass, Superclass, LocalMethods, QueryMsg) ->
    %% Start with local methods (maps each selector to {CurrentClass, MethodInfo})
    LocalFlattened = maps:fold(fun(Selector, MethodInfo, Acc) ->
        maps:put(Selector, {CurrentClass, MethodInfo}, Acc)
    end, #{}, LocalMethods),
    
    %% Walk up the hierarchy and merge inherited methods
    case Superclass of
        none ->
            %% At root - return local methods
            LocalFlattened;
        SuperclassName ->
            %% Get inherited methods from superclass
            case beamtalk_class_registry:whereis_class(SuperclassName) of
                undefined ->
                    %% BT-510: Superclass not registered yet (out-of-order loading).
                    %% Return local methods only; invalidate_subclass_flattened_tables
                    %% will trigger a rebuild once the superclass registers.
                    ?LOG_DEBUG("Superclass unavailable during init, flattened methods incomplete",
                                 #{class => CurrentClass, superclass => SuperclassName}),
                    LocalFlattened;
                SuperclassPid ->
                    SuperclassFlattenedMethods = try gen_server:call(SuperclassPid, QueryMsg, 5000) of
                        {ok, Methods} -> Methods;
                        _ -> #{}
                    catch
                        _:_ -> #{}
                    end,
                    
                    %% Merge: local methods override inherited
                    maps:merge(SuperclassFlattenedMethods, LocalFlattened)
            end
    end.

%% @doc Convert a class method selector to its module function name.
%% Class methods are generated with a 'class_' prefix, e.g.
%% `class defaultValue => 42` becomes `class_defaultValue/1`.
%% Uses list_to_existing_atom to prevent atom table exhaustion from unknown selectors.
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    list_to_existing_atom("class_" ++ atom_to_list(Selector)).

%% BT-440: Check if a class method selector is a test execution command.
%% These selectors need special handling to avoid gen_server deadlock
%% (test execution calls back into the class system).
is_test_execution_selector(runAll) -> true;
is_test_execution_selector('run:') -> true;
is_test_execution_selector(_) -> false.
