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

%% API
-export([
    start_link/1,
    start_link/2,
    whereis_class/1,
    all_classes/0,
    new/1,
    new/2,
    methods/1,
    superclass/1,
    method/2,
    has_method/2,
    put_method/3,
    put_method/4,
    instance_variables/1,
    add_before/3,
    add_after/3,
    super_dispatch/3,
    class_name/1,
    module_name/1,
    create_subclass/3
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
    block => fun()
}.

-record(class_state, {
    name :: class_name(),
    module :: atom(),
    superclass :: class_name() | none,
    instance_methods = #{} :: #{selector() => method_info()},
    class_methods = #{} :: #{selector() => method_info()},
    instance_variables = [] :: [atom()],
    class_variables = #{} :: map(),
    method_source = #{} :: #{selector() => binary()},
    before_methods = #{} :: #{selector() => [fun()]},
    after_methods = #{} :: #{selector() => [fun()]},
    dynamic_methods = #{} :: #{selector() => fun()},  % For dynamic classes: actual closures
    flattened_methods = #{} :: #{selector() => {class_name(), method_info()}}  % ADR 0006 Phase 2: cached method table including inherited
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a class process with full options.
-spec start_link(class_name(), map()) -> {ok, pid()} | {error, term()}.
start_link(ClassName, ClassInfo) ->
    %% Use gen_server's built-in registration to avoid race conditions
    %% This atomically checks and registers the name
    RegName = registry_name(ClassName),
    gen_server:start_link({local, RegName}, ?MODULE, {ClassName, ClassInfo}, []).

%% @doc Start a class process with minimal info (for testing).
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(ClassInfo) ->
    ClassName = maps:get(name, ClassInfo),
    start_link(ClassName, ClassInfo).

%% @doc Look up a class by name.
-spec whereis_class(class_name()) -> pid() | undefined.
whereis_class(ClassName) ->
    RegName = registry_name(ClassName),
    erlang:whereis(RegName).

%% @doc Get all class processes.
-spec all_classes() -> [pid()].
all_classes() ->
    pg:get_members(beamtalk_classes).

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

%% @doc Get a compiled method object.
%%
%% Accepts either a class process pid or a class name atom.
%% Returns a CompiledMethod map or nil if the method is not found.
-spec method(pid() | class_name(), selector()) -> map() | nil.
method(ClassPid, Selector) when is_pid(ClassPid) ->
    gen_server:call(ClassPid, {method, Selector});
method(ClassName, Selector) when is_atom(ClassName) ->
    case whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(does_not_understand, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, '>>'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Class not found. Is it loaded?">>),
            error(Error2);
        Pid ->
            gen_server:call(Pid, {method, Selector})
    end.

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
            {error, missing_class_field_in_state};
        CurrentClass ->
            %% Look up the current class process
            case whereis_class(CurrentClass) of
                undefined ->
                    {error, {class_not_found, CurrentClass}};
                ClassPid ->
                    %% Get the superclass
                    case superclass(ClassPid) of
                        none ->
                            {error, {no_superclass, CurrentClass, Selector}};
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
    case whereis_class(SuperclassName) of
        undefined ->
            {error, {superclass_not_found, SuperclassName}};
        _SuperclassPid ->
            %% Extract fields from ClassSpec
            InstanceVars = maps:get(instance_variables, ClassSpec, []),
            InstanceMethods = maps:get(instance_methods, ClassSpec, #{}),
            
            %% Validate and convert methods
            try convert_methods_to_info(InstanceMethods) of
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
                            {error, {class_already_exists, ClassName}};
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
    ensure_pg_started(),
    
    %% Name registration is handled by gen_server:start_link({local, Name}, ...)
    %% Join pg group for all_classes enumeration
    ok = pg:join(beamtalk_classes, self()),
    
    %% Extract class info
    Superclass = maps:get(superclass, ClassInfo, none),
    InstanceMethods = maps:get(instance_methods, ClassInfo, #{}),
    
    %% Build flattened method table (ADR 0006 Phase 2)
    FlattenedMethods = build_flattened_methods(ClassName, Superclass, InstanceMethods),
    
    %% Build state
    State = #class_state{
        name = ClassName,
        module = maps:get(module, ClassInfo, ClassName),
        superclass = Superclass,
        instance_methods = InstanceMethods,
        class_methods = maps:get(class_methods, ClassInfo, #{}),
        instance_variables = maps:get(instance_variables, ClassInfo, []),
        class_variables = maps:get(class_variables, ClassInfo, #{}),
        method_source = maps:get(method_source, ClassInfo, #{}),
        before_methods = maps:get(before_methods, ClassInfo, #{}),
        after_methods = maps:get(after_methods, ClassInfo, #{}),
        dynamic_methods = maps:get(dynamic_methods, ClassInfo, #{}),
        flattened_methods = FlattenedMethods
    },
    %% ADR 0006 Phase 2: Notify existing subclasses to rebuild their flattened
    %% tables. Handles out-of-order registration (e.g., Counter registered
    %% before Actor) — subclasses that had incomplete tables now pick up our methods.
    invalidate_subclass_flattened_tables(ClassName),
    {ok, State}.

handle_call({new, Args}, _From, #class_state{
    name = ClassName,
    module = Module,
    dynamic_methods = DynamicMethods,
    instance_variables = InstanceVars
} = State) ->
    %% Check if this is a dynamic class
    case Module of
        beamtalk_dynamic_object ->
            %% Dynamic class - spawn beamtalk_dynamic_object instance
            %% Build initial state with methods and fields
            InitState = #{
                '$beamtalk_class' => ClassName,
                '__class_pid__' => self(),
                '__methods__' => DynamicMethods
            },
            %% Initialize instance variables from Args (expected to be a map or proplist)
            InitStateWithFields = case Args of
                [FieldMap] when is_map(FieldMap) ->
                    maps:merge(InitState, FieldMap);
                _ ->
                    %% Initialize all instance variables to nil
                    InitFields = lists:foldl(fun(Var, Acc) ->
                        maps:put(Var, nil, Acc)
                    end, InitState, InstanceVars),
                    InitFields
            end,
            
            %% Spawn the dynamic object
            case beamtalk_dynamic_object:start_link(ClassName, InitStateWithFields) of
                {ok, Pid} ->
                    %% Wrap in beamtalk_object record
                    Obj = #beamtalk_object{
                        class = ClassName,
                        class_mod = beamtalk_dynamic_object,
                        pid = Pid
                    },
                    {reply, {ok, Obj}, State};
                Error ->
                    {reply, Error, State}
            end;
        _ ->
            %% Compiled class - use module's spawn function
            case erlang:apply(Module, spawn, [Args]) of
                {ok, Pid} ->
                    %% Wrap in beamtalk_object record for consistency
                    Obj = #beamtalk_object{
                        class = ClassName,
                        class_mod = Module,
                        pid = Pid
                    },
                    {reply, {ok, Obj}, State};
                Error ->
                    {reply, Error, State}
            end
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
    invalidate_subclass_flattened_tables(State#class_state.name),
    
    {reply, ok, NewState};

handle_call(instance_variables, _From, #class_state{instance_variables = IVars} = State) ->
    {reply, IVars, State};

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

%% Internal query for super_dispatch - get the class's behavior module
handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State}.

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
                    Error = {error, not_supported_in_async},
                    FuturePid ! {reject, Error},
                    {noreply, State};
                _ ->
                    %% Unknown message
                    Error = {error, {unknown_class_message, Selector}},
                    FuturePid ! {reject, Error},
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end.

handle_info({rebuild_flattened, ChangedClass}, #class_state{
    name = Name,
    superclass = Superclass,
    instance_methods = InstanceMethods
} = State) ->
    %% ADR 0006 Phase 2: A parent class changed — rebuild if we inherit from it.
    %% Wrapped in try/catch: if any ancestor process is dead/busy, we skip the
    %% rebuild rather than crashing this class process. The slow-path dispatch
    %% will still find inherited methods correctly.
    try inherits_from(Superclass, ChangedClass) of
        true ->
            NewFlattened = build_flattened_methods(Name, Superclass, InstanceMethods),
            {noreply, State#class_state{flattened_methods = NewFlattened}};
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
    NewFlattened = build_flattened_methods(
        NewState#class_state.name,
        NewState#class_state.superclass,
        NewState#class_state.instance_methods
    ),
    %% Invalidate subclass tables in case hot_reload modified our methods
    invalidate_subclass_flattened_tables(NewState#class_state.name),
    {ok, NewState#class_state{flattened_methods = NewFlattened}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% Ensure pg (process groups) is started.
%% pg is used for tracking all class processes.
ensure_pg_started() ->
    case whereis(pg) of
        undefined ->
            %% pg not running - start it
            case pg:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _Pid ->
            ok
    end.

registry_name(ClassName) ->
    list_to_atom("beamtalk_class_" ++ atom_to_list(ClassName)).

notify_instances(_ClassName, _NewMethods) ->
    %% TODO: Once beamtalk_object_instances is updated to work with per-class processes,
    %% this will broadcast method table updates to running instances.
    %% For now, this is a no-op.
    ok.

%% @private
%% @doc Broadcast rebuild_flattened to all class processes except ourselves.
%%
%% ADR 0006 Phase 2: When a class's methods change, all subclasses need to
%% rebuild their flattened tables to include the new/changed method.
%% Uses pg group for fire-and-forget broadcast.
-spec invalidate_subclass_flattened_tables(class_name()) -> ok.
invalidate_subclass_flattened_tables(ChangedClass) ->
    AllClasses = pg:get_members(beamtalk_classes),
    Self = self(),
    lists:foreach(fun(Pid) ->
        case Pid of
            Self -> ok;  % Skip ourselves
            _ -> Pid ! {rebuild_flattened, ChangedClass}
        end
    end, AllClasses),
    ok.

%% @private
%% @doc Check if a class inherits from a given ancestor (walks superclass chain).
-spec inherits_from(class_name() | none, class_name()) -> boolean().
inherits_from(none, _Ancestor) ->
    false;
inherits_from(SuperclassName, Ancestor) when SuperclassName =:= Ancestor ->
    true;
inherits_from(SuperclassName, Ancestor) ->
    case whereis_class(SuperclassName) of
        undefined -> false;
        SuperclassPid ->
            case gen_server:call(SuperclassPid, superclass, 5000) of
                none -> false;
                GrandparentName -> inherits_from(GrandparentName, Ancestor)
            end
    end.

%% @private
%% Convert dynamic method closures to method_info maps.
%% This allows dynamic classes to register with the same structure as compiled classes.
-spec convert_methods_to_info(#{selector() => fun()}) -> #{selector() => method_info()}.
convert_methods_to_info(Methods) ->
    maps:map(fun(Selector, Fun) ->
        %% Extract arity from function (dynamic methods must be arity 3: Self, Args, State)
        {arity, Arity} = erlang:fun_info(Fun, arity),
        %% Validate that method has correct arity
        case Arity of
            3 -> ok;
            _ ->
                Error0 = beamtalk_error:new(arity_mismatch, 'DynamicClass'),
                Error1 = beamtalk_error:with_selector(Error0, Selector),
                Error2 = beamtalk_error:with_hint(Error1, <<"Dynamic methods must be arity 3 (Self, Args, State)">>),
                Error3 = beamtalk_error:with_details(Error2, #{actual_arity => Arity, expected_arity => 3}),
                error(Error3)
        end,
        #{
            arity => Arity,
            block => Fun
        }
    end, Methods).

%% @private
%% Find and invoke a method in the superclass chain.
%% Recursively walks up the inheritance hierarchy until the method is found.
%% Returns {reply, Result, NewState} matching the actor dispatch protocol.
-spec find_and_invoke_super_method(class_name(), selector(), list(), map()) ->
    {reply, term(), map()} | {error, term()}.
find_and_invoke_super_method(ClassName, Selector, Args, State) ->
    case whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
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
                            {error, {method_not_found_in_superclass, Selector}};
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
            case whereis_class(SuperclassName) of
                undefined ->
                    %% Superclass not registered yet (bootstrap ordering)
                    %% Just return local methods - flattening will be incomplete
                    %% but will be fixed on next hot reload
                    LocalFlattened;
                SuperclassPid ->
                    %% Get the superclass's flattened methods
                    %% Wrapped in try/catch: if superclass is busy/restarting,
                    %% degrade gracefully to local-only methods (slow path
                    %% will still find inherited methods at dispatch time)
                    SuperclassFlattenedMethods = try gen_server:call(SuperclassPid, get_flattened_methods, 5000) of
                        {ok, Methods} -> Methods;
                        _ -> #{}
                    catch
                        _:_ -> #{}
                    end,
                    
                    %% Merge: local methods override inherited
                    %% maps:merge prefers second argument on collision, so put inherited first
                    maps:merge(SuperclassFlattenedMethods, LocalFlattened)
            end
    end.
