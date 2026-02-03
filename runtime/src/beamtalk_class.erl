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
-module(beamtalk_class).
-behaviour(gen_server).

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
    put_method/3,
    put_method/4,
    instance_variables/1,
    add_before/3,
    add_after/3,
    super_dispatch/3,
    class_name/1,
    module_name/1
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
    after_methods = #{} :: #{selector() => [fun()]}
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
-spec new(pid()) -> {ok, pid()} | {error, term()}.
new(ClassPid) ->
    new(ClassPid, []).

%% @doc Create a new instance with initialization arguments.
-spec new(pid(), list()) -> {ok, pid()} | {error, term()}.
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
-spec method(pid(), selector()) -> map() | nil.
method(ClassPid, Selector) ->
    gen_server:call(ClassPid, {method, Selector}).

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
%% This is used when compiling `super` sends in methods. The State map
%% must contain a `'__class__'` field indicating the current class.
%%
%% Returns `{reply, Result, NewState}` to match the actor dispatch protocol.
%%
%% Example generated code:
%% ```erlang
%% %% In subclass method:
%% {reply, Result, NewState} = beamtalk_class:super_dispatch(State, 'methodName', [arg1, arg2])
%% ```
-spec super_dispatch(map(), selector(), list()) -> {reply, term(), map()} | {error, term()}.
super_dispatch(State, Selector, Args) ->
    %% Extract the current class from state
    case maps:find('__class__', State) of
        {ok, CurrentClass} ->
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
            end;
        error ->
            {error, missing_class_field_in_state}
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
    
    %% Build state
    State = #class_state{
        name = ClassName,
        module = maps:get(module, ClassInfo, ClassName),
        superclass = maps:get(superclass, ClassInfo, none),
        instance_methods = maps:get(instance_methods, ClassInfo, #{}),
        class_methods = maps:get(class_methods, ClassInfo, #{}),
        instance_variables = maps:get(instance_variables, ClassInfo, []),
        class_variables = maps:get(class_variables, ClassInfo, #{}),
        method_source = maps:get(method_source, ClassInfo, #{}),
        before_methods = maps:get(before_methods, ClassInfo, #{}),
        after_methods = maps:get(after_methods, ClassInfo, #{})
    },
    {ok, State}.

handle_call({new, Args}, _From, #class_state{module = Module} = State) ->
    %% Spawn new instance via the class's behavior module
    %% TODO: This will integrate with beamtalk_actor once that's refactored
    case erlang:apply(Module, spawn, [Args]) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        Error ->
            {reply, Error, State}
    end;

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
                '__class__' => 'CompiledMethod',
                '__selector__' => Selector,
                '__source__' => Src,
                '__method_info__' => MethodInfo
            },
            {reply, MethodObj, State};
        error ->
            {reply, nil, State}
    end;

handle_call({put_method, Selector, Fun, Source}, _From, State) ->
    MethodInfo = #{block => Fun},
    NewMethods = maps:put(Selector, MethodInfo, State#class_state.instance_methods),
    NewSource = maps:put(Selector, Source, State#class_state.method_source),
    NewState = State#class_state{
        instance_methods = NewMethods,
        method_source = NewSource
    },
    
    %% Notify running instances to pick up new method table
    notify_instances(State#class_state.name, NewMethods),
    
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

%% Internal query for super_dispatch - get the class's behavior module
handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    %% TODO: Once beamtalk_instances is updated to work with per-class processes,
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
