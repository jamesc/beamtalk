%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Global class registry for Beamtalk.
%%
%% Like Smalltalk's global `Smalltalk` dictionary, this module maintains
%% a central registry of class metadata. The Erlang code server only knows
%% about modules, not class hierarchies, instance variables, or method
%% dictionaries needed for metaprogramming.
%%
%% ClassInfo structure:
%% ```
%% #{
%%   module => atom(),              % compiled BEAM module name
%%   superclass => atom() | none,   % parent class name
%%   methods => #{selector() => method_info()},
%%   instance_variables => [atom()],
%%   class_variables => map(),
%%   source_file => string() | undefined
%% }
%% ```
-module(beamtalk_classes).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link_unnamed/0,  % for testing - no name registration
    register_class/2,      % register_class(Name, ClassInfo) - production API
    register_class/3,      % register_class(ServerRef, Name, ClassInfo) - with Pid
    lookup/1,              % lookup(Name) - production API
    lookup/2,              % lookup(ServerRef, Name) - with Pid
    all_classes/0,         % all_classes() - production API
    all_classes/1,         % all_classes(ServerRef) - with Pid
    subclasses_of/1,       % subclasses_of(Name) - production API
    subclasses_of/2,       % subclasses_of(ServerRef, Name) - with Pid
    add_method/3,          % add_method(Class, Selector, Block) - production API
    add_method/4,          % add_method(ServerRef, Class, Selector, Block) - with Pid
    remove_method/2,       % remove_method(Class, Selector) - production API
    remove_method/3,       % remove_method(ServerRef, Class, Selector) - with Pid
    super_dispatch/3,      % super_dispatch(State, Selector, Args) - super method dispatch
    super_dispatch/4       % super_dispatch(ServerRef, State, Selector, Args) - with Pid
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
%% Method metadata - may contain arity (from class definition) or block (from live updates)
-type method_info() :: #{
    arity => non_neg_integer(),        % optional: method arity from definition
    block => fun()                     % optional: runtime block for live development
}.
-type class_info() :: #{
    module := atom(),
    superclass := class_name() | none,
    methods := #{selector() => method_info()},
    instance_variables := [atom()],
    class_variables := map(),
    source_file => string()
}.

-record(state, {
    classes = #{} :: #{class_name() => class_info()}
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the class registry server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start an unnamed registry server (for testing).
%% Does not register a name, allowing multiple instances in tests.
-spec start_link_unnamed() -> {ok, pid()} | {error, term()}.
start_link_unnamed() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Register a class with the given metadata.
-spec register_class(pid() | atom(), class_name(), class_info()) -> ok | {error, term()}.
register_class(ServerRef, Name, ClassInfo) ->
    gen_server:call(ServerRef, {register_class, Name, ClassInfo}).

%% @doc Register a class using the globally registered server (production API).
-spec register_class(class_name(), class_info()) -> ok | {error, term()}.
register_class(Name, ClassInfo) ->
    register_class(?MODULE, Name, ClassInfo).

%% @doc Look up a class by name.
-spec lookup(pid() | atom(), class_name()) -> {ok, class_info()} | undefined.
lookup(ServerRef, Name) ->
    gen_server:call(ServerRef, {lookup, Name}).

%% @doc Look up a class using the globally registered server (production API).
-spec lookup(class_name()) -> {ok, class_info()} | undefined.
lookup(Name) ->
    lookup(?MODULE, Name).

%% @doc Get all registered class names.
-spec all_classes(pid() | atom()) -> [class_name()].
all_classes(ServerRef) ->
    gen_server:call(ServerRef, all_classes).

%% @doc Get all classes using the globally registered server (production API).
-spec all_classes() -> [class_name()].
all_classes() ->
    all_classes(?MODULE).

%% @doc Get all direct subclasses of a class.
-spec subclasses_of(pid() | atom(), class_name()) -> [class_name()].
subclasses_of(ServerRef, Name) ->
    gen_server:call(ServerRef, {subclasses_of, Name}).

%% @doc Get subclasses using the globally registered server (production API).
-spec subclasses_of(class_name()) -> [class_name()].
subclasses_of(Name) ->
    subclasses_of(?MODULE, Name).

%% @doc Add or update a method on a class (for live development).
-spec add_method(pid() | atom(), class_name(), selector(), fun()) -> ok | {error, class_not_found}.
add_method(ServerRef, Class, Selector, Block) ->
    gen_server:call(ServerRef, {add_method, Class, Selector, Block}).

%% @doc Add method using the globally registered server (production API).
-spec add_method(class_name(), selector(), fun()) -> ok | {error, class_not_found}.
add_method(Class, Selector, Block) ->
    add_method(?MODULE, Class, Selector, Block).

%% @doc Remove a method from a class (for live development).
-spec remove_method(pid() | atom(), class_name(), selector()) -> ok | {error, class_not_found}.
remove_method(ServerRef, Class, Selector) ->
    gen_server:call(ServerRef, {remove_method, Class, Selector}).

%% @doc Remove method using the globally registered server (production API).
-spec remove_method(class_name(), selector()) -> ok | {error, class_not_found}.
remove_method(Class, Selector) ->
    remove_method(?MODULE, Class, Selector).

%% @doc Dispatches a message to the superclass implementation.
%%
%% Looks up the superclass of State's __class__, finds the method
%% in the superclass method table, and invokes it with the given arguments.
%%
%% The function walks the inheritance chain recursively until it finds
%% the method or reaches the root class.
%%
%% @param State The current actor state (must contain __class__ field)
%% @param Selector The method selector atom (e.g., 'increment', 'at:put:')
%% @param Args List of arguments for the method
%% @returns For compiled modules: {reply, Result, NewState} (actor protocol).
%%          For runtime blocks: varies based on block implementation.
-spec super_dispatch(map(), selector(), list()) -> {term(), term()} | {term(), term(), term()} | {error, term()}.
super_dispatch(State, Selector, Args) ->
    super_dispatch(?MODULE, State, Selector, Args).

%% @doc Super dispatch with explicit server reference (for testing).
-spec super_dispatch(pid() | atom(), map(), selector(), list()) -> {term(), term()} | {term(), term(), term()} | {error, term()}.
super_dispatch(ServerRef, State, Selector, Args) ->
    %% Extract the current class from state
    case maps:find('__class__', State) of
        {ok, CurrentClass} ->
            %% Look up the current class info
            case lookup(ServerRef, CurrentClass) of
                {ok, ClassInfo} ->
                    %% Get the superclass
                    case maps:get(superclass, ClassInfo) of
                        none ->
                            %% No superclass - cannot dispatch super
                            {error, {no_superclass, CurrentClass, Selector}};
                        Superclass ->
                            %% Find and invoke the method in the superclass chain
                            find_and_invoke_super_method(ServerRef, Superclass, Selector, Args, State)
                    end;
                undefined ->
                    {error, {class_not_found, CurrentClass}}
            end;
        error ->
            {error, missing_class_field_in_state}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({register_class, Name, ClassInfo}, _From, State) ->
    %% Validate required fields
    RequiredFields = [module, superclass, methods, instance_variables, class_variables],
    case validate_class_info(ClassInfo, RequiredFields) of
        ok ->
            NewClasses = maps:put(Name, ClassInfo, State#state.classes),
            {reply, ok, State#state{classes = NewClasses}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({lookup, Name}, _From, State) ->
    Result = case maps:find(Name, State#state.classes) of
        {ok, ClassInfo} -> {ok, ClassInfo};
        error -> undefined
    end,
    {reply, Result, State};

handle_call(all_classes, _From, State) ->
    ClassNames = maps:keys(State#state.classes),
    {reply, ClassNames, State};

handle_call({subclasses_of, ParentName}, _From, State) ->
    Subclasses = maps:fold(
        fun(Name, ClassInfo, Acc) ->
                case maps:get(superclass, ClassInfo, undefined) of
                    ParentName ->
                        [Name | Acc];
                    _ ->
                        Acc
                end
        end,
        [],
        State#state.classes
    ),
    {reply, Subclasses, State};

handle_call({add_method, Class, Selector, Block}, _From, State) ->
    case maps:find(Class, State#state.classes) of
        {ok, ClassInfo} ->
            Methods = maps:get(methods, ClassInfo),
            ExistingMethodInfo = maps:get(Selector, Methods, #{}),
            NewMethodInfo = ExistingMethodInfo#{block => Block},
            NewMethods = maps:put(Selector, NewMethodInfo, Methods),
            NewClassInfo = maps:put(methods, NewMethods, ClassInfo),
            NewClasses = maps:put(Class, NewClassInfo, State#state.classes),
            {reply, ok, State#state{classes = NewClasses}};
        error ->
            {reply, {error, class_not_found}, State}
    end;

handle_call({remove_method, Class, Selector}, _From, State) ->
    case maps:find(Class, State#state.classes) of
        {ok, ClassInfo} ->
            Methods = maps:get(methods, ClassInfo),
            NewMethods = maps:remove(Selector, Methods),
            NewClassInfo = maps:put(methods, NewMethods, ClassInfo),
            NewClasses = maps:put(Class, NewClassInfo, State#state.classes),
            {reply, ok, State#state{classes = NewClasses}};
        error ->
            {reply, {error, class_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% Validate that ClassInfo contains all required fields
validate_class_info(ClassInfo, RequiredFields) ->
    MissingFields = lists:filter(
        fun(Field) -> not maps:is_key(Field, ClassInfo) end,
        RequiredFields
    ),
    case MissingFields of
        [] -> ok;
        _ -> {error, {missing_fields, MissingFields}}
    end.

%% @private
%% Find and invoke a method in the superclass chain.
%% Recursively walks up the inheritance hierarchy until the method is found.
-spec find_and_invoke_super_method(pid() | atom(), class_name(), selector(), list(), map()) ->
    {term(), map()} | {error, term()}.
find_and_invoke_super_method(ServerRef, ClassName, Selector, Args, State) ->
    case lookup(ServerRef, ClassName) of
        {ok, ClassInfo} ->
            Methods = maps:get(methods, ClassInfo),
            case maps:find(Selector, Methods) of
                {ok, MethodInfo} ->
                    %% Found the method - invoke it
                    invoke_super_method(ClassInfo, Selector, MethodInfo, Args, State);
                error ->
                    %% Method not found in this class, try superclass
                    case maps:get(superclass, ClassInfo) of
                        none ->
                            %% Reached root, method not found
                            {error, {method_not_found_in_superclass, Selector}};
                        Superclass ->
                            %% Recursively search in superclass
                            find_and_invoke_super_method(ServerRef, Superclass, Selector, Args, State)
                    end
            end;
        undefined ->
            {error, {class_not_found, ClassName}}
    end.

%% @private
%% Invoke a method found in the superclass.
-spec invoke_super_method(class_info(), selector(), method_info(), list(), map()) ->
    {term(), map()}.
invoke_super_method(ClassInfo, Selector, MethodInfo, Args, State) ->
    case maps:find(block, MethodInfo) of
        {ok, Block} ->
            %% Runtime block available (from live development) - call it
            apply(Block, Args);
        error ->
            %% No runtime block, must call the compiled module
            Module = maps:get(module, ClassInfo),
            %% Construct Self object reference for dispatch/4 (BT-161)
            Self = beamtalk_actor:make_self(State),
            %% Call the module's dispatch function with the selector
            %% This maintains the actor protocol (returns {reply, Result, NewState})
            Module:dispatch(Selector, Args, Self, State)
    end.
