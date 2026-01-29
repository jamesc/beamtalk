%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Global class registry for Beamtalk.
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
    register_class/2,      % register_class(Name, ClassInfo)
    lookup/1,              % lookup(Name) -> ClassInfo | undefined
    all_classes/0,         % all_classes() -> [Name]
    subclasses_of/1,       % subclasses_of(Name) -> [Name]
    add_method/3,          % add_method(Class, Selector, Block) - for live dev
    remove_method/2        % remove_method(Class, Selector)
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-type class_name() :: atom().
-type selector() :: atom().
-type method_info() :: map().
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

%% @doc Register a class with the given metadata.
-spec register_class(class_name(), class_info()) -> ok.
register_class(Name, ClassInfo) ->
    gen_server:call(?MODULE, {register_class, Name, ClassInfo}).

%% @doc Look up a class by name.
-spec lookup(class_name()) -> {ok, class_info()} | undefined.
lookup(Name) ->
    gen_server:call(?MODULE, {lookup, Name}).

%% @doc Get all registered class names.
-spec all_classes() -> [class_name()].
all_classes() ->
    gen_server:call(?MODULE, all_classes).

%% @doc Get all direct subclasses of a class.
-spec subclasses_of(class_name()) -> [class_name()].
subclasses_of(Name) ->
    gen_server:call(?MODULE, {subclasses_of, Name}).

%% @doc Add or update a method on a class (for live development).
-spec add_method(class_name(), selector(), fun()) -> ok | {error, class_not_found}.
add_method(Class, Selector, Block) ->
    gen_server:call(?MODULE, {add_method, Class, Selector, Block}).

%% @doc Remove a method from a class (for live development).
-spec remove_method(class_name(), selector()) -> ok | {error, class_not_found}.
remove_method(Class, Selector) ->
    gen_server:call(?MODULE, {remove_method, Class, Selector}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call({register_class, Name, ClassInfo}, _From, State) ->
    NewClasses = maps:put(Name, ClassInfo, State#state.classes),
    {reply, ok, State#state{classes = NewClasses}};

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
        fun(Name, #{superclass := Super}, Acc) when Super =:= ParentName ->
                [Name | Acc];
            (_Name, _ClassInfo, Acc) ->
                Acc
        end,
        [],
        State#state.classes
    ),
    {reply, Subclasses, State};

handle_call({add_method, Class, Selector, Block}, _From, State) ->
    case maps:find(Class, State#state.classes) of
        {ok, ClassInfo} ->
            Methods = maps:get(methods, ClassInfo),
            MethodInfo = #{block => Block},
            NewMethods = maps:put(Selector, MethodInfo, Methods),
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
