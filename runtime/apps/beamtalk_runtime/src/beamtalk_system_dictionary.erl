%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc SystemDictionary actor - Beamtalk system introspection singleton.
%%%
%%% This gen_server implements the SystemDictionary actor that backs the
%%% `Beamtalk` global object. It provides system-wide introspection:
%%% - Class registry access (allClasses, classNamed:)
%%% - Workspace globals (globals)
%%% - Version information (version)
%%%
%%% This is Phase 1 of ADR 0010: Global Objects and Singleton Dispatch.
%%% The actor coexists with the current beamtalk_stdlib.erl dispatch.
%%% Migration to use this actor will happen in later phases.
%%%
%%% ## Message Protocol
%%%
%%% SystemDictionary responds to these selectors:
%%% - `allClasses` - Returns list of all class names
%%% - `classNamed:` - Returns class info or error for given name
%%% - `globals` - Returns workspace bindings map
%%% - `version` - Returns Beamtalk version string
%%%
%%% All methods are synchronous (gen_server:call) as they return values
%%% that callers need immediately.
%%%
%%% ## Example Usage
%%%
%%% ```erlang
%%% %% Start the SystemDictionary actor
%%% {ok, Pid} = beamtalk_system_dictionary:start_link(),
%%%
%%% %% Query all classes
%%% Classes = gen_server:call(Pid, {allClasses, []}),
%%%
%%% %% Look up a specific class
%%% ClassPid = gen_server:call(Pid, {'classNamed:', [<<"Counter">>]}),
%%%
%%% %% Get workspace globals
%%% Globals = gen_server:call(Pid, {globals, []}),
%%%
%%% %% Get version
%%% Version = gen_server:call(Pid, {version, []}).
%%% ```
-module(beamtalk_system_dictionary).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    has_method/1
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

-type selector() :: atom().

-record(system_dict_state, {
    version :: binary(),
    workspace :: atom() | undefined  % Workspace name for future phases
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the SystemDictionary actor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the SystemDictionary actor with options.
%%
%% Options:
%%   {workspace, atom()} - Workspace name (for future phases)
%%   {version, binary()} - Version string (defaults to "0.1.0-dev")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Check if SystemDictionary supports a given method selector.
%%
%% Used by dispatch compatibility layer. Returns true if the selector
%% corresponds to one of the supported methods.
-spec has_method(selector()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the SystemDictionary actor state.
-spec init(proplists:proplist()) -> {ok, #system_dict_state{}}.
init(Opts) ->
    Version = proplists:get_value(version, Opts, <<"0.1.0-dev">>),
    Workspace = proplists:get_value(workspace, Opts, undefined),
    
    State = #system_dict_state{
        version = Version,
        workspace = Workspace
    },
    {ok, State}.

%% @doc Handle synchronous method calls.
%%
%% Dispatches to the appropriate method implementation based on selector.
-spec handle_call(term(), {pid(), term()}, #system_dict_state{}) ->
    {reply, term(), #system_dict_state{}} | {stop, term(), term(), #system_dict_state{}}.

%% allClasses - Returns list of all class names
handle_call({allClasses, []}, _From, State) ->
    Classes = handle_all_classes(),
    {reply, Classes, State};

%% classNamed: - Returns class pid or error for given class name
handle_call({'classNamed:', [ClassName]}, _From, State) ->
    Result = handle_class_named(ClassName),
    {reply, Result, State};

%% globals - Returns workspace bindings map
handle_call({globals, []}, _From, State) ->
    Globals = handle_globals(),
    {reply, Globals, State};

%% version - Returns Beamtalk version string
handle_call({version, []}, _From, State) ->
    Version = State#system_dict_state.version,
    {reply, Version, State};

%% Unknown selector - return error
handle_call({UnknownSelector, _Args}, _From, State) ->
    Error = {error, {unknown_selector, UnknownSelector}},
    {reply, Error, State};

%% Unknown call format
handle_call(Request, _From, State) ->
    Error = {error, {unknown_call_format, Request}},
    {reply, Error, State}.

%% @doc Handle asynchronous messages.
%%
%% SystemDictionary methods are all synchronous, so we don't expect
%% cast messages. Log a warning if we receive one.
-spec handle_cast(term(), #system_dict_state{}) -> {noreply, #system_dict_state{}}.
handle_cast(Msg, State) ->
    logger:warning("SystemDictionary received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #system_dict_state{}) -> {noreply, #system_dict_state{}}.
handle_info(Info, State) ->
    logger:debug("SystemDictionary received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #system_dict_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #system_dict_state{}, term()) -> {ok, #system_dict_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all registered class names.
%%
%% Delegates to beamtalk_object_class:all_classes/0 which returns a list
%% of class process pids, then extracts the class name from each pid.
-spec handle_all_classes() -> [atom()].
handle_all_classes() ->
    ClassPids = beamtalk_object_class:all_classes(),
    lists:map(
        fun(Pid) ->
            beamtalk_object_class:class_name(Pid)
        end,
        ClassPids
    ).

%% @doc Look up a class by name.
%%
%% Delegates to beamtalk_object_class:whereis_class/1.
%% Returns the class pid if found, or {error, {class_not_found, ClassName}}.
-spec handle_class_named(binary() | atom()) -> pid() | {error, {class_not_found, term()}}.
handle_class_named(ClassName) when is_binary(ClassName) ->
    %% Convert binary to atom for class lookup
    %% Use binary_to_existing_atom to avoid creating atoms at runtime
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_class_named(ClassAtom)
    catch
        error:badarg ->
            %% Class name doesn't exist as an atom, so class doesn't exist
            {error, {class_not_found, ClassName}}
    end;
handle_class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        Pid when is_pid(Pid) ->
            Pid
    end.

%% @doc Get workspace global bindings.
%%
%% Phase 1: Returns empty map as workspace bindings are not yet implemented.
%% Phase 2 (ADR 0010): Will query workspace state for injected bindings.
-spec handle_globals() -> map().
handle_globals() ->
    %% Phase 1: No workspace bindings yet
    %% Phase 2: Will query workspace state via persistent_term or workspace process
    #{}.

