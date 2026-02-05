%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace metadata tracking
%%%
%%% Stores metadata about the workspace including:
%%% - Workspace ID
%%% - Project path
%%% - Creation timestamp
%%% - Last activity timestamp
%%%
%%% This module provides a gen_server that tracks workspace state
%%% and can be queried by other components (e.g., idle monitor).

-module(beamtalk_workspace_meta).
-behaviour(gen_server).

%% Public API
-export([start_link/1, get_metadata/0, update_activity/0, get_last_activity/0]).
-export([register_actor/1, unregister_actor/1, supervised_actors/0]).
-export([register_module/1, loaded_modules/0]).
-export([get/0]).  % Alias for get_metadata/0

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ETS_TABLE, beamtalk_workspace_registry).

-record(state, {
    workspace_id :: binary(),
    project_path :: binary(),
    created_at :: integer(),
    last_activity :: integer(),
    node_name :: atom(),
    repl_port :: inet:port_number() | undefined,
    supervised_actors :: [pid()],
    loaded_modules :: [atom()],
    metadata_path :: string()
}).

-type metadata() :: #{
    workspace_id => binary(),
    project_path => binary(),
    created_at => integer(),
    last_activity => integer(),
    node_name => atom(),
    repl_port => inet:port_number() | undefined,
    supervised_actors => [pid()],
    loaded_modules => [atom()]
}.

-export_type([metadata/0]).

%%% Public API

%% @doc Start the workspace metadata server.
-spec start_link(metadata()) -> {ok, pid()} | {error, term()}.
start_link(InitialMetadata) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitialMetadata, []).

%% @doc Get all workspace metadata.
-spec get_metadata() -> {ok, metadata()} | {error, not_started}.
get_metadata() ->
    try
        gen_server:call(?MODULE, get_metadata)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%% @doc Alias for get_metadata/0 (ADR 0004 API)
-spec get() -> {ok, metadata()} | {error, not_started}.
get() ->
    get_metadata().

%% @doc Update the last activity timestamp to now.
%% Called by other components when activity occurs (message sent, code loaded, etc.)
-spec update_activity() -> ok.
update_activity() ->
    try
        gen_server:cast(?MODULE, update_activity)
    catch
        exit:{noproc, _} ->
            ok  % Gracefully handle if server not running
    end.

%% @doc Get the last activity timestamp.
%% Returns the timestamp in seconds since epoch, or {error, not_started}.
-spec get_last_activity() -> {ok, integer()} | {error, not_started}.
get_last_activity() ->
    try
        gen_server:call(?MODULE, get_last_activity)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%% @doc Register a supervised actor PID.
-spec register_actor(pid()) -> ok.
register_actor(Pid) when is_pid(Pid) ->
    try
        gen_server:cast(?MODULE, {register_actor, Pid})
    catch
        exit:{noproc, _} ->
            ok
    end.

%% @doc Unregister a supervised actor PID.
-spec unregister_actor(pid()) -> ok.
unregister_actor(Pid) when is_pid(Pid) ->
    try
        gen_server:cast(?MODULE, {unregister_actor, Pid})
    catch
        exit:{noproc, _} ->
            ok
    end.

%% @doc Get list of supervised actor PIDs.
-spec supervised_actors() -> {ok, [pid()]} | {error, not_started}.
supervised_actors() ->
    try
        gen_server:call(?MODULE, supervised_actors)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%% @doc Register a loaded module.
-spec register_module(atom()) -> ok.
register_module(Module) when is_atom(Module) ->
    try
        gen_server:cast(?MODULE, {register_module, Module})
    catch
        exit:{noproc, _} ->
            ok
    end.

%% @doc Get list of loaded modules.
-spec loaded_modules() -> {ok, [atom()]} | {error, not_started}.
loaded_modules() ->
    try
        gen_server:call(?MODULE, loaded_modules)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%%% gen_server callbacks

%% @private
init(InitialMetadata) ->
    WorkspaceId = maps:get(workspace_id, InitialMetadata),
    ProjectPath = maps:get(project_path, InitialMetadata),
    CreatedAt = maps:get(created_at, InitialMetadata),
    ReplPort = maps:get(repl_port, InitialMetadata, undefined),
    Now = erlang:system_time(second),
    
    %% Create ETS table for workspace registry (if not already exists)
    case ets:whereis(?ETS_TABLE) of
        undefined ->
            _Tid = ets:new(?ETS_TABLE, [named_table, public, set, {read_concurrency, true}]);
        _Tid ->
            ok  % Table already exists
    end,
    
    %% Compute metadata path
    Home = os:getenv("HOME", "/tmp"),
    MetadataPath = filename:join([Home, ".beamtalk", "workspaces", 
                                  binary_to_list(WorkspaceId), "metadata.json"]),
    
    State = #state{
        workspace_id = WorkspaceId,
        project_path = ProjectPath,
        created_at = CreatedAt,
        last_activity = Now,
        node_name = node(),
        repl_port = ReplPort,
        supervised_actors = [],
        loaded_modules = [],
        metadata_path = MetadataPath
    },
    
    %% Store initial state in ETS
    store_state_in_ets(State),
    
    %% Load persisted metadata if exists
    State2 = load_metadata_from_disk(State),
    
    {ok, State2}.

%% @private
handle_call(get_metadata, _From, State) ->
    Metadata = #{
        workspace_id => State#state.workspace_id,
        project_path => State#state.project_path,
        created_at => State#state.created_at,
        last_activity => State#state.last_activity,
        node_name => State#state.node_name,
        repl_port => State#state.repl_port,
        supervised_actors => State#state.supervised_actors,
        loaded_modules => State#state.loaded_modules
    },
    {reply, {ok, Metadata}, State};

handle_call(get_last_activity, _From, State) ->
    {reply, {ok, State#state.last_activity}, State};

handle_call(supervised_actors, _From, State) ->
    {reply, {ok, State#state.supervised_actors}, State};

handle_call(loaded_modules, _From, State) ->
    {reply, {ok, State#state.loaded_modules}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(update_activity, State) ->
    Now = erlang:system_time(second),
    State2 = State#state{last_activity = Now},
    store_state_in_ets(State2),
    persist_metadata_to_disk(State2),
    {noreply, State2};

handle_cast({register_actor, Pid}, State) ->
    Actors = State#state.supervised_actors,
    State2 = case lists:member(Pid, Actors) of
        true -> State;
        false ->
            monitor(process, Pid),  % Monitor so we can clean up on exit
            State#state{supervised_actors = [Pid | Actors]}
    end,
    store_state_in_ets(State2),
    persist_metadata_to_disk(State2),
    {noreply, State2};

handle_cast({unregister_actor, Pid}, State) ->
    Actors = State#state.supervised_actors,
    State2 = State#state{supervised_actors = lists:delete(Pid, Actors)},
    store_state_in_ets(State2),
    persist_metadata_to_disk(State2),
    {noreply, State2};

handle_cast({register_module, Module}, State) ->
    Modules = State#state.loaded_modules,
    State2 = case lists:member(Module, Modules) of
        true -> State;
        false -> State#state{loaded_modules = [Module | Modules]}
    end,
    store_state_in_ets(State2),
    persist_metadata_to_disk(State2),
    {noreply, State2};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Actor exited, remove from tracked list
    Actors = State#state.supervised_actors,
    State2 = State#state{supervised_actors = lists:delete(Pid, Actors)},
    store_state_in_ets(State2),
    persist_metadata_to_disk(State2),
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Persist final state before shutting down
    persist_metadata_to_disk(State),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% @private
%% Store state in ETS for fast read access
store_state_in_ets(State) ->
    case ets:whereis(?ETS_TABLE) of
        undefined -> ok;  % Table doesn't exist yet, skip
        _Tid -> ets:insert(?ETS_TABLE, {metadata, State})
    end.

%% @private
%% Load metadata from disk if available
load_metadata_from_disk(State) ->
    Path = State#state.metadata_path,
    case file:read_file(Path) of
        {ok, Binary} ->
            try jsx:decode(Binary, [return_maps]) of
                Map ->
                    %% Restore supervised_actors (list of PIDs stored as strings)
                    Actors = maps:get(<<"supervised_actors">>, Map, []),
                    ActorPids = [list_to_pid(binary_to_list(P)) || P <- Actors,
                                 is_binary(P)],
                    %% Note: PIDs from disk won't be valid, but we keep the structure
                    
                    %% Restore loaded_modules
                    Modules = maps:get(<<"loaded_modules">>, Map, []),
                    ModuleAtoms = [binary_to_existing_atom(M, utf8) || M <- Modules,
                                   is_binary(M)],
                    
                    State#state{
                        supervised_actors = ActorPids,
                        loaded_modules = ModuleAtoms
                    }
            catch
                _:_ -> State  % Failed to parse, use default
            end;
        {error, enoent} ->
            State;  % No metadata file yet
        {error, _} ->
            State  % Failed to read, use default
    end.

%% @private
%% Persist metadata to disk in JSON format
persist_metadata_to_disk(State) ->
    Path = State#state.metadata_path,
    %% Ensure directory exists
    Dir = filename:dirname(Path),
    _ = filelib:ensure_dir(filename:join(Dir, "dummy")),
    
    %% Build JSON metadata (PIDs as strings, atoms as strings)
    Metadata = #{
        <<"workspace_id">> => State#state.workspace_id,
        <<"project_path">> => State#state.project_path,
        <<"created_at">> => State#state.created_at,
        <<"last_active">> => State#state.last_activity,
        <<"node_name">> => atom_to_binary(State#state.node_name, utf8),
        <<"repl_port">> => case State#state.repl_port of
            undefined -> null;
            Port -> Port
        end,
        <<"supervised_actors">> => [list_to_binary(pid_to_list(P)) || 
                                    P <- State#state.supervised_actors],
        <<"loaded_modules">> => [atom_to_binary(M, utf8) || 
                                 M <- State#state.loaded_modules]
    },
    
    %% Write to disk
    Json = jsx:encode(Metadata, [{space, 2}, {indent, 2}]),
    file:write_file(Path, Json).
