%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_meta).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Workspace metadata tracking

Stores metadata about the workspace including:
- Workspace ID
- Project path
- Creation timestamp
- Last activity timestamp

This module provides a gen_server that tracks workspace state
and can be queried by other components (e.g., idle monitor).
""".

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/1, get_metadata/0, update_activity/0, get_last_activity/0]).
-export([register_actor/1, unregister_actor/1, supervised_actors/0]).
-export([register_module/1, register_module/2, unregister_module/1, loaded_modules/0]).
-export([set_class_source/2, get_class_source/1]).
%% BT-1685: File mtime tracking for incremental load-project.
-export([set_file_mtime/2, get_file_mtimes/0, clear_file_mtimes/0, remove_file_mtime/1]).
-export([get_package_name/0]).
% Alias for get_metadata/0
-export([get/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(WORKSPACE_META_TABLE, beamtalk_workspace_registry).
% Debounce disk writes to every 2 seconds
-define(PERSIST_DELAY_MS, 2000).

-record(state, {
    workspace_id :: binary(),
    project_path :: binary() | undefined,
    package_name :: binary() | undefined,
    created_at :: integer(),
    last_activity :: integer(),
    node_name :: atom(),
    repl_port :: inet:port_number() | undefined,
    repl :: boolean(),
    supervised_actors :: [pid()],
    loaded_modules :: #{atom() => string() | undefined},
    class_sources :: #{binary() => string()},
    %% BT-1685: Map from absolute file path (string) to mtime (erlang:universaltime()).
    %% Used by incremental load-project to detect changed files.
    file_mtimes :: #{string() => calendar:datetime()},
    metadata_path :: string() | undefined,
    persist_timer :: reference() | undefined,
    monitor_refs :: #{pid() => reference()}
}).

%% init_metadata/0 is the input contract for start_link/1.
%% It extends metadata() with init-only fields (repl) that are consumed during
%% initialisation and not exposed through get_metadata/0.
-type init_metadata() :: #{
    workspace_id := binary(),
    project_path => binary() | undefined,
    created_at := integer(),
    repl_port => inet:port_number() | undefined,
    repl => boolean()
}.

%% metadata/0 is the output contract returned by get_metadata/0.
-type metadata() :: #{
    workspace_id => binary(),
    project_path => binary() | undefined,
    package_name => binary() | undefined,
    created_at => integer(),
    last_activity => integer(),
    node_name => atom(),
    repl_port => inet:port_number() | undefined,
    supervised_actors => [pid()],
    loaded_modules => [atom()]
}.

-export_type([init_metadata/0, metadata/0]).

%%% Public API

-doc "Start the workspace metadata server.".
-spec start_link(init_metadata()) -> {ok, pid()} | {error, term()}.
start_link(InitialMetadata) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitialMetadata, []).

-doc "Get all workspace metadata.".
-spec get_metadata() -> {ok, metadata()} | {error, not_started}.
get_metadata() ->
    try
        gen_server:call(?MODULE, get_metadata)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

-doc "Alias for get_metadata/0 (ADR 0004 API)".
-spec get() -> {ok, metadata()} | {error, not_started}.
get() ->
    get_metadata().

-doc """
Get the package name for the current workspace.
Returns `undefined` if no package is configured or the server is not started.
""".
-spec get_package_name() -> binary() | undefined.
get_package_name() ->
    try
        gen_server:call(?MODULE, get_package_name)
    catch
        exit:{noproc, _} ->
            undefined
    end.

-doc """
Update the last activity timestamp to now.
Called by other components when activity occurs (message sent, code loaded, etc.)
""".
-spec update_activity() -> ok.
update_activity() ->
    try
        gen_server:cast(?MODULE, update_activity)
    catch
        exit:{noproc, _} ->
            % Gracefully handle if server not running
            ok
    end.

-doc """
Get the last activity timestamp.
Returns the timestamp in seconds since epoch, or {error, not_started}.
""".
-spec get_last_activity() -> {ok, integer()} | {error, not_started}.
get_last_activity() ->
    try
        gen_server:call(?MODULE, get_last_activity)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

-doc "Register a supervised actor PID.".
-spec register_actor(pid()) -> ok.
register_actor(Pid) when is_pid(Pid) ->
    try
        gen_server:cast(?MODULE, {register_actor, Pid})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc "Unregister a supervised actor PID.".
-spec unregister_actor(pid()) -> ok.
unregister_actor(Pid) when is_pid(Pid) ->
    try
        gen_server:cast(?MODULE, {unregister_actor, Pid})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc "Get list of supervised actor PIDs.".
-spec supervised_actors() -> {ok, [pid()]} | {error, not_started}.
supervised_actors() ->
    try
        gen_server:call(?MODULE, supervised_actors)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

-doc "Register a loaded module with no source path.".
-spec register_module(atom()) -> ok.
register_module(Module) ->
    register_module(Module, undefined).

-doc "Register a loaded module with its .bt source file path.".
-spec register_module(atom(), string() | binary() | undefined) -> ok.
register_module(Module, SourcePath) when is_atom(Module) ->
    %% Normalise binary to string so persist_metadata_to_disk/1 always sees a list.
    NormalizedPath = normalize_source_path(SourcePath),
    try
        gen_server:cast(?MODULE, {register_module, Module, NormalizedPath})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc """
Unregister a loaded module (BT-1239: called when a class is removed from the system).
""".
-spec unregister_module(atom()) -> ok.
unregister_module(Module) when is_atom(Module) ->
    try
        gen_server:cast(?MODULE, {unregister_module, Module})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc "Get list of loaded modules with their source paths.".
-spec loaded_modules() -> {ok, [{atom(), string() | undefined}]} | {error, not_started}.
loaded_modules() ->
    try
        gen_server:call(?MODULE, loaded_modules)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

-doc "Store source text for a class (for later method patching via >>).".
-spec set_class_source(binary(), string()) -> ok.
set_class_source(ClassName, Source) when is_binary(ClassName) ->
    try
        gen_server:call(?MODULE, {set_class_source, ClassName, Source})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc """
Get stored source text for a class. Returns undefined if not found or server not started.
""".
-spec get_class_source(binary()) -> string() | undefined.
get_class_source(ClassName) when is_binary(ClassName) ->
    try
        gen_server:call(?MODULE, {get_class_source, ClassName})
    catch
        exit:{noproc, _} ->
            undefined
    end.

-doc """
Store the mtime for a loaded .bt file (BT-1685).
Called after each successful file load during load-project.
""".
-spec set_file_mtime(string(), calendar:datetime()) -> ok.
set_file_mtime(FilePath, Mtime) when is_list(FilePath) ->
    try
        gen_server:cast(?MODULE, {set_file_mtime, FilePath, Mtime})
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc """
Get all tracked file mtimes (BT-1685).
Returns a map from absolute file path to its mtime at last load.
""".
-spec get_file_mtimes() -> {ok, #{string() => calendar:datetime()}} | {error, not_started}.
get_file_mtimes() ->
    try
        gen_server:call(?MODULE, get_file_mtimes)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

-doc """
Clear all tracked file mtimes (BT-1685).
Used when force-reloading a project.
""".
-spec clear_file_mtimes() -> ok.
clear_file_mtimes() ->
    try
        gen_server:cast(?MODULE, clear_file_mtimes)
    catch
        exit:{noproc, _} ->
            ok
    end.

-doc """
Remove mtime tracking for a single file (BT-1685).
Used when a file is detected as deleted during incremental reload.
""".
-spec remove_file_mtime(string()) -> ok.
remove_file_mtime(FilePath) when is_list(FilePath) ->
    try
        gen_server:cast(?MODULE, {remove_file_mtime, FilePath})
    catch
        exit:{noproc, _} ->
            ok
    end.

%%% gen_server callbacks

init(InitialMetadata) ->
    WorkspaceId = maps:get(workspace_id, InitialMetadata),
    ProjectPath = maps:get(project_path, InitialMetadata, undefined),
    %% BT-775: Auto-detect package name from beamtalk.toml at project_path
    PackageName = detect_package_name(ProjectPath),
    CreatedAt = maps:get(created_at, InitialMetadata),
    ReplPort = maps:get(repl_port, InitialMetadata, undefined),
    ReplMode = maps:get(repl, InitialMetadata, true),
    Now = erlang:system_time(second),

    %% Create ETS table for workspace registry (if not already exists)
    case ets:whereis(?WORKSPACE_META_TABLE) of
        undefined ->
            _Tid = ets:new(?WORKSPACE_META_TABLE, [
                named_table, public, set, {read_concurrency, true}
            ]);
        _Tid ->
            % Table already exists
            ok
    end,

    %% Compute metadata path — undefined in run mode (repl=false) so no disk registration.
    MetadataPath =
        case ReplMode of
            false ->
                undefined;
            true ->
                case beamtalk_platform:home_dir() of
                    false ->
                        CacheDir = filename:basedir(user_cache, "beamtalk"),
                        filename:join([
                            CacheDir,
                            "workspaces",
                            binary_to_list(WorkspaceId),
                            "metadata.json"
                        ]);
                    Home ->
                        filename:join([
                            Home,
                            ".beamtalk",
                            "workspaces",
                            binary_to_list(WorkspaceId),
                            "metadata.json"
                        ])
                end
        end,

    State = #state{
        workspace_id = WorkspaceId,
        project_path = ProjectPath,
        package_name = PackageName,
        created_at = CreatedAt,
        last_activity = Now,
        node_name = node(),
        repl_port = ReplPort,
        repl = ReplMode,
        supervised_actors = [],
        loaded_modules = #{},
        class_sources = #{},
        file_mtimes = #{},
        metadata_path = MetadataPath,
        persist_timer = undefined,
        monitor_refs = #{}
    },

    %% Store initial state in ETS
    store_state_in_ets(State),

    %% Load persisted metadata if exists
    State2 = load_metadata_from_disk(State),

    {ok, State2}.

handle_call(get_metadata, _From, State) ->
    Metadata = #{
        workspace_id => State#state.workspace_id,
        project_path => State#state.project_path,
        package_name => State#state.package_name,
        created_at => State#state.created_at,
        last_activity => State#state.last_activity,
        node_name => State#state.node_name,
        repl_port => State#state.repl_port,
        supervised_actors => State#state.supervised_actors,
        loaded_modules => maps:keys(State#state.loaded_modules)
    },
    {reply, {ok, Metadata}, State};
handle_call(get_package_name, _From, State) ->
    {reply, State#state.package_name, State};
handle_call(get_last_activity, _From, State) ->
    {reply, {ok, State#state.last_activity}, State};
handle_call(supervised_actors, _From, State) ->
    {reply, {ok, State#state.supervised_actors}, State};
handle_call(loaded_modules, _From, State) ->
    {reply, {ok, maps:to_list(State#state.loaded_modules)}, State};
handle_call({get_class_source, ClassName}, _From, State) ->
    Result = maps:get(ClassName, State#state.class_sources, undefined),
    {reply, Result, State};
handle_call({set_class_source, ClassName, Source}, _From, State) ->
    Sources = State#state.class_sources,
    State2 = State#state{class_sources = Sources#{ClassName => Source}},
    store_state_in_ets(State2),
    {reply, ok, schedule_persist(State2)};
handle_call(get_file_mtimes, _From, State) ->
    {reply, {ok, State#state.file_mtimes}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(update_activity, State) ->
    Now = erlang:system_time(second),
    State2 = State#state{last_activity = Now},
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_cast({register_actor, Pid}, State) ->
    Actors = State#state.supervised_actors,
    State2 =
        case lists:member(Pid, Actors) of
            true ->
                State;
            false ->
                Ref = monitor(process, Pid),
                MonRefs = State#state.monitor_refs,
                State#state{
                    supervised_actors = [Pid | Actors],
                    monitor_refs = MonRefs#{Pid => Ref}
                }
        end,
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_cast({unregister_actor, Pid}, State) ->
    State2 = remove_actor(Pid, State),
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_cast({register_module, Module, NewSource}, State) ->
    Modules = State#state.loaded_modules,
    %% Preserve an existing non-undefined source if the new registration has none.
    EffectiveSource =
        case NewSource of
            undefined -> maps:get(Module, Modules, undefined);
            _ -> NewSource
        end,
    State2 = State#state{loaded_modules = Modules#{Module => EffectiveSource}},
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_cast({unregister_module, Module}, State) ->
    %% BT-1239: Remove a module when removeFromSystem is called.
    Modules = State#state.loaded_modules,
    State2 = State#state{loaded_modules = maps:remove(Module, Modules)},
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_cast({set_file_mtime, FilePath, Mtime}, State) ->
    Mtimes = State#state.file_mtimes,
    State2 = State#state{file_mtimes = Mtimes#{FilePath => Mtime}},
    store_state_in_ets(State2),
    {noreply, State2};
handle_cast(clear_file_mtimes, State) ->
    State2 = State#state{file_mtimes = #{}},
    store_state_in_ets(State2),
    {noreply, State2};
handle_cast({remove_file_mtime, FilePath}, State) ->
    Mtimes = State#state.file_mtimes,
    State2 = State#state{file_mtimes = maps:remove(FilePath, Mtimes)},
    store_state_in_ets(State2),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(persist_to_disk, State) ->
    persist_metadata_to_disk(State),
    {noreply, State#state{persist_timer = undefined}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Actor exited, remove from tracked list and monitor refs
    State2 = remove_actor(Pid, State),
    store_state_in_ets(State2),
    {noreply, schedule_persist(State2)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel any pending persist timer before shutdown
    case State#state.persist_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    %% Persist final state before shutting down
    persist_metadata_to_disk(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

-doc """
Schedule a debounced persist to disk.
Cancels any pending timer and resets the debounce window.
No-op in run mode (metadata_path = undefined).
""".
schedule_persist(#state{metadata_path = undefined} = State) ->
    State;
schedule_persist(#state{persist_timer = OldTimer} = State) ->
    case OldTimer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    NewRef = erlang:send_after(?PERSIST_DELAY_MS, self(), persist_to_disk),
    State#state{persist_timer = NewRef}.

-doc "Store state in ETS for fast read access".
store_state_in_ets(State) ->
    case ets:whereis(?WORKSPACE_META_TABLE) of
        % Table doesn't exist yet, skip
        undefined -> ok;
        _Tid -> ets:insert(?WORKSPACE_META_TABLE, {metadata, State})
    end.

-doc """
Load metadata from disk if available.
Skipped in run mode (metadata_path = undefined).
""".
load_metadata_from_disk(#state{metadata_path = undefined} = State) ->
    State;
load_metadata_from_disk(State) ->
    Path = State#state.metadata_path,
    case file:read_file(Path) of
        {ok, Binary} ->
            try json:decode(Binary) of
                Map ->
                    %% Do NOT restore supervised_actors from disk - PIDs are
                    %% not valid across node restarts. The monitor-based cleanup
                    %% handles tracking for the current session only.

                    %% Restore loaded_modules with source paths (atoms persist across restarts).
                    %% Handles both old format ([binary()]) and new format ([#{name,source}]).
                    ModulesRaw = maps:get(<<"loaded_modules">>, Map, []),
                    Modules =
                        case ModulesRaw of
                            ModList when is_list(ModList) -> ModList;
                            _ -> []
                        end,
                    ModuleAtoms = lists:filtermap(
                        fun
                            (Bin) when is_binary(Bin) ->
                                %% Old format: just the module name
                                case safe_existing_atom(Bin) of
                                    undefined -> false;
                                    Atom -> {true, {Atom, undefined}}
                                end;
                            (#{<<"name">> := NameBin} = Entry) ->
                                %% New format: #{name, source}
                                case safe_existing_atom(NameBin) of
                                    undefined ->
                                        false;
                                    Atom ->
                                        RawSource = maps:get(<<"source">>, Entry, null),
                                        Source =
                                            case RawSource of
                                                null -> undefined;
                                                _ -> normalize_source_path(RawSource)
                                            end,
                                        {true, {Atom, Source}}
                                end;
                            (_) ->
                                false
                        end,
                        Modules
                    ),

                    %% Restore timestamps and project path if present
                    CreatedAt =
                        case maps:get(<<"created_at">>, Map, State#state.created_at) of
                            CreatedAtValue when is_integer(CreatedAtValue) -> CreatedAtValue;
                            _ -> State#state.created_at
                        end,
                    LastActive =
                        case maps:get(<<"last_active">>, Map, State#state.last_activity) of
                            LastActiveValue when is_integer(LastActiveValue) -> LastActiveValue;
                            _ -> State#state.last_activity
                        end,
                    ProjectPath =
                        case maps:get(<<"project_path">>, Map, State#state.project_path) of
                            ProjectPathValue when is_binary(ProjectPathValue) -> ProjectPathValue;
                            _ -> State#state.project_path
                        end,

                    %% Restore class sources map (binary class name → source string).
                    ClassSourcesRaw = maps:get(<<"class_sources">>, Map, #{}),
                    ClassSources =
                        case is_map(ClassSourcesRaw) of
                            true ->
                                maps:fold(
                                    fun
                                        (K, V, Acc) when is_binary(K), is_binary(V) ->
                                            Acc#{K => binary_to_list(V)};
                                        (_, _, Acc) ->
                                            Acc
                                    end,
                                    #{},
                                    ClassSourcesRaw
                                );
                            false ->
                                #{}
                        end,

                    State#state{
                        project_path = ProjectPath,
                        created_at = CreatedAt,
                        last_activity = LastActive,
                        % Always start fresh
                        supervised_actors = [],
                        loaded_modules = maps:from_list(ModuleAtoms),
                        class_sources = ClassSources,
                        %% BT-1685: File mtimes always start fresh — files may
                        %% have changed between sessions, so first load-project
                        %% after restart always does a full load.
                        file_mtimes = #{}
                    }
            catch
                % Failed to parse, use default
                _:_ -> State
            end;
        {error, enoent} ->
            % No metadata file yet
            State;
        {error, _} ->
            % Failed to read, use default
            State
    end.

-doc """
Persist metadata to disk in JSON format.
Skipped in run mode (metadata_path = undefined — no workspace registration).
""".
persist_metadata_to_disk(#state{metadata_path = undefined}) ->
    ok;
persist_metadata_to_disk(State) ->
    Path = State#state.metadata_path,
    %% Ensure directory exists
    Dir = filename:dirname(Path),
    _ = filelib:ensure_dir(filename:join(Dir, "dummy")),

    %% Build JSON metadata (PIDs as strings, atoms as strings)
    Metadata = #{
        <<"workspace_id">> => State#state.workspace_id,
        <<"project_path">> =>
            case State#state.project_path of
                undefined -> null;
                PP -> PP
            end,
        <<"created_at">> => State#state.created_at,
        <<"last_active">> => State#state.last_activity,
        <<"node_name">> => atom_to_binary(State#state.node_name, utf8),
        <<"repl_port">> =>
            case State#state.repl_port of
                undefined -> null;
                Port -> Port
            end,
        <<"supervised_actors">> => [
            list_to_binary(pid_to_list(P))
         || P <- State#state.supervised_actors
        ],
        <<"loaded_modules">> => [
            #{
                <<"name">> => atom_to_binary(M, utf8),
                <<"source">> =>
                    case S of
                        undefined ->
                            null;
                        _ ->
                            case unicode:characters_to_binary(S) of
                                Bin when is_binary(Bin) -> Bin;
                                _ -> null
                            end
                    end
            }
         || M := S <- State#state.loaded_modules
        ],
        <<"class_sources">> => maps:fold(
            fun(ClassName, Source, Acc) ->
                SourceBin =
                    case unicode:characters_to_binary(Source) of
                        Bin when is_binary(Bin) -> Bin;
                        _ -> null
                    end,
                case SourceBin of
                    null -> Acc;
                    _ -> Acc#{ClassName => SourceBin}
                end
            end,
            #{},
            State#state.class_sources
        )
    },

    %% Write to disk
    Json = beamtalk_json:prettify_term(Metadata),
    case file:write_file(Path, Json) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to persist workspace metadata to ~s: ~p",
                [Path, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            {error, Reason}
    end.

-doc """
Remove an actor PID from the supervised list and cancel its monitor.
Used by both explicit unregister and DOWN message handling.
""".
-spec remove_actor(pid(), #state{}) -> #state{}.
remove_actor(Pid, State) ->
    MonRefs = State#state.monitor_refs,
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Pid, MonRefs) of
        {ok, Ref} -> demonitor(Ref, [flush]);
        error -> ok
    end,
    State#state{
        supervised_actors = lists:delete(Pid, State#state.supervised_actors),
        monitor_refs = maps:remove(Pid, MonRefs)
    }.

-doc """
Normalise a source path value to a string (list).
Accepts binary, list, or undefined. Returns undefined on any conversion failure.
""".
-spec normalize_source_path(term()) -> string() | undefined.
normalize_source_path(undefined) ->
    undefined;
normalize_source_path(B) when is_binary(B) ->
    case unicode:characters_to_list(B) of
        L when is_list(L) -> L;
        _ -> undefined
    end;
normalize_source_path(S) when is_list(S) ->
    S;
normalize_source_path(_) ->
    undefined.

safe_existing_atom(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> Atom
    catch
        _:_ -> undefined
    end.

-doc """
BT-775: Detect the package name from beamtalk.toml at the given project path.
Uses simple regex extraction — no TOML parser needed since we only need
the `name = "..."` field from the `[package]` section.
""".
-spec detect_package_name(binary() | undefined) -> binary() | undefined.
detect_package_name(undefined) ->
    undefined;
detect_package_name(ProjectPath) when is_binary(ProjectPath) ->
    ManifestPath = filename:join(binary_to_list(ProjectPath), "beamtalk.toml"),
    case file:read_file(ManifestPath) of
        {ok, Content} ->
            extract_package_name(Content);
        {error, _} ->
            undefined
    end.

-doc """
Extract the package name from beamtalk.toml content.
Matches `name = "..."` after `[package]` section header.
""".
-spec extract_package_name(binary()) -> binary() | undefined.
extract_package_name(Content) ->
    %% Find the [package] section and extract name = "value"
    case re:run(Content, <<"\\[package\\]">>, [{capture, none}]) of
        match ->
            %% Extract name = "value" (TOML only supports double-quoted strings)
            case
                re:run(
                    Content,
                    <<"name\\s*=\\s*\"([a-z][a-z0-9_]*)\"">>,
                    [{capture, [1], binary}]
                )
            of
                {match, [Name]} ->
                    Name;
                nomatch ->
                    undefined
            end;
        nomatch ->
            undefined
    end.
