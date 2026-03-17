%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Logging configuration API for the Beamtalk runtime.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% Provides functions to query and control OTP logger settings from
%%% Beamtalk code. BeamtalkInterface delegates to this module for
%%% log-level management, per-subsystem debug toggling, and logger
%%% introspection.
%%%
%%% Debug targets are tracked in an ETS table so that
%%% `activeDebugTargets/0` can report what is currently enabled.
%%%
%%% `enableDebug/1` is polymorphic on argument type:
%%% - **Symbol** (atom) — enables debug for a named subsystem
%%% - **Class reference** (`#beamtalk_object{}` with class tag) —
%%%   installs a primary logger filter for `beamtalk_class` metadata
%%% - **Actor instance** (`#beamtalk_object{}` with PID) —
%%%   sets per-process debug level (transient, lost on restart)

-module(beamtalk_logging_config).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    logLevel/0,
    logLevel/1,
    debugTargets/0,
    enableDebug/1,
    disableDebug/1,
    activeDebugTargets/0,
    disableAllDebug/0,
    loggerInfo/0
]).

%% Internal
-export([ensure_table/0]).

%% Exported for testing
-export([mcp_signal_path/0]).

-define(DEBUG_TABLE, beamtalk_debug_targets).

-define(VALID_LEVELS, [
    emergency, alert, critical, error, warning, notice, info, debug
]).

%%====================================================================
%% Subsystem registry
%%====================================================================

-spec subsystem_modules(atom()) -> {module_level, [module()]} | {domain, [atom()]} | {mcp_signal, []} | unknown.
subsystem_modules(actor) ->
    {module_level, [beamtalk_actor]};
subsystem_modules(supervisor) ->
    {module_level, [beamtalk_supervisor, beamtalk_actor_sup, beamtalk_runtime_sup]};
subsystem_modules(dispatch) ->
    {module_level, [beamtalk_dispatch, beamtalk_message_dispatch, beamtalk_class_dispatch]};
subsystem_modules(compiler) ->
    {module_level, [beamtalk_compiler_server, beamtalk_compiler_port, beamtalk_repl_compiler]};
subsystem_modules(workspace) ->
    {module_level, [beamtalk_workspace_sup, beamtalk_workspace_bootstrap]};
subsystem_modules(stdlib) ->
    {module_level, [beamtalk_stdlib, beamtalk_class_registry]};
subsystem_modules(hotreload) ->
    {module_level, [beamtalk_hot_reload]};
subsystem_modules(mcp) ->
    {mcp_signal, []};
subsystem_modules(runtime) ->
    {domain, [beamtalk, runtime]};
subsystem_modules(user) ->
    {domain, [beamtalk, user]};
subsystem_modules(_) ->
    unknown.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Return the current OTP primary log level as an atom.
-spec logLevel() -> atom().
logLevel() ->
    #{level := Level} = logger:get_primary_config(),
    Level.

%% @doc Set the OTP primary log level. Returns `nil` on success or a
%% `#beamtalk_error{}` for an invalid level.
-spec logLevel(atom()) -> nil | #beamtalk_error{}.
logLevel(Level) when is_atom(Level) ->
    case lists:member(Level, ?VALID_LEVELS) of
        true ->
            ok = logger:set_primary_config(level, Level),
            nil;
        false ->
            beamtalk_error:with_message(
                beamtalk_error:new(type_error, 'BeamtalkInterface', logLevel),
                iolist_to_binary(
                    io_lib:format(
                        "Invalid log level: ~p. Valid levels: ~p",
                        [Level, ?VALID_LEVELS]
                    )
                )
            )
    end;
logLevel(Level) ->
    beamtalk_error:with_message(
        beamtalk_error:new(type_error, 'BeamtalkInterface', logLevel),
        iolist_to_binary(
            io_lib:format(
                "Log level must be an atom, got: ~p",
                [Level]
            )
        )
    ).

%% @doc Return the list of available debug target symbols.
-spec debugTargets() -> [atom()].
debugTargets() ->
    [actor, supervisor, dispatch, compiler, workspace, stdlib, hotreload, mcp, runtime, user].

%% @doc Enable debug logging for a subsystem, class, or actor instance.
%%
%% Accepts three argument types:
%% - Atom (symbol): enables debug for a named subsystem
%% - Class reference (#beamtalk_object with class tag): installs a primary
%%   logger filter matching #{beamtalk_class => ClassName} metadata and sets
%%   module level on the compiled class module
%% - Actor instance (#beamtalk_object with PID): sets per-process debug level
%%   (transient — lost on actor restart)
-spec enableDebug(term()) -> nil | #beamtalk_error{}.
enableDebug(Target) when is_atom(Target) ->
    case subsystem_modules(Target) of
        {module_level, Modules} ->
            ensure_table(),
            lists:foreach(fun(M) -> logger:set_module_level(M, debug) end, Modules),
            FilterIds =
                case Target of
                    supervisor -> enable_supervisor_progress();
                    _ -> []
                end,
            ets:insert(?DEBUG_TABLE, {Target, subsystem, FilterIds}),
            nil;
        {mcp_signal, _} ->
            ensure_table(),
            case write_mcp_signal_file() of
                ok ->
                    ets:insert(?DEBUG_TABLE, {Target, subsystem, []}),
                    nil;
                {error, Reason} ->
                    beamtalk_error:with_message(
                        beamtalk_error:new(runtime_error, 'BeamtalkInterface', enableDebug),
                        iolist_to_binary(
                            io_lib:format(
                                "Failed to write MCP debug signal file: ~p",
                                [Reason]
                            )
                        )
                    )
            end;
        {domain, Domain} ->
            ensure_table(),
            FilterId = list_to_atom("beamtalk_debug_domain_" ++ atom_to_list(Target)),
            FilterFun = fun(LogEvent, _Extra) ->
                case LogEvent of
                    #{meta := #{domain := D}} ->
                        case lists:prefix(Domain, D) of
                            true -> LogEvent;
                            false -> ignore
                        end;
                    _ ->
                        ignore
                end
            end,
            %% Add a handler with a domain filter so debug-level messages
            %% from this domain reach the default handler.
            logger:add_handler(FilterId, logger_std_h, #{
                level => debug,
                filter_default => stop,
                filters => [{FilterId, {FilterFun, []}}],
                config => #{type => standard_io}
            }),
            ets:insert(?DEBUG_TABLE, {Target, subsystem, [FilterId]}),
            nil;
        unknown ->
            beamtalk_error:with_message(
                beamtalk_error:new(type_error, 'BeamtalkInterface', enableDebug),
                iolist_to_binary(
                    io_lib:format(
                        "Unknown debug target: ~p. Available: ~p",
                        [Target, debugTargets()]
                    )
                )
            )
    end;
enableDebug(Target) ->
    case detect_target_type(Target) of
        {class, ClassName} ->
            enable_class_debug(ClassName);
        {actor, Pid, ClassName} ->
            enable_actor_debug(Pid, ClassName);
        unknown ->
            beamtalk_error:with_message(
                beamtalk_error:new(type_error, 'BeamtalkInterface', enableDebug),
                iolist_to_binary(
                    io_lib:format(
                        "enableDebug: expects a symbol, class, or actor, got: ~p",
                        [Target]
                    )
                )
            )
    end.

%% @doc Disable debug logging for a subsystem, class, or actor instance.
-spec disableDebug(term()) -> nil | #beamtalk_error{}.
disableDebug(Target) when is_atom(Target) ->
    case subsystem_modules(Target) of
        {module_level, Modules} ->
            ensure_table(),
            lists:foreach(fun(M) -> logger:unset_module_level(M) end, Modules),
            case Target of
                supervisor -> disable_supervisor_progress();
                _ -> ok
            end,
            ets:delete(?DEBUG_TABLE, Target),
            nil;
        {mcp_signal, _} ->
            ensure_table(),
            remove_mcp_signal_file(),
            ets:delete(?DEBUG_TABLE, Target),
            nil;
        {domain, _Domain} ->
            ensure_table(),
            case ets:lookup(?DEBUG_TABLE, Target) of
                [{Target, subsystem, FilterIds}] ->
                    lists:foreach(fun(Id) -> logger:remove_handler(Id) end, FilterIds),
                    ets:delete(?DEBUG_TABLE, Target),
                    nil;
                [] ->
                    nil
            end;
        unknown ->
            beamtalk_error:with_message(
                beamtalk_error:new(type_error, 'BeamtalkInterface', disableDebug),
                iolist_to_binary(
                    io_lib:format(
                        "Unknown debug target: ~p. Available: ~p",
                        [Target, debugTargets()]
                    )
                )
            )
    end;
disableDebug(Target) ->
    case detect_target_type(Target) of
        {class, ClassName} ->
            disable_class_debug(ClassName);
        {actor, Pid, _ClassName} ->
            disable_actor_debug(Pid);
        unknown ->
            beamtalk_error:with_message(
                beamtalk_error:new(type_error, 'BeamtalkInterface', disableDebug),
                iolist_to_binary(
                    io_lib:format(
                        "disableDebug: expects a symbol, class, or actor, got: ~p",
                        [Target]
                    )
                )
            )
    end.

%% @doc Return the list of currently enabled debug targets.
%%
%% Returns a mixed list:
%% - Atoms for subsystem targets (e.g. `actor`, `compiler`)
%% - `{class, ClassName}` tuples for class debug targets
%% - `{actor, Pid, ClassName}` tuples for per-actor debug targets
-spec activeDebugTargets() -> list().
activeDebugTargets() ->
    ensure_table(),
    lists:map(
        fun
            ({Name, subsystem, _Ids}) ->
                Name;
            ({ClassName, user_class, _Ids}) ->
                {class, ClassName};
            ({Pid, actor_instance, {ClassName, _FilterIds}}) ->
                {actor, Pid, ClassName}
        end,
        ets:tab2list(?DEBUG_TABLE)
    ).

%% @doc Disable all debug targets and clear module-level overrides.
-spec disableAllDebug() -> nil.
disableAllDebug() ->
    ensure_table(),
    Targets = ets:tab2list(?DEBUG_TABLE),
    lists:foreach(
        fun
            ({Target, subsystem, FilterIds}) ->
                case subsystem_modules(Target) of
                    {module_level, Modules} ->
                        lists:foreach(fun(M) -> logger:unset_module_level(M) end, Modules),
                        case Target of
                            supervisor -> disable_supervisor_progress();
                            _ -> ok
                        end;
                    {mcp_signal, _} ->
                        remove_mcp_signal_file();
                    {domain, _Domain} ->
                        lists:foreach(fun(Id) -> logger:remove_handler(Id) end, FilterIds);
                    unknown ->
                        ok
                end;
            ({ClassName, user_class, FilterIds}) ->
                lists:foreach(fun(Id) -> logger:remove_primary_filter(Id) end, FilterIds),
                case beamtalk_class_module_table:lookup(ClassName) of
                    {ok, Module} -> logger:unset_module_level(Module);
                    not_found -> ok
                end;
            ({_Pid, actor_instance, {_ClassName, FilterIds}}) ->
                lists:foreach(fun(Id) -> logger:remove_primary_filter(Id) end, FilterIds)
        end,
        Targets
    ),
    ets:delete_all_objects(?DEBUG_TABLE),
    nil.

%% @doc Return a formatted string describing the current logger state.
-spec loggerInfo() -> binary().
loggerInfo() ->
    Level = logLevel(),
    Active = activeDebugTargets(),
    ActiveStr =
        case Active of
            [] ->
                <<"  (none)">>;
            _ ->
                iolist_to_binary(
                    lists:map(
                        fun(T) ->
                            format_debug_target(T)
                        end,
                        Active
                    )
                )
        end,
    LogFile = find_log_file(),
    iolist_to_binary(
        io_lib:format(
            "Log level: ~s\nFormat: text\nLog file: ~s\nActive debug targets:\n~s",
            [Level, LogFile, ActiveStr]
        )
    ).

%%====================================================================
%% Internal helpers — target type detection
%%====================================================================

%% @doc Detect whether a non-atom target is a class reference or actor instance.
-spec detect_target_type(term()) -> {class, atom()} | {actor, pid(), atom()} | unknown.
detect_target_type(#beamtalk_object{class = ClassTag, pid = Pid}) ->
    case beamtalk_class_registry:is_class_name(ClassTag) of
        true ->
            %% Class reference — strip " class" suffix to get the actual class name
            ClassStr = atom_to_list(ClassTag),
            ClassName = list_to_atom(lists:sublist(ClassStr, length(ClassStr) - 6)),
            {class, ClassName};
        false ->
            %% Actor instance — has a PID and a plain class name
            {actor, Pid, ClassTag}
    end;
detect_target_type(_) ->
    unknown.

%%====================================================================
%% Internal helpers — class debug
%%====================================================================

%% @doc Enable debug logging for a user class.
%%
%% Installs a primary logger filter that allows debug events where
%% `#{beamtalk_class => ClassName}` metadata matches. Also sets module
%% level on the compiled class module for direct `?LOG_*` calls.
-spec enable_class_debug(atom()) -> nil | #beamtalk_error{}.
enable_class_debug(ClassName) ->
    ensure_table(),
    FilterId = list_to_atom("beamtalk_debug_class_" ++ atom_to_list(ClassName)),
    FilterFun = fun(LogEvent, _Extra) ->
        case LogEvent of
            #{meta := #{beamtalk_class := C}} when C =:= ClassName ->
                LogEvent;
            _ ->
                ignore
        end
    end,
    logger:add_primary_filter(FilterId, {FilterFun, []}),
    %% Also set module level on the compiled class module
    case beamtalk_class_module_table:lookup(ClassName) of
        {ok, Module} ->
            logger:set_module_level(Module, debug);
        not_found ->
            ok
    end,
    ets:insert(?DEBUG_TABLE, {ClassName, user_class, [FilterId]}),
    nil.

%% @doc Disable debug logging for a user class.
-spec disable_class_debug(atom()) -> nil.
disable_class_debug(ClassName) ->
    ensure_table(),
    case ets:lookup(?DEBUG_TABLE, ClassName) of
        [{ClassName, user_class, FilterIds}] ->
            lists:foreach(fun(Id) -> logger:remove_primary_filter(Id) end, FilterIds),
            case beamtalk_class_module_table:lookup(ClassName) of
                {ok, Module} -> logger:unset_module_level(Module);
                not_found -> ok
            end,
            ets:delete(?DEBUG_TABLE, ClassName),
            nil;
        [] ->
            nil
    end.

%%====================================================================
%% Internal helpers — actor debug
%%====================================================================

%% @doc Enable debug logging for a specific actor process.
%%
%% Installs a primary logger filter that allows debug events from
%% the actor's PID. This is transient — lost when the actor restarts.
-spec enable_actor_debug(pid(), atom()) -> nil.
enable_actor_debug(Pid, ClassName) ->
    ensure_table(),
    FilterId = list_to_atom(
        "beamtalk_debug_actor_" ++ pid_to_list(Pid)
    ),
    FilterFun = fun(LogEvent, _Extra) ->
        case LogEvent of
            #{meta := #{pid := P}} when P =:= Pid ->
                LogEvent;
            _ ->
                ignore
        end
    end,
    logger:add_primary_filter(FilterId, {FilterFun, []}),
    ets:insert(?DEBUG_TABLE, {Pid, actor_instance, {ClassName, [FilterId]}}),
    nil.

%% @doc Disable debug logging for a specific actor process.
-spec disable_actor_debug(pid()) -> nil.
disable_actor_debug(Pid) ->
    ensure_table(),
    case ets:lookup(?DEBUG_TABLE, Pid) of
        [{Pid, actor_instance, {_ClassName, FilterIds}}] ->
            lists:foreach(fun(Id) -> logger:remove_primary_filter(Id) end, FilterIds),
            ets:delete(?DEBUG_TABLE, Pid),
            nil;
        [] ->
            nil
    end.

%%====================================================================
%% Internal helpers — formatting
%%====================================================================

%% @doc Format a debug target entry for loggerInfo output.
-spec format_debug_target(term()) -> iodata().
format_debug_target({class, ClassName}) ->
    [<<"  ">>, atom_to_binary(ClassName, utf8), <<" (class) -> metadata filter\n">>];
format_debug_target({actor, Pid, ClassName}) ->
    PidBin = list_to_binary(pid_to_list(Pid)),
    [
        <<"  ">>,
        atom_to_binary(ClassName, utf8),
        <<" actor (">>,
        PidBin,
        <<") -> process filter\n">>
    ];
format_debug_target(T) when is_atom(T) ->
    ModulesStr =
        case subsystem_modules(T) of
            {module_level, Ms} ->
                iolist_to_binary(
                    lists:join(
                        <<", ">>,
                        [atom_to_binary(M, utf8) || M <- Ms]
                    )
                );
            {mcp_signal, _} ->
                <<"signal file (cross-process)">>;
            {domain, D} ->
                iolist_to_binary(
                    io_lib:format("domain=~p", [D])
                );
            unknown ->
                <<"(unknown)">>
        end,
    [<<"  ">>, atom_to_binary(T, utf8), <<" -> ">>, ModulesStr, <<"\n">>].

%%====================================================================
%% Internal helpers — ETS and logger
%%====================================================================

%% @doc Ensure the debug-targets ETS table exists. Creates it on first access.
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?DEBUG_TABLE) of
        undefined ->
            try
                ets:new(?DEBUG_TABLE, [set, named_table, public]),
                ok
            catch
                error:badarg ->
                    %% Another process created it between our check and the new call.
                    ok
            end;
        _Tid ->
            ok
    end.

%% @doc Enable OTP progress reports for the supervisor subsystem by removing
%% the default progress-report filter.
-spec enable_supervisor_progress() -> [atom()].
enable_supervisor_progress() ->
    case logger:get_handler_config(default) of
        {ok, #{filters := Filters}} ->
            case lists:keyfind(otp_progress, 1, Filters) of
                false ->
                    [];
                _ ->
                    logger:remove_handler_filter(default, otp_progress),
                    [otp_progress]
            end;
        _ ->
            []
    end.

%% @doc Re-install the OTP progress report filter on the default handler.
-spec disable_supervisor_progress() -> ok.
disable_supervisor_progress() ->
    logger:add_handler_filter(
        default,
        otp_progress,
        {fun logger_filters:progress/2, stop}
    ),
    ok.

%% @doc Try to find the current log file path from the default handler config.
-spec find_log_file() -> binary().
find_log_file() ->
    case logger:get_handler_config(default) of
        {ok, #{config := #{file := File}}} ->
            iolist_to_binary(File);
        _ ->
            <<"(standard_io)">>
    end.

%%====================================================================
%% Internal helpers — MCP debug signal file
%%====================================================================

%% @doc Return the path to the MCP debug signal file for the current workspace.
%%
%% The signal file lives at `~/.beamtalk/workspaces/{id}/mcp_debug_enabled`.
%% The MCP server (Rust process) watches for this file and adjusts its
%% tracing filter level accordingly.
-spec mcp_signal_path() -> {ok, string()} | {error, term()}.
mcp_signal_path() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{workspace_id := WorkspaceId}} ->
            case beamtalk_platform:home_dir() of
                false ->
                    CacheDir = filename:basedir(user_cache, "beamtalk"),
                    {ok,
                        filename:join([
                            CacheDir,
                            "workspaces",
                            binary_to_list(WorkspaceId),
                            "mcp_debug_enabled"
                        ])};
                Home ->
                    {ok,
                        filename:join([
                            Home,
                            ".beamtalk",
                            "workspaces",
                            binary_to_list(WorkspaceId),
                            "mcp_debug_enabled"
                        ])}
            end;
        {error, _} ->
            {error, workspace_not_started}
    end.

%% @doc Write the MCP debug signal file to enable debug logging in the MCP server.
-spec write_mcp_signal_file() -> ok | {error, term()}.
write_mcp_signal_file() ->
    case mcp_signal_path() of
        {ok, Path} ->
            ok = filelib:ensure_dir(Path),
            case file:write_file(Path, <<"debug\n">>) of
                ok ->
                    ?LOG_INFO(#{
                        msg => "MCP debug signal file written",
                        path => Path
                    }),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        msg => "Failed to write MCP debug signal file",
                        path => Path,
                        reason => Reason
                    }),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Remove the MCP debug signal file to disable debug logging in the MCP server.
-spec remove_mcp_signal_file() -> ok.
remove_mcp_signal_file() ->
    case mcp_signal_path() of
        {ok, Path} ->
            case file:delete(Path) of
                ok ->
                    ?LOG_INFO(#{
                        msg => "MCP debug signal file removed",
                        path => Path
                    }),
                    ok;
                {error, enoent} ->
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING(#{
                        msg => "Failed to remove MCP debug signal file",
                        path => Path,
                        reason => Reason
                    }),
                    ok
            end;
        {error, _} ->
            ok
    end.
