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

-define(DEBUG_TABLE, beamtalk_debug_targets).

-define(VALID_LEVELS, [
    emergency, alert, critical, error, warning, notice, info, debug
]).

%%====================================================================
%% Subsystem registry
%%====================================================================

-spec subsystem_modules(atom()) -> {module_level, [module()]} | {domain, [atom()]} | unknown.
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
    [actor, supervisor, dispatch, compiler, workspace, stdlib, hotreload, runtime, user].

%% @doc Enable debug logging for a named subsystem.
%% Returns `nil` on success or `#beamtalk_error{}` for unknown targets.
-spec enableDebug(atom()) -> nil | #beamtalk_error{}.
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
    beamtalk_error:with_message(
        beamtalk_error:new(type_error, 'BeamtalkInterface', enableDebug),
        iolist_to_binary(
            io_lib:format("Debug target must be an atom, got: ~p", [Target])
        )
    ).

%% @doc Disable debug logging for a named subsystem.
%% Returns `nil` on success or `#beamtalk_error{}` for unknown targets.
-spec disableDebug(atom()) -> nil | #beamtalk_error{}.
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
    beamtalk_error:with_message(
        beamtalk_error:new(type_error, 'BeamtalkInterface', disableDebug),
        iolist_to_binary(
            io_lib:format("Debug target must be an atom, got: ~p", [Target])
        )
    ).

%% @doc Return the list of currently enabled debug targets.
-spec activeDebugTargets() -> [atom()].
activeDebugTargets() ->
    ensure_table(),
    [Name || {Name, _Type, _Ids} <- ets:tab2list(?DEBUG_TABLE)].

%% @doc Disable all debug targets and clear module-level overrides.
-spec disableAllDebug() -> nil.
disableAllDebug() ->
    ensure_table(),
    Targets = ets:tab2list(?DEBUG_TABLE),
    lists:foreach(
        fun({Target, _Type, FilterIds}) ->
            case subsystem_modules(Target) of
                {module_level, Modules} ->
                    lists:foreach(fun(M) -> logger:unset_module_level(M) end, Modules),
                    case Target of
                        supervisor -> disable_supervisor_progress();
                        _ -> ok
                    end;
                {domain, _Domain} ->
                    lists:foreach(fun(Id) -> logger:remove_handler(Id) end, FilterIds);
                unknown ->
                    ok
            end
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
                            ModulesStr =
                                case subsystem_modules(T) of
                                    {module_level, Ms} ->
                                        iolist_to_binary(
                                            lists:join(
                                                <<", ">>,
                                                [atom_to_binary(M, utf8) || M <- Ms]
                                            )
                                        );
                                    {domain, D} ->
                                        iolist_to_binary(
                                            io_lib:format("domain=~p", [D])
                                        );
                                    unknown ->
                                        <<"(unknown)">>
                                end,
                            io_lib:format("  ~s → ~s\n", [T, ModulesStr])
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
%% Internal helpers
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
