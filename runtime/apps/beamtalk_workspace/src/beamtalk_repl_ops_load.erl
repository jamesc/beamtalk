%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for load-file, load-source, reload, unload, and modules operations.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_load).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, resolve_class_to_module/1, resolve_module_atoms/2]).

%% @doc Handle load/reload/unload/modules ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"load-file">>, Params, Msg, SessionPid) ->
    Path = binary_to_list(maps:get(<<"path">>, Params, <<>>)),
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            Warnings = collect_load_warnings(Classes),
            beamtalk_repl_protocol:encode_loaded(
                Classes, Msg, fun beamtalk_repl_json:term_to_json/1, Warnings
            );
        {error, Reason} ->
            WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end;
handle(<<"load-source">>, Params, Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty source">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter Beamtalk source code to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    Warnings = collect_load_warnings(Classes),
                    beamtalk_repl_protocol:encode_loaded(
                        Classes, Msg, fun beamtalk_repl_json:term_to_json/1, Warnings
                    );
                {error, Reason} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"reload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case maps:get(<<"path">>, Params, undefined) of
        undefined when ModuleBin =/= <<>> ->
            case beamtalk_repl_errors:safe_to_existing_atom(ModuleBin) of
                {ok, ModuleAtom} ->
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
                    case beamtalk_repl_modules:get_module_info(ModuleAtom, Tracker) of
                        {ok, Info} ->
                            case beamtalk_repl_modules:get_source_file(Info) of
                                undefined ->
                                    Err0 = beamtalk_error:new(no_source_file, 'Module'),
                                    Err1 = beamtalk_error:with_message(
                                        Err0,
                                        iolist_to_binary([
                                            <<"No source file recorded for module: ">>,
                                            ModuleBin
                                        ])
                                    ),
                                    Err2 = beamtalk_error:with_hint(
                                        Err1,
                                        <<"Use :load <path> to load it first.">>
                                    ),
                                    beamtalk_repl_protocol:encode_error(
                                        Err2,
                                        Msg,
                                        fun beamtalk_repl_json:format_error_message/1
                                    );
                                SourcePath ->
                                    do_reload(SourcePath, ModuleAtom, Msg, SessionPid)
                            end;
                        {error, not_found} ->
                            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                            Err1 = beamtalk_error:with_message(
                                Err0,
                                iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])
                            ),
                            Err2 = beamtalk_error:with_hint(
                                Err1,
                                <<"Use :load <path> to load it first.">>
                            ),
                            beamtalk_repl_protocol:encode_error(
                                Err2,
                                Msg,
                                fun beamtalk_repl_json:format_error_message/1
                            )
                    end;
                {error, badarg} ->
                    Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                    Err1 = beamtalk_error:with_message(
                        Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])
                    ),
                    Err2 = beamtalk_error:with_hint(
                        Err1,
                        <<"Use :load <path> to load it first.">>
                    ),
                    beamtalk_repl_protocol:encode_error(
                        Err2,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1
                    )
            end;
        undefined ->
            Err0 = beamtalk_error:new(missing_argument, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                <<"Missing module name for reload">>
            ),
            Err2 = beamtalk_error:with_hint(
                Err1,
                <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        Path ->
            PathStr = binary_to_list(Path),
            do_reload(PathStr, undefined, Msg, SessionPid)
    end;
handle(<<"unload">>, Params, Msg, SessionPid) ->
    %% BT-1239: Restore unload op — fully removes class from system (actors, gen_server,
    %% BEAM module, workspace_meta, session tracker).
    ClassNameBin = maps:get(<<"module">>, Params, <<>>),
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {error, badarg} ->
            Err0 = beamtalk_error:new(class_not_found, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary([<<"Class not found: '">>, ClassNameBin, <<"'">>])
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        {ok, ClassName} ->
            case beamtalk_runtime_api:remove_class_from_system(ClassName) of
                {ok, ModuleName} ->
                    %% Clean up workspace-level and session-level tracking.
                    beamtalk_workspace_meta:unregister_module(ModuleName),
                    beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName),
                    beamtalk_repl_protocol:encode_result(
                        iolist_to_binary([
                            <<"Class '">>, ClassNameBin, <<"' removed from system">>
                        ]),
                        Msg,
                        fun beamtalk_repl_json:term_to_json/1
                    );
                {error, Err} ->
                    beamtalk_repl_protocol:encode_error(
                        Err, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"modules">>, _Params, Msg, SessionPid) ->
    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
    %% BT-1239: Filter out stale tracker entries for modules purged via removeFromSystem.
    %% code:is_loaded/1 returns false for modules that have been code:delete'd.
    AllTrackedModules = beamtalk_repl_modules:list_modules(Tracker),
    TrackedModules = lists:filter(fun({N, _}) -> code:is_loaded(N) =/= false end, AllTrackedModules),
    RegistryPid = whereis(beamtalk_actor_registry),
    %% Build a module-atom → Beamtalk-class-name map so the UI shows class names,
    %% not BEAM module atoms. Without this, names like 'beamtalk_counter_v1_abc' appear
    %% instead of 'Counter', and the methods op fails because it looks up by class name.
    ModToClass = module_to_class_name_map(),
    ModulesWithInfo = lists:map(
        fun({ModName, ModInfo}) ->
            ActorCount = beamtalk_repl_modules:get_actor_count(ModName, RegistryPid, Tracker),
            Info0 = beamtalk_repl_modules:format_module_info(ModInfo, ActorCount),
            ClassName = maps:get(ModName, ModToClass, maps:get(name, Info0)),
            Info = Info0#{name => ClassName},
            {ModName, Info}
        end,
        TrackedModules
    ),
    %% Merge workspace-level classes (bootstrap-activated) that are not in the
    %% session tracker. These are loaded at startup via beamtalk_workspace_bootstrap
    %% and registered in workspace_meta but never added to any session tracker.
    WorkspaceExtra =
        case beamtalk_workspace_meta:loaded_modules() of
            {ok, WsMods} ->
                TrackedSet = maps:from_list([{N, true} || {N, _} <- TrackedModules]),
                lists:filtermap(
                    fun({ModName, SourcePath}) ->
                        case maps:is_key(ModName, TrackedSet) of
                            true ->
                                false;
                            false ->
                                %% BT-1239: Skip stale workspace_meta entries for modules
                                %% that have been purged (e.g. via removeFromSystem).
                                case code:is_loaded(ModName) of
                                    false ->
                                        false;
                                    _ ->
                                        ClassName = maps:get(
                                            ModName, ModToClass, atom_to_binary(ModName, utf8)
                                        ),
                                        ResolvedPath =
                                            case SourcePath of
                                                undefined -> resolve_source_path(ModName);
                                                _ -> SourcePath
                                            end,
                                        Info = #{
                                            name => ClassName,
                                            source_file => ResolvedPath,
                                            actor_count => 0,
                                            load_time => 0,
                                            time_ago => "startup"
                                        },
                                        {true, {ModName, Info}}
                                end
                        end
                    end,
                    WsMods
                );
            _ ->
                []
        end,
    beamtalk_repl_protocol:encode_modules(
        ModulesWithInfo ++ WorkspaceExtra, Msg, fun beamtalk_repl_json:term_to_json/1
    ).

%%% Internal helpers

%% @private
%% @doc Collect collision warnings for the loaded classes after a file load.
%% BT-737: Drains warnings from the ETS table keyed by class name and
%% formats them as human-readable binary strings for the protocol response.
-spec collect_load_warnings([map()]) -> [binary()].
collect_load_warnings(Classes) ->
    %% Use safe_to_existing_atom (not list_to_atom) to avoid leaking atoms for
    %% user-defined class names. Class atoms must already exist after loading.
    ClassNames = lists:filtermap(
        fun
            (#{name := N}) when N =/= "" ->
                case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(N)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            (_) ->
                false
        end,
        Classes
    ),
    Collisions = beamtalk_runtime_api:drain_class_warnings_by_names(ClassNames),
    [
        format_collision_warning(ClassName, OldModule, NewModule)
     || {ClassName, OldModule, NewModule} <- Collisions
    ].

%% @private
-spec format_collision_warning(atom(), atom(), atom()) -> binary().
format_collision_warning(ClassName, OldModule, NewModule) ->
    iolist_to_binary([
        "Class '",
        atom_to_binary(ClassName, utf8),
        "' redefined (was ",
        atom_to_binary(OldModule, utf8),
        ", now ",
        atom_to_binary(NewModule, utf8),
        ")"
    ]).

%% @private
-spec do_reload(string(), atom() | undefined, beamtalk_repl_protocol:protocol_msg(), pid()) ->
    binary().
do_reload(Path, ModuleAtom, Msg, SessionPid) ->
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            %% BT-737: Collect any cross-package collision warnings from this load.
            Warnings = collect_load_warnings(Classes),
            {ActorCount, MigrationFailures} =
                trigger_actor_code_change(ModuleAtom, Classes),
            beamtalk_repl_json:encode_reloaded(
                Classes, ActorCount, MigrationFailures, Msg, Warnings
            );
        {error, Reason} ->
            WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end.

%% @private
-spec trigger_actor_code_change(atom() | undefined, [map()]) ->
    {non_neg_integer(), [{pid(), term()}]}.
trigger_actor_code_change(ModuleAtom, Classes) ->
    ModuleAtoms = lists:usort(resolve_module_atoms(ModuleAtom, Classes)),
    case whereis(beamtalk_actor_registry) of
        undefined ->
            {0, []};
        RegistryPid ->
            {Count, FailsRev} = lists:foldl(
                fun(Mod, {CountAcc, FailAcc}) ->
                    case beamtalk_repl_actors:get_pids_for_module(RegistryPid, Mod) of
                        {ok, []} ->
                            {CountAcc, FailAcc};
                        {ok, Pids} ->
                            {ok, Upgraded, Failures} =
                                beamtalk_runtime_api:trigger_code_change(Mod, Pids),
                            NewFailAcc = lists:foldl(
                                fun(F, A) -> [F | A] end, FailAcc, Failures
                            ),
                            {CountAcc + Upgraded, NewFailAcc};
                        {error, _} ->
                            {CountAcc, FailAcc}
                    end
                end,
                {0, []},
                ModuleAtoms
            ),
            {Count, lists:reverse(FailsRev)}
    end.

%% @private
-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(
        fun(ClassMap) ->
            case maps:get(name, ClassMap, "") of
                "" ->
                    false;
                Name when is_list(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(Name)) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                Name when is_binary(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(Name) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                _ ->
                    false
            end
        end,
        Classes
    ).

%% @private
-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    resolve_class_to_module(ClassName, ClassPids).

resolve_class_to_module(ClassName, []) ->
    ClassName;
resolve_class_to_module(ClassName, [Pid | Rest]) ->
    try
        case beamtalk_runtime_api:class_name(Pid) of
            ClassName ->
                beamtalk_runtime_api:module_name(Pid);
            _ ->
                resolve_class_to_module(ClassName, Rest)
        end
    catch
        _:_ ->
            resolve_class_to_module(ClassName, Rest)
    end.

%% @private
%% @doc Resolve a source path for a module when workspace_meta has none.
%% Reads the beamtalk_source module attribute embedded by the compiler (BT-845/BT-860).
-spec resolve_source_path(atom()) -> string().
resolve_source_path(ModName) ->
    try
        Attrs = erlang:get_module_info(ModName, attributes),
        case proplists:get_value(beamtalk_source, Attrs) of
            [Path] when is_list(Path), Path =/= "" -> Path;
            _ -> "unknown"
        end
    catch
        _:_ -> "unknown"
    end.

%% @private
%% @doc Build a map from BEAM module atom to Beamtalk class name binary.
%% Queries all registered class processes in a single pass.
-spec module_to_class_name_map() -> #{atom() => binary()}.
module_to_class_name_map() ->
    Pids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    lists:foldl(
        fun(Pid, Acc) ->
            try
                ModAtom = beamtalk_runtime_api:module_name(Pid),
                ClassName = beamtalk_runtime_api:class_name(Pid),
                Acc#{ModAtom => atom_to_binary(ClassName, utf8)}
            catch
                _:_ -> Acc
            end
        end,
        #{},
        Pids
    ).
