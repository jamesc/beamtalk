%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for load-file, load-source, reload, unload, and modules operations.
%%%
%%% **DDD Context:** REPL
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
            beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun beamtalk_repl_json:term_to_json/1);
        {error, Reason} ->
            WrappedReason = beamtalk_repl_server:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end;

handle(<<"load-source">>, Params, Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty source">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter Beamtalk source code to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun beamtalk_repl_json:term_to_json/1);
                {error, Reason} ->
                    WrappedReason = beamtalk_repl_server:ensure_structured_error(Reason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
            end
    end;

handle(<<"reload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case maps:get(<<"path">>, Params, undefined) of
        undefined when ModuleBin =/= <<>> ->
            case beamtalk_repl_server:safe_to_existing_atom(ModuleBin) of
                {ok, ModuleAtom} ->
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
                    case beamtalk_repl_modules:get_module_info(ModuleAtom, Tracker) of
                        {ok, Info} ->
                            case beamtalk_repl_modules:get_source_file(Info) of
                                undefined ->
                                    Err0 = beamtalk_error:new(no_source_file, 'Module'),
                                    Err1 = beamtalk_error:with_message(Err0,
                                        iolist_to_binary([<<"No source file recorded for module: ">>,
                                                          ModuleBin])),
                                    Err2 = beamtalk_error:with_hint(Err1,
                                        <<"Use :load <path> to load it first.">>),
                                    beamtalk_repl_protocol:encode_error(
                                        Err2, Msg,
                                        fun beamtalk_repl_json:format_error_message/1);
                                SourcePath ->
                                    do_reload(SourcePath, ModuleAtom, Msg, SessionPid)
                            end;
                        {error, not_found} ->
                            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                            Err1 = beamtalk_error:with_message(Err0,
                                iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                            Err2 = beamtalk_error:with_hint(Err1,
                                <<"Use :load <path> to load it first.">>),
                            beamtalk_repl_protocol:encode_error(
                                Err2, Msg,
                                fun beamtalk_repl_json:format_error_message/1)
                    end;
                {error, badarg} ->
                    Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                    Err1 = beamtalk_error:with_message(Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                    Err2 = beamtalk_error:with_hint(Err1,
                        <<"Use :load <path> to load it first.">>),
                    beamtalk_repl_protocol:encode_error(
                        Err2, Msg,
                        fun beamtalk_repl_json:format_error_message/1)
            end;
        undefined ->
            Err0 = beamtalk_error:new(missing_argument, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0,
                <<"Missing module name for reload">>),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        Path ->
            PathStr = binary_to_list(Path),
            do_reload(PathStr, undefined, Msg, SessionPid)
    end;

handle(<<"modules">>, _Params, Msg, SessionPid) ->
    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
    TrackedModules = beamtalk_repl_modules:list_modules(Tracker),
    RegistryPid = whereis(beamtalk_actor_registry),
    ModulesWithInfo = lists:map(
        fun({ModName, ModInfo}) ->
            ActorCount = beamtalk_repl_modules:get_actor_count(ModName, RegistryPid, Tracker),
            Info = beamtalk_repl_modules:format_module_info(ModInfo, ActorCount),
            {ModName, Info}
        end,
        TrackedModules
    ),
    beamtalk_repl_protocol:encode_modules(ModulesWithInfo, Msg, fun beamtalk_repl_json:term_to_json/1);

handle(<<"unload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case ModuleBin of
        <<>> ->
            Err0 = beamtalk_error:new(missing_argument, 'Module'),
            Err1 = beamtalk_error:with_message(Err0,
                <<"Missing module name for unload">>),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Usage: :unload <ModuleName>">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_server:safe_to_existing_atom(ModuleBin) of
                {error, badarg} ->
                    Err0 = beamtalk_error:new(module_not_found, 'Module'),
                    Err1 = beamtalk_error:with_message(Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                    Err2 = beamtalk_error:with_hint(Err1, <<"Module was never loaded">>),
                    beamtalk_repl_protocol:encode_error(
                        Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
                {ok, Module} ->
                    ResolvedModule = case code:is_loaded(Module) of
                        {file, _} -> Module;
                        false -> resolve_class_to_module(Module)
                    end,
                    case beamtalk_repl_shell:unload_module(SessionPid, ResolvedModule) of
                        ok ->
                            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
                        {error, #beamtalk_error{} = Err} ->
                            beamtalk_repl_protocol:encode_error(
                                Err, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end
            end
    end.

%%% Internal helpers

%% @private
-spec do_reload(string(), atom() | undefined, beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
do_reload(Path, ModuleAtom, Msg, SessionPid) ->
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            {ActorCount, MigrationFailures} =
                trigger_actor_code_change(ModuleAtom, Classes),
            beamtalk_repl_json:encode_reloaded(Classes, ActorCount, MigrationFailures, Msg);
        {error, Reason} ->
            WrappedReason = beamtalk_repl_server:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end.

%% @private
-spec trigger_actor_code_change(atom() | undefined, [map()]) ->
    {non_neg_integer(), [{pid(), term()}]}.
trigger_actor_code_change(ModuleAtom, Classes) ->
    ModuleAtoms = lists:usort(resolve_module_atoms(ModuleAtom, Classes)),
    case whereis(beamtalk_actor_registry) of
        undefined -> {0, []};
        RegistryPid ->
            {Count, FailsRev} = lists:foldl(fun(Mod, {CountAcc, FailAcc}) ->
                case beamtalk_repl_actors:get_pids_for_module(RegistryPid, Mod) of
                    {ok, []} ->
                        {CountAcc, FailAcc};
                    {ok, Pids} ->
                        {ok, Upgraded, Failures} =
                            beamtalk_hot_reload:trigger_code_change(Mod, Pids),
                        NewFailAcc = lists:foldl(
                            fun(F, A) -> [F | A] end, FailAcc, Failures),
                        {CountAcc + Upgraded, NewFailAcc};
                    {error, _} ->
                        {CountAcc, FailAcc}
                end
            end, {0, []}, ModuleAtoms),
            {Count, lists:reverse(FailsRev)}
    end.

%% @private
-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(fun(ClassMap) ->
        case maps:get(name, ClassMap, "") of
            "" -> false;
            Name when is_list(Name) ->
                case beamtalk_repl_server:safe_to_existing_atom(list_to_binary(Name)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            Name when is_binary(Name) ->
                case beamtalk_repl_server:safe_to_existing_atom(Name) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            _ -> false
        end
    end, Classes).

%% @private
-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids = try beamtalk_class_registry:all_classes()
                catch _:_ -> [] end,
    resolve_class_to_module(ClassName, ClassPids).

resolve_class_to_module(ClassName, []) ->
    ClassName;
resolve_class_to_module(ClassName, [Pid | Rest]) ->
    try
        case gen_server:call(Pid, class_name, 1000) of
            ClassName ->
                gen_server:call(Pid, module_name, 1000);
            _ ->
                resolve_class_to_module(ClassName, Rest)
        end
    catch _:_ ->
        resolve_class_to_module(ClassName, Rest)
    end.
