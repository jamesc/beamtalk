%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bootstrap worker for singleton class variables (ADR 0019 Phase 2).
%%%
%%% Sets class variables on singleton stdlib classes after workspace supervisor
%%% starts the singleton actors. Monitors singleton PIDs and re-sets class
%%% variables when children restart.
%%%
%%% Also activates compiled project modules from `_build/dev/ebin/` at startup
%%% (BT-739). When a project path is provided, scans that directory for
%%% `bt@*.beam` modules (excluding `bt@stdlib@*`) and calls `register_class/0`
%%% on each, making them visible in the class registry without requiring `:load`.
%%%
%%% Singleton mapping derived from beamtalk_workspace_config:singletons/0.
%%%
%%% **DDD Context:** Workspace Context

-module(beamtalk_workspace_bootstrap).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).
-export([find_bt_modules_in_dir/1, activate_project_modules/1]).
-export([sort_modules_by_dependency/2]).
-ifdef(TEST).
-export([is_valid_module_name/1]).
-endif.

%% Maximum number of project modules activated per startup (BT-747).
%% Prevents atom table exhaustion if _build/dev/ebin/ contains excessive files.
-define(MAX_PROJECT_MODULES, 1000).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-record(state, {
    monitors = #{} :: #{reference() => {ClassName :: atom(), RegName :: atom()}}
}).

%% @doc Start the bootstrap worker without project module activation.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(undefined).

%% @doc Start the bootstrap worker.
%% When ProjectPath is a binary path to a project root, compiled modules from
%% `{ProjectPath}/_build/dev/ebin/` matching `bt@*` (excluding `bt@stdlib@*`)
%% are activated after singleton bootstrap. Pass `undefined` to skip activation.
-spec start_link(binary() | undefined) -> {ok, pid()} | {error, term()}.
start_link(ProjectPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ProjectPath], []).

%% @private
init([ProjectPath]) ->
    %% Create the user-bindings ETS table here so that this long-lived process
    %% owns it. If created inside a short-lived eval worker instead, the table
    %% would be deleted when that worker exits (ETS tables are deleted on owner
    %% process exit unless an heir is specified).
    beamtalk_workspace_interface_primitives:create_bindings_table(),
    %% Ensure beamtalk_interface is loaded so that all its exported
    %% function names (e.g. findClass, allClasses) are in the atom table.
    %% The sealed-Object BeamtalkInterface dispatches via beamtalk_message_dispatch
    %% which uses list_to_existing_atom to resolve selector→function name; if
    %% the module is not yet loaded, the atom won't exist and dispatch fails.
    _ = code:ensure_loaded(beamtalk_interface),
    State = bootstrap_all(#state{}),
    %% Activate project modules synchronously before returning so that
    %% beamtalk_repl_server (the next child) does not write the port file
    %% until all compiled project classes are registered and visible.
    %% The gen_server name is registered by OTP before init/1 is called, so
    %% any DOWN signals from monitored singletons that arrive during activation
    %% safely queue in the mailbox and are handled immediately after we return.
    activate_project_modules(ProjectPath),
    {ok, State}.

%% @private
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    case maps:get(MonRef, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        {ClassName, RegName} ->
            Monitors = maps:remove(MonRef, State#state.monitors),
            NewState = State#state{monitors = Monitors},
            %% Re-bootstrap after a short delay to allow the supervisor
            %% to restart the child process
            erlang:send_after(100, self(), {rebootstrap, ClassName, RegName, 0}),
            {noreply, NewState}
    end;
handle_info({rebootstrap, ClassName, RegName, Retries}, State) when Retries < 5 ->
    case erlang:whereis(RegName) of
        undefined ->
            erlang:send_after(200, self(), {rebootstrap, ClassName, RegName, Retries + 1}),
            {noreply, State};
        _Pid ->
            NewState = bootstrap_singleton(ClassName, RegName, State),
            {noreply, NewState}
    end;
handle_info({rebootstrap, ClassName, RegName, _Retries}, State) ->
    ?LOG_ERROR("Bootstrap: failed to rewire singleton after retries", #{
        class => ClassName, name => RegName
    }),
    {noreply, State};
handle_info({rebootstrap_value, ClassName, Module, Retries}, State) when Retries < 5 ->
    bootstrap_value_singleton(ClassName, Module, Retries),
    {noreply, State};
handle_info({rebootstrap_value, ClassName, _Module, _Retries}, State) ->
    ?LOG_ERROR("Bootstrap: failed to wire value singleton after retries", #{
        class => ClassName
    }),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%% Internal functions

%% @private Bootstrap all singletons.
bootstrap_all(State) ->
    ActorState = lists:foldl(
        fun(#{class_name := ClassName, binding_name := RegName}, AccState) ->
            bootstrap_singleton(ClassName, RegName, AccState)
        end,
        State,
        beamtalk_workspace_config:singletons()
    ),
    %% Bootstrap value singletons (sealed Object subclass:, no gen_server process).
    %% Create a value object instance and set the class variable `current`.
    lists:foreach(
        fun(#{class_name := ClassName, module := Module}) ->
            bootstrap_value_singleton(ClassName, Module, 0)
        end,
        beamtalk_workspace_config:value_singletons()
    ),
    ActorState.

%% @private Bootstrap a single singleton: set class var and monitor.
bootstrap_singleton(ClassName, RegName, State) ->
    case erlang:whereis(RegName) of
        undefined ->
            ?LOG_WARNING("Bootstrap: singleton not registered yet", #{name => RegName}),
            State;
        Pid ->
            Obj = build_object_ref(ClassName, Pid),
            _ = set_class_variable(ClassName, Obj),
            MonRef = erlang:monitor(process, Pid),
            ?LOG_DEBUG("Bootstrap: wired singleton", #{class => ClassName, pid => Pid}),
            Monitors = maps:put(MonRef, {ClassName, RegName}, State#state.monitors),
            State#state{monitors = Monitors}
    end.

%% @private Bootstrap a value singleton: create a tagged-map instance and set class var.
%% Value singletons are sealed Object subclasses (no gen_server process). The
%% instance is created by calling `Module:new()` and set as the `current` class var.
%% Schedules a retry (via `rebootstrap_value`) if the class is not yet loaded.
-spec bootstrap_value_singleton(atom(), module(), non_neg_integer()) -> ok.
bootstrap_value_singleton(ClassName, Module, Retries) ->
    try
        Obj = Module:new(),
        case set_class_variable(ClassName, Obj) of
            ok ->
                ?LOG_DEBUG("Bootstrap: wired value singleton", #{class => ClassName});
            {error, class_not_found} ->
                ?LOG_WARNING("Bootstrap: value singleton class not loaded yet", #{
                    class => ClassName
                }),
                erlang:send_after(200, self(), {rebootstrap_value, ClassName, Module, Retries + 1})
        end
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            ?LOG_WARNING("Bootstrap: value singleton class not loaded yet", #{class => ClassName}),
            erlang:send_after(200, self(), {rebootstrap_value, ClassName, Module, Retries + 1});
        _:Reason ->
            ?LOG_WARNING("Bootstrap: failed to initialize value singleton", #{
                class => ClassName, reason => Reason
            }),
            erlang:send_after(200, self(), {rebootstrap_value, ClassName, Module, Retries + 1})
    end.

%% @private Build the beamtalk_object reference tuple for a singleton.
build_object_ref(ClassName, Pid) ->
    {beamtalk_object, ClassName, class_module(ClassName), Pid}.

%% @private Map class name to its Erlang module using workspace config.
class_module(ClassName) ->
    Singletons = beamtalk_workspace_config:singletons(),
    case lists:search(fun(#{class_name := C}) -> C =:= ClassName end, Singletons) of
        {value, #{module := Module}} ->
            Module;
        false ->
            Err0 = beamtalk_error:new(class_not_found, ClassName),
            Err = beamtalk_error:with_hint(
                Err0,
                <<"Not a registered workspace singleton.">>
            ),
            error(Err)
    end.

%% @private Set the `current` class variable on the class.
%% Returns `ok` on success or `{error, class_not_found}` if the class is not
%% yet registered (so callers can detect failure and schedule a retry).
-spec set_class_variable(atom(), term()) -> ok | {error, class_not_found}.
set_class_variable(ClassName, Obj) ->
    try
        beamtalk_runtime_api:set_class_var(ClassName, current, Obj)
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            {error, class_not_found}
    end.

%% @private Activate compiled project modules from _build/dev/ebin/ (BT-739).
%% Scans `{ProjectPath}/_build/dev/ebin/` for `bt@*.beam` files (excluding
%% `bt@stdlib@*`), sorts them by superclass dependency order (BT-745), loads
%% each, calls register_class/0, and registers the module with workspace_meta.
%% Failures are logged but do not abort startup.
-spec activate_project_modules(binary() | undefined) -> ok.
activate_project_modules(undefined) ->
    ok;
activate_project_modules(ProjectPath) when is_binary(ProjectPath), byte_size(ProjectPath) > 0 ->
    EbinDir = filename:absname(
        filename:join([binary_to_list(ProjectPath), "_build", "dev", "ebin"])
    ),
    _ = code:add_pathz(EbinDir),
    Modules = find_bt_modules_in_dir(EbinDir),
    Sorted = sort_modules_by_dependency(EbinDir, Modules),
    lists:foreach(fun activate_project_module/1, Sorted);
activate_project_modules(_Other) ->
    ok.

%% @doc Scan a directory for bt@*.beam files that are not stdlib modules.
%% Returns a list of module atoms. Returns [] for missing or unreadable dirs.
%% Capped at ?MAX_PROJECT_MODULES to guard against atom table exhaustion (BT-747).
%% Filters eligible filenames first (no atom creation), sorts for deterministic
%% selection across platforms, then caps before converting to atoms.
-spec find_bt_modules_in_dir(string()) -> [module()].
find_bt_modules_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Eligible = lists:sort(lists:filter(fun is_project_beam_file/1, Files)),
            Count = length(Eligible),
            Capped =
                case Count > ?MAX_PROJECT_MODULES of
                    true ->
                        ?LOG_WARNING(
                            "Bootstrap: too many project modules, capping at limit",
                            #{found => Count, limit => ?MAX_PROJECT_MODULES, dir => Dir}
                        ),
                        lists:sublist(Eligible, ?MAX_PROJECT_MODULES);
                    false ->
                        Eligible
                end,
            lists:filtermap(fun beam_file_to_project_module/1, Capped);
        {error, _} ->
            []
    end.

%% @private Check if a filename is a project beam file (bt@* but not bt@stdlib@*).
%% This performs no atom creation — used for counting before the cap is applied.
-spec is_project_beam_file(string()) -> boolean().
is_project_beam_file(File) ->
    case filename:extension(File) of
        ".beam" ->
            ModName = filename:rootname(filename:basename(File)),
            is_user_class_module(ModName) andalso
                is_valid_module_name(ModName);
        _ ->
            false
    end.

%% @private Map a filename to a project module atom, or false to skip it.
%% Validates that the module name contains only characters valid in Beamtalk
%% module names (alphanumeric, @, _) before calling list_to_atom/1 (BT-747).
%%
%% Threat model: filenames come from the filesystem (_build/dev/ebin/), which
%% may contain arbitrary .beam files from broken builds or malicious content.
%% Since Erlang atoms are not garbage-collected, calling list_to_atom/1 on
%% unvalidated filenames risks permanent atom table exhaustion (default limit
%% 1,048,576 in OTP 26+). Character validation ensures only well-formed
%% Beamtalk module names are converted to atoms.
-spec beam_file_to_project_module(string()) -> {true, module()} | false.
beam_file_to_project_module(File) ->
    case filename:extension(File) of
        ".beam" ->
            ModName = filename:rootname(filename:basename(File)),
            IsProject = is_user_class_module(ModName),
            case IsProject of
                true ->
                    case is_valid_module_name(ModName) of
                        true ->
                            {true, list_to_atom(ModName)};
                        false ->
                            ?LOG_WARNING(
                                "Bootstrap: skipping module with invalid name",
                                #{filename => File}
                            ),
                            false
                    end;
                false ->
                    false
            end;
        _ ->
            false
    end.

%% @private Returns true if ModName is a user-defined class module.
%% User class modules have the bt@* prefix but exclude bt@stdlib@* stdlib modules.
-spec is_user_class_module(string()) -> boolean().
is_user_class_module(ModName) ->
    lists:prefix("bt@", ModName) andalso not lists:prefix("bt@stdlib@", ModName).

%% @doc Validate that a module name contains only characters valid in Beamtalk
%% module names: alphanumeric (a-z, A-Z, 0-9), @, and _.
-spec is_valid_module_name(string()) -> boolean().
is_valid_module_name([]) ->
    false;
is_valid_module_name(Name) ->
    lists:all(fun is_valid_module_char/1, Name).

%% @private
-spec is_valid_module_char(char()) -> boolean().
is_valid_module_char(C) when C >= $a, C =< $z -> true;
is_valid_module_char(C) when C >= $A, C =< $Z -> true;
is_valid_module_char(C) when C >= $0, C =< $9 -> true;
is_valid_module_char($@) -> true;
is_valid_module_char($_) -> true;
is_valid_module_char(_) -> false.

%% @private Sort modules by superclass dependency order (BT-745).
%% Reads the `beamtalk_class` attribute from each BEAM file using beam_lib
%% to determine {ClassName, SuperclassName}, then topologically sorts so
%% superclasses are loaded before subclasses. Modules without the attribute
%% (e.g., older compiled modules) are placed first as they have no known deps.
-spec sort_modules_by_dependency(string(), [module()]) -> [module()].
sort_modules_by_dependency(_EbinDir, []) ->
    [];
sort_modules_by_dependency(EbinDir, Modules) ->
    {WithClass, WithoutClass} = lists:foldl(
        fun(Mod, {WC, WOC}) ->
            case extract_class_info(EbinDir, Mod) of
                {ok, ClassName, Superclass} ->
                    {[{Mod, ClassName, Superclass} | WC], WOC};
                error ->
                    {WC, [Mod | WOC]}
            end
        end,
        {[], []},
        Modules
    ),
    Sorted = topo_sort(lists:reverse(WithClass)),
    lists:reverse(WithoutClass) ++ [Mod || {Mod, _, _} <- Sorted].

%% @private Extract class name and superclass from a BEAM file's attributes.
%% Reads the `beamtalk_class` module attribute without loading the module.
-spec extract_class_info(string(), module()) -> {ok, atom(), atom()} | error.
extract_class_info(EbinDir, ModuleName) ->
    BeamFile = filename:join(EbinDir, atom_to_list(ModuleName) ++ ".beam"),
    case beam_lib:chunks(BeamFile, [attributes]) of
        {ok, {_, [{attributes, Attrs}]}} ->
            case proplists:get_value(beamtalk_class, Attrs) of
                [{ClassName, Superclass}] ->
                    {ok, ClassName, Superclass};
                [{ClassName, Superclass} | _] ->
                    %% Multi-class module: use primary (first) class
                    {ok, ClassName, Superclass};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% @private Topologically sort class entries so superclasses come before subclasses.
%% Same wave-based algorithm as beamtalk_stdlib:topo_sort/1 (BT-745).
-spec topo_sort([{module(), atom(), atom()}]) -> [{module(), atom(), atom()}].
topo_sort(Entries) ->
    ClassSet = sets:from_list([Class || {_, Class, _} <- Entries]),
    topo_sort_waves(Entries, ClassSet, sets:new(), []).

-spec topo_sort_waves(
    [{module(), atom(), atom()}],
    sets:set(atom()),
    sets:set(atom()),
    [{module(), atom(), atom()}]
) -> [{module(), atom(), atom()}].
topo_sort_waves([], _ClassSet, _Emitted, Acc) ->
    lists:reverse(Acc);
topo_sort_waves(Remaining, ClassSet, Emitted, Acc) ->
    {Ready, Deferred} = lists:partition(
        fun({_Mod, _Class, Super}) ->
            (not sets:is_element(Super, ClassSet)) orelse sets:is_element(Super, Emitted)
        end,
        Remaining
    ),
    case Ready of
        [] ->
            ?LOG_WARNING(
                "Bootstrap topo_sort: unresolvable dependencies",
                #{remaining => [C || {_, C, _} <- Deferred]}
            ),
            lists:reverse(Acc) ++ Deferred;
        _ ->
            NewEmitted = lists:foldl(
                fun({_, Class, _}, S) -> sets:add_element(Class, S) end, Emitted, Ready
            ),
            topo_sort_waves(Deferred, ClassSet, NewEmitted, lists:reverse(Ready) ++ Acc)
    end.

%% @private Ensure a module is loaded, call register_class/0, and track it.
-spec activate_project_module(module()) -> ok.
activate_project_module(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {module, ModuleName} ->
            try_register_class(ModuleName),
            SourcePath = extract_source_path(ModuleName),
            beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
            ?LOG_DEBUG("Bootstrap: activated project module", #{module => ModuleName});
        {error, Reason} ->
            ?LOG_WARNING(
                "Bootstrap: failed to load project module",
                #{module => ModuleName, reason => Reason}
            )
    end.

%% @private Extract the .bt source file path from the module's beamtalk_source attribute.
%% The compiler embeds `beamtalk_source = ["path/to/file.bt"]` in every user module
%% (BT-845/BT-860). Returns undefined for stdlib modules or older compiled modules.
-spec extract_source_path(module()) -> string() | undefined.
extract_source_path(ModuleName) ->
    try
        Attrs = erlang:get_module_info(ModuleName, attributes),
        case proplists:get_value(beamtalk_source, Attrs) of
            [Path] when is_list(Path) -> Path;
            _ -> undefined
        end
    catch
        _:_ -> undefined
    end.

%% @private Call register_class/0 on a module if it exports one.
-spec try_register_class(module()) -> ok.
try_register_class(ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            try
                _ = ModuleName:register_class(),
                ok
            catch
                _:Err ->
                    ?LOG_WARNING(
                        "Bootstrap: register_class/0 failed",
                        #{module => ModuleName, error => Err}
                    )
            end;
        false ->
            ok
    end.
