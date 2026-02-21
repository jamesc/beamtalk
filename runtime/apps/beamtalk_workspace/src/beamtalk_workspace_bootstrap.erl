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
%%% **DDD Context:** Workspace

-module(beamtalk_workspace_bootstrap).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_continue/2, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).
-export([find_bt_modules_in_dir/1, activate_project_modules/1]).
-export([sort_modules_by_dependency/2]).

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
    State = bootstrap_all(#state{}),
    %% Defer project module activation to handle_continue so init/1 returns
    %% promptly and the gen_server is registered before doing I/O.
    {ok, State, {continue, {activate_project_modules, ProjectPath}}}.

%% @private
handle_continue({activate_project_modules, ProjectPath}, State) ->
    activate_project_modules(ProjectPath),
    {noreply, State}.

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
    ?LOG_ERROR("Bootstrap: failed to rewire singleton after retries", #{class => ClassName, name => RegName}),
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
    lists:foldl(
        fun(#{class_name := ClassName, binding_name := RegName}, AccState) ->
            bootstrap_singleton(ClassName, RegName, AccState)
        end,
        State,
        beamtalk_workspace_config:singletons()
    ).

%% @private Bootstrap a single singleton: set class var and monitor.
bootstrap_singleton(ClassName, RegName, State) ->
    case erlang:whereis(RegName) of
        undefined ->
            ?LOG_WARNING("Bootstrap: singleton not registered yet", #{name => RegName}),
            State;
        Pid ->
            Obj = build_object_ref(ClassName, Pid),
            set_class_variable(ClassName, Obj),
            MonRef = erlang:monitor(process, Pid),
            ?LOG_DEBUG("Bootstrap: wired singleton", #{class => ClassName, pid => Pid}),
            Monitors = maps:put(MonRef, {ClassName, RegName}, State#state.monitors),
            State#state{monitors = Monitors}
    end.

%% @private Build the beamtalk_object reference tuple for a singleton.
build_object_ref(ClassName, Pid) ->
    {beamtalk_object, ClassName, class_module(ClassName), Pid}.

%% @private Map class name to its Erlang module using workspace config.
class_module(ClassName) ->
    Singletons = beamtalk_workspace_config:singletons(),
    case lists:search(fun(#{class_name := C}) -> C =:= ClassName end, Singletons) of
        {value, #{module := Module}} -> Module;
        false ->
            Err0 = beamtalk_error:new(class_not_found, ClassName),
            Err = beamtalk_error:with_hint(Err0,
                <<"Not a registered workspace singleton.">>),
            error(Err)
    end.

%% @private Set the `current` class variable on the class.
set_class_variable(ClassName, Obj) ->
    try
        beamtalk_object_class:set_class_var(ClassName, current, Obj)
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            ?LOG_WARNING("Bootstrap: class not loaded yet", #{class => ClassName})
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
    EbinDir = filename:absname(filename:join([binary_to_list(ProjectPath), "_build", "dev", "ebin"])),
    _ = code:add_pathz(EbinDir),
    Modules = find_bt_modules_in_dir(EbinDir),
    Sorted = sort_modules_by_dependency(EbinDir, Modules),
    lists:foreach(fun activate_project_module/1, Sorted);
activate_project_modules(_Other) ->
    ok.

%% @doc Scan a directory for bt@*.beam files that are not stdlib modules.
%% Returns a list of module atoms. Returns [] for missing or unreadable dirs.
-spec find_bt_modules_in_dir(string()) -> [module()].
find_bt_modules_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:filtermap(fun beam_file_to_project_module/1, Files);
        {error, _} ->
            []
    end.

%% @private Map a filename to a project module atom, or false to skip it.
-spec beam_file_to_project_module(string()) -> {true, module()} | false.
beam_file_to_project_module(File) ->
    case filename:extension(File) of
        ".beam" ->
            ModName = filename:rootname(filename:basename(File)),
            IsProject = lists:prefix("bt@", ModName) andalso
                        not lists:prefix("bt@stdlib@", ModName),
            case IsProject of
                true  -> {true, list_to_atom(ModName)};
                false -> false
            end;
        _ ->
            false
    end.

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
    [{module(), atom(), atom()}], sets:set(atom()), sets:set(atom()),
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
            beamtalk_workspace_meta:register_module(ModuleName),
            ?LOG_DEBUG("Bootstrap: activated project module", #{module => ModuleName});
        {error, Reason} ->
            ?LOG_WARNING("Bootstrap: failed to load project module",
                         #{module => ModuleName, reason => Reason})
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
                    ?LOG_WARNING("Bootstrap: register_class/0 failed",
                                 #{module => ModuleName, error => Err})
            end;
        false ->
            ok
    end.
