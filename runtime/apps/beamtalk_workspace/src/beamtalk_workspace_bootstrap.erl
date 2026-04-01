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
-export([activate_project_modules/1]).

%% Backwards-compatible re-exports — callers should migrate to
%% beamtalk_module_activation directly.
-export([find_bt_modules_in_dir/1, sort_modules_by_dependency/2, is_valid_module_name/1]).

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
        class => ClassName,
        name => RegName,
        domain => [beamtalk, runtime]
    }),
    {noreply, State};
handle_info({rebootstrap_value, ClassName, Module, Retries}, State) when Retries < 5 ->
    bootstrap_value_singleton(ClassName, Module, Retries),
    {noreply, State};
handle_info({rebootstrap_value, ClassName, _Module, _Retries}, State) ->
    ?LOG_ERROR("Bootstrap: failed to wire value singleton after retries", #{
        class => ClassName,
        domain => [beamtalk, runtime]
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
            ?LOG_WARNING("Bootstrap: singleton not registered yet", #{
                name => RegName, domain => [beamtalk, runtime]
            }),
            State;
        Pid ->
            Obj = build_object_ref(ClassName, Pid),
            _ = set_class_variable(ClassName, Obj),
            MonRef = erlang:monitor(process, Pid),
            ?LOG_DEBUG("Bootstrap: wired singleton", #{
                class => ClassName, pid => Pid, domain => [beamtalk, runtime]
            }),
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
                ?LOG_DEBUG("Bootstrap: wired value singleton", #{
                    class => ClassName, domain => [beamtalk, runtime]
                });
            {error, class_not_found} ->
                ?LOG_WARNING("Bootstrap: value singleton class not loaded yet", #{
                    class => ClassName,
                    domain => [beamtalk, runtime]
                }),
                erlang:send_after(200, self(), {rebootstrap_value, ClassName, Module, Retries + 1})
        end
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            ?LOG_WARNING("Bootstrap: value singleton class not loaded yet", #{
                class => ClassName, domain => [beamtalk, runtime]
            }),
            erlang:send_after(200, self(), {rebootstrap_value, ClassName, Module, Retries + 1});
        _:Reason ->
            ?LOG_WARNING("Bootstrap: failed to initialize value singleton", #{
                class => ClassName,
                reason => Reason,
                domain => [beamtalk, runtime]
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
        %% set_class_var returns the value that was set (from gen_server reply),
        %% not `ok`. Normalise to ok for callers.
        _ = beamtalk_runtime_api:set_class_var(ClassName, current, Obj),
        ok
    catch
        error:#beamtalk_error{kind = class_not_found} ->
            {error, class_not_found}
    end.

%% @doc Activate compiled project modules from _build/dev/ebin/ (BT-739).
%%
%% Delegates to `beamtalk_module_activation:activate_ebin/2` with a callback
%% that registers modules in workspace_meta and stores source text for
%% `ClassName >> method => body` support (BT-1174).
-spec activate_project_modules(binary() | undefined) -> ok.
activate_project_modules(undefined) ->
    ok;
activate_project_modules(ProjectPath) when is_binary(ProjectPath), byte_size(ProjectPath) > 0 ->
    EbinDir = filename:absname(
        filename:join([binary_to_list(ProjectPath), "_build", "dev", "ebin"])
    ),
    beamtalk_module_activation:activate_ebin(EbinDir, #{
        on_activate => fun on_project_module_activated/1
    });
activate_project_modules(_Other) ->
    ok.

%% @private Callback for project module activation.
%% Registers the module in workspace_meta and stores source text.
-spec on_project_module_activated({module(), string() | undefined}) -> ok.
on_project_module_activated({Module, SourcePath}) ->
    beamtalk_workspace_meta:register_module(Module, SourcePath),
    store_bootstrap_class_source(Module, SourcePath).

%% @private Read source file content and store per-class in workspace_meta so
%% that `ClassName >> method => body` works for bootstrap-loaded classes (BT-1174).
-spec store_bootstrap_class_source(module(), string() | undefined) -> ok.
store_bootstrap_class_source(_ModuleName, undefined) ->
    ok;
store_bootstrap_class_source(ModuleName, SourcePath) ->
    case file:read_file(SourcePath) of
        {ok, Binary} ->
            Source = binary_to_list(Binary),
            ClassNames = beamtalk_module_activation:extract_class_names(ModuleName),
            lists:foreach(
                fun(ClassName) ->
                    beamtalk_workspace_meta:set_class_source(
                        atom_to_binary(ClassName, utf8), Source
                    )
                end,
                ClassNames
            );
        {error, Reason} ->
            ?LOG_DEBUG(
                "Bootstrap: could not read source file for class source storage",
                #{
                    module => ModuleName,
                    path => SourcePath,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            )
    end.

%% @doc Backwards-compatible delegate to beamtalk_module_activation.
-spec find_bt_modules_in_dir(string()) -> [module()].
find_bt_modules_in_dir(Dir) ->
    beamtalk_module_activation:find_bt_modules_in_dir(Dir).

%% @doc Backwards-compatible delegate to beamtalk_module_activation.
-spec sort_modules_by_dependency(string(), [module()]) -> [module()].
sort_modules_by_dependency(EbinDir, Modules) ->
    beamtalk_module_activation:sort_modules_by_dependency(EbinDir, Modules).

%% @doc Backwards-compatible delegate to beamtalk_module_activation.
-spec is_valid_module_name(string()) -> boolean().
is_valid_module_name(Name) ->
    beamtalk_module_activation:is_valid_module_name(Name).
