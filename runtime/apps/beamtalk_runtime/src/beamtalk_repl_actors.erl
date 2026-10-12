%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_actors).
-behaviour(gen_server).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Node-wide actor registry (registered as `beamtalk_actor_registry`).

Owned by `beamtalk_runtime_sup`, so it exists in every boot context
(`beamtalk test`, `run`, REPL, releases). It backs `Node current actors`
(ADR 0129 amendment, BT-3633) and the REPL actor ops.

## Lifecycle

- Started by `beamtalk_runtime_sup` with the runtime application
- Every actor is tracked automatically from its lifecycle-start telemetry
  (`track_spawned/2`); the REPL spawn path additionally registers explicitly
  (`register_actor/4`, idempotent)
- Actors are automatically unregistered when they terminate (via monitor)

## Actor Metadata

```erlang
#{
  pid => pid(),
  class => atom(),        %% Class name (e.g., 'Counter')
  module => atom(),       %% Module name (e.g., beamtalk_repl_eval_42)
  spawned_at => integer() %% erlang:system_time(second)
}
```
""".

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/1,
    register_actor/4,
    unregister_actor/2,
    list_actors/1,
    kill_actor/2,
    get_actor/2,
    count_actors_for_module/2,
    get_pids_for_module/2,
    on_actor_spawned/4,
    track_spawned/2,
    list_objects/0,
    object_at/1
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

-record(state, {
    actors :: #{pid() => actor_metadata()},
    monitors :: #{reference() => pid()}
}).

-type actor_metadata() :: #{
    pid => pid(),
    class => atom(),
    module => atom(),
    spawned_at => integer()
}.

-export_type([actor_metadata/0]).

%%% Public API

-doc "Start the actor registry with its registered name (`beamtalk_actor_registry`).".
-spec start_link(registered) -> {ok, pid()} | {error, term()}.
start_link(registered) ->
    gen_server:start_link({local, beamtalk_actor_registry}, ?MODULE, [], []).

-doc """
Register an actor with the registry.
Monitors the actor so it can be automatically unregistered on termination.
""".
-spec register_actor(pid(), pid(), atom(), atom()) -> ok.
register_actor(RegistryPid, ActorPid, ClassName, ModuleName) ->
    gen_server:call(RegistryPid, {register, ActorPid, ClassName, ModuleName}).

-doc """
Actor spawn callback for beamtalk_runtime integration.
This is registered via application:set_env by beamtalk_workspace_app:start/2.
Handles each tracking step individually: if registry registration fails,
we skip workspace_meta registration to avoid inconsistent state.
Returns ok on success, {error, Reason} on failure.
""".
-spec on_actor_spawned(pid(), pid(), atom(), atom()) -> ok | {error, term()}.
on_actor_spawned(RegistryPid, ActorPid, ClassName, ModuleName) ->
    case try_register_actor(RegistryPid, ActorPid, ClassName, ModuleName) of
        ok ->
            %% Registry succeeded — notify the optional workspace hook
            %% (set by beamtalk_workspace_app; absent under `beamtalk test`).
            run_spawn_hook(ActorPid, ClassName),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("REPL actor registry registration failed", #{
                registry_pid => RegistryPid,
                actor_pid => ActorPid,
                class => ClassName,
                module => ModuleName,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            {error, {registry_failed, Reason}}
    end.

-doc """
Register actor with the REPL actor registry (gen_server:call).
Returns ok on success, or {error, Reason} if registration fails.
""".
-spec try_register_actor(pid(), pid(), atom(), atom()) -> ok | {error, term()}.
try_register_actor(RegistryPid, ActorPid, ClassName, ModuleName) ->
    try
        register_actor(RegistryPid, ActorPid, ClassName, ModuleName)
    catch
        exit:{noproc, _} ->
            {error, registry_not_running};
        exit:Reason ->
            {error, {registry_exit, Reason}};
        Kind:Reason ->
            {error, {Kind, Reason}}
    end.

-doc """
Run the optional `actor_spawned_hook` (`{Module, Function}` in the
`beamtalk_runtime` app env, called as `Module:Function(ActorPid, ClassName)`).
The workspace application installs one to keep `beamtalk_workspace_meta`
informed; the runtime itself has no dependency on the workspace. Failures are
logged and never fail the spawn (the actor is already tracked by the registry).
""".
-spec run_spawn_hook(pid(), atom()) -> ok.
run_spawn_hook(ActorPid, ClassName) ->
    case application:get_env(beamtalk_runtime, actor_spawned_hook) of
        {ok, {Mod, Fun}} ->
            %% bt-catcher-audit: not-applicable-no-block - workspace Erlang spawn hook, not a
            %% Beamtalk block
            try
                Mod:Fun(ActorPid, ClassName),
                ok
            catch
                Kind:Reason ->
                    ?LOG_WARNING("Actor spawned hook failed", #{
                        actor_pid => ActorPid,
                        class => ClassName,
                        kind => Kind,
                        reason => Reason,
                        domain => [beamtalk, runtime]
                    }),
                    ok
            end;
        _ ->
            ok
    end.

-doc """
Track a freshly started actor (called from every actor's lifecycle-start
telemetry, so registration does not depend on the REPL spawn path).

Fire-and-forget (`gen_server:cast`) so actor init never blocks on, or fails
because of, the registry. A no-op when the registry is not running. The
class's backing module is resolved inside the registry process.
""".
-spec track_spawned(pid(), atom()) -> ok.
track_spawned(ActorPid, ClassName) when is_pid(ActorPid), is_atom(ClassName) ->
    case erlang:whereis(beamtalk_actor_registry) of
        undefined -> ok;
        RegistryPid -> gen_server:cast(RegistryPid, {track, ActorPid, ClassName})
    end;
track_spawned(_ActorPid, _ClassName) ->
    ok.

-doc """
Every live actor on this node as a `#beamtalk_object{}` reference (backs
`Node current actors`). `[]` when the registry is not running.
""".
-spec list_objects() -> [tuple()].
list_objects() ->
    case erlang:whereis(beamtalk_actor_registry) of
        undefined ->
            [];
        RegistryPid ->
            lists:filtermap(fun wrap_actor/1, list_actors(RegistryPid))
    end.

-doc """
Look up one live actor by pid string (`"<0.132.0>"`); `nil` when the string is
not a pid, the pid is untracked or dead (backs `Node current actorAt:`).
""".
-spec object_at(term()) -> tuple() | nil.
object_at(PidStr) when is_binary(PidStr) ->
    object_at(binary_to_list(PidStr));
object_at(PidStr) when is_list(PidStr) ->
    try list_to_pid(PidStr) of
        Pid ->
            case erlang:whereis(beamtalk_actor_registry) of
                undefined ->
                    nil;
                RegistryPid ->
                    case get_actor(RegistryPid, Pid) of
                        {ok, Metadata} ->
                            case wrap_actor(Metadata) of
                                {true, Obj} -> Obj;
                                false -> nil
                            end;
                        {error, not_found} ->
                            nil
                    end
            end
    catch
        error:badarg -> nil
    end;
object_at(_) ->
    nil.

-doc "Unregister an actor from the registry.".
-spec unregister_actor(pid(), pid()) -> ok.
unregister_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {unregister, ActorPid}).

-doc "List all registered actors with their metadata.".
-spec list_actors(pid()) -> [actor_metadata()].
list_actors(RegistryPid) ->
    gen_server:call(RegistryPid, list_actors).

-doc """
Kill a specific actor.
Returns ok if killed, {error, not_found} if not registered.
""".
-spec kill_actor(pid(), pid()) -> ok | {error, not_found}.
kill_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {kill, ActorPid}).

-doc """
Get metadata for a specific actor.
Returns {ok, Metadata} or {error, not_found}.
""".
-spec get_actor(pid(), pid()) -> {ok, actor_metadata()} | {error, not_found}.
get_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {get_actor, ActorPid}).

-doc """
Count how many actors are using a specific module.
Returns {ok, Count} where Count is the number of actors from that module.
""".
-spec count_actors_for_module(pid(), atom()) -> {ok, non_neg_integer()} | {error, term()}.
count_actors_for_module(RegistryPid, ModuleName) ->
    gen_server:call(RegistryPid, {count_for_module, ModuleName}).

-doc """
Get PIDs of all actors using a specific module.
Used by hot reload to trigger sys:change_code/4 after module reload.
""".
-spec get_pids_for_module(pid(), atom()) -> {ok, [pid()]} | {error, term()}.
get_pids_for_module(RegistryPid, ModuleName) ->
    gen_server:call(RegistryPid, {pids_for_module, ModuleName}).

%%% gen_server callbacks

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{actors = #{}, monitors = #{}}}.

handle_call({register, ActorPid, ClassName, ModuleName}, _From, State) ->
    NewState = do_register(ActorPid, ClassName, ModuleName, State),
    %% Actor lifecycle is published as an `ActorSpawned` system
    %% announcement from `beamtalk_actor`'s telemetry mirror, consumed via the
    %% SystemAnnouncer bus — the registry no longer broadcasts to subscribers.
    {reply, ok, NewState};
handle_call({unregister, ActorPid}, _From, State) ->
    #state{actors = Actors, monitors = Monitors} = State,

    %% Find and demonitor all references for this actor
    MonitorRefs = [Ref || Ref := Pid <- Monitors, Pid =:= ActorPid],
    lists:foreach(fun erlang:demonitor/1, MonitorRefs),

    %% Remove monitor references
    NewMonitors = maps:filter(
        fun(_Ref, Pid) -> Pid =/= ActorPid end,
        Monitors
    ),

    NewActors = maps:remove(ActorPid, Actors),
    {reply, ok, State#state{actors = NewActors, monitors = NewMonitors}};
handle_call(list_actors, _From, State) ->
    #state{actors = Actors} = State,
    ActorList = maps:values(Actors),
    {reply, ActorList, State};
handle_call({kill, ActorPid}, _From, State) ->
    #state{actors = Actors} = State,
    case maps:is_key(ActorPid, Actors) of
        true ->
            %% Kill the actor and wait for it to die before replying.
            %% The registry already monitors this actor (see register call), so a
            %% DOWN message will arrive in our mailbox and be handled by handle_info.
            %% We use a fresh one-shot monitor here only for the synchronous wait —
            %% the existing monitor continues to trigger the actor-unregister path.
            Ref = erlang:monitor(process, ActorPid),
            exit(ActorPid, kill),
            Reply =
                receive
                    {'DOWN', Ref, process, ActorPid, _Reason} -> ok
                after 5000 ->
                    erlang:demonitor(Ref, [flush]),
                    {error, timeout}
                end,
            {reply, Reply, State};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_actor, ActorPid}, _From, State) ->
    #state{actors = Actors} = State,
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(ActorPid, Actors) of
        {ok, Metadata} ->
            {reply, {ok, Metadata}, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({count_for_module, ModuleName}, _From, State) ->
    #state{actors = Actors} = State,
    Count = maps:fold(
        fun(_Pid, #{module := Module}, Acc) ->
            case Module of
                ModuleName -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        Actors
    ),
    {reply, {ok, Count}, State};
handle_call({pids_for_module, ModuleName}, _From, State) ->
    #state{actors = Actors} = State,
    Pids = maps:fold(
        fun(Pid, #{module := Module}, Acc) ->
            case Module of
                ModuleName -> [Pid | Acc];
                _ -> Acc
            end
        end,
        [],
        Actors
    ),
    {reply, {ok, Pids}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({track, ActorPid, ClassName}, State) ->
    {noreply, do_register(ActorPid, ClassName, resolve_module(ClassName), State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    #state{actors = Actors, monitors = Monitors} = State,
    case maps:find(MonitorRef, Monitors) of
        {ok, ActorPid} ->
            %% Actor terminated — unregister. Actor stop is published as
            %% an `ActorStopped` system announcement from `beamtalk_actor`'s
            %% telemetry mirror (consumed via the SystemAnnouncer bus); the
            %% registry no longer broadcasts to subscribers.
            NewActors = maps:remove(ActorPid, Actors),
            NewMonitors = maps:remove(MonitorRef, Monitors),
            {noreply, State#state{actors = NewActors, monitors = NewMonitors}};
        error ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Registered actors are deliberately left alone: the registry tracks every
    %% actor on the node, and their own supervisors own their lifecycle.
    ok.

%%% Internal Functions

-spec do_register(pid(), atom(), atom(), #state{}) -> #state{}.
do_register(ActorPid, ClassName, ModuleName, #state{actors = Actors, monitors = Monitors} = State) ->
    case maps:is_key(ActorPid, Actors) of
        true ->
            %% Already tracked (e.g. lifecycle track + REPL register): keep the
            %% original entry, but upgrade an unresolved module.
            case maps:get(ActorPid, Actors) of
                #{module := undefined} = Old when ModuleName =/= undefined ->
                    State#state{actors = Actors#{ActorPid => Old#{module => ModuleName}}};
                _ ->
                    State
            end;
        false ->
            %% Monitor the actor so we know when it terminates
            MonitorRef = erlang:monitor(process, ActorPid),
            Metadata = #{
                pid => ActorPid,
                class => ClassName,
                module => ModuleName,
                spawned_at => erlang:system_time(second)
            },
            State#state{
                actors = Actors#{ActorPid => Metadata},
                monitors = Monitors#{MonitorRef => ActorPid}
            }
    end.

-doc "Resolve the backing module of a class name; `undefined` when unknown.".
-spec wrap_actor(actor_metadata()) -> {true, tuple()} | false.
wrap_actor(#{pid := Pid, class := Class} = Meta) ->
    Module =
        case maps:get(module, Meta, undefined) of
            undefined -> resolve_module(Class);
            M -> M
        end,
    case Module =/= undefined andalso is_process_alive(Pid) of
        true -> {true, {beamtalk_object, Class, Module, Pid}};
        false -> false
    end.

-spec resolve_module(atom()) -> atom().
resolve_module(ClassName) ->
    try beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> undefined;
        ClassPid -> beamtalk_object_class:module_name_safe(ClassPid)
    catch
        _:_ -> undefined
    end.

code_change(OldVsn, State, Extra) ->
    beamtalk_runtime_api:hot_reload_code_change(OldVsn, State, Extra).
