%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Actor registry for REPL sessions
%%%
%%% Tracks actor PIDs spawned during a REPL session to keep them alive
%%% across eval cycles. Each actor is registered with metadata including
%%% its class name, module, and spawn time.
%%%
%%% ## Lifecycle
%%%
%%% - Registry is started when a REPL session starts
%%% - Actors register themselves after spawn
%%% - Actors are automatically unregistered when they terminate (via monitor)
%%% - Registry terminates when REPL session ends, killing all actors
%%%
%%% ## Actor Metadata
%%%
%%% ```erlang
%%% #{
%%%   pid => pid(),
%%%   class => atom(),        %% Class name (e.g., 'Counter')
%%%   module => atom(),       %% Module name (e.g., beamtalk_repl_eval_42)
%%%   spawned_at => integer() %% erlang:system_time(second)
%%% }
%%% ```

-module(beamtalk_repl_actors).
-behaviour(gen_server).

%% Public API
-export([start_link/0, start_link/1, register_actor/4, unregister_actor/2, 
         list_actors/1, kill_actor/2, get_actor/2, count_actors_for_module/2,
         on_actor_spawned/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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

%% @doc Start the actor registry without a registered name (legacy REPL).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Start the actor registry with a registered name (workspace mode).
-spec start_link(registered) -> {ok, pid()} | {error, term()}.
start_link(registered) ->
    gen_server:start_link({local, beamtalk_actor_registry}, ?MODULE, [], []).

%% @doc Register an actor with the registry.
%% Monitors the actor so it can be automatically unregistered on termination.
-spec register_actor(pid(), pid(), atom(), atom()) -> ok.
register_actor(RegistryPid, ActorPid, ClassName, ModuleName) ->
    gen_server:call(RegistryPid, {register, ActorPid, ClassName, ModuleName}).

%% @doc Actor spawn callback for beamtalk_runtime integration.
%% This is registered via application:set_env by beamtalk_workspace_app:start/2.
%% Best-effort: catches errors so the runtime spawn path never crashes.
-spec on_actor_spawned(pid(), pid(), atom(), atom()) -> ok.
on_actor_spawned(RegistryPid, ActorPid, ClassName, ModuleName) ->
    try
        register_actor(RegistryPid, ActorPid, ClassName, ModuleName),
        beamtalk_workspace_meta:register_actor(ActorPid)
    catch
        Kind:Reason ->
            logger:warning("REPL actor tracking failed", #{
                kind => Kind,
                reason => Reason,
                actor_pid => ActorPid,
                class => ClassName
            })
    end,
    ok.

%% @doc Unregister an actor from the registry.
-spec unregister_actor(pid(), pid()) -> ok.
unregister_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {unregister, ActorPid}).

%% @doc List all registered actors with their metadata.
-spec list_actors(pid()) -> [actor_metadata()].
list_actors(RegistryPid) ->
    gen_server:call(RegistryPid, list_actors).

%% @doc Kill a specific actor.
%% Returns ok if killed, {error, not_found} if not registered.
-spec kill_actor(pid(), pid()) -> ok | {error, not_found}.
kill_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {kill, ActorPid}).

%% @doc Get metadata for a specific actor.
%% Returns {ok, Metadata} or {error, not_found}.
-spec get_actor(pid(), pid()) -> {ok, actor_metadata()} | {error, not_found}.
get_actor(RegistryPid, ActorPid) ->
    gen_server:call(RegistryPid, {get_actor, ActorPid}).

%% @doc Count how many actors are using a specific module.
%% Returns {ok, Count} where Count is the number of actors from that module.
-spec count_actors_for_module(pid(), atom()) -> {ok, non_neg_integer()} | {error, term()}.
count_actors_for_module(RegistryPid, ModuleName) ->
    gen_server:call(RegistryPid, {count_for_module, ModuleName}).

%%% gen_server callbacks

%% @private
init([]) ->
    {ok, #state{actors = #{}, monitors = #{}}}.

%% @private
handle_call({register, ActorPid, ClassName, ModuleName}, _From, State) ->
    #state{actors = Actors, monitors = Monitors} = State,
    
    %% Monitor the actor so we know when it terminates
    MonitorRef = erlang:monitor(process, ActorPid),
    
    Metadata = #{
        pid => ActorPid,
        class => ClassName,
        module => ModuleName,
        spawned_at => erlang:system_time(second)
    },
    
    NewActors = maps:put(ActorPid, Metadata, Actors),
    NewMonitors = maps:put(MonitorRef, ActorPid, Monitors),
    
    {reply, ok, State#state{actors = NewActors, monitors = NewMonitors}};

handle_call({unregister, ActorPid}, _From, State) ->
    #state{actors = Actors, monitors = Monitors} = State,
    
    %% Find and demonitor all references for this actor
    MonitorRefs = [Ref || {Ref, Pid} <- maps:to_list(Monitors), Pid =:= ActorPid],
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
            %% Kill the actor (gen_server will trap the exit and clean up)
            exit(ActorPid, kill),
            {reply, ok, State};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_actor, ActorPid}, _From, State) ->
    #state{actors = Actors} = State,
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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, ActorPid, _Reason}, State) ->
    %% Actor terminated - automatically unregister it
    #state{actors = Actors, monitors = Monitors} = State,
    
    NewActors = maps:remove(ActorPid, Actors),
    NewMonitors = maps:remove(MonitorRef, Monitors),
    
    {noreply, State#state{actors = NewActors, monitors = NewMonitors}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Kill all registered actors when registry terminates
    #state{actors = Actors} = State,
    maps:foreach(
        fun(_Pid, #{pid := ActorPid}) ->
            exit(ActorPid, shutdown)
        end,
        Actors
    ),
    ok.

%% @private
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).
