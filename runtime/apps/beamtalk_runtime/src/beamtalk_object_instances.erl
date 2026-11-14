%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_instances).
-behaviour(gen_server).

%%% **DDD Context:** Object System Context

-moduledoc """
Instance tracking registry for Beamtalk.

Tracks all instances of each class to support Smalltalk-style reflection:
- `allInstances` - get all instances of a class
- `instanceCount` - count instances of a class
- hot-patching notifications (iterate instances when class changes)

Each instance is registered as a `{Class, Pid}` entry in an ETS bag table.
A monitor process automatically cleans up entries when instances terminate.

A second, pid-keyed `set` table (`beamtalk_instance_class`, `{Pid, Class}`)
is maintained next to it so `class_of/1` answers in O(1) (BT-4113): the bag
is keyed by class, and finding a pid's class there is a full-table scan,
which made every actor send O(live actors) while it sat on the send path.

Both tables name `beamtalk_ets_heir` as heir (BT-4123). If this server
crashes the heir keeps them, and the supervisor-restarted server reclaims
them and re-monitors every registered pid, so registrations survive the
restart. They are lost only if the heir dies too while it holds them (a
runtime-level failure; see `beamtalk_ets_heir`): the tables come back empty
and already-running actors stay unregistered until they are respawned.

See also: docs/beamtalk-ddd-model.md "Instance Tracking"
""".

%% API
-export([
    start_link/0,
    register/2,
    unregister/2,
    all/1,
    count/1,
    each/2,
    class_of/1
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

-define(INSTANCE_REGISTRY, beamtalk_instance_registry).
%% BT-4113: pid-keyed reverse index, `{Pid, Class}`.
-define(INSTANCE_CLASS, beamtalk_instance_class).

-type class_name() :: atom().

-record(state, {
    monitors = #{} :: #{{class_name(), pid()} => reference()}
}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the instance registry server.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Register an instance of a class.
The instance will be automatically unregistered when the process terminates.
""".
-spec register(class_name(), pid()) -> ok.
register(Class, Pid) when is_atom(Class), is_pid(Pid) ->
    %% BT-4845: no round trip to the registry process. Every actor start used
    %% to `gen_server:call` this singleton from the spawner, so concurrent
    %% starts queued on one process (and each spawner slept and woke around
    %% the call). The tables are public: write the rows here, so
    %% `all/1` and `class_of/1` see the actor as soon as `register/2`
    %% returns, exactly as with the call, and cast only the monitor, which
    %% the server owns. The rows go in before the cast, so the server's
    %% cleanup (a `'DOWN'`, even an immediate `noproc` one for an actor that
    %% already exited) always follows the insert and removes it.
    try
        true = ets:insert(?INSTANCE_CLASS, {Pid, Class}),
        true = ets:insert(?INSTANCE_REGISTRY, {Class, Pid}),
        gen_server:cast(?MODULE, {monitor, Class, Pid})
    catch
        %% Tables missing (registry not running, or lost with its heir):
        %% take the old path, which fails the way it always did.
        error:badarg -> gen_server:call(?MODULE, {register, Class, Pid})
    end.

-doc "Manually unregister an instance of a class.".
-spec unregister(class_name(), pid()) -> ok.
unregister(Class, Pid) when is_atom(Class), is_pid(Pid) ->
    gen_server:call(?MODULE, {unregister, Class, Pid}).

-doc """
Get all live instances of a class.
Filters out dead processes.

NOTE: There is a race condition where a process may die between the alive
check and when the caller uses the pid. Callers must handle `noproc` errors.
""".
-spec all(class_name()) -> [pid()].
all(Class) when is_atom(Class) ->
    [Pid || {_, Pid} <- ets:lookup(?INSTANCE_REGISTRY, Class), erlang:is_process_alive(Pid)].

-doc """
Count the live instances of a class.
More efficient than `length(all(Class))` for large instance counts.
""".
-spec count(class_name()) -> non_neg_integer().
count(Class) when is_atom(Class) ->
    lists:foldl(
        fun({_, Pid}, Acc) ->
            case erlang:is_process_alive(Pid) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        ets:lookup(?INSTANCE_REGISTRY, Class)
    ).

-doc """
The class a local pid was registered under, or `undefined` (a pid that was
never registered, a remote pid, or the registry not running). One `ets:lookup`
on the pid-keyed index — O(1) however many instances are live (BT-4113).
Invariant: a pid is registered under one class (`spawn` registers each
actor once). The index keeps one row per pid, so if a pid were registered
under several classes the latest registration would win, and unregistering
any of them would leave `class_of/1` answering `undefined`.
""".
-spec class_of(pid()) -> class_name() | undefined.
class_of(Pid) ->
    try ets:lookup(?INSTANCE_CLASS, Pid) of
        [{_, Class}] -> Class;
        [] -> undefined
    catch
        %% Table missing: the registry is not running, or it and the heir
        %% both died and the table was lost (BT-4123).
        error:badarg -> undefined
    end.

-doc """
Iterate all live instances of a class with a function.
The function is called as `Fun(Pid)` for each instance.
""".
-spec each(class_name(), fun((pid()) -> any())) -> ok.
each(Class, Fun) when is_atom(Class), is_function(Fun, 1) ->
    lists:foreach(Fun, all(Class)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% BT-4123: both tables name `beamtalk_ets_heir` as heir, so a crash of
    %% this server leaves them alive under the heir and the restarted server
    %% reclaims them — actors register once at spawn and nothing re-registers
    %% them, so losing the tables would orphan every live actor (dispatch
    %% telemetry attributing to `unknown`, empty `bottlenecks:`).
    ok = beamtalk_ets_heir:ensure_table(?INSTANCE_REGISTRY, [named_table, public, bag]),
    ok = beamtalk_ets_heir:ensure_table(?INSTANCE_CLASS, [
        named_table, public, set, {read_concurrency, true}
    ]),
    ok = sync_class_index(),
    {ok, #state{monitors = monitor_registered()}}.

handle_call({register, Class, Pid}, _From, State) ->
    Key = {Class, Pid},
    case maps:is_key(Key, State#state.monitors) of
        true ->
            %% Already registered
            {reply, ok, State};
        false ->
            %% Insert into ETS
            ets:insert(?INSTANCE_REGISTRY, {Class, Pid}),
            ets:insert(?INSTANCE_CLASS, {Pid, Class}),
            %% Monitor the process for auto-cleanup
            Ref = erlang:monitor(process, Pid),
            NewMonitors = maps:put(Key, Ref, State#state.monitors),
            {reply, ok, State#state{monitors = NewMonitors}}
    end;
handle_call({unregister, Class, Pid}, _From, State) ->
    Key = {Class, Pid},
    case maps:take(Key, State#state.monitors) of
        {Ref, NewMonitors} ->
            erlang:demonitor(Ref, [flush]),
            ets:delete_object(?INSTANCE_REGISTRY, {Class, Pid}),
            ets:delete_object(?INSTANCE_CLASS, {Pid, Class}),
            {reply, ok, State#state{monitors = NewMonitors}};
        error ->
            %% Not monitored (yet): `register/2` writes the rows before its
            %% monitor cast arrives (BT-4845), so drop them here too.
            ets:delete_object(?INSTANCE_REGISTRY, {Class, Pid}),
            ets:delete_object(?INSTANCE_CLASS, {Pid, Class}),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({monitor, Class, Pid}, State) ->
    %% BT-4845: the monitor half of `register/2`. Skipped when the row is
    %% already gone (an `unregister/2` that ran first) or already monitored.
    Key = {Class, Pid},
    case maps:is_key(Key, State#state.monitors) of
        true ->
            {noreply, State};
        false ->
            case ets:match_object(?INSTANCE_REGISTRY, {Class, Pid}) of
                [] ->
                    {noreply, State};
                _ ->
                    Ref = erlang:monitor(process, Pid),
                    {noreply, State#state{monitors = maps:put(Key, Ref, State#state.monitors)}}
            end
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Find which classes this Pid was registered under
    %% This is more efficient than scanning all monitors when there are many instances
    %% BT-4845: the pid-keyed index answers in O(1) (a pid is registered under
    %% one class, see class_of/1). The bag can only be searched by value, a scan
    %% of the whole registry per exit, which kept up only because the
    %% synchronous `register/2` once held spawners to the server's pace; the
    %% scan stays as the fallback for a pid the index does not know.
    Classes =
        case ets:lookup(?INSTANCE_CLASS, Pid) of
            [{_, Class}] -> [Class];
            [] -> [C || [C] <- ets:match(?INSTANCE_REGISTRY, {'$1', Pid})]
        end,

    %% Remove from ETS
    ets:delete(?INSTANCE_CLASS, Pid),
    lists:foreach(
        fun(Class) ->
            ets:delete_object(?INSTANCE_REGISTRY, {Class, Pid})
        end,
        Classes
    ),

    %% Remove from monitors map
    NewMonitors = lists:foldl(
        fun(Class, Monitors) ->
            maps:remove({Class, Pid}, Monitors)
        end,
        State#state.monitors,
        Classes
    ),
    {noreply, State#state{monitors = NewMonitors}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% The tables pass to `beamtalk_ets_heir` when this process exits and the
    %% next server reclaims them in init/1 (BT-4123).
    ok.

code_change(OldVsn, State, Extra) ->
    ensure_class_index(),
    %% A registry upgraded in place never re-runs init/1: give the tables
    %% the heir it would have set (BT-4123).
    ok = beamtalk_ets_heir:maybe_set_heir(?INSTANCE_REGISTRY),
    ok = beamtalk_ets_heir:maybe_set_heir(?INSTANCE_CLASS),
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal
%%====================================================================

-doc """
Monitor every pid already in the registry and return the monitor map.

Fresh tables are empty, so this only does work after a restart that
reclaimed the tables from the heir (BT-4123): the previous server's monitors
died with it. A pid that died while no server was running gets an immediate
`'DOWN'` (reason `noproc`), which `handle_info/2` cleans up like any other.
""".
-spec monitor_registered() -> #{{class_name(), pid()} => reference()}.
monitor_registered() ->
    ets:foldl(
        fun({Class, Pid}, Acc) ->
            Key = {Class, Pid},
            case maps:is_key(Key, Acc) of
                true -> Acc;
                false -> Acc#{Key => erlang:monitor(process, Pid)}
            end
        end,
        #{},
        ?INSTANCE_REGISTRY
    ).

-doc """
Create and fill the pid-keyed index when upgrading from a registry that
predates it (BT-4113): `class_of/1` answers `undefined` until this runs.
Runs in the registry process, which owns both tables.
""".
ensure_class_index() ->
    case ets:whereis(?INSTANCE_CLASS) of
        undefined ->
            ?INSTANCE_CLASS = ets:new(?INSTANCE_CLASS, [
                named_table, public, set, {read_concurrency, true}
            ]),
            sync_class_index();
        _ ->
            ok
    end.

-doc """
Make the pid-keyed index agree with the bag, which is the source of truth:
add a `{Pid, Class}` row for every bag row and drop index rows whose pid has
no bag row.

Each write updates the two tables in two steps, so a server killed between
them leaves the tables out of step. Before BT-4123 that state died with the
tables; now it survives under the heir, so `init/1` repairs it before
re-monitoring. Rows are added before stale ones are dropped, so a concurrent
`class_of/1` never sees a registered pid's row missing.
""".
-spec sync_class_index() -> ok.
sync_class_index() ->
    Pairs = ets:tab2list(?INSTANCE_REGISTRY),
    true = ets:insert(?INSTANCE_CLASS, [{Pid, Class} || {Class, Pid} <- Pairs]),
    InBag = maps:from_list([{Pid, true} || {_, Pid} <- Pairs]),
    Stale = ets:foldl(
        fun({Pid, _Class}, Acc) ->
            case maps:is_key(Pid, InBag) of
                true -> Acc;
                false -> [Pid | Acc]
            end
        end,
        [],
        ?INSTANCE_CLASS
    ),
    lists:foreach(fun(Pid) -> ets:delete(?INSTANCE_CLASS, Pid) end, Stale).
