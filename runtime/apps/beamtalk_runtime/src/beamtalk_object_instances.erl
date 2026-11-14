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

Each instance is registered as a `{{Class, Pid}}` row in an ETS `ordered_set`
(the *class table*, `beamtalk_instance_registry`). This server monitors every
registered pid and removes its rows when it exits.

The class table is an `ordered_set` keyed by `{Class, Pid}`, not a `bag` keyed
by class (BT-4842): inserting into or deleting from a bag walks every row
that shares the key, so each registration and each exit cost O(instances of
that class). With one class spawning fast, that cost fed on itself: a slow
server left dead rows behind, which made the next insert and delete slower
still. In an `ordered_set` both are O(log n), and `all/1` is a range read
over the `{Class, _}` prefix.

A second, pid-keyed `set` table (`beamtalk_instance_class`, `{Pid, Class}`)
is maintained next to it so `class_of/1` answers in O(1) (BT-4113): the class
table is keyed by class first, and finding a pid's class there is a
full-table scan, which made every actor send O(live actors) while it sat on
the send path.

## Why registration is a synchronous call

`register/2` is a `gen_server:call`, and this process is the only writer of
both tables. That call is what holds spawners to the pace of the registry
(BT-4842). BT-4845 replaced it with rows written by the caller plus a cast for
the monitor. Under sustained concurrent starts spawners then outran this
process: its mailbox, and the dead rows it had not removed yet, grew without
bound, and throughput fell by more than an order of magnitude. Bounding this
mailbox alone was not enough: `beamtalk_actor_registry` gets a cast from
every actor start, had been paced only by this call, and flooded in turn. Do
not make `register/2` asynchronous without bounding both mailboxes;
`sustained_concurrent_register_keeps_up_test_` pins this one.

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

-ifdef(TEST).
-export([drop_stale_index_rows/1, migrate_bag_table/0]).
-endif.

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
%% BT-4842: `ordered_set` keyed `{Class, Pid}`; see the moduledoc.
-define(INSTANCE_REGISTRY_OPTS, [named_table, public, ordered_set]).

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
    %% A call on purpose: it paces spawners to this process (see the
    %% moduledoc, BT-4842).
    gen_server:call(?MODULE, {register, Class, Pid}).

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
    [Pid || Pid <- pids_of(Class), erlang:is_process_alive(Pid)].

-doc """
Count the live instances of a class.
More efficient than `length(all(Class))` for large instance counts.
""".
-spec count(class_name()) -> non_neg_integer().
count(Class) when is_atom(Class) ->
    lists:foldl(
        fun(Pid, Acc) ->
            case erlang:is_process_alive(Pid) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        pids_of(Class)
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
    ok = beamtalk_ets_heir:ensure_table(?INSTANCE_REGISTRY, ?INSTANCE_REGISTRY_OPTS),
    ok = migrate_bag_table(),
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
            ets:insert(?INSTANCE_REGISTRY, {{Class, Pid}}),
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
            ets:delete(?INSTANCE_REGISTRY, {Class, Pid}),
            ets:delete_object(?INSTANCE_CLASS, {Pid, Class}),
            {reply, ok, State#state{monitors = NewMonitors}};
        error ->
            %% Not monitored: drop any rows all the same (e.g. written
            %% straight into the public tables).
            ets:delete(?INSTANCE_REGISTRY, {Class, Pid}),
            ets:delete_object(?INSTANCE_CLASS, {Pid, Class}),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Find which classes this Pid was registered under
    %% This is more efficient than scanning all monitors when there are many instances
    %% BT-4845: the pid-keyed index answers in O(1) (a pid is registered under
    %% one class, see class_of/1). The class table is keyed by class first, so
    %% finding a pid there is a scan of the whole registry per exit; the scan
    %% stays as the fallback for a pid the index does not know.
    Classes =
        case ets:lookup(?INSTANCE_CLASS, Pid) of
            [{_, Class}] -> [Class];
            [] -> [C || [C] <- ets:match(?INSTANCE_REGISTRY, {{'$1', Pid}})]
        end,

    %% Remove from ETS
    ets:delete(?INSTANCE_CLASS, Pid),
    lists:foreach(
        fun(Class) ->
            ets:delete(?INSTANCE_REGISTRY, {Class, Pid})
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
    %% BT-4842: a registry upgraded in place still holds the `bag` table.
    ok = migrate_bag_table(),
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
Every pid registered under `Class`, dead ones included. A range read: the
key's first element is bound, so the `ordered_set` visits only that class's
rows.
""".
-spec pids_of(class_name()) -> [pid()].
pids_of(Class) ->
    ets:select(?INSTANCE_REGISTRY, [{{{Class, '$1'}}, [], ['$1']}]).

-doc """
Replace a class table that is still a `bag` of `{Class, Pid}` rows (a table
created before BT-4842 and kept across a hot upgrade, or reclaimed from the
heir by a newer server) with the `ordered_set` of `{{Class, Pid}}` rows.

The table does not exist for a moment; a reader that hits that window sees
what it sees when the registry is not running.
""".
-spec migrate_bag_table() -> ok.
migrate_bag_table() ->
    case ets:info(?INSTANCE_REGISTRY, type) of
        bag ->
            Rows = ets:tab2list(?INSTANCE_REGISTRY),
            true = ets:delete(?INSTANCE_REGISTRY),
            ok = beamtalk_ets_heir:ensure_table(?INSTANCE_REGISTRY, ?INSTANCE_REGISTRY_OPTS),
            true = ets:insert(?INSTANCE_REGISTRY, [
                {{Class, Pid}}
             || {Class, Pid} <- Rows, is_atom(Class), is_pid(Pid)
            ]),
            ok;
        _ ->
            ok
    end.

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
        fun({{_Class, Pid} = Key}, Acc) ->
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
Make the pid-keyed index agree with the class table, which is the source of
truth: add a `{Pid, Class}` row for every class-table row and drop index rows
whose pid has no class-table row.

Each write updates the two tables in two steps, so a server killed between
them leaves the tables out of step. Before BT-4123 that state died with the
tables; now it survives under the heir, so `init/1` repairs it before
re-monitoring. Rows are added before stale ones are dropped, so a concurrent
`class_of/1` never sees a registered pid's row missing.
""".
-spec sync_class_index() -> ok.
sync_class_index() ->
    Pairs = ets:tab2list(?INSTANCE_REGISTRY),
    true = ets:insert(?INSTANCE_CLASS, [{Pid, Class} || {{Class, Pid}} <- Pairs]),
    InBag = maps:from_list([{Pid, true} || {{_, Pid}} <- Pairs]),
    drop_stale_index_rows(InBag).

-doc """
Drop index rows whose pid is not in `InBag`, a snapshot of the class table's
pids.

This server is the only writer through the API, so the snapshot is normally
exact. The live re-check only guards rows written straight into the public
tables from outside the API after the snapshot was taken: a candidate is
dropped only if the live class table has no row for it either.
""".
-spec drop_stale_index_rows(#{pid() => true}) -> ok.
drop_stale_index_rows(InBag) ->
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
    lists:foreach(
        fun(Pid) ->
            case ets:match(?INSTANCE_REGISTRY, {{'_', Pid}}) of
                [] -> ets:delete(?INSTANCE_CLASS, Pid);
                _ -> ok
            end
        end,
        Stale
    ).
