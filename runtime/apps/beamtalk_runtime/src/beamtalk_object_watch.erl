%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_watch).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Per-object change subscriptions — the push substrate for the live Inspector
(ADR 0095 §5, BT-2489, Cockpit Phase 3).

ADR 0095 ships the Inspector poll-only: a LiveView pane re-issues `refresh` on a
timer. This module is the documented *opt-in push* follow-up §5 specified. A
consumer (the Cockpit Inspector pane, the MCP inspector) subscribes to a single
actor pid; when that actor commits a state write, its dispatch path
(`beamtalk_actor:log_dispatch_complete/5`) notices the pid is watched and asks
this server to publish the change.

## Opt-in by subscription, not by class declaration

Publishing on *every* actor state mutation is expensive (ADR 0095 §5), so it is
opt-in. ADR 0095 sketched an `observable` class modifier, but v1 must be
parser-free, so observability is keyed by **subscription**: an actor becomes
observable exactly while at least one consumer watches its pid. The hot dispatch
path pays only a single `ets:member/2` read against a public set table
(`?WATCHED_TABLE`) — microseconds, no message — and short-circuits to nothing
when the actor is not watched (the common case). Only watched actors ever publish.

## What a subscriber receives

A subscribed pid receives, after each committed state write on the watched actor:

```
{object_changed, Pid :: pid(), ChangedSlots :: [atom()]}
```

`ChangedSlots` is the list of user-visible state keys whose values differed
(ADR 0095 §5's `changedSlots`; `[]` means "unknown — refresh all"). This is a
*refresh trigger*, mirroring the `bindings` stream (BT-2399): the consumer
re-issues `inspect`/`refresh` to read the new snapshot, keeping actor opacity
intact (the snapshot is still a guarded `sys:get_state`, never a hot-path read).

## ObjectStateChanged system announcement

In parallel, the watched-actor change is published as an `ObjectStateChanged`
announcement on the `SystemAnnouncer` bus (ADR 0093 substrate, the schema ADR
0095 §5 reserved), so in-image stdlib subscribers can react with
`SystemAnnouncer current when: ObjectStateChanged do: [...]` without going
through this Erlang facade. The direct `{object_changed, ...}` message is the
low-latency path for the dist-attached Cockpit pane; the announcement is the
in-image language-level path. Both fire only for watched pids.

## Lifecycle

The server monitors both the subscriber pid and the watched actor pid. When a
subscriber dies, its subscription is dropped; when the last subscriber for a pid
is dropped (or the watched actor dies), the pid is removed from `?WATCHED_TABLE`
so the dispatch path stops paying for it.

## Delivery guarantees & known gaps

* **At-least-zero, possibly-one-stray.** `is_watched/1` reads the public table
  directly on the actor's hot path, while subscribe/unsubscribe/publish are all
  serialised casts. So a change committed just as a subscriber unsubscribes may
  still deliver one final `{object_changed, ...}` after the client considers
  itself unsubscribed. This is harmless for a refresh-trigger stream — a
  consumer must tolerate an `{object_changed, Pid, _}` for a pid it no longer
  tracks (ignore it).
* **Congestion-tolerant delivery.** Pushes use `erlang:send/3` with `nosuspend`
  so a slow or partitioned remote subscriber can never block this single
  process (which fans out *every* watched actor's changes). A dropped trigger
  just means the consumer re-reads on its next refresh tick — acceptable because
  the message carries no irreplaceable data, only "something changed."
* **Crash resync is the client's job.** This is a supervised `one_for_one`
  worker; if it crashes and restarts, the watchers map and all monitors are
  lost and the public table comes back empty. Remote (dist-attached) subscribers
  are *not* notified and will silently stop receiving pushes. A live consumer
  must therefore re-issue `subscribe_object` on dist reconnect / pane remount
  (the Cockpit Inspector does this on LiveView mount, BT-2492), not only once.
""".

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/0,
    subscribe/2,
    unsubscribe/2,
    is_watched/1,
    publish_change/3,
    watched_pids/0
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

%% Public set table of watched actor pids, read directly (no message) from the
%% actor dispatch hot path. Owned by this gen_server; writes go through it.
-define(WATCHED_TABLE, beamtalk_object_watch_pids).
-define(SERVER, ?MODULE).

-record(state, {
    %% Watched actor pid => #{subscriber_pid => monitor_ref}
    watchers = #{} :: #{pid() => #{pid() => reference()}},
    %% Monitor ref => {watched | subscriber, pid()} for 'DOWN' cleanup.
    %% A subscriber watching N pids has N subscriber monitors keyed here; a
    %% watched actor has one watched monitor.
    monitors = #{} :: #{reference() => {watched | subscriber, pid()}}
}).

%%% Public API

-doc "Start the per-object change subscription server with a registered name.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc """
Subscribe `Subscriber` to committed state-change events on the actor `Pid`.
The subscriber receives `{object_changed, Pid, ChangedSlots}` after each state
write on `Pid`. Idempotent: re-subscribing the same pair is a no-op.
""".
-spec subscribe(pid(), pid()) -> ok.
subscribe(Pid, Subscriber) when is_pid(Pid), is_pid(Subscriber) ->
    gen_server:cast(?SERVER, {subscribe, Pid, Subscriber}).

-doc "Unsubscribe `Subscriber` from state-change events on the actor `Pid`.".
-spec unsubscribe(pid(), pid()) -> ok.
unsubscribe(Pid, Subscriber) when is_pid(Pid), is_pid(Subscriber) ->
    gen_server:cast(?SERVER, {unsubscribe, Pid, Subscriber}).

-doc """
Fast, message-free membership check used by the actor dispatch hot path to decide
whether an actor's state write needs publishing. Returns `false` cheaply when the
server is not running or the pid is not watched (the common case).
""".
-spec is_watched(pid()) -> boolean().
is_watched(Pid) when is_pid(Pid) ->
    try
        ets:member(?WATCHED_TABLE, Pid)
    catch
        %% Table absent (server not started yet, or torn down) — nothing watched.
        error:badarg -> false
    end.

-doc """
Publish a committed state change for the actor `Pid`. Called by
`beamtalk_actor:log_dispatch_complete/5` only when `is_watched(Pid)` is true and
at least one user-visible slot changed. `ActorClass` is the actor's class atom
captured at the source (the just-committed state), used for the
`ObjectStateChanged` announcement; `ChangedSlots` is the list of changed state
keys (`[]` = "unknown, refresh all"). Best-effort and fire-and-forget; an absent
server silently drops the event so it never delays actor dispatch.
""".
-spec publish_change(pid(), atom(), [atom()]) -> ok.
publish_change(Pid, ActorClass, ChangedSlots) when
    is_pid(Pid), is_atom(ActorClass), is_list(ChangedSlots)
->
    case erlang:whereis(?SERVER) of
        undefined -> ok;
        Server -> gen_server:cast(Server, {publish_change, Pid, ActorClass, ChangedSlots})
    end.

-doc "Return the current set of watched actor pids (testing/introspection).".
-spec watched_pids() -> [pid()].
watched_pids() ->
    try
        [Pid || {Pid} <- ets:tab2list(?WATCHED_TABLE)]
    catch
        error:badarg -> []
    end.

%%% gen_server callbacks

init([]) ->
    %% Public so the actor dispatch hot path reads it directly; this server is
    %% the sole writer (protected would block external reads).
    _ = ets:new(?WATCHED_TABLE, [named_table, set, public, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({subscribe, Pid, Subscriber}, State) ->
    {noreply, do_subscribe(Pid, Subscriber, State)};
handle_cast({unsubscribe, Pid, Subscriber}, State) ->
    {noreply, do_unsubscribe(Pid, Subscriber, State)};
handle_cast({publish_change, Pid, ActorClass, ChangedSlots}, State) ->
    do_publish(Pid, ActorClass, ChangedSlots, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    {noreply, handle_down(MonRef, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

-spec do_subscribe(pid(), pid(), #state{}) -> #state{}.
do_subscribe(Pid, Subscriber, State) ->
    #state{watchers = Watchers, monitors = Monitors} = State,
    Subs0 = maps:get(Pid, Watchers, #{}),
    case maps:is_key(Subscriber, Subs0) of
        true ->
            %% Already subscribed to this pid — idempotent.
            State;
        false ->
            %% First watcher for this pid arms a watched-actor monitor and marks
            %% the pid in the public table the dispatch path reads.
            Monitors1 =
                case maps:size(Subs0) of
                    0 ->
                        true = ets:insert(?WATCHED_TABLE, {Pid}),
                        WatchedRef = erlang:monitor(process, Pid),
                        Monitors#{WatchedRef => {watched, Pid}};
                    _ ->
                        Monitors
                end,
            SubRef = erlang:monitor(process, Subscriber),
            Subs1 = Subs0#{Subscriber => SubRef},
            Monitors2 = Monitors1#{SubRef => {subscriber, Subscriber}},
            State#state{
                watchers = Watchers#{Pid => Subs1},
                monitors = Monitors2
            }
    end.

-spec do_unsubscribe(pid(), pid(), #state{}) -> #state{}.
do_unsubscribe(Pid, Subscriber, State) ->
    #state{watchers = Watchers, monitors = Monitors} = State,
    case maps:find(Pid, Watchers) of
        {ok, Subs0} ->
            case maps:find(Subscriber, Subs0) of
                {ok, SubRef} ->
                    erlang:demonitor(SubRef, [flush]),
                    Subs1 = maps:remove(Subscriber, Subs0),
                    Monitors1 = maps:remove(SubRef, Monitors),
                    drop_if_empty(Pid, Subs1, Watchers, Monitors1, State);
                error ->
                    State
            end;
        error ->
            State
    end.

%% Remove a pid's watcher entry when no subscribers remain, tearing down the
%% watched-actor monitor and the public-table mark.
-spec drop_if_empty(pid(), map(), map(), map(), #state{}) -> #state{}.
drop_if_empty(Pid, Subs, Watchers, Monitors, State) when map_size(Subs) =:= 0 ->
    ets:delete(?WATCHED_TABLE, Pid),
    Monitors1 = demonitor_watched(Pid, Monitors),
    State#state{
        watchers = maps:remove(Pid, Watchers),
        monitors = Monitors1
    };
drop_if_empty(Pid, Subs, Watchers, Monitors, State) ->
    State#state{
        watchers = Watchers#{Pid => Subs},
        monitors = Monitors
    }.

%% Drop the watched-actor monitor for Pid (there is at most one).
-spec demonitor_watched(pid(), map()) -> map().
demonitor_watched(Pid, Monitors) ->
    maps:filter(
        fun
            (Ref, {watched, P}) when P =:= Pid ->
                erlang:demonitor(Ref, [flush]),
                false;
            (_Ref, _Tag) ->
                true
        end,
        Monitors
    ).

-spec handle_down(reference(), #state{}) -> #state{}.
handle_down(MonRef, State) ->
    #state{monitors = Monitors} = State,
    case maps:find(MonRef, Monitors) of
        {ok, {watched, Pid}} ->
            %% The watched actor died — drop the whole entry and notify nobody
            %% (subscribers learn of death via ActorStopped, ADR 0093).
            drop_watched(Pid, State);
        {ok, {subscriber, Subscriber}} ->
            %% A subscriber died — remove it from every pid it watched.
            drop_subscriber(Subscriber, MonRef, State);
        error ->
            State
    end.

-spec drop_watched(pid(), #state{}) -> #state{}.
drop_watched(Pid, State) ->
    #state{watchers = Watchers, monitors = Monitors} = State,
    ets:delete(?WATCHED_TABLE, Pid),
    Subs = maps:get(Pid, Watchers, #{}),
    %% Drop the subscriber monitors registered under this pid's entry, plus the
    %% watched monitor.
    Monitors1 = maps:fold(
        fun(_Subscriber, SubRef, Acc) ->
            erlang:demonitor(SubRef, [flush]),
            maps:remove(SubRef, Acc)
        end,
        Monitors,
        Subs
    ),
    Monitors2 = demonitor_watched(Pid, Monitors1),
    State#state{
        watchers = maps:remove(Pid, Watchers),
        monitors = Monitors2
    }.

-spec drop_subscriber(pid(), reference(), #state{}) -> #state{}.
drop_subscriber(Subscriber, MonRef, State) ->
    #state{watchers = Watchers, monitors = Monitors} = State,
    Monitors1 = maps:remove(MonRef, Monitors),
    %% Remove the subscriber from each pid it watched; tidy now-empty pids.
    {Watchers1, Monitors2} = maps:fold(
        fun(Pid, Subs0, {WAcc, MAcc}) ->
            case maps:take(Subscriber, Subs0) of
                {_Ref, Subs1} when map_size(Subs1) =:= 0 ->
                    ets:delete(?WATCHED_TABLE, Pid),
                    MAcc1 = demonitor_watched(Pid, MAcc),
                    {maps:remove(Pid, WAcc), MAcc1};
                {_Ref, Subs1} ->
                    {WAcc#{Pid => Subs1}, MAcc};
                error ->
                    {WAcc, MAcc}
            end
        end,
        {Watchers, Monitors1},
        Watchers
    ),
    State#state{watchers = Watchers1, monitors = Monitors2}.

-spec do_publish(pid(), atom(), [atom()], #state{}) -> ok.
do_publish(Pid, ActorClass, ChangedSlots, #state{watchers = Watchers}) ->
    %% Direct low-latency push to dist-attached subscribers (the Cockpit pane).
    %% `nosuspend` so a slow/partitioned remote subscriber can never block this
    %% single fan-out process — a dropped trigger is recovered on the consumer's
    %% next refresh tick (the message carries no irreplaceable data).
    case maps:find(Pid, Watchers) of
        {ok, Subs} ->
            maps:foreach(
                fun(Subscriber, _Ref) ->
                    _ = erlang:send(
                        Subscriber, {object_changed, Pid, ChangedSlots}, [nosuspend]
                    )
                end,
                Subs
            );
        error ->
            ok
    end,
    %% In-image language-level path: an ObjectStateChanged announcement on the
    %% SystemAnnouncer bus (ADR 0093 substrate, ADR 0095 §5 schema). Guarded and
    %% fault-isolated so a missing bus or handler error never delays dispatch.
    announce_state_changed(Pid, ActorClass, ChangedSlots),
    ok.

-spec announce_state_changed(pid(), atom(), [atom()]) -> ok.
announce_state_changed(Pid, ActorClass, ChangedSlots) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _ ->
            try
                beamtalk_announcements:system_announce('ObjectStateChanged', #{
                    pid => Pid,
                    actorClass => ActorClass,
                    changedSlots => ChangedSlots
                })
            catch
                _:_ -> ok
            end
    end,
    ok.
