%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_announcements).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Runtime event bus for the typed Announcements substrate (ADR 0093 Layer 1).

Phase 1 — the runtime foundation. Stands up the `beamtalk_announcements`
gen_server, a dedicated `beamtalk_pg` scope, and the ETS subscription table, and
proves typed dispatch end-to-end: `subscribe/4`, `unsubscribe/1`, and an async
`announce/2`.

BT-2439 delivered the foundation with exact-class matching; BT-2440 adds the MRO
(superclass-chain) walk to `announce/2`: an event is delivered to subscribers of
its class *or any ancestor*, with the walk performed at announce time (live
hierarchy changes respected) and delivery de-duplicated per subscription.

Design (the `beamtalk_xref` / `beamtalk_trace_store` discipline — the gen_server
is NOT in the dispatch path):

- **Subscription table = ETS, gen_server owns writes only.** Each subscription
  is a row `{SubRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag}` in a
  `set` keyed by the unique `SubRef` the returned `Subscription` wraps. Keying by
  `SubRef` — not `{Class, Pid}` — means a process may hold multiple distinct
  subscriptions to the same class (Pharo's rule): a second `subscribe` adds a
  row, it never replaces the first. A secondary by-class index
  (`ordered_set` on `{AnnouncementClass, SubRef}`) lets `announce/2` fetch the
  matching subscribers for one class without scanning the whole table, and the
  MRO walk fetches it once per class on the event's superclass chain.

- **Writes serialise through the gen_server.** `subscribe/4` / `unsubscribe/1`
  are `gen_server:call`s so the table writes are serialised and the bus can arm
  one `erlang:monitor/2` per distinct subscriber pid (ref-counted across that
  pid's subscriptions, so the monitor is only dropped when its last subscription
  goes). A dead subscriber is pruned automatically on the `DOWN` message —
  no manual cleanup.

- **Dispatch runs caller-side, off concurrent ETS reads.** `announce/2` does a
  `read_concurrency` ETS lookup of the by-class index in the *announcing*
  process and fans out with direct `Pid ! Msg`. Routing dispatch through the
  gen_server would serialise every event through one mailbox (and, once the sync
  path lands, self-deadlock on re-entrant announce) — keeping it caller-side
  eliminates both.

- **Crash-survivable tables.** Both ETS tables are created with
  `{heir, SupervisorPid, ...}` so they survive a bus crash (the
  `beamtalk_trace_store` pattern). On restart the gen_server re-arms monitors
  from the surviving rows. The full crash→restart dead-pid prune is BT-2442;
  Phase 1 re-arms and keeps the data.

Out of scope here (later issues): the sync `announceAndWait:` path / `doOnce:` /
`when:send:to:` / handler fault isolation (BT-2441), and the heir crash re-arm
dead-pid prune (BT-2442).

See also: docs/ADR/0093-announcements-event-substrate.md §1
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").

%% API — lifecycle
-export([start_link/0]).

%% API — write path (through the gen_server)
-export([
    subscribe/4,
    unsubscribe/1,
    is_active/1
]).

%% API — dispatch (caller-side, off concurrent ETS reads)
-export([
    announce/2
]).

%% API — introspection (direct ETS reads)
-export([
    subscription_count/0,
    subscribers_of/1
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

%% Table names
-define(SUBS_TABLE, beamtalk_announcement_subs).
-define(BY_CLASS_TABLE, beamtalk_announcement_by_class).

%% Dedicated pg scope (ADR 0093 §1 — NOT the default scope).
-define(PG_SCOPE, beamtalk_pg).

%%====================================================================
%% Types
%%====================================================================

-type announcement_class() :: atom().
-type sub_ref() :: reference().
-type handler() :: term().

%% A subscription row in the primary `set` table, keyed by `SubRef`.
-type sub_row() :: {
    sub_ref(),
    announcement_class(),
    pid(),
    handler(),
    OnceFlag :: boolean()
}.

-export_type([announcement_class/0, sub_ref/0, handler/0, sub_row/0]).

%% gen_server state: the per-subscriber monitor bookkeeping. `monitors` maps a
%% subscriber pid to `{MonitorRef, RefCount}` so a pid with several
%% subscriptions is monitored exactly once and the monitor is demonitored only
%% when its last subscription is removed.
-record(state, {
    monitors = #{} :: #{pid() => {reference(), pos_integer()}}
}).

%%====================================================================
%% API — lifecycle
%%====================================================================

-doc "Start the announcements bus gen_server.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% API — write path
%%====================================================================

-doc """
Register a subscription: deliver announcements of exactly `AnnouncementClass`
to `SubscriberPid`, carrying `Handler` (an opaque term the stdlib veneer
interprets — a block ref, `{send, Sel, Receiver}`, etc.). `OnceFlag` marks a
`doOnce` subscription (consumed atomically on first delivery — the consume path
lands in BT-2441; Phase 1 stores the flag).

Returns `{ok, SubRef}` where `SubRef` is the unique reference the stdlib
`Subscription` object wraps. Each call mints a *distinct* `SubRef`, so a process
may hold several subscriptions to the same class — re-subscribing never silently
replaces (Pharo's rule).

Synchronous (serialised write). The bus arms one `erlang:monitor/2` per distinct
subscriber pid so a dead subscriber's rows are pruned automatically.
""".
-spec subscribe(announcement_class(), pid(), handler(), boolean()) ->
    {ok, sub_ref()} | {error, #beamtalk_error{}}.
subscribe(AnnouncementClass, SubscriberPid, Handler, OnceFlag) when
    is_atom(AnnouncementClass), is_pid(SubscriberPid), is_boolean(OnceFlag)
->
    gen_server:call(?MODULE, {subscribe, AnnouncementClass, SubscriberPid, Handler, OnceFlag});
subscribe(AnnouncementClass, SubscriberPid, _Handler, OnceFlag) ->
    {error, bad_subscribe_args(AnnouncementClass, SubscriberPid, OnceFlag)}.

-doc """
Remove the subscription identified by `SubRef`. Idempotent — unsubscribing an
unknown or already-removed `SubRef` is a no-op that returns `ok`. Synchronous
(serialised write); demonitors the subscriber pid when this was its last
subscription.
""".
-spec unsubscribe(sub_ref()) -> ok.
unsubscribe(SubRef) when is_reference(SubRef) ->
    gen_server:call(?MODULE, {unsubscribe, SubRef});
unsubscribe(_SubRef) ->
    ok.

-doc """
Whether `SubRef` still names a live subscription. Direct ETS membership read —
does not go through the gen_server. `false` after `unsubscribe/1` or after the
subscriber process died.
""".
-spec is_active(sub_ref()) -> boolean().
is_active(SubRef) when is_reference(SubRef) ->
    case ets:whereis(?SUBS_TABLE) of
        undefined -> false;
        _ -> ets:member(?SUBS_TABLE, SubRef)
    end;
is_active(_SubRef) ->
    false.

%%====================================================================
%% API — dispatch (caller-side)
%%====================================================================

-doc """
Announce `Event` (an announcement payload) to every subscriber of `EventClass`
*or any of its ancestors*, asynchronously. Runs entirely in the *calling*
process: it walks the event class's superclass chain via `read_concurrency` ETS
metadata reads (`beamtalk_class_metadata:lookup_superclass/1`), gathers the
matching subscriptions from the by-class index at each level, and sends a direct
`Pid ! {beamtalk_announcement, SubRef, EventClass, Handler, Event}` to each live
subscriber. Returns `ok` immediately (fire-and-forget).

MRO matching (BT-2440): subscribe to `UIEvent` and you receive `ButtonClicked`.
The walk happens at announce time, so live hierarchy changes are respected. Each
delivered message carries `EventClass` — the *announced* class — not the ancestor
the subscription matched on.

De-duplication is **per subscription**: a `SubRef` is delivered to at most once
per `announce`, even though the MRO walk visits several classes. Because a
subscription is registered against exactly one class, the dedup set guards the
pathological cases — a corrupted hierarchy that revisits a class, or a future
many-class subscription — rather than the common path. A process that holds two
distinct subscriptions matching the event still receives two messages (one per
`SubRef`, Pharo's rule).

The walk stops gracefully at the root (`{ok, none}`), at a class whose metadata
row is absent (`not_found` — e.g. a class removed mid-walk), and at a depth cap
(`?MAX_HIERARCHY_DEPTH`) that bounds a corrupted cyclic hierarchy.

A subscriber whose process has already died (its `DOWN` not yet processed by the
bus) is skipped via an `is_process_alive/1` guard, so a stale row never produces
a send to a dead pid.
""".
-spec announce(announcement_class(), term()) -> ok.
announce(EventClass, Event) when is_atom(EventClass) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            ok;
        _ ->
            _ = walk_and_deliver(EventClass, EventClass, Event, sets:new([{version, 2}]), 0),
            ok
    end.

-doc """
Walk the superclass chain from `CurrentClass` and deliver `Event` (announced as
`EventClass`) to every subscription matching a class on the walk, de-duplicated
per `SubRef` via the `Delivered` set. Returns the updated `Delivered` set.

Terminates when the chain reaches the root (`{ok, none}`), a class with no
metadata row (`not_found` — truncation), or the `?MAX_HIERARCHY_DEPTH` cap (a
corrupted cyclic hierarchy). Caller-side; no bus call.
""".
-spec walk_and_deliver(
    announcement_class(), announcement_class(), term(), sets:set(sub_ref()), non_neg_integer()
) -> sets:set(sub_ref()).
walk_and_deliver(_CurrentClass, EventClass, _Event, Delivered, Depth) when
    Depth > ?MAX_HIERARCHY_DEPTH
->
    %% Runs in the *announcing* caller's process (announce/2 is caller-side), not
    %% the bus process, so the `domain` metadata set in init/1 does not apply —
    %% set it explicitly, matching bad_subscribe_args/3.
    ?LOG_WARNING(#{
        event => announcement_mro_walk_truncated,
        reason => max_depth_exceeded,
        event_class => EventClass,
        max_depth => ?MAX_HIERARCHY_DEPTH,
        domain => [beamtalk, announcements]
    }),
    Delivered;
walk_and_deliver(CurrentClass, EventClass, Event, Delivered, Depth) ->
    %% Deliver to every subscription registered against `CurrentClass`, skipping
    %% any SubRef already delivered to on this walk (per-subscription de-dup).
    IndexRows = ets:match_object(?BY_CLASS_TABLE, {{CurrentClass, '_'}}),
    Delivered1 = lists:foldl(
        fun({{_Class, SubRef}}, Acc) ->
            case sets:is_element(SubRef, Acc) of
                true ->
                    Acc;
                false ->
                    deliver(SubRef, EventClass, Event),
                    sets:add_element(SubRef, Acc)
            end
        end,
        Delivered,
        IndexRows
    ),
    %% Walk to the superclass, reading the live metadata at announce time. A
    %% missing row (`not_found`) truncates the walk gracefully (class removed
    %% mid-walk); the root (`{ok, none}`) ends it normally.
    case beamtalk_class_metadata:lookup_superclass(CurrentClass) of
        {ok, none} ->
            Delivered1;
        {ok, Super} ->
            walk_and_deliver(Super, EventClass, Event, Delivered1, Depth + 1);
        not_found ->
            Delivered1
    end.

-doc """
Deliver `Event` to one subscription if it is still live and its subscriber
process is alive. Skips silently otherwise (the row may have been unsubscribed
or the subscriber may have died between the index read and here — a race that is
harmless: a missed delivery to a process that is gone). Caller-side; no bus call.
""".
-spec deliver(sub_ref(), announcement_class(), term()) -> ok.
deliver(SubRef, EventClass, Event) ->
    case ets:lookup(?SUBS_TABLE, SubRef) of
        [{SubRef, _Class, SubscriberPid, Handler, _Once}] ->
            case is_process_alive(SubscriberPid) of
                true ->
                    SubscriberPid ! {beamtalk_announcement, SubRef, EventClass, Handler, Event},
                    ok;
                false ->
                    ok
            end;
        [] ->
            ok
    end.

%%====================================================================
%% API — introspection
%%====================================================================

-doc "Total number of live subscriptions across all classes. Direct ETS read.".
-spec subscription_count() -> non_neg_integer().
subscription_count() ->
    case ets:whereis(?SUBS_TABLE) of
        undefined -> 0;
        _ -> ets:info(?SUBS_TABLE, size)
    end.

-doc """
The `SubRef`s currently subscribed to exactly `AnnouncementClass`. Direct ETS
read of the by-class index — does not go through the gen_server.
""".
-spec subscribers_of(announcement_class()) -> [sub_ref()].
subscribers_of(AnnouncementClass) when is_atom(AnnouncementClass) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            [];
        _ ->
            [
                SubRef
             || {{_Class, SubRef}} <- ets:match_object(
                    ?BY_CLASS_TABLE, {{AnnouncementClass, '_'}}
                )
            ]
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, announcements]}),

    %% Dedicated Beamtalk pg scope (ADR 0093 §1) — kept separate from the
    %% default `pg` scope used for class-registry membership, and from any pg
    %% use on a connected non-Beamtalk node. The scope is the membership
    %% registry for SystemAnnouncer in later phases; standing it up here means
    %% the scope exists before any subscriber tries to join a group.
    ensure_pg_scope(),

    %% Heir is the supervisor so both tables survive a bus crash (the
    %% beamtalk_trace_store pattern); on restart we re-arm monitors from the
    %% inherited rows.
    SupPid = find_supervisor(),
    ensure_tables(SupPid),

    %% Re-arm monitors for any subscriptions that survived a crash via heir.
    Monitors = rearm_monitors(),

    ?LOG_INFO(#{
        event => announcements_started,
        tables => [?SUBS_TABLE, ?BY_CLASS_TABLE],
        pg_scope => ?PG_SCOPE,
        rearmed_subscribers => maps:size(Monitors)
    }),
    {ok, #state{monitors = Monitors}}.

handle_call({subscribe, AnnouncementClass, SubscriberPid, Handler, OnceFlag}, _From, State) ->
    SubRef = make_ref(),
    true = ets:insert(?SUBS_TABLE, {SubRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag}),
    true = ets:insert(?BY_CLASS_TABLE, {{AnnouncementClass, SubRef}}),
    NewState = arm_monitor(SubscriberPid, State),
    {reply, {ok, SubRef}, NewState};
handle_call({unsubscribe, SubRef}, _From, State) ->
    NewState =
        case ets:lookup(?SUBS_TABLE, SubRef) of
            [{SubRef, AnnouncementClass, SubscriberPid, _Handler, _Once}] ->
                remove_subscription(SubRef, AnnouncementClass),
                disarm_monitor(SubscriberPid, State);
            [] ->
                %% Idempotent — unknown / already-removed SubRef.
                State
        end,
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) ->
    %% A monitored subscriber died — prune all of its subscriptions and drop the
    %% monitor bookkeeping. Auto-cleanup, no manual unsubscribe needed.
    NewState = prune_subscriber(Pid, State),
    {noreply, NewState};
handle_info({'ETS-TRANSFER', TableName, _FromPid, _HeirData}, State) ->
    %% Inherited a table back from the supervisor heir after a crash/restart.
    ?LOG_INFO(#{event => announcements_table_inherited, table => TableName}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Tables are heir'd to the supervisor and survive; monitors die with this
    %% process. The restarted bus re-arms from the surviving rows.
    ok.

code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal: setup
%%====================================================================

-doc """
Start the dedicated `beamtalk_pg` scope if it is not already running.
Idempotent — a scope already started (e.g. after a bus restart, since the pg
scope process is separate from this gen_server) is left as-is. `pg:start_link/1`
links the scope to whoever starts it; on a restart the existing scope keeps
running under its original owner, which is fine — we only need it to exist.
""".
-spec ensure_pg_scope() -> ok.
ensure_pg_scope() ->
    case whereis(?PG_SCOPE) of
        undefined ->
            case pg:start_link(?PG_SCOPE) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok
            end;
        _Pid ->
            ok
    end,
    ok.

-doc "Find the supervisor pid for the ETS heir option (self if not supervised).".
-spec find_supervisor() -> pid().
find_supervisor() ->
    case whereis(beamtalk_runtime_sup) of
        undefined -> self();
        Pid -> Pid
    end.

-doc """
Create both ETS tables if they do not already exist (they may already exist,
inherited from the supervisor heir after a crash). The primary table is a `set`
keyed by `SubRef`; the by-class index is an `ordered_set` keyed by
`{AnnouncementClass, SubRef}`. Both are `public` so caller-side `announce/2`
reads work regardless of which process currently owns the table after an
inheritance, and both carry `{read_concurrency, true}` for the concurrent
dispatch reads.
""".
-spec ensure_tables(pid()) -> ok.
ensure_tables(SupPid) ->
    ensure_table(?SUBS_TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {heir, SupPid, ?SUBS_TABLE}
    ]),
    ensure_table(?BY_CLASS_TABLE, [
        named_table,
        public,
        ordered_set,
        {read_concurrency, true},
        {heir, SupPid, ?BY_CLASS_TABLE}
    ]),
    ok.

-spec ensure_table(atom(), [term()]) -> ok.
ensure_table(Name, Opts) ->
    case ets:whereis(Name) of
        undefined ->
            _ = ets:new(Name, Opts),
            ok;
        _Tid ->
            %% Inherited via heir after a crash — public table, reads/writes
            %% work regardless of ownership; the data is preserved.
            ok
    end.

-doc """
Re-arm one monitor per distinct subscriber pid found in the surviving primary
table (after a crash→restart with heir-preserved rows). Returns the monitor
bookkeeping map. A pid that died during the crash→restart gap is still monitored
here; its `DOWN` fires immediately (`erlang:monitor/2` of a dead pid sends an
instant `DOWN`), so the stale row is pruned via the normal `DOWN` path. The
eager `is_process_alive/1` prune of such rows is BT-2442; Phase 1 relies on the
immediate `DOWN`.
""".
-spec rearm_monitors() -> #{pid() => {reference(), pos_integer()}}.
rearm_monitors() ->
    Rows = ets:tab2list(?SUBS_TABLE),
    lists:foldl(
        fun({_SubRef, _Class, SubscriberPid, _Handler, _Once}, Acc) ->
            case Acc of
                #{SubscriberPid := {MonRef, Count}} ->
                    Acc#{SubscriberPid => {MonRef, Count + 1}};
                _ ->
                    MonRef = erlang:monitor(process, SubscriberPid),
                    Acc#{SubscriberPid => {MonRef, 1}}
            end
        end,
        #{},
        Rows
    ).

%%====================================================================
%% Internal: monitor bookkeeping
%%====================================================================

-doc """
Arm a monitor for `SubscriberPid` if not already monitored, otherwise bump its
ref-count. One monitor per distinct subscriber pid, ref-counted across that
pid's subscriptions.
""".
-spec arm_monitor(pid(), #state{}) -> #state{}.
arm_monitor(SubscriberPid, #state{monitors = Monitors} = State) ->
    NewMonitors =
        case Monitors of
            #{SubscriberPid := {MonRef, Count}} ->
                Monitors#{SubscriberPid => {MonRef, Count + 1}};
            _ ->
                MonRef = erlang:monitor(process, SubscriberPid),
                Monitors#{SubscriberPid => {MonRef, 1}}
        end,
    State#state{monitors = NewMonitors}.

-doc """
Decrement `SubscriberPid`'s monitor ref-count; demonitor and drop the entry when
it reaches zero (the pid's last subscription was removed).
""".
-spec disarm_monitor(pid(), #state{}) -> #state{}.
disarm_monitor(SubscriberPid, #state{monitors = Monitors} = State) ->
    NewMonitors =
        case Monitors of
            #{SubscriberPid := {MonRef, Count}} when Count =< 1 ->
                erlang:demonitor(MonRef, [flush]),
                maps:remove(SubscriberPid, Monitors);
            #{SubscriberPid := {MonRef, Count}} ->
                Monitors#{SubscriberPid => {MonRef, Count - 1}};
            _ ->
                Monitors
        end,
    State#state{monitors = NewMonitors}.

-doc """
Prune every subscription owned by a dead `Pid` (on its `DOWN`) and drop its
monitor entry. Removes the rows from both tables.
""".
-spec prune_subscriber(pid(), #state{}) -> #state{}.
prune_subscriber(Pid, #state{monitors = Monitors} = State) ->
    %% Find and remove all of this pid's subscription rows from both tables.
    Rows = ets:match_object(?SUBS_TABLE, {'_', '_', Pid, '_', '_'}),
    lists:foreach(
        fun({SubRef, AnnouncementClass, _Pid, _Handler, _Once}) ->
            remove_subscription(SubRef, AnnouncementClass)
        end,
        Rows
    ),
    State#state{monitors = maps:remove(Pid, Monitors)}.

%%====================================================================
%% Internal: table writes
%%====================================================================

-doc """
Remove one subscription's rows from both the primary table and the by-class
index. Runs inside the gen_server (serialised write).
""".
-spec remove_subscription(sub_ref(), announcement_class()) -> ok.
remove_subscription(SubRef, AnnouncementClass) ->
    true = ets:delete(?SUBS_TABLE, SubRef),
    true = ets:delete(?BY_CLASS_TABLE, {AnnouncementClass, SubRef}),
    ok.

%%====================================================================
%% Internal: structured errors
%%====================================================================

-doc "Build the `#beamtalk_error{}` for an invalid `subscribe/4` argument set.".
-spec bad_subscribe_args(term(), term(), term()) -> #beamtalk_error{}.
bad_subscribe_args(AnnouncementClass, SubscriberPid, OnceFlag) ->
    %% This runs in the *caller's* process (validation precedes the
    %% gen_server:call), not the bus process, so the `domain` metadata set in
    %% init/1 does not apply — set it explicitly on this call.
    ?LOG_ERROR(#{
        event => invalid_subscribe_args,
        announcement_class => AnnouncementClass,
        subscriber => SubscriberPid,
        once_flag => OnceFlag,
        domain => [beamtalk, announcements]
    }),
    #beamtalk_error{
        kind = invalid_argument,
        class = 'Announcer',
        selector = 'subscribe',
        message =
            <<"subscribe expects an atom class, a pid subscriber, and a boolean once flag">>,
        hint = <<"check the announcement class and subscriber arguments">>,
        details = #{
            announcement_class => AnnouncementClass,
            subscriber => SubscriberPid,
            once_flag => OnceFlag
        }
    }.
