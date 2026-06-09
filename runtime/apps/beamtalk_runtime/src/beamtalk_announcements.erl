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

BT-2441 adds the synchronous, fault-isolated path and the once-only consume:

- **`announceAndWait/2,3` — synchronous gather, caller-side.** Walks the same MRO
  chain as `announce/2` to collect matching subscriptions, then `spawn_monitor`s
  one transient process *per matching subscription* to run that subscription's
  handler. The caller's `receive` loop gathers `{Ref, ok}` acks and treats a
  `{'DOWN', Ref, process, _, Reason}` as the handler-crashed signal — logged
  `domain => [beamtalk, announcements]`, never re-raised into the caller or a
  sibling handler. A per-handler timeout (5s default, configurable in ms via
  `announceAndWait/3`) ejects the loop so a slow or wedged handler can never leave
  the caller blocked forever. Because the gather runs entirely in the *caller's*
  process — there is no shared gen_server in the path — an `announceAndWait/2`
  issued from inside a handler is reentrant-safe and cannot deadlock.

- **Handler invocation (`run_handler/2`).** The transient process runs the stored
  handler: a `fun/1` (block) is applied to the event, a `fun/0` is applied with no
  args, and a `{send, Selector, Receiver}` handler (the `when:send:to:` form) is
  dispatched via `beamtalk_message_dispatch:send/3` with the event as the sole
  argument. Any other opaque term is a no-op success (the async path delivers it
  as a message to the subscriber instead — see `deliver/3`).

- **`doOnce` consumed atomically.** A subscription registered with `OnceFlag =
  true` is consumed with an atomic `ets:take/2` on its unique `SubRef` at delivery
  time, on both the async (`announce/2`) and sync (`announceAndWait/2,3`) paths.
  With caller-side dispatch two announces may race for the same once-only row;
  whichever wins the `take` delivers and the loser gets `[]` and skips it, so a
  `doOnce` subscription fires **at most once** even under concurrent announcers.
  The by-class index row is removed alongside the primary row so the consumed
  subscription leaves no stale index entry.

Design (the `beamtalk_xref` / `beamtalk_trace_store` discipline — the gen_server
is NOT in the dispatch path):

- **Subscription table = ETS, gen_server owns writes only.** Each subscription
  is a row `{SubRef, AnnouncerRef, AnnouncementClass, SubscriberPid, Handler,
  OnceFlag}` in a `set` keyed by the unique `SubRef` the returned `Subscription`
  wraps. Keying by `SubRef` — not `{Class, Pid}` — means a process may hold
  multiple distinct subscriptions to the same class (Pharo's rule): a second
  `subscribe` adds a row, it never replaces the first. The `AnnouncerRef`
  dimension scopes each subscription to one announcer namespace so distinct
  `Announcer` instances are isolated (BT-2454): a `reference()` per
  `Announcer new`, or the `?SYSTEM_ANNOUNCER_REF` atom for the system bus and the
  raw Layer 1 API. A secondary by-class index (`ordered_set` on
  `{AnnouncerRef, AnnouncementClass, SubRef}`) lets `announce/2` fetch the
  matching subscribers for one announcer+class without scanning the whole table,
  and the MRO walk fetches it once per class on the event's superclass chain.

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
  from the surviving rows and eagerly prunes any subscriber that died during the
  crash→restart gap via `is_process_alive/1` (BT-2442).

Out of scope here (later issues): the stdlib typed veneer classes — `Announcer` /
`Announcement` / `Subscription` (BT-2444).

See also: docs/ADR/0093-announcements-event-substrate.md §1
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").

%% API — lifecycle
-export([start_link/0]).

%% API — write path (through the gen_server)
-export([
    subscribe/4,
    subscribe/5,
    unsubscribe/1,
    is_active/1
]).

%% API — dispatch (caller-side, off concurrent ETS reads)
-export([
    announce/2,
    announce/3,
    announceAndWait/2,
    announceAndWait/3
]).

%% API — introspection (direct ETS reads)
-export([
    subscription_count/0,
    subscribers_of/1
]).

%% FFI shims — introspection / navigation veneer (BT-2444 / BT-2454)
-export([
    'subscriptionNodes'/1,
    'subscriptionNodesFor'/2,
    'subscriptionCountOn'/1,
    'navigationFor'/1,
    'navSubscriptionNodes'/1,
    'navSubscriptionNodesFor'/2,
    'navAnnouncedClasses'/1
]).

%% FFI shims for stdlib veneer (BT-2443)
-export([
    'newAnnouncer'/0,
    'systemAnnouncer'/0,
    'systemAnnounceAndWaitError'/1,
    'whenDo'/3,
    'whenSendTo'/4,
    'whenDoOnce'/3,
    'announceOn'/2,
    'announceAndWaitOn'/2,
    'announceAndWaitOn'/3,
    'unsubscribeReceiver'/2,
    'unsubscribeRef'/1,
    'isActiveRef'/1,
    ensure_started/0
]).

%% System emit points (ADR 0093 §2 — runtime publishes well-known system events
%% on the singleton SystemAnnouncer). Called from runtime modules (e.g.
%% beamtalk_object_class, beamtalk_actor, beamtalk_supervisor) at the triggering
%% action, after the action's metadata writes commit.
-export([
    system_announce/2
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

%% Per-handler timeout for the synchronous `announceAndWait/2` gather (ADR 0093
%% §1 — "defaults to 5 s", configurable in ms via `announceAndWait/3`).
-define(DEFAULT_ANNOUNCE_TIMEOUT, 5000).

%% The well-known announcer namespace shared by the runtime system bus and the
%% raw Layer 1 API (BT-2454). Per-instance `Announcer new` handles each get a
%% unique `reference()` namespace, isolated from this and from each other; the
%% atom-keyed system namespace is where `SystemAnnouncer` and `system_announce/2`
%% publish, and where the low-level `subscribe/4` / `announce/2` primitives
%% operate by default. Matches the ref carried by `systemAnnouncer/0`.
-define(SYSTEM_ANNOUNCER_REF, beamtalk_system_announcer).

%%====================================================================
%% Types
%%====================================================================

-type announcement_class() :: atom().
-type sub_ref() :: reference().
%% An announcer namespace: a unique `reference()` for a per-instance `Announcer`,
%% or the well-known `?SYSTEM_ANNOUNCER_REF` atom for the system bus / Layer 1
%% (BT-2454).
-type announcer_ref() :: reference() | atom().
-type handler() :: term().

%% Beamtalk object types for FFI type inference (BT-2443).
-type announcer() :: #{'$beamtalk_class' := 'Announcer', atom() => term()}.
-type system_announcer() :: #{'$beamtalk_class' := 'SystemAnnouncer', atom() => term()}.
-type subscription() :: #{'$beamtalk_class' := 'Subscription', atom() => term()}.

%% An immutable `SubscriptionNode` snapshot record (BT-2444 / ADR 0093 §7). A
%% tagged map minted here so the Beamtalk type checker infers FFI results as
%% `SubscriptionNode`. Fields mirror the `sealed typed Value subclass`
%% declaration: the subscribed-to event class (a class object), the announcer
%% scope it was read from, the live subscriber pid, the handler kind
%% (`#do | #send | #doOnce`), and the `once` flag.
-type subscription_node() :: #{'$beamtalk_class' := 'SubscriptionNode', atom() => term()}.

%% An `AnnouncementNavigation` handle (BT-2454): a tagged map carrying the
%% announcer scope it navigates, minted by `navigationFor/1`. Tagged so the
%% Beamtalk type checker infers FFI results as `AnnouncementNavigation`.
-type announcement_navigation() :: #{
    '$beamtalk_class' := 'AnnouncementNavigation', atom() => term()
}.

%% A subscription row in the primary `set` table, keyed by the unique `SubRef`.
%% The `AnnouncerRef` dimension scopes the row to one announcer namespace so
%% distinct announcers are isolated (BT-2454).
-type sub_row() :: {
    sub_ref(),
    announcer_ref(),
    announcement_class(),
    pid(),
    handler(),
    OnceFlag :: boolean()
}.

-export_type([
    announcement_class/0,
    sub_ref/0,
    announcer_ref/0,
    handler/0,
    sub_row/0,
    announcer/0,
    system_announcer/0,
    subscription/0,
    subscription_node/0,
    announcement_navigation/0
]).

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
subscribe(AnnouncementClass, SubscriberPid, Handler, OnceFlag) ->
    %% Layer 1 (raw) API — subscribes on the shared system announcer namespace.
    %% Per-instance announcers go through `subscribe/5` with their own ref.
    subscribe(?SYSTEM_ANNOUNCER_REF, AnnouncementClass, SubscriberPid, Handler, OnceFlag).

-doc """
As `subscribe/4`, but scoped to the announcer namespace `AnnouncerRef`
(BT-2454) — a `reference()` for a per-instance `Announcer`, or the
`?SYSTEM_ANNOUNCER_REF` atom for the system bus. A subscription registered under
one announcer is never matched by an `announce` on another.
""".
-spec subscribe(announcer_ref(), announcement_class(), pid(), handler(), boolean()) ->
    {ok, sub_ref()} | {error, #beamtalk_error{}}.
subscribe(AnnouncerRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag) when
    is_atom(AnnouncementClass), is_pid(SubscriberPid), is_boolean(OnceFlag)
->
    gen_server:call(
        ?MODULE, {subscribe, AnnouncerRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag}
    );
subscribe(_AnnouncerRef, AnnouncementClass, SubscriberPid, _Handler, OnceFlag) ->
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
    %% Layer 1 (raw) API — dispatches on the shared system announcer namespace.
    announce(?SYSTEM_ANNOUNCER_REF, EventClass, Event).

-doc """
As `announce/2`, but scoped to the announcer namespace `AnnouncerRef` (BT-2454):
only subscriptions registered on that announcer (or an ancestor class within it)
are delivered to. Fire-and-forget, caller-side.
""".
-spec announce(announcer_ref(), announcement_class(), term()) -> ok.
announce(AnnouncerRef, EventClass, Event) when is_atom(EventClass) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            ok;
        _ ->
            lists:foreach(
                fun(SubRef) -> deliver(SubRef, EventClass, Event) end,
                collect_matching(AnnouncerRef, EventClass)
            )
    end.

-doc """
Announce `Event` synchronously: deliver to every matching subscriber *and wait*
for each handler to complete, with per-handler fault isolation and a default 5 s
timeout. Equivalent to `announceAndWait(EventClass, Event,
?DEFAULT_ANNOUNCE_TIMEOUT)`. See `announceAndWait/3`.
""".
-spec announceAndWait(announcement_class(), term()) -> ok.
announceAndWait(EventClass, Event) when is_atom(EventClass) ->
    announceAndWait(EventClass, Event, ?DEFAULT_ANNOUNCE_TIMEOUT).

-doc """
Announce `Event` synchronously to every subscriber of `EventClass` *or any of its
ancestors* and gather each handler's completion before returning. Runs entirely
in the *calling* process — there is no shared gen_server in the path, so an
`announceAndWait/2,3` issued from inside a handler is reentrant-safe and cannot
deadlock.

Matching uses the same MRO walk and per-subscription de-duplication as
`announce/2`. For each matching subscription the bus `spawn_monitor`s one
transient process that runs the subscription's handler (see `run_handler/2`) and
sends a `{handler_ack, ReplyRef}` ack to the caller on success. The caller's
`receive` loop gathers those acks; a `{'DOWN', MonRef, process, _, Reason}` with
an abnormal `Reason` is the handler-crashed signal — caught, logged
`domain => [beamtalk, announcements]`, and **never** re-raised into the caller or
propagated to sibling handlers (each runs in its own process). A per-handler
`Timeout` (ms) ejects the wait so a slow or wedged handler cannot block the caller
forever; on timeout every still-pending handler is demonitored (a final
`DOWN`/ack flushed) and logged, and the call returns — the lingering handler
process is left to finish or die on its own (it is unlinked, so it cannot harm the
caller).

`doOnce` subscriptions are consumed with the same atomic `ets:take/2` as the async
path, so a once-only subscription fires at most once across concurrent
`announce`/`announceAndWait` callers. Returns `ok` once every spawned handler has
acked, crashed, or timed out.
""".
-spec announceAndWait(announcement_class(), term(), timeout()) -> ok.
announceAndWait(EventClass, Event, Timeout) when is_atom(EventClass) ->
    %% Layer 1 (raw) API — dispatches on the shared system announcer namespace.
    do_announce_and_wait(?SYSTEM_ANNOUNCER_REF, EventClass, Event, Timeout).

-doc """
The announcer-scoped core of the synchronous path (BT-2454): gather the matching
subscriptions for `AnnouncerRef`+`EventClass` (MRO), spawn one monitored handler
process per surviving claim, and gather every reply under `Timeout`. Shared by
the Layer 1 `announceAndWait/2,3` (system namespace) and the per-instance veneer
`announceAndWaitOn/2,3`.
""".
-spec do_announce_and_wait(announcer_ref(), announcement_class(), term(), timeout()) -> ok.
do_announce_and_wait(AnnouncerRef, EventClass, Event, Timeout) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            ok;
        _ ->
            %% Spawn one monitored handler process per matching subscription that
            %% survives the atomic claim (doOnce consume happens here, so the
            %% concurrent-race guarantee holds). A subscription whose subscriber
            %% process has already died is skipped — its handler is not run — for
            %% parity with the async `deliver/3` guard (a doOnce row is still
            %% consumed by the claim, matching the async path). Then gather every
            %% reply.
            Pending = lists:foldl(
                fun(SubRef, Acc) ->
                    case claim_row(SubRef) of
                        {ok, _Class, SubscriberPid, Handler, _Once} ->
                            case is_process_alive(SubscriberPid) of
                                true ->
                                    spawn_handler(
                                        EventClass, Event, Handler, SubscriberPid, Acc
                                    );
                                false ->
                                    Acc
                            end;
                        not_found ->
                            Acc
                    end
                end,
                #{},
                collect_matching(AnnouncerRef, EventClass)
            ),
            gather_replies(Pending, EventClass, Timeout)
    end.

-doc """
Collect the `SubRef`s of every subscription on `AnnouncerRef` matching
`EventClass` *or any of its ancestors* (BT-2454 — scoped to the announcer
namespace), de-duplicated per `SubRef`, by walking the event class's superclass
chain (`beamtalk_class_metadata:lookup_superclass/1`) and reading the by-class
index (keyed `{AnnouncerRef, Class, SubRef}`) at each level. Caller-side; no bus
call. Used by every dispatch path — `announce/2,3`, `announceAndWait` (via
`do_announce_and_wait/4`), and the veneer `dispatch_veneer_async/3` — which then
deliver to / spawn handlers for the returned refs; the actual subscription row
(handler, once flag, liveness) is read at delivery time, so this list is purely a
matching set.

The walk stops gracefully at the root (`{ok, none}`), at a class whose metadata
row is absent (`not_found` — e.g. a class removed mid-walk), and at the
`?MAX_HIERARCHY_DEPTH` cap that bounds a corrupted cyclic hierarchy.
""".
-spec collect_matching(announcer_ref(), announcement_class()) -> [sub_ref()].
collect_matching(AnnouncerRef, EventClass) ->
    Refs = walk_collect(AnnouncerRef, EventClass, EventClass, sets:new([{version, 2}]), [], 0),
    lists:reverse(Refs).

-spec walk_collect(
    announcer_ref(),
    announcement_class(),
    announcement_class(),
    sets:set(sub_ref()),
    [sub_ref()],
    non_neg_integer()
) -> [sub_ref()].
walk_collect(_AnnouncerRef, _CurrentClass, EventClass, _Seen, Acc, Depth) when
    Depth > ?MAX_HIERARCHY_DEPTH
->
    %% Runs in the *announcing* caller's process (dispatch is caller-side), not the
    %% bus process, so the `domain` metadata set in init/1 does not apply — set it
    %% explicitly, matching bad_subscribe_args/3.
    ?LOG_WARNING(#{
        event => announcement_mro_walk_truncated,
        reason => max_depth_exceeded,
        event_class => EventClass,
        max_depth => ?MAX_HIERARCHY_DEPTH,
        domain => [beamtalk, announcements]
    }),
    Acc;
walk_collect(AnnouncerRef, CurrentClass, EventClass, Seen, Acc, Depth) ->
    %% Gather every subscription registered against `AnnouncerRef`+`CurrentClass`,
    %% skipping any SubRef already seen on this walk (per-subscription de-dup).
    IndexRows = ets:match_object(?BY_CLASS_TABLE, {{AnnouncerRef, CurrentClass, '_'}}),
    {Seen1, Acc1} = lists:foldl(
        fun({{_Ann, _Class, SubRef}}, {SeenAcc, RefAcc}) ->
            case sets:is_element(SubRef, SeenAcc) of
                true -> {SeenAcc, RefAcc};
                false -> {sets:add_element(SubRef, SeenAcc), [SubRef | RefAcc]}
            end
        end,
        {Seen, Acc},
        IndexRows
    ),
    %% Walk to the superclass, reading the live metadata at announce time. A
    %% missing row (`not_found`) truncates the walk gracefully (class removed
    %% mid-walk); the root (`{ok, none}`) ends it normally.
    case beamtalk_class_metadata:lookup_superclass(CurrentClass) of
        {ok, none} ->
            Acc1;
        {ok, Super} ->
            walk_collect(AnnouncerRef, Super, EventClass, Seen1, Acc1, Depth + 1);
        not_found ->
            Acc1
    end.

-doc """
Deliver `Event` to one subscription (async path) if it is still live and its
subscriber process is alive. A `doOnce` subscription is consumed atomically via
`claim_row/1` so it fires at most once even under concurrent announcers; a
non-once subscription is read without removal. Skips silently when the row is
gone (unsubscribed, already consumed by a racing once-only delivery, or the
subscriber died between the index read and here — all harmless). Caller-side; no
bus call.
""".
-spec deliver(sub_ref(), announcement_class(), term()) -> ok.
deliver(SubRef, EventClass, Event) ->
    case claim_row(SubRef) of
        {ok, _Class, SubscriberPid, Handler, _Once} ->
            case is_process_alive(SubscriberPid) of
                true ->
                    SubscriberPid ! {beamtalk_announcement, SubRef, EventClass, Handler, Event},
                    ok;
                false ->
                    ok
            end;
        not_found ->
            ok
    end.

-doc """
Read a subscription row for delivery, consuming it if it is a `doOnce`
subscription. For a once-only row (`OnceFlag = true`) the read is an atomic
`ets:take/2` on the unique `SubRef`: exactly one concurrent caller gets the row
and delivers, every other gets `not_found` and skips — so the subscription fires
at most once. The by-class index entry is removed alongside the consumed row so no
stale index entry lingers. A non-once row is read with `ets:lookup/2` (left in
place). Returns `not_found` for an unknown / already-consumed / unsubscribed ref.
""".
-spec claim_row(sub_ref()) ->
    {ok, announcement_class(), pid(), handler(), boolean()} | not_found.
claim_row(SubRef) ->
    case ets:lookup(?SUBS_TABLE, SubRef) of
        [{SubRef, _AnnouncerRef, Class, Pid, Handler, false}] ->
            {ok, Class, Pid, Handler, false};
        [{SubRef, _AnnouncerRef, _Class, _Pid, _Handler, true}] ->
            %% doOnce — atomically consume on the unique key. Whichever concurrent
            %% caller wins the `take` delivers; the rest get `[]` and skip.
            case ets:take(?SUBS_TABLE, SubRef) of
                [{SubRef, AnnouncerRef, Class, Pid, Handler, true}] ->
                    true = ets:delete(?BY_CLASS_TABLE, {AnnouncerRef, Class, SubRef}),
                    {ok, Class, Pid, Handler, true};
                [] ->
                    not_found
            end;
        [] ->
            not_found
    end.

%%====================================================================
%% Internal: synchronous gather (announceAndWait)
%%====================================================================

-doc """
Spawn one monitored transient process to run `Handler` for the synchronous path,
recording it in the `Pending` map keyed by its monitor `Ref`. The value is
`{ReplyRef, HandlerPid, SubscriberPid}`: `ReplyRef` is a fresh reference minted in
the caller and closed over by the spawned process, which — on a clean run of
`run_handler/2` — sends `{handler_ack, ReplyRef}` back to the caller. If the
handler raises, the process exits abnormally and no ack is sent; the monitor
`DOWN` (keyed by the same `MonRef`) carries the reason and `gather_replies/3`
treats it as the isolated-crash signal. Two refs are needed because the child
cannot know its own monitor ref; `gather_replies/3` correlates the ack's
`ReplyRef` back to the monitor `Ref`. `HandlerPid` is kept so a timed-out handler
can be killed; `SubscriberPid` for the crash/timeout diagnostic log. The handler
runs under `spawn_monitor` (monitored, **not** linked), so neither its crash nor a
kill signal can reach the caller.
""".
-spec spawn_handler(announcement_class(), term(), handler(), pid(), Pending) -> Pending when
    Pending :: #{reference() => {reference(), pid(), pid()}}.
spawn_handler(_EventClass, Event, Handler, SubscriberPid, Pending) ->
    Caller = self(),
    ReplyRef = make_ref(),
    {HandlerPid, MonRef} = spawn_monitor(fun() ->
        ok = run_handler(Handler, Event),
        Caller ! {handler_ack, ReplyRef},
        ok
    end),
    Pending#{MonRef => {ReplyRef, HandlerPid, SubscriberPid}}.

-doc """
Run one subscription's stored handler for the synchronous path, returning `ok` on
completion. Handler forms:

- a `fun/1` (a block taking the event) is applied to `Event`;
- a `fun/0` (a no-arg block) is applied with no args;
- `{send, Selector, Receiver}` (the `when:send:to:` form) is dispatched via
  `beamtalk_message_dispatch:send(Receiver, Selector, [Event])`;
- any other opaque term is a no-op success — the runtime layer does not interpret
  arbitrary stdlib handler terms in the sync path (the async path delivers them as
  a message to the subscriber instead).

Runs inside the transient `spawn_monitor` process; any exception it raises exits
that process and surfaces to the caller as a monitor `DOWN`, isolated from siblings
and the announcer (`gather_replies/3`).
""".
-spec run_handler(handler(), term()) -> ok.
run_handler(Handler, Event) when is_function(Handler, 1) ->
    _ = Handler(Event),
    ok;
run_handler(Handler, _Event) when is_function(Handler, 0) ->
    _ = Handler(),
    ok;
run_handler({send, Selector, Receiver}, Event) when is_atom(Selector) ->
    _ = beamtalk_message_dispatch:send(Receiver, Selector, [Event]),
    ok;
run_handler(_Handler, _Event) ->
    ok.

-doc """
Gather replies from every spawned handler, returning `ok` once each has acked,
crashed, or timed out. `Pending` maps each handler's monitor `MonRef` to its
`{ReplyRef, HandlerPid, SubscriberPid}`. The loop blocks on `Timeout`; on timeout
every still-pending handler is killed (it ran longer than allowed), demonitored,
and logged, then the call returns — so a slow handler can never wedge the caller. A
handler crash (an abnormal monitor `DOWN`) is logged and dropped — never re-raised
— so siblings and the announcer are unaffected.
""".
-spec gather_replies(
    #{reference() => {reference(), pid(), pid()}}, announcement_class(), timeout()
) -> ok.
gather_replies(Pending, _EventClass, _Timeout) when map_size(Pending) =:= 0 ->
    ok;
gather_replies(Pending, EventClass, Timeout) ->
    receive
        {handler_ack, ReplyRef} ->
            %% Correlate the ack's ReplyRef back to its monitor ref. An ack from a
            %% no-longer-pending handler (e.g. one already counted as timed out) is
            %% ignored.
            case find_by_reply_ref(ReplyRef, Pending) of
                {ok, MonRef} ->
                    erlang:demonitor(MonRef, [flush]),
                    gather_replies(maps:remove(MonRef, Pending), EventClass, Timeout);
                error ->
                    gather_replies(Pending, EventClass, Timeout)
            end;
        {'DOWN', MonRef, process, _Pid, Reason} when is_map_key(MonRef, Pending) ->
            #{MonRef := {_ReplyRef, _HandlerPid, SubscriberPid}} = Pending,
            %% `normal` is a clean exit that lost the race with its own ack (or a
            %% handler that returned without our ack ever mattering); only abnormal
            %% reasons are an isolated crash worth logging.
            case Reason of
                normal -> ok;
                _ -> log_handler_crash(EventClass, SubscriberPid, Reason)
            end,
            gather_replies(maps:remove(MonRef, Pending), EventClass, Timeout)
    after Timeout ->
        %% Per-handler timeout: eject every still-pending handler so a slow or
        %% wedged handler cannot block the caller forever. Kill the lingering
        %% handler process (it is monitored, not linked, so the kill cannot reach
        %% the caller), demonitor (flushing its `DOWN`), and flush any
        %% already-queued `{handler_ack, ReplyRef}` so a handler that completed
        %% just as the timeout fired leaves no stale message in the caller's
        %% mailbox.
        maps:foreach(
            fun(MonRef, {ReplyRef, HandlerPid, SubscriberPid}) ->
                exit(HandlerPid, kill),
                _ = erlang:demonitor(MonRef, [flush]),
                flush_ack(ReplyRef),
                log_handler_timeout(EventClass, SubscriberPid, Timeout)
            end,
            Pending
        ),
        ok
    end.

-doc """
Remove a single already-queued `{handler_ack, ReplyRef}` from the caller's
mailbox if present (non-blocking). Called when a handler times out, so a handler
that acked just as the timeout fired does not leave a stale message behind.
`ReplyRef` is unique per spawned handler, so this is selective.
""".
-spec flush_ack(reference()) -> ok.
flush_ack(ReplyRef) ->
    receive
        {handler_ack, ReplyRef} -> ok
    after 0 -> ok
    end.

-doc "Find the monitor ref in `Pending` whose value carries `ReplyRef`.".
-spec find_by_reply_ref(reference(), #{reference() => {reference(), pid(), pid()}}) ->
    {ok, reference()} | error.
find_by_reply_ref(ReplyRef, Pending) ->
    maps:fold(
        fun
            (MonRef, {RR, _HandlerPid, _SubscriberPid}, error) when RR =:= ReplyRef ->
                {ok, MonRef};
            (_MonRef, _V, Acc) ->
                Acc
        end,
        error,
        Pending
    ).

-doc "Log an isolated handler crash on the synchronous path (never re-raised).".
-spec log_handler_crash(announcement_class(), pid(), term()) -> ok.
log_handler_crash(EventClass, SubscriberPid, Reason) ->
    %% Caller-side process, not the bus — set `domain` explicitly (ADR 0093 §1).
    ?LOG_ERROR(#{
        event => announcement_handler_crashed,
        event_class => EventClass,
        subscriber => SubscriberPid,
        reason => Reason,
        domain => [beamtalk, announcements]
    }),
    ok.

-doc "Log a handler that exceeded the per-handler timeout on the synchronous path.".
-spec log_handler_timeout(announcement_class(), pid(), timeout()) -> ok.
log_handler_timeout(EventClass, SubscriberPid, Timeout) ->
    ?LOG_WARNING(#{
        event => announcement_handler_timeout,
        event_class => EventClass,
        subscriber => SubscriberPid,
        timeout_ms => Timeout,
        domain => [beamtalk, announcements]
    }),
    ok.

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
    %% Layer 1 (raw) API — the shared system announcer namespace.
    subscribers_of(?SYSTEM_ANNOUNCER_REF, AnnouncementClass).

-doc """
The `SubRef`s subscribed to exactly `AnnouncementClass` within the announcer
namespace `AnnouncerRef` (BT-2454). Direct ETS read of the by-class index.
""".
-spec subscribers_of(announcer_ref(), announcement_class()) -> [sub_ref()].
subscribers_of(AnnouncerRef, AnnouncementClass) when is_atom(AnnouncementClass) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            [];
        _ ->
            [
                SubRef
             || {{_Ann, _Class, SubRef}} <- ets:match_object(
                    ?BY_CLASS_TABLE, {{AnnouncerRef, AnnouncementClass, '_'}}
                )
            ]
    end.

%%====================================================================
%% FFI shims — introspection / navigation veneer (BT-2444 / BT-2454)
%%
%% Read-only snapshots of the subscription graph as `SubscriptionNode` value
%% records (ADR 0093 §7). All reads are direct ETS reads off the live tables —
%% never routed through the gen_server — and never mutate. Each read is scoped to
%% the announcer's own namespace (BT-2454): a per-instance `Announcer` reports
%% only its own subscriptions, and the system bus reports only the system
%% namespace.
%%====================================================================

-doc """
Snapshot of `Announcer`'s own live subscriptions as a list of `SubscriptionNode`
value records, each stamped with `Announcer` as its `announcer` field. Scoped to
the announcer's namespace (BT-2454). Backs `Announcer subscriptions`
(self-inspection) and `AnnouncementNavigation subscriptions`. Direct read of the
primary `set` table; `[]` when the bus has never started.
""".
-spec 'subscriptionNodes'(announcer() | system_announcer()) -> [subscription_node()].
'subscriptionNodes'(Announcer) ->
    case ets:whereis(?SUBS_TABLE) of
        undefined ->
            [];
        _ ->
            AnnouncerRef = announcer_ref(Announcer),
            [
                subscription_node(Row, Announcer)
             || Row <- ets:match_object(
                    ?SUBS_TABLE, {'_', AnnouncerRef, '_', '_', '_', '_'}
                )
            ]
    end.

-doc """
Snapshot of `Announcer`'s subscriptions to exactly `ClassRef` as
`SubscriptionNode` records, each stamped with `Announcer`. Scoped to the
announcer's namespace (BT-2454). Backs `Announcer subscribersOf:` and
`AnnouncementNavigation subscribersOf:`. `ClassRef` is resolved to its class-name
atom the same way subscribe does (`class_name/1`), so a class object or a bare
atom both work. Reads the by-class index for the `SubRef`s, then the primary
table for each row; `[]` for an unknown / never-subscribed class.
""".
-spec 'subscriptionNodesFor'(announcer() | system_announcer(), term()) ->
    [subscription_node()].
'subscriptionNodesFor'(Announcer, ClassRef) ->
    Class = class_name(ClassRef),
    AnnouncerRef = announcer_ref(Announcer),
    case ets:whereis(?SUBS_TABLE) of
        undefined ->
            [];
        _ ->
            lists:filtermap(
                fun(SubRef) ->
                    %% Re-assert the announcer scope on the primary row (binds
                    %% `AnnouncerRef`), so this read stays correct even if the
                    %% by-class index and primary table ever drift — the index is
                    %% the scoping authority, but we never surface a cross-announcer
                    %% row stamped with the wrong `Announcer`.
                    case ets:lookup(?SUBS_TABLE, SubRef) of
                        [{_SubRef, AnnouncerRef, _Class, _Pid, _Handler, _Once} = Row] ->
                            {true, subscription_node(Row, Announcer)};
                        _ ->
                            false
                    end
                end,
                subscribers_of(AnnouncerRef, Class)
            )
    end.

-doc """
The distinct event classes subscribed to within the announcer namespace
`AnnouncerRef`, as class objects — the "announced classes" in active use on that
announcer (ADR 0093 §7, scoped per BT-2454). Reads the by-class index, filters to
`AnnouncerRef`, dedupes the class atoms, and resolves each to its class object
(atoms with no live class are dropped). `[]` when the bus has never started.
""".
-spec announced_classes(announcer_ref()) -> [term()].
announced_classes(AnnouncerRef) ->
    case ets:whereis(?BY_CLASS_TABLE) of
        undefined ->
            [];
        _ ->
            ClassAtoms = lists:usort([
                Class
             || {{Ann, Class, _SubRef}} <- ets:tab2list(?BY_CLASS_TABLE), Ann =:= AnnouncerRef
            ]),
            lists:filtermap(
                fun(ClassAtom) ->
                    case class_object(ClassAtom) of
                        nil -> false;
                        Obj -> {true, Obj}
                    end
                end,
                ClassAtoms
            )
    end.

-doc """
Number of live subscriptions on `Announcer` (its own namespace, BT-2454) — backs
`Announcer subscriptionCount`. Direct ETS read of the primary table filtered to
the announcer's ref.
""".
-spec 'subscriptionCountOn'(announcer() | system_announcer()) -> non_neg_integer().
'subscriptionCountOn'(Announcer) ->
    case ets:whereis(?SUBS_TABLE) of
        undefined ->
            0;
        _ ->
            AnnouncerRef = announcer_ref(Announcer),
            %% Count matching rows without materialising them (no list build).
            ets:select_count(?SUBS_TABLE, [
                {{'_', AnnouncerRef, '_', '_', '_', '_'}, [], [true]}
            ])
    end.

%%====================================================================
%% FFI shims — AnnouncementNavigation (BT-2444 / BT-2454)
%%
%% The navigation is an FFI-minted handle that carries the announcer scope it
%% navigates (mirroring how `Announcer` carries its ref). `default` wraps the
%% system bus; `of: anAnnouncer` wraps that announcer. The query shims extract
%% the embedded announcer and delegate to the per-announcer reads above.
%%====================================================================

-doc """
Mint an `AnnouncementNavigation` handle scoped to `Announcer` (a per-instance
`Announcer`, or `SystemAnnouncer current` for `default`). The announcer is
carried in the handle so the navigation's queries scope to it.
""".
-spec 'navigationFor'(announcer() | system_announcer()) -> announcement_navigation().
'navigationFor'(Announcer) ->
    #{'$beamtalk_class' => 'AnnouncementNavigation', announcer => Announcer}.

-doc "Snapshot of the navigated announcer's subscriptions. Backs `AnnouncementNavigation subscriptions`.".
-spec 'navSubscriptionNodes'(announcement_navigation()) -> [subscription_node()].
'navSubscriptionNodes'(#{announcer := Announcer}) ->
    'subscriptionNodes'(Announcer);
'navSubscriptionNodes'(_) ->
    [].

-doc "Snapshot of the navigated announcer's subscriptions to `ClassRef`. Backs `AnnouncementNavigation subscribersOf:`.".
-spec 'navSubscriptionNodesFor'(announcement_navigation(), term()) -> [subscription_node()].
'navSubscriptionNodesFor'(#{announcer := Announcer}, ClassRef) ->
    'subscriptionNodesFor'(Announcer, ClassRef);
'navSubscriptionNodesFor'(_, _) ->
    [].

-doc "Distinct event classes in use on the navigated announcer. Backs `AnnouncementNavigation announcedClasses`.".
-spec 'navAnnouncedClasses'(announcement_navigation()) -> [term()].
'navAnnouncedClasses'(#{announcer := Announcer}) ->
    announced_classes(announcer_ref(Announcer));
'navAnnouncedClasses'(_) ->
    [].

%% Build a `SubscriptionNode` value record (tagged map) from a primary-table
%% row, stamping the supplied `Announcer` scope. The handler kind is derived
%% from the row's handler term and once flag: a `{send, _, _}` tuple is `#send`;
%% a once-only block is `#doOnce`; any other block is `#do`. The announcement
%% class atom is resolved to its class object (or `nil` if unloaded).
-spec subscription_node(sub_row(), announcer() | system_announcer()) ->
    subscription_node().
subscription_node({_SubRef, _AnnouncerRef, Class, SubscriberPid, Handler, Once}, Announcer) ->
    #{
        '$beamtalk_class' => 'SubscriptionNode',
        announcementClass => class_object(Class),
        announcer => Announcer,
        subscriber => SubscriberPid,
        handlerKind => handler_kind(Handler, Once),
        once => Once
    }.

%% Classify a stored handler term into the public `handlerKind` symbol.
-spec handler_kind(handler(), boolean()) -> 'do' | 'send' | 'doOnce'.
handler_kind({send, _Selector, _Receiver}, _Once) ->
    'send';
handler_kind(_Handler, true) ->
    'doOnce';
handler_kind(_Handler, false) ->
    'do'.

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

    %% Re-arm monitors for subscriptions that survived a crash via heir;
    %% prune any subscriber that died during the crash→restart gap (BT-2442).
    RowsBefore = ets:info(?SUBS_TABLE, size),
    Monitors = rearm_monitors(),
    RowsAfter = ets:info(?SUBS_TABLE, size),
    PrunedSubscriptions = RowsBefore - RowsAfter,

    ?LOG_INFO(#{
        event => announcements_started,
        tables => [?SUBS_TABLE, ?BY_CLASS_TABLE],
        pg_scope => ?PG_SCOPE,
        rearmed_subscribers => maps:size(Monitors),
        pruned_dead_subscriptions => PrunedSubscriptions
    }),
    {ok, #state{monitors = Monitors}}.

handle_call(
    {subscribe, AnnouncerRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag}, _From, State
) ->
    SubRef = make_ref(),
    true = ets:insert(
        ?SUBS_TABLE, {SubRef, AnnouncerRef, AnnouncementClass, SubscriberPid, Handler, OnceFlag}
    ),
    true = ets:insert(?BY_CLASS_TABLE, {{AnnouncerRef, AnnouncementClass, SubRef}}),
    NewState = arm_monitor(SubscriberPid, State),
    {reply, {ok, SubRef}, NewState};
handle_call({unsubscribe, SubRef}, _From, State) ->
    NewState =
        case ets:lookup(?SUBS_TABLE, SubRef) of
            [{SubRef, AnnouncerRef, AnnouncementClass, SubscriberPid, _Handler, _Once}] ->
                remove_subscription(SubRef, AnnouncerRef, AnnouncementClass),
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
            try pg:start_link(?PG_SCOPE) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok
            catch
                %% pg may not be available in all environments (e.g. minimal
                %% BUnit test runner). The pg scope is only needed for
                %% SystemAnnouncer cluster membership — local dispatch works
                %% without it.
                _:_ -> ok
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
bookkeeping map. A pid that died during the crash→restart gap is pruned eagerly
via `is_process_alive/1` — its rows are removed from both tables immediately so
no stale pids accumulate (BT-2442). Live pids get a fresh `erlang:monitor/2`.
""".
-spec rearm_monitors() -> #{pid() => {reference(), pos_integer()}}.
rearm_monitors() ->
    Rows = ets:tab2list(?SUBS_TABLE),
    lists:foldl(
        fun({SubRef, AnnouncerRef, Class, SubscriberPid, _Handler, _Once}, Acc) ->
            case is_process_alive(SubscriberPid) of
                false ->
                    %% Dead during the crash→restart gap — prune immediately.
                    remove_subscription(SubRef, AnnouncerRef, Class),
                    Acc;
                true ->
                    case Acc of
                        #{SubscriberPid := {MonRef, Count}} ->
                            Acc#{SubscriberPid => {MonRef, Count + 1}};
                        _ ->
                            MonRef = erlang:monitor(process, SubscriberPid),
                            Acc#{SubscriberPid => {MonRef, 1}}
                    end
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
monitor entry. Removes the rows from both tables. The table read is guarded:
a `DOWN` can arrive while the subscription table is momentarily absent (during
the crash→restart heir hand-off, or test teardown), in which case there is
nothing to prune — only the monitor bookkeeping is dropped.
""".
-spec prune_subscriber(pid(), #state{}) -> #state{}.
prune_subscriber(Pid, #state{monitors = Monitors} = State) ->
    %% Find and remove all of this pid's subscription rows from both tables.
    case ets:whereis(?SUBS_TABLE) of
        undefined ->
            ok;
        _ ->
            Rows = ets:match_object(?SUBS_TABLE, {'_', '_', '_', Pid, '_', '_'}),
            lists:foreach(
                fun({SubRef, AnnouncerRef, AnnouncementClass, _Pid, _Handler, _Once}) ->
                    remove_subscription(SubRef, AnnouncerRef, AnnouncementClass)
                end,
                Rows
            )
    end,
    State#state{monitors = maps:remove(Pid, Monitors)}.

%%====================================================================
%% Internal: table writes
%%====================================================================

-doc """
Remove one subscription's rows from both the primary table and the by-class
index. Runs inside the gen_server (serialised write).
""".
-spec remove_subscription(sub_ref(), announcer_ref(), announcement_class()) -> ok.
remove_subscription(SubRef, AnnouncerRef, AnnouncementClass) ->
    true = ets:delete(?SUBS_TABLE, SubRef),
    true = ets:delete(?BY_CLASS_TABLE, {AnnouncerRef, AnnouncementClass, SubRef}),
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

%%====================================================================
%% FFI shims — stdlib veneer (BT-2443)
%%====================================================================

-doc """
Mint a fresh Announcer handle (a tagged map wrapping a unique reference).
Called from `Announcer new` via `(Erlang beamtalk_announcements) newAnnouncer`.
""".
-spec 'newAnnouncer'() -> announcer().
'newAnnouncer'() ->
    #{'$beamtalk_class' => 'Announcer', ref => make_ref()}.

-doc """
Return the singleton SystemAnnouncer handle. The system bus uses the atom
`beamtalk_system_announcer` as its announcer ref (not a `reference()`) —
distinguished from per-instance announcer refs so the dispatch path can treat
it as the shared bus. Called from `SystemAnnouncer current`.
""".
-spec 'systemAnnouncer'() -> system_announcer().
'systemAnnouncer'() ->
    #{'$beamtalk_class' => 'SystemAnnouncer', ref => beamtalk_system_announcer}.

-doc """
Raise UnsupportedOperation when `announceAndWait:` is called on the system bus.
The unused argument is the event passed from the Beamtalk FFI dispatch.
""".
-spec 'systemAnnounceAndWaitError'(term()) -> no_return().
'systemAnnounceAndWaitError'(_Event) ->
    Error = #beamtalk_error{
        kind = unsupported_operation,
        class = 'SystemAnnouncer',
        selector = 'announceAndWait:',
        message = <<"announceAndWait: is not available on SystemAnnouncer (async-only bus)">>,
        hint =
            <<"Use announce: for the system bus, or use a per-instance Announcer for sync dispatch">>,
        details = #{}
    },
    beamtalk_error:raise(Error).

-doc """
Subscribe via `when:do:` — wraps `subscribe/4` and returns a Subscription handle.
Called from `Announcer when: aClass do: aBlock`.
""".
-spec 'whenDo'(announcer(), term(), term()) -> subscription().
'whenDo'(Announcer, Class, Handler) ->
    subscribe_and_wrap(Announcer, Class, Handler, false).

-doc """
Subscribe via `when:send:to:` — wraps the selector/receiver into a handler tuple.
Called from `Announcer when: aClass send: sel to: receiver`.
""".
-spec 'whenSendTo'(announcer(), term(), term(), term()) -> subscription().
'whenSendTo'(Announcer, Class, Selector, Receiver) ->
    subscribe_and_wrap(Announcer, Class, {send, Selector, Receiver}, false).

-doc """
Subscribe via `when:doOnce:` — wraps `subscribe/4` with OnceFlag=true.
Called from `Announcer when: aClass doOnce: aBlock`.
""".
-spec 'whenDoOnce'(announcer(), term(), term()) -> subscription().
'whenDoOnce'(Announcer, Class, Handler) ->
    subscribe_and_wrap(Announcer, Class, Handler, true).

-doc """
Announce an event asynchronously on a specific announcer.
Called from `Announcer announce: anEvent`. Extracts the event class from
the event's `$beamtalk_class` key and dispatches.
""".
-spec 'announceOn'(announcer(), term()) -> nil.
'announceOn'(Announcer, Event) ->
    ensure_started(),
    dispatch_veneer_async(announcer_ref(Announcer), event_class(Event), Event),
    nil.

-doc """
Dispatch `Event` (an announcement payload map) asynchronously to every veneer
subscriber of `EventClass` *or any of its ancestors* (MRO match).

For the stdlib veneer, handlers are invoked caller-side rather than by sending
raw `{beamtalk_announcement, ...}` messages to subscriber mailboxes: the raw
`announce/2` requires a Beamtalk actor message loop to process, but a veneer
handler is a Block / `{send, ...}` tuple. Each handler runs in its own transient
process (fire-and-forget) so a slow or crashing handler never blocks the
publisher or its siblings — async semantics (ADR 0093 §1), mirroring the sync
path's `spawn_monitor` minus the gather. Shared by `announceOn/2` (per-instance
announcers) and `system_announce/2` (the system bus). Scoped to `AnnouncerRef`
(BT-2454): only subscriptions on that announcer match.
""".
-spec dispatch_veneer_async(announcer_ref(), announcement_class(), term()) -> ok.
dispatch_veneer_async(AnnouncerRef, EventClass, Event) ->
    lists:foreach(
        fun(SubRef) ->
            case claim_row(SubRef) of
                {ok, _Class, SubscriberPid, Handler, _Once} ->
                    case is_process_alive(SubscriberPid) of
                        true ->
                            _ = spawn(fun() ->
                                try
                                    run_handler(Handler, Event)
                                catch
                                    _:_ -> ok
                                end
                            end),
                            ok;
                        false ->
                            ok
                    end;
                not_found ->
                    ok
            end
        end,
        collect_matching(AnnouncerRef, EventClass)
    ),
    ok.

-doc """
Announce a well-known system event on the singleton `SystemAnnouncer` (ADR 0093
§2). `EventClass` is the announcement subclass atom (e.g. `'ClassLoaded'`) and
`Fields` is a map of its `field:` slots (e.g. `#{className => 'Counter'}`). Builds
the immutable event payload — a tagged `Value` map of the same shape the stdlib
keyword constructor produces — and dispatches it to system-bus subscribers via
the async veneer path.

Async-only and fault-isolated: callers in the runtime invoke this *after* the
triggering action's metadata writes commit, and it never blocks or fails the
caller (the dispatch is fire-and-forget per subscriber). A bus that is not yet
started is started on demand via `ensure_started/0`. Returns `ok`.
""".
-spec system_announce(announcement_class(), map()) -> ok.
system_announce(EventClass, Fields) when is_atom(EventClass), is_map(Fields) ->
    ensure_started(),
    Event = Fields#{'$beamtalk_class' => EventClass},
    dispatch_veneer_async(?SYSTEM_ANNOUNCER_REF, EventClass, Event).

-doc """
Announce an event synchronously on a specific announcer (default 5s timeout).
Called from `Announcer announceAndWait: anEvent`.
""".
-spec 'announceAndWaitOn'(announcer(), term()) -> nil.
'announceAndWaitOn'(Announcer, Event) ->
    ensure_started(),
    EventClass = event_class(Event),
    do_announce_and_wait(announcer_ref(Announcer), EventClass, Event, ?DEFAULT_ANNOUNCE_TIMEOUT),
    nil.

-doc """
Announce an event synchronously with a custom timeout.
Called from `Announcer announceAndWait: anEvent timeout: ms`.
""".
-spec 'announceAndWaitOn'(announcer(), term(), integer()) -> nil.
'announceAndWaitOn'(Announcer, Event, Timeout) ->
    ensure_started(),
    EventClass = event_class(Event),
    do_announce_and_wait(announcer_ref(Announcer), EventClass, Event, Timeout),
    nil.

-doc """
Remove all subscriptions held by a specific receiver pid *on this announcer*
(BT-2454 — scoped to the announcer's namespace, so a receiver's subscriptions on
other announcers are untouched). Called from `Announcer unsubscribe: receiver`.
""".
-spec 'unsubscribeReceiver'(announcer(), term()) -> nil.
'unsubscribeReceiver'(Announcer, Receiver) ->
    ensure_started(),
    ReceiverPid = extract_pid(Receiver),
    case ReceiverPid of
        undefined ->
            nil;
        Pid ->
            %% Find and remove this pid's subscriptions on this announcer only. We
            %% use the gen_server path to ensure monitor bookkeeping is updated.
            AnnouncerRef = announcer_ref(Announcer),
            Rows = ets:match_object(?SUBS_TABLE, {'_', AnnouncerRef, '_', Pid, '_', '_'}),
            lists:foreach(
                fun({SubRef, _Ann, _Class, _Pid, _Handler, _Once}) ->
                    unsubscribe(SubRef)
                end,
                Rows
            ),
            nil
    end.

-doc """
Unsubscribe a specific subscription by its SubRef (wrapped in a Subscription map).
Called from `Subscription unsubscribe`.
""".
-spec 'unsubscribeRef'(subscription()) -> nil.
'unsubscribeRef'(#{ref := SubRef}) ->
    ensure_started(),
    unsubscribe(SubRef),
    nil;
'unsubscribeRef'(_) ->
    nil.

-doc """
Check if a subscription is still active by its SubRef.
Called from `Subscription isActive`.
""".
-spec 'isActiveRef'(subscription()) -> boolean().
'isActiveRef'(#{ref := SubRef}) ->
    is_active(SubRef);
'isActiveRef'(_) ->
    false.

%%====================================================================
%% Internal: FFI helpers
%%====================================================================

-doc """
Subscribe on behalf of the calling process and return a Subscription handle.
Common implementation for `whenDo`, `whenSendTo`, and `whenDoOnce`.
Ensures the gen_server is running (auto-starts if needed, e.g. in BUnit tests
where the full runtime app may not be started).
""".
-spec subscribe_and_wrap(announcer() | system_announcer(), term(), handler(), boolean()) ->
    subscription().
subscribe_and_wrap(Announcer, ClassRef, Handler, OnceFlag) ->
    ensure_started(),
    Class = class_name(ClassRef),
    AnnouncerRef = announcer_ref(Announcer),
    case subscribe(AnnouncerRef, Class, self(), Handler, OnceFlag) of
        {ok, SubRef} ->
            #{'$beamtalk_class' => 'Subscription', ref => SubRef};
        {error, Error} ->
            erlang:error(Error)
    end.

-doc """
Ensure the announcements gen_server is running. Called from FFI shims before
any operation that requires the gen_server (subscribe, unsubscribe). In the
full runtime, the supervisor starts the gen_server; in BUnit or REPL contexts
where only the stdlib is loaded, this auto-starts it standalone.
""".
-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            %% Start standalone (unlinked from caller) so it persists across
            %% multiple BUnit tests and doesn't die with the caller.
            case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                {error, Reason} -> erlang:error({announcements_start_failed, Reason})
            end;
        _Pid ->
            ok
    end.

-doc """
Extract the class name atom from a class reference. A class reference may be:
- An atom (the class name directly, e.g. from Erlang code or a resolved literal)
- A beamtalk_object tuple with a `"ClassName class"` tag (a live class object)
- A beamtalk_object tuple with a 'Metaclass' class tag
- A map with `$beamtalk_class` (a tagged map class representation)
""".
-spec class_name(term()) -> atom().
class_name(Class) when is_atom(Class) ->
    Class;
class_name({beamtalk_object, Tag, _Module, Pid}) when is_atom(Tag), is_pid(Pid) ->
    %% A class object — the tag is "ClassName class". Extract the class name
    %% by stripping the " class" suffix, or by querying the class gen_server.
    TagStr = atom_to_list(Tag),
    case lists:suffix(" class", TagStr) of
        true ->
            NameStr = lists:sublist(TagStr, length(TagStr) - 6),
            list_to_existing_atom(NameStr);
        false ->
            %% Fallback: query the class gen_server
            case beamtalk_class_registry:class_name_for_pid(Pid) of
                {ok, Name} -> Name;
                _ -> Tag
            end
    end;
class_name(#{'$beamtalk_class' := Class}) when is_atom(Class) ->
    Class;
class_name(_) ->
    'Announcement'.

-doc """
The Beamtalk class object for a class-name atom, or `nil` when the class is not
loaded. Mirrors `beamtalk_process_navigation:class_object/1` so introspection
snapshots can carry rich class objects (navigable) rather than bare atoms.
""".
-spec class_object(atom() | term()) -> term() | nil.
class_object(nil) ->
    nil;
class_object(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> nil;
        Pid when is_pid(Pid) -> beamtalk_class_registry:class_object_from_pid(Pid)
    end;
class_object(_) ->
    nil.

-doc """
Extract the announcer namespace ref from an announcer handle (BT-2454). A
per-instance `Announcer` / `SystemAnnouncer` map carries its ref under the `ref`
key (a `reference()`, or the `?SYSTEM_ANNOUNCER_REF` atom for the system bus).
Anything else (a bare atom passed directly, or an unrecognised term) falls back
to the shared system namespace, matching the Layer 1 default.
""".
-spec announcer_ref(announcer() | system_announcer() | term()) -> announcer_ref().
announcer_ref(#{ref := Ref}) ->
    Ref;
announcer_ref(Ref) when is_reference(Ref); is_atom(Ref) ->
    Ref;
announcer_ref(_) ->
    ?SYSTEM_ANNOUNCER_REF.

-doc """
Extract the announcement class atom from an event (its `$beamtalk_class` key).
""".
-spec event_class(term()) -> announcement_class().
event_class(#{'$beamtalk_class' := Class}) when is_atom(Class) ->
    Class;
event_class(Event) when is_atom(Event) ->
    Event;
event_class(_Event) ->
    'Announcement'.

-doc """
Extract a pid from a receiver argument (may be a Beamtalk object with a pid,
a raw pid, or an actor).
""".
-spec extract_pid(term()) -> pid() | undefined.
extract_pid(Pid) when is_pid(Pid) ->
    Pid;
extract_pid(#{pid := Pid}) when is_pid(Pid) ->
    Pid;
extract_pid(_) ->
    undefined.
