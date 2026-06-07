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
    unsubscribe/1,
    is_active/1
]).

%% API — dispatch (caller-side, off concurrent ETS reads)
-export([
    announce/2,
    announceAndWait/2,
    announceAndWait/3
]).

%% API — introspection (direct ETS reads)
-export([
    subscription_count/0,
    subscribers_of/1
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

%%====================================================================
%% Types
%%====================================================================

-type announcement_class() :: atom().
-type sub_ref() :: reference().
-type handler() :: term().

%% Beamtalk object types for FFI type inference (BT-2443).
-type announcer() :: #{'$beamtalk_class' := 'Announcer', atom() => term()}.
-type system_announcer() :: #{'$beamtalk_class' := 'SystemAnnouncer', atom() => term()}.
-type subscription() :: #{'$beamtalk_class' := 'Subscription', atom() => term()}.

%% A subscription row in the primary `set` table, keyed by `SubRef`.
-type sub_row() :: {
    sub_ref(),
    announcement_class(),
    pid(),
    handler(),
    OnceFlag :: boolean()
}.

-export_type([
    announcement_class/0,
    sub_ref/0,
    handler/0,
    sub_row/0,
    announcer/0,
    system_announcer/0,
    subscription/0
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
            lists:foreach(
                fun(SubRef) -> deliver(SubRef, EventClass, Event) end,
                collect_matching(EventClass)
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
                collect_matching(EventClass)
            ),
            gather_replies(Pending, EventClass, Timeout)
    end.

-doc """
Collect the `SubRef`s of every subscription matching `EventClass` *or any of its
ancestors*, de-duplicated per `SubRef`, by walking the event class's superclass
chain (`beamtalk_class_metadata:lookup_superclass/1`) and reading the by-class
index at each level. Caller-side; no bus call. Used by both `announce/2` (async)
and `announceAndWait/3` (sync), which then deliver to / spawn handlers for the
returned refs — the actual subscription row (handler, once flag, liveness) is read
at delivery time, so this list is purely a matching set.

The walk stops gracefully at the root (`{ok, none}`), at a class whose metadata
row is absent (`not_found` — e.g. a class removed mid-walk), and at the
`?MAX_HIERARCHY_DEPTH` cap that bounds a corrupted cyclic hierarchy.
""".
-spec collect_matching(announcement_class()) -> [sub_ref()].
collect_matching(EventClass) ->
    Refs = walk_collect(EventClass, EventClass, sets:new([{version, 2}]), [], 0),
    lists:reverse(Refs).

-spec walk_collect(
    announcement_class(), announcement_class(), sets:set(sub_ref()), [sub_ref()], non_neg_integer()
) -> [sub_ref()].
walk_collect(_CurrentClass, EventClass, _Seen, Acc, Depth) when
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
walk_collect(CurrentClass, EventClass, Seen, Acc, Depth) ->
    %% Gather every subscription registered against `CurrentClass`, skipping any
    %% SubRef already seen on this walk (per-subscription de-dup).
    IndexRows = ets:match_object(?BY_CLASS_TABLE, {{CurrentClass, '_'}}),
    {Seen1, Acc1} = lists:foldl(
        fun({{_Class, SubRef}}, {SeenAcc, RefAcc}) ->
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
            walk_collect(Super, EventClass, Seen1, Acc1, Depth + 1);
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
        [{SubRef, Class, Pid, Handler, false}] ->
            {ok, Class, Pid, Handler, false};
        [{SubRef, _Class, _Pid, _Handler, true}] ->
            %% doOnce — atomically consume on the unique key. Whichever concurrent
            %% caller wins the `take` delivers; the rest get `[]` and skip.
            case ets:take(?SUBS_TABLE, SubRef) of
                [{SubRef, Class, Pid, Handler, true}] ->
                    true = ets:delete(?BY_CLASS_TABLE, {Class, SubRef}),
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
        fun({SubRef, Class, SubscriberPid, _Handler, _Once}, Acc) ->
            case is_process_alive(SubscriberPid) of
                false ->
                    %% Dead during the crash→restart gap — prune immediately.
                    remove_subscription(SubRef, Class),
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
            Rows = ets:match_object(?SUBS_TABLE, {'_', '_', Pid, '_', '_'}),
            lists:foreach(
                fun({SubRef, AnnouncementClass, _Pid, _Handler, _Once}) ->
                    remove_subscription(SubRef, AnnouncementClass)
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
'whenDo'(_Announcer, Class, Handler) ->
    subscribe_and_wrap(Class, Handler, false).

-doc """
Subscribe via `when:send:to:` — wraps the selector/receiver into a handler tuple.
Called from `Announcer when: aClass send: sel to: receiver`.
""".
-spec 'whenSendTo'(announcer(), term(), term(), term()) -> subscription().
'whenSendTo'(_Announcer, Class, Selector, Receiver) ->
    subscribe_and_wrap(Class, {send, Selector, Receiver}, false).

-doc """
Subscribe via `when:doOnce:` — wraps `subscribe/4` with OnceFlag=true.
Called from `Announcer when: aClass doOnce: aBlock`.
""".
-spec 'whenDoOnce'(announcer(), term(), term()) -> subscription().
'whenDoOnce'(_Announcer, Class, Handler) ->
    subscribe_and_wrap(Class, Handler, true).

-doc """
Announce an event asynchronously on a specific announcer.
Called from `Announcer announce: anEvent`. Extracts the event class from
the event's `$beamtalk_class` key and dispatches.
""".
-spec 'announceOn'(announcer(), term()) -> nil.
'announceOn'(_Announcer, Event) ->
    ensure_started(),
    EventClass = event_class(Event),
    %% For the stdlib veneer, invoke handlers directly (caller-side) rather than
    %% sending raw messages to subscriber mailboxes. The raw `announce/2` sends
    %% `{beamtalk_announcement, ...}` which requires a Beamtalk actor message loop
    %% to process. The sync path (`announceAndWait`) already runs handlers via
    %% spawn_monitor — for the async path we run them in-process without waiting.
    lists:foreach(
        fun(SubRef) ->
            case claim_row(SubRef) of
                {ok, _Class, SubscriberPid, Handler, _Once} ->
                    case is_process_alive(SubscriberPid) of
                        true ->
                            try
                                run_handler(Handler, Event)
                            catch
                                _:_ -> ok
                            end;
                        false ->
                            ok
                    end;
                not_found ->
                    ok
            end
        end,
        collect_matching(EventClass)
    ),
    nil.

-doc """
Announce an event synchronously on a specific announcer (default 5s timeout).
Called from `Announcer announceAndWait: anEvent`.
""".
-spec 'announceAndWaitOn'(announcer(), term()) -> nil.
'announceAndWaitOn'(_Announcer, Event) ->
    ensure_started(),
    EventClass = event_class(Event),
    announceAndWait(EventClass, Event),
    nil.

-doc """
Announce an event synchronously with a custom timeout.
Called from `Announcer announceAndWait: anEvent timeout: ms`.
""".
-spec 'announceAndWaitOn'(announcer(), term(), integer()) -> nil.
'announceAndWaitOn'(_Announcer, Event, Timeout) ->
    ensure_started(),
    EventClass = event_class(Event),
    announceAndWait(EventClass, Event, Timeout),
    nil.

-doc """
Remove all subscriptions held by a specific receiver pid on a given announcer.
Called from `Announcer unsubscribe: receiver`.
""".
-spec 'unsubscribeReceiver'(announcer(), term()) -> nil.
'unsubscribeReceiver'(_Announcer, Receiver) ->
    ReceiverPid = extract_pid(Receiver),
    case ReceiverPid of
        undefined ->
            nil;
        Pid ->
            %% Find and remove all subscriptions for this pid. We use the
            %% gen_server path to ensure monitor bookkeeping is updated.
            Rows = ets:match_object(?SUBS_TABLE, {'_', '_', Pid, '_', '_'}),
            lists:foreach(
                fun({SubRef, _Class, _Pid, _Handler, _Once}) ->
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
-spec subscribe_and_wrap(term(), handler(), boolean()) -> subscription().
subscribe_and_wrap(ClassRef, Handler, OnceFlag) ->
    ensure_started(),
    Class = class_name(ClassRef),
    case subscribe(Class, self(), Handler, OnceFlag) of
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
