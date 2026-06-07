%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_announcements_tests).

-moduledoc """
EUnit tests for `beamtalk_announcements` (BT-2439 / BT-2440 / BT-2441 / BT-2442 /
ADR 0093 Phase 1).

Covers the Phase-1 acceptance criteria: deliver to a subscriber of the exact
class, a dead subscriber pruned automatically via monitor `DOWN`, an unrelated
class delivering nothing, distinct subscriptions to the same class (Pharo's
multi-subscription rule), idempotent unsubscribe, and the introspection reads.

BT-2440 adds the MRO (superclass-chain) matching: delivery to subscribers of an
ancestor class, per-subscription de-duplication across the walk, and graceful
truncation when a class's metadata row is removed mid-hierarchy.

BT-2441 adds the synchronous + once-only + message-send forms with fault
isolation: `announceAndWait/2,3` running each handler in its own monitored
process; a `doOnce` subscription consumed atomically (exactly-once under N
concurrent announcers); the `{send, Sel, Receiver}` (`when:send:to:`) handler
form; per-handler fault isolation (a crashing handler is caught and the caller
still returns, siblings unaffected); a per-handler timeout that ejects a wedged
handler; and reentrant `announceAndWait/2` from inside a handler (no deadlock).

BT-2442 adds the heir crash-survival dead-pid prune: on restart after a bus crash,
the gen_server re-reads the heir-preserved ETS table, re-arms monitors for live
subscribers, and eagerly prunes any subscriber that died during the crash→restart
gap (whose `DOWN` was lost to the dead bus process).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    %% Use an already-running supervised server when one exists (the runtime app
    %% starts the bus under the supervisor); otherwise stand a fresh one up for
    %% tests that run outside the application.
    Pid =
        case whereis(beamtalk_announcements) of
            undefined ->
                {ok, P} = beamtalk_announcements:start_link(),
                P;
            P ->
                P
        end,
    clear_all_subscriptions(),
    Pid.

cleanup(_Pid) ->
    %% Don't stop the server — supervised in the runtime app. Clear all rows so
    %% the next test starts empty even if it picks up the same gen_server.
    clear_all_subscriptions(),
    ok.

clear_all_subscriptions() ->
    %% Tables are `public`, so we can wipe them directly. Route the monitor-state
    %% reset through the gen_server so its bookkeeping does not leak across tests.
    try
        ets:delete_all_objects(beamtalk_announcement_subs),
        ets:delete_all_objects(beamtalk_announcement_by_class),
        sys:replace_state(beamtalk_announcements, fun(S) ->
            %% Clear the in-state monitor map (record field 2).
            setelement(2, S, #{})
        end)
    catch
        _:_ -> ok
    end,
    %% Drop any class-hierarchy rows the MRO tests insert, so a stale superclass
    %% link cannot leak into a later test's announce walk.
    lists:foreach(
        fun(C) ->
            catch beamtalk_class_metadata:delete(C)
        end,
        ['ButtonClicked', 'UIEvent', 'DomainEvent', 'PriceChanged', 'OtherEvent']
    ),
    ok.

%%====================================================================
%% Test subscriber: echoes received announcements back to a collector.
%%====================================================================

%% Spawn a subscriber that forwards every `beamtalk_announcement` message it
%% receives to `Collector`, so the test process can assert on delivery. Replies
%% to a `{stop, From}` message so the test can kill it deterministically.
spawn_subscriber(Collector) ->
    spawn(fun() -> subscriber_loop(Collector) end).

%% Stop a spawned subscriber deterministically so it does not linger across test
%% cases. Sends the `{stop, self()}` message `subscriber_loop/1` handles and
%% waits for the ack; falls back to `exit/2` if the process is wedged or already
%% gone.
stop_subscriber(Sub) ->
    Sub ! {stop, self()},
    receive
        {stopped, Sub} -> ok
    after 500 ->
        exit(Sub, kill),
        ok
    end.

subscriber_loop(Collector) ->
    receive
        {beamtalk_announcement, SubRef, EventClass, Handler, Event} ->
            Collector ! {received, self(), SubRef, EventClass, Handler, Event},
            subscriber_loop(Collector);
        {stop, From} ->
            From ! {stopped, self()},
            ok
    end.

%% Block until a `{received, ...}` arrives or fail after a timeout.
expect_received() ->
    receive
        {received, _Pid, SubRef, EventClass, Handler, Event} ->
            {SubRef, EventClass, Handler, Event}
    after 1000 ->
        ?assert(false)
    end.

%% Assert that NO `{received, ...}` arrives within a short window.
refute_received() ->
    receive
        {received, _Pid, _SubRef, _EventClass, _Handler, _Event} ->
            ?assert(false)
    after 200 ->
        ok
    end.

%%====================================================================
%% Deliver to a subscriber of the exact class
%%====================================================================

deliver_to_subscriber_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef} = beamtalk_announcements:subscribe(
                        'PriceChanged', Sub, my_handler, false
                    ),
                    ?assert(beamtalk_announcements:is_active(SubRef)),

                    ok = beamtalk_announcements:announce('PriceChanged', {price, 42}),
                    {GotRef, GotClass, GotHandler, GotEvent} = expect_received(),
                    ?assertEqual(SubRef, GotRef),
                    ?assertEqual('PriceChanged', GotClass),
                    ?assertEqual(my_handler, GotHandler),
                    ?assertEqual({price, 42}, GotEvent)
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% An unrelated class delivers nothing (no spurious cross-class match)
%%====================================================================

single_class_match_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, _SubRef} = beamtalk_announcements:subscribe(
                        'PriceChanged', Sub, h, false
                    ),
                    %% Announcing an *unrelated* class delivers nothing — with
                    %% no metadata rows the MRO walk truncates immediately and
                    %% 'OtherEvent' is not an ancestor of 'PriceChanged'.
                    ok = beamtalk_announcements:announce('OtherEvent', {x, 1}),
                    refute_received(),
                    %% The matching class still delivers.
                    ok = beamtalk_announcements:announce('PriceChanged', {x, 2}),
                    {_R, 'PriceChanged', h, {x, 2}} = expect_received()
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Distinct subscriptions to the same class (Pharo's multi-sub rule)
%%====================================================================

multiple_subscriptions_same_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef1} = beamtalk_announcements:subscribe('E', Sub, h1, false),
                    {ok, SubRef2} = beamtalk_announcements:subscribe('E', Sub, h2, false),
                    ?assertNotEqual(SubRef1, SubRef2),
                    ?assertEqual(2, beamtalk_announcements:subscription_count()),
                    ?assertEqual(
                        lists:sort([SubRef1, SubRef2]),
                        lists:sort(beamtalk_announcements:subscribers_of('E'))
                    ),

                    ok = beamtalk_announcements:announce('E', payload),
                    %% Both subscriptions deliver (one message per subscription).
                    R1 = expect_received(),
                    R2 = expect_received(),
                    Handlers = lists:sort([element(3, R1), element(3, R2)]),
                    ?assertEqual([h1, h2], Handlers)
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Unsubscribe removes exactly one subscription, is idempotent
%%====================================================================

unsubscribe_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef1} = beamtalk_announcements:subscribe('E', Sub, h1, false),
                    {ok, SubRef2} = beamtalk_announcements:subscribe('E', Sub, h2, false),

                    ok = beamtalk_announcements:unsubscribe(SubRef1),
                    ?assertNot(beamtalk_announcements:is_active(SubRef1)),
                    ?assert(beamtalk_announcements:is_active(SubRef2)),
                    ?assertEqual([SubRef2], beamtalk_announcements:subscribers_of('E')),

                    %% Idempotent: removing the same ref again is a no-op `ok`.
                    ?assertEqual(ok, beamtalk_announcements:unsubscribe(SubRef1)),
                    %% Unknown ref is also a no-op.
                    ?assertEqual(ok, beamtalk_announcements:unsubscribe(make_ref())),

                    ok = beamtalk_announcements:announce('E', payload),
                    {_R, 'E', h2, payload} = expect_received(),
                    %% Only the surviving subscription delivered.
                    refute_received()
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Dead subscriber pruned automatically via monitor DOWN
%%====================================================================

dead_subscriber_pruned_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                {ok, SubRef} = beamtalk_announcements:subscribe('E', Sub, h, false),
                ?assert(beamtalk_announcements:is_active(SubRef)),
                ?assertEqual(1, beamtalk_announcements:subscription_count()),

                %% Kill the subscriber and wait for it to be gone.
                MRef = erlang:monitor(process, Sub),
                exit(Sub, kill),
                receive
                    {'DOWN', MRef, process, Sub, _} -> ok
                after 1000 -> ?assert(false)
                end,

                %% The bus processes the DOWN asynchronously; sync on it via a
                %% gen_server call (subscribe of a throwaway ref forces the
                %% mailbox to drain past the DOWN). Poll until pruned.
                ok = wait_until(fun() ->
                    beamtalk_announcements:subscription_count() =:= 0
                end),
                ?assertNot(beamtalk_announcements:is_active(SubRef)),
                ?assertEqual([], beamtalk_announcements:subscribers_of('E'))
            end)
        ]
    end}.

%%====================================================================
%% No subscribers — announce is a harmless no-op
%%====================================================================

announce_no_subscribers_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                ?assertEqual(ok, beamtalk_announcements:announce('Nobody', whatever)),
                ?assertEqual(0, beamtalk_announcements:subscription_count())
            end)
        ]
    end}.

%%====================================================================
%% Invalid subscribe args return a structured #beamtalk_error{}
%%====================================================================

invalid_subscribe_args_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Non-atom class.
                {error, Err1} = beamtalk_announcements:subscribe(
                    "NotAnAtom", self(), h, false
                ),
                ?assertMatch(#beamtalk_error{kind = invalid_argument, class = 'Announcer'}, Err1),
                %% Non-pid subscriber.
                {error, Err2} = beamtalk_announcements:subscribe(
                    'E', not_a_pid, h, false
                ),
                ?assertMatch(#beamtalk_error{kind = invalid_argument}, Err2)
            end)
        ]
    end}.

%%====================================================================
%% MRO ancestor delivery — subscribe to an ancestor, receive a subclass event
%%====================================================================

%% Hierarchy: ButtonClicked -> UIEvent -> DomainEvent (root). A subscriber of any
%% ancestor receives an announce of the subclass; the delivered EventClass is the
%% *announced* class, not the matched ancestor.
mro_ancestor_delivery_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                build_hierarchy(),
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    %% Subscribe to the *ancestor*, announce the *subclass*.
                    {ok, SubRef} = beamtalk_announcements:subscribe(
                        'UIEvent', Sub, h, false
                    ),
                    ok = beamtalk_announcements:announce('ButtonClicked', {click, 1}),
                    {GotRef, GotClass, GotHandler, GotEvent} = expect_received(),
                    ?assertEqual(SubRef, GotRef),
                    %% Carries the announced class, not the matched ancestor.
                    ?assertEqual('ButtonClicked', GotClass),
                    ?assertEqual(h, GotHandler),
                    ?assertEqual({click, 1}, GotEvent),
                    %% No second delivery from a higher ancestor for this one sub.
                    refute_received()
                after
                    stop_subscriber(Sub)
                end
            end),
            ?_test(begin
                %% Root-class subscriber also receives a deep subclass event.
                build_hierarchy(),
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, _} = beamtalk_announcements:subscribe('DomainEvent', Sub, root_h, false),
                    ok = beamtalk_announcements:announce('ButtonClicked', deep),
                    {_R, 'ButtonClicked', root_h, deep} = expect_received(),
                    refute_received()
                after
                    stop_subscriber(Sub)
                end
            end),
            ?_test(begin
                %% A sibling-class subscriber is NOT matched (not on the chain).
                build_hierarchy(),
                beamtalk_class_metadata:insert('OtherEvent', undefined, undefined, 'DomainEvent'),
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, _} = beamtalk_announcements:subscribe('OtherEvent', Sub, sib, false),
                    ok = beamtalk_announcements:announce('ButtonClicked', x),
                    refute_received()
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Per-subscription de-dup across the MRO walk
%%====================================================================

%% A process holding two subscriptions that both match the event (one on the
%% subclass, one on an ancestor) receives TWO messages — one per subscription
%% (Pharo's rule) — and the matcher never double-sends a single subscription even
%% though the walk visits several ancestor classes.
mro_per_subscription_dedup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                build_hierarchy(),
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    %% Two distinct subscriptions on the same pid: one on the
                    %% exact class, one on an ancestor. Both match 'ButtonClicked'.
                    {ok, SubRefSub} = beamtalk_announcements:subscribe(
                        'ButtonClicked', Sub, on_sub, false
                    ),
                    {ok, SubRefAnc} = beamtalk_announcements:subscribe(
                        'UIEvent', Sub, on_anc, false
                    ),
                    ?assertNotEqual(SubRefSub, SubRefAnc),

                    ok = beamtalk_announcements:announce('ButtonClicked', e),
                    %% Exactly two deliveries — one per subscription.
                    R1 = expect_received(),
                    R2 = expect_received(),
                    refute_received(),
                    Handlers = lists:sort([element(3, R1), element(3, R2)]),
                    ?assertEqual([on_anc, on_sub], Handlers),
                    %% Each subscription delivered exactly once (distinct refs).
                    Refs = lists:sort([element(1, R1), element(1, R2)]),
                    ?assertEqual(lists:sort([SubRefSub, SubRefAnc]), Refs)
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Class removed mid-walk — truncation handled gracefully (no crash)
%%====================================================================

%% If an intermediate class's metadata row is gone, the walk stops at that point
%% with no crash: subscribers below the gap still receive the event, ancestors
%% above it do not.
mro_truncation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                build_hierarchy(),
                Collector = self(),
                SubMid = spawn_subscriber(Collector),
                SubTop = spawn_subscriber(Collector),
                try
                    {ok, RefMid} = beamtalk_announcements:subscribe(
                        'UIEvent', SubMid, mid, false
                    ),
                    {ok, _RefTop} = beamtalk_announcements:subscribe(
                        'DomainEvent', SubTop, top, false
                    ),
                    %% Remove the 'UIEvent' metadata row: the walk from
                    %% 'ButtonClicked' reads superclass = 'UIEvent', delivers to
                    %% RefMid, then lookup_superclass('UIEvent') is not_found and
                    %% the walk truncates before reaching 'DomainEvent'.
                    beamtalk_class_metadata:delete('UIEvent'),

                    ok = beamtalk_announcements:announce('ButtonClicked', t),
                    %% The mid-level subscriber still got it (delivered before the
                    %% gap); a single, well-formed delivery, no crash.
                    {GotRef, 'ButtonClicked', mid, t} = expect_received(),
                    ?assertEqual(RefMid, GotRef),
                    %% The above-the-gap root subscriber gets nothing.
                    refute_received(),
                    %% The bus survived (still answers calls).
                    ?assert(is_integer(beamtalk_announcements:subscription_count()))
                after
                    stop_subscriber(SubMid),
                    stop_subscriber(SubTop)
                end
            end)
        ]
    end}.

%%====================================================================
%% Corrupted cyclic hierarchy — depth cap bounds the walk, no crash, dedup holds
%%====================================================================

%% A metadata cycle (A -> B -> A) cannot loop forever: the depth cap truncates
%% the walk, the bus does not crash, and a subscriber on a class in the cycle
%% still receives exactly one delivery (per-subscription de-dup spans revisits).
mro_cyclic_hierarchy_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Cycle: 'CycA' -> 'CycB' -> 'CycA' -> ...
                beamtalk_class_metadata:insert('CycA', undefined, undefined, 'CycB'),
                beamtalk_class_metadata:insert('CycB', undefined, undefined, 'CycA'),
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef} = beamtalk_announcements:subscribe('CycA', Sub, cyc, false),
                    ok = beamtalk_announcements:announce('CycA', boom),
                    %% Exactly one delivery despite the walk revisiting 'CycA'.
                    {GotRef, 'CycA', cyc, boom} = expect_received(),
                    ?assertEqual(SubRef, GotRef),
                    refute_received(),
                    %% Bus survived the bounded walk.
                    ?assert(is_integer(beamtalk_announcements:subscription_count()))
                after
                    stop_subscriber(Sub),
                    catch beamtalk_class_metadata:delete('CycA'),
                    catch beamtalk_class_metadata:delete('CycB')
                end
            end)
        ]
    end}.

%%====================================================================
%% announceAndWait — synchronous gather runs each handler and returns
%%====================================================================

%% A subscriber with a block handler that records the event into the test
%% process's mailbox; the sync gather runs the handler in its own process and
%% returns only once it has acked.
announce_and_wait_runs_handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_idle(),
                try
                    Handler = fun(Event) -> Collector ! {ran, Event} end,
                    {ok, _SubRef} = beamtalk_announcements:subscribe(
                        'SyncEvent', Sub, Handler, false
                    ),
                    ok = beamtalk_announcements:announceAndWait('SyncEvent', {payload, 1}),
                    %% The handler ran (the sync gather waited for it).
                    receive
                        {ran, Got} -> ?assertEqual({payload, 1}, Got)
                    after 1000 -> ?assert(false)
                    end
                after
                    stop_idle(Sub)
                end
            end),
            ?_test(begin
                %% No subscribers — a harmless no-op that returns ok.
                ?assertEqual(ok, beamtalk_announcements:announceAndWait('Nobody', x))
            end)
        ]
    end}.

%%====================================================================
%% announceAndWait — MRO matching (ancestor subscriber runs its handler)
%%====================================================================

announce_and_wait_mro_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                build_hierarchy(),
                Collector = self(),
                Sub = spawn_idle(),
                try
                    Handler = fun(E) -> Collector ! {anc_ran, E} end,
                    {ok, _} = beamtalk_announcements:subscribe('UIEvent', Sub, Handler, false),
                    %% Announce the subclass synchronously; the ancestor handler runs.
                    ok = beamtalk_announcements:announceAndWait('ButtonClicked', click),
                    receive
                        {anc_ran, Got} -> ?assertEqual(click, Got)
                    after 1000 -> ?assert(false)
                    end
                after
                    stop_idle(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% when:send:to: — a {send, Selector, Receiver} handler dispatches a message
%%====================================================================

%% The handler form for `when:send:to:` is `{send, Selector, Receiver}`. On the
%% sync path the runtime dispatches `Receiver perform: Selector with: Event` via
%% `beamtalk_message_dispatch:send/3`. We exercise it against a real Beamtalk
%% value (an Integer) so the dispatch goes through the genuine runtime path; the
%% full runtime app is started so value dispatch (the extensions table) is live.
when_send_to_dispatches_message_test_() ->
    {setup, fun setup_with_dispatch/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Sub = spawn_idle(),
                try
                    %% {send, Selector, Receiver}: dispatch `printString` to the
                    %% Integer 42 — a real value method. The sync gather runs the
                    %% handler via beamtalk_message_dispatch and the dispatch
                    %% succeeds, so the call returns ok.
                    {ok, _} = beamtalk_announcements:subscribe(
                        'PrintCmd', Sub, {send, 'printString', 42}, false
                    ),
                    ?assertEqual(
                        ok, beamtalk_announcements:announceAndWait('PrintCmd', ignored)
                    )
                after
                    stop_idle(Sub)
                end
            end),
            ?_test(begin
                %% A {send,...} to a selector the receiver does not understand
                %% raises in the handler process — proving the {send,...} form is
                %% genuinely dispatched (an opaque no-op would never crash). The
                %% crash is isolated: the caller still returns ok.
                Sub = spawn_idle(),
                try
                    {ok, _} = beamtalk_announcements:subscribe(
                        'BogusCmd', Sub, {send, bogusSelectorXyz, 7}, false
                    ),
                    ?assertEqual(
                        ok, beamtalk_announcements:announceAndWait('BogusCmd', ignored)
                    )
                after
                    stop_idle(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Fault isolation — a crashing handler is caught, caller returns, siblings ok
%%====================================================================

announce_and_wait_handler_crash_isolated_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 10,
                ?_test(begin
                    Collector = self(),
                    Sub = spawn_idle(),
                    try
                        Crashing = fun(_E) -> error(boom) end,
                        Healthy = fun(E) -> Collector ! {healthy_ran, E} end,
                        {ok, _} = beamtalk_announcements:subscribe(
                            'FaultEvent', Sub, Crashing, false
                        ),
                        {ok, _} = beamtalk_announcements:subscribe(
                            'FaultEvent', Sub, Healthy, false
                        ),
                        %% The caller returns ok despite the crashing handler, and the
                        %% sibling handler still ran (isolation).
                        ?assertEqual(
                            ok, beamtalk_announcements:announceAndWait('FaultEvent', e)
                        ),
                        receive
                            {healthy_ran, Got} -> ?assertEqual(e, Got)
                        after 1000 -> ?assert(false)
                        end,
                        %% The announcer process (this test process) survived intact.
                        ?assert(is_integer(beamtalk_announcements:subscription_count()))
                    after
                        stop_idle(Sub)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% Per-handler timeout — a wedged handler does not block the caller forever
%%====================================================================

announce_and_wait_timeout_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 10,
                ?_test(begin
                    Sub = spawn_idle(),
                    try
                        %% A handler that sleeps far longer than the timeout.
                        Wedged = fun(_E) -> timer:sleep(60000) end,
                        {ok, _} = beamtalk_announcements:subscribe('SlowEvent', Sub, Wedged, false),
                        Start = erlang:monotonic_time(millisecond),
                        %% A short per-handler timeout ejects the wedged handler.
                        ?assertEqual(
                            ok, beamtalk_announcements:announceAndWait('SlowEvent', e, 100)
                        ),
                        Elapsed = erlang:monotonic_time(millisecond) - Start,
                        %% Returned promptly (well under the wedged 60s sleep).
                        ?assert(Elapsed < 5000)
                    after
                        stop_idle(Sub)
                    end
                end)},
            {timeout, 10,
                ?_test(begin
                    %% A handler that finishes just *after* a tight timeout: it acks,
                    %% but the call has already timed out and returned. The stale
                    %% `{handler_ack, _}` must NOT linger in the caller's mailbox (the
                    %% timeout path flushes it).
                    Sub = spawn_idle(),
                    try
                        %% Sleeps past the 50ms timeout, then completes (sends its ack).
                        Lagging = fun(_E) -> timer:sleep(150) end,
                        {ok, _} = beamtalk_announcements:subscribe('LagEvent', Sub, Lagging, false),
                        ?assertEqual(
                            ok, beamtalk_announcements:announceAndWait('LagEvent', e, 50)
                        ),
                        %% Give the lagging handler time to finish and (try to) ack.
                        timer:sleep(300),
                        %% No stale ack message of any shape lingers in this mailbox.
                        receive
                            {handler_ack, _} -> ?assert(false)
                        after 50 -> ok
                        end
                    after
                        stop_idle(Sub)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% Reentrant announceAndWait — from inside a handler, no deadlock
%%====================================================================

announce_and_wait_reentrant_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 10,
                ?_test(begin
                    Collector = self(),
                    Sub = spawn_idle(),
                    try
                        %% Inner subscription whose handler records the inner event.
                        InnerHandler = fun(E) -> Collector ! {inner, E} end,
                        {ok, _} = beamtalk_announcements:subscribe(
                            'InnerEvent', Sub, InnerHandler, false
                        ),
                        %% Outer handler re-announces synchronously from inside itself.
                        OuterHandler = fun(_E) ->
                            ok = beamtalk_announcements:announceAndWait('InnerEvent', nested),
                            Collector ! {outer_done}
                        end,
                        {ok, _} = beamtalk_announcements:subscribe(
                            'OuterEvent', Sub, OuterHandler, false
                        ),
                        %% No shared gen_server in the path, so the reentrant sync
                        %% announce does not deadlock; both fire and the call returns.
                        ?assertEqual(
                            ok, beamtalk_announcements:announceAndWait('OuterEvent', start)
                        ),
                        receive
                            {inner, Got} -> ?assertEqual(nested, Got)
                        after 1000 -> ?assert(false)
                        end,
                        receive
                            {outer_done} -> ok
                        after 1000 -> ?assert(false)
                        end
                    after
                        stop_idle(Sub)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% doOnce — fires at most once for a single announcer
%%====================================================================

do_once_single_announcer_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef} = beamtalk_announcements:subscribe(
                        'OnceEvent', Sub, h, true
                    ),
                    ?assert(beamtalk_announcements:is_active(SubRef)),
                    %% First announce delivers and consumes the subscription.
                    ok = beamtalk_announcements:announce('OnceEvent', first),
                    {GotRef, 'OnceEvent', h, first} = expect_received(),
                    ?assertEqual(SubRef, GotRef),
                    %% The subscription is gone (consumed atomically).
                    ok = wait_until(fun() ->
                        not beamtalk_announcements:is_active(SubRef)
                    end),
                    ?assertEqual([], beamtalk_announcements:subscribers_of('OnceEvent')),
                    %% A second announce delivers nothing.
                    ok = beamtalk_announcements:announce('OnceEvent', second),
                    refute_received()
                after
                    stop_subscriber(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% doOnce — exactly-one delivery under N concurrent announcers
%%====================================================================

%% N processes race to announce the same once-only subscription concurrently.
%% Exactly one delivery must occur — the atomic ets:take guarantees a single
%% winner regardless of how the announces interleave.
do_once_concurrent_announcers_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 30,
                ?_test(begin
                    Collector = self(),
                    Sub = spawn_subscriber(Collector),
                    try
                        {ok, _SubRef} = beamtalk_announcements:subscribe(
                            'RaceEvent', Sub, once_h, true
                        ),
                        %% Fan out N concurrent announcers.
                        N = 50,
                        Parent = self(),
                        Barrier = make_ref(),
                        Pids = [
                            spawn(fun() ->
                                %% Wait for the go signal so they fire as simultaneously
                                %% as the scheduler allows.
                                receive
                                    {go, Barrier} -> ok
                                end,
                                beamtalk_announcements:announce('RaceEvent', payload),
                                Parent ! {announced, self()}
                            end)
                         || _ <- lists:seq(1, N)
                        ],
                        [P ! {go, Barrier} || P <- Pids],
                        %% Wait for all announcers to finish.
                        [
                            receive
                                {announced, P} -> ok
                            after 5000 -> ?assert(false)
                            end
                         || P <- Pids
                        ],
                        %% Exactly one delivery total, despite N concurrent announces.
                        {_R, 'RaceEvent', once_h, payload} = expect_received(),
                        refute_received(),
                        ?assertEqual([], beamtalk_announcements:subscribers_of('RaceEvent'))
                    after
                        stop_subscriber(Sub)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% doOnce — consumed on the synchronous path too
%%====================================================================

do_once_sync_path_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_idle(),
                try
                    Handler = fun(E) -> Collector ! {once_sync, E} end,
                    {ok, SubRef} = beamtalk_announcements:subscribe(
                        'OnceSyncEvent', Sub, Handler, true
                    ),
                    ok = beamtalk_announcements:announceAndWait('OnceSyncEvent', a),
                    receive
                        {once_sync, GotA} -> ?assertEqual(a, GotA)
                    after 1000 -> ?assert(false)
                    end,
                    %% Consumed — the subscription is gone.
                    ?assertNot(beamtalk_announcements:is_active(SubRef)),
                    %% A second sync announce delivers nothing (no handler runs).
                    ok = beamtalk_announcements:announceAndWait('OnceSyncEvent', b),
                    receive
                        {once_sync, _} -> ?assert(false)
                    after 200 -> ok
                    end
                after
                    stop_idle(Sub)
                end
            end)
        ]
    end}.

%%====================================================================
%% Heir crash-survival: live subscriptions survive, dead-in-gap pruned (BT-2442)
%%====================================================================

%% Simulates a bus crash→restart with heir-preserved tables: subscriptions created
%% before the crash survive after restart; a subscriber that dies during the gap
%% (while no bus is running to process its DOWN) is pruned eagerly on restart via
%% the `is_process_alive/1` check in `rearm_monitors/0`.
%%
%% These tests run last (EUnit processes test generators in file order) and use
%% their own setup that ensures a fresh bus exists after the test completes,
%% avoiding interference with other test fixtures.
heir_crash_survival_test_() ->
    {timeout, 15,
        ?_test(begin
            %% We need a fake supervisor registered as `beamtalk_runtime_sup` so the
            %% bus uses it as the ETS heir and the tables survive a bus crash. If a
            %% real supervisor is already registered (full app running), use it.
            FakeSup = ensure_heir_process(),
            try
                %% Stop any existing bus (which may have wrong heir) and restart
                %% fresh so the tables get created with the fake supervisor as heir.
                stop_existing_bus(),
                {ok, _} = beamtalk_announcements:start_link(),
                clear_all_subscriptions(),

                Collector = self(),
                %% Two subscribers: one will stay alive, one will die during the gap.
                LiveSub = spawn_subscriber(Collector),
                DyingSub = spawn_subscriber(Collector),
                try
                    %% Subscribe both before the crash.
                    {ok, LiveRef} = beamtalk_announcements:subscribe(
                        'SurvivalEvent', LiveSub, live_h, false
                    ),
                    {ok, DyingRef} = beamtalk_announcements:subscribe(
                        'SurvivalEvent', DyingSub, dying_h, false
                    ),
                    ?assert(beamtalk_announcements:is_active(LiveRef)),
                    ?assert(beamtalk_announcements:is_active(DyingRef)),
                    ?assertEqual(2, beamtalk_announcements:subscription_count()),

                    %% Kill the bus (simulating a crash). Unlink first so the kill does
                    %% not propagate to the test process. The tables survive via heir
                    %% to the fake supervisor.
                    BusPid = whereis(beamtalk_announcements),
                    unlink(BusPid),
                    BusMon = erlang:monitor(process, BusPid),
                    exit(BusPid, kill),
                    receive
                        {'DOWN', BusMon, process, BusPid, _} -> ok
                    after 2000 -> ?assert(false)
                    end,

                    %% Kill one subscriber during the gap — its DOWN goes nowhere
                    %% because the bus process is dead.
                    DyingMon = erlang:monitor(process, DyingSub),
                    exit(DyingSub, kill),
                    receive
                        {'DOWN', DyingMon, process, DyingSub, _} -> ok
                    after 1000 -> ?assert(false)
                    end,

                    %% The tables still exist (heir-preserved), confirm the rows are
                    %% still there (both subscriptions still in ETS).
                    ?assertEqual(2, ets:info(beamtalk_announcement_subs, size)),

                    %% Restart the bus — it re-reads the heir-preserved tables and
                    %% re-arms monitors, pruning dead-in-gap pids.
                    {ok, _NewBus} = beamtalk_announcements:start_link(),

                    %% The live subscriber's subscription survived.
                    ?assert(beamtalk_announcements:is_active(LiveRef)),
                    %% The dead-during-gap subscriber is pruned.
                    ?assertNot(beamtalk_announcements:is_active(DyingRef)),
                    ?assertEqual(1, beamtalk_announcements:subscription_count()),
                    ?assertEqual([LiveRef], beamtalk_announcements:subscribers_of('SurvivalEvent')),

                    %% Deliver to the live subscriber — proves the re-armed monitor
                    %% and subscription are fully operational after restart.
                    ok = beamtalk_announcements:announce('SurvivalEvent', post_crash),
                    {GotRef, 'SurvivalEvent', live_h, post_crash} = expect_received(),
                    ?assertEqual(LiveRef, GotRef),
                    %% No delivery to the pruned subscription.
                    refute_received()
                after
                    stop_subscriber(LiveSub),
                    catch stop_subscriber(DyingSub),
                    clear_all_subscriptions()
                end
            after
                stop_heir_process(FakeSup)
            end
        end)}.

%%====================================================================
%% Heir crash-survival: dead subscriber monitor fires after restart (BT-2442)
%%====================================================================

%% Verifies that a subscriber which dies *after* restart is still auto-pruned via
%% the re-armed monitor. This confirms re-arm actually wires up working monitors.
heir_rearm_monitor_fires_on_later_death_test_() ->
    {timeout, 15,
        ?_test(begin
            FakeSup = ensure_heir_process(),
            try
                stop_existing_bus(),
                {ok, _} = beamtalk_announcements:start_link(),
                clear_all_subscriptions(),

                Collector = self(),
                Sub = spawn_subscriber(Collector),
                try
                    {ok, SubRef} = beamtalk_announcements:subscribe(
                        'RearmEvent', Sub, h, false
                    ),
                    ?assert(beamtalk_announcements:is_active(SubRef)),

                    %% Kill and restart the bus. Unlink first so the kill does not
                    %% propagate to the test process.
                    BusPid = whereis(beamtalk_announcements),
                    unlink(BusPid),
                    BusMon = erlang:monitor(process, BusPid),
                    exit(BusPid, kill),
                    receive
                        {'DOWN', BusMon, process, BusPid, _} -> ok
                    after 2000 -> ?assert(false)
                    end,
                    {ok, _NewBus} = beamtalk_announcements:start_link(),

                    %% Subscription survived.
                    ?assert(beamtalk_announcements:is_active(SubRef)),

                    %% Now kill the subscriber *after* restart — the re-armed monitor
                    %% should fire and prune it.
                    SubMon = erlang:monitor(process, Sub),
                    exit(Sub, kill),
                    receive
                        {'DOWN', SubMon, process, Sub, _} -> ok
                    after 1000 -> ?assert(false)
                    end,

                    %% Wait for the bus to process the DOWN and prune.
                    ok = wait_until(fun() ->
                        beamtalk_announcements:subscription_count() =:= 0
                    end),
                    ?assertNot(beamtalk_announcements:is_active(SubRef)),
                    ?assertEqual([], beamtalk_announcements:subscribers_of('RearmEvent'))
                after
                    catch stop_subscriber(Sub),
                    clear_all_subscriptions()
                end
            after
                stop_heir_process(FakeSup)
            end
        end)}.

%%====================================================================
%% Helpers
%%====================================================================

%% Stand up a 3-level class hierarchy in the metadata table for the MRO tests:
%% ButtonClicked -> UIEvent -> DomainEvent (root, superclass = none).
%% Rows carry only the superclass link (module/selectors undefined — the MRO walk
%% reads only superclass). cleanup/1 drops these rows after each test.
build_hierarchy() ->
    beamtalk_class_metadata:insert('DomainEvent', undefined, undefined, none),
    beamtalk_class_metadata:insert('UIEvent', undefined, undefined, 'DomainEvent'),
    beamtalk_class_metadata:insert('ButtonClicked', undefined, undefined, 'UIEvent'),
    ok.

%% Set up for the `when:send:to:` test: the standalone bus (via `setup/0`) plus
%% the `beamtalk_extensions` table that real value dispatch reads. Initialising
%% that table is all the `{send, Selector, Receiver}` path needs to dispatch a
%% genuine value method (e.g. `Integer printString`) — no need to start the whole
%% runtime application (which would clash with the standalone bus other tests in
%% this module leave registered). `init/0` is idempotent if the app already
%% created the table.
setup_with_dispatch() ->
    Pid = setup(),
    catch beamtalk_extensions:init(),
    Pid.

%% Spawn an idle process to act as the subscriber pid for synchronous-path tests.
%% On the sync path the subscriber pid is used only to arm the bus monitor and for
%% crash/timeout diagnostics — the handler runs in a transient process the bus
%% spawns, not in this pid — so an inert process that just waits to be stopped is
%% all that is needed (and keeping it alive prevents the bus pruning its rows).
spawn_idle() ->
    spawn(fun idle_loop/0).

idle_loop() ->
    receive
        {stop, From} -> From ! {stopped, self()};
        _ -> idle_loop()
    end.

stop_idle(Pid) ->
    Pid ! {stop, self()},
    receive
        {stopped, Pid} -> ok
    after 500 ->
        exit(Pid, kill),
        ok
    end.

%% Ensure the bus process is registered and running. Used by the heir tests that
%% operate independently of the `{setup, ...}` fixtures.
ensure_bus() ->
    case whereis(beamtalk_announcements) of
        undefined ->
            {ok, _} = beamtalk_announcements:start_link(),
            ok;
        _Pid ->
            ok
    end.

%% Stop any running bus and delete existing ETS tables so a fresh bus can be
%% started with the correct heir. Used by heir tests that need to control the
%% table lifecycle.
stop_existing_bus() ->
    case whereis(beamtalk_announcements) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            Mon = erlang:monitor(process, Pid),
            gen_server:stop(Pid, normal, 2000),
            receive
                {'DOWN', Mon, process, Pid, _} -> ok
            after 2000 ->
                exit(Pid, kill),
                receive
                    {'DOWN', Mon, process, Pid, _} -> ok
                after 1000 -> ok
                end
            end
    end,
    %% Delete any leftover tables so they get recreated with the correct heir.
    catch ets:delete(beamtalk_announcement_subs),
    catch ets:delete(beamtalk_announcement_by_class),
    ok.

%% Ensure a process registered as `beamtalk_runtime_sup` exists to act as the ETS
%% heir in tests. If one already exists (full app running), returns `existing` and
%% `stop_heir_process/1` is a no-op. Otherwise spawns a fake supervisor process
%% that just receives ETS-TRANSFER messages and holds the tables.
ensure_heir_process() ->
    case whereis(beamtalk_runtime_sup) of
        undefined ->
            Pid = spawn(fun heir_loop/0),
            true = register(beamtalk_runtime_sup, Pid),
            {fake, Pid};
        _Pid ->
            existing
    end.

stop_heir_process(existing) ->
    ok;
stop_heir_process({fake, Pid}) ->
    unregister(beamtalk_runtime_sup),
    exit(Pid, kill),
    ok.

heir_loop() ->
    receive
        {'ETS-TRANSFER', _Table, _FromPid, _HeirData} ->
            heir_loop();
        _ ->
            heir_loop()
    end.

%% Poll `Pred` until it returns true, or fail after ~2s.
wait_until(Pred) ->
    wait_until(Pred, 200).

wait_until(_Pred, 0) ->
    ?assert(false);
wait_until(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_until(Pred, N - 1)
    end.
