%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_announcements_tests).

-moduledoc """
EUnit tests for `beamtalk_announcements` (BT-2439 / BT-2440 / ADR 0093 Phase 1).

Covers the Phase-1 acceptance criteria: deliver to a subscriber of the exact
class, a dead subscriber pruned automatically via monitor `DOWN`, an unrelated
class delivering nothing, distinct subscriptions to the same class (Pharo's
multi-subscription rule), idempotent unsubscribe, and the introspection reads.

BT-2440 adds the MRO (superclass-chain) matching: delivery to subscribers of an
ancestor class, per-subscription de-duplication across the walk, and graceful
truncation when a class's metadata row is removed mid-hierarchy.
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
