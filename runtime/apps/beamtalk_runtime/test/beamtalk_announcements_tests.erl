%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_announcements_tests).

-moduledoc """
EUnit tests for `beamtalk_announcements` (BT-2439 / ADR 0093 Phase 1).

Covers the Phase-1 acceptance criteria: deliver to a subscriber of the exact
class, a dead subscriber pruned automatically via monitor `DOWN`, single-class
match (no MRO walk yet), distinct subscriptions to the same class (Pharo's
multi-subscription rule), idempotent unsubscribe, and the introspection reads.
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
    ok.

%%====================================================================
%% Test subscriber: echoes received announcements back to a collector.
%%====================================================================

%% Spawn a subscriber that forwards every `beamtalk_announcement` message it
%% receives to `Collector`, so the test process can assert on delivery. Replies
%% to a `{stop, From}` message so the test can kill it deterministically.
spawn_subscriber(Collector) ->
    spawn(fun() -> subscriber_loop(Collector) end).

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
            end)
        ]
    end}.

%%====================================================================
%% Single-class match — no MRO walk (Phase 1)
%%====================================================================

single_class_match_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Collector = self(),
                Sub = spawn_subscriber(Collector),
                {ok, _SubRef} = beamtalk_announcements:subscribe(
                    'PriceChanged', Sub, h, false
                ),
                %% Announcing a *different* class delivers nothing — no MRO/match
                %% across class names in Phase 1.
                ok = beamtalk_announcements:announce('OtherEvent', {x, 1}),
                refute_received(),
                %% The matching class still delivers.
                ok = beamtalk_announcements:announce('PriceChanged', {x, 2}),
                {_R, 'PriceChanged', h, {x, 2}} = expect_received()
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
%% Helpers
%%====================================================================

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
