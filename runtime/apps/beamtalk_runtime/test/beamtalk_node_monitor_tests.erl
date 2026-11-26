%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_node_monitor_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_node_monitor` gen_server callbacks (ADR 0126).

Covers: `skew_count/1` (the `{skew_count, _}` call path), the unknown-call
fallback, `skew_detected` and `skew_cleared` casts and their interactions,
the unknown-cast fallback, and `handle_info/2` for own `nodedown`, peer
`nodedown`, and unrecognised messages. The BT-5240 tests drive
`handle_info/2` / `handle_cast/2` directly on a state value, so the
reconnect and reload orderings they check are exact, not timing-dependent.

Shape-skew connect/reload paths (`check_shape_skew_on_connect/4`,
`check_one_peer_class/5`, `announce_if_skewed/7`) require a two-node setup
and are exercised by `beamtalk_dist_shape_skew_tests`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

%% Reset own_name (element 2) and skew (element 4) in the gen_server state
%% so every test starts from a clean baseline.  The record is not exported
%% from the source module, so we address the elements positionally.
%%
%% The tally only accepts casts for connected peers (BT-5232), so the two fake
%% peers these tests cast about start with an open, empty entry — the state a
%% real `nodeup` leaves behind, generation included (BT-5240).
reset_state() ->
    Connected = #{
        'peer@host' => beamtalk_node_monitor:peer_entry(make_ref()),
        'peer@testhost' => beamtalk_node_monitor:peer_entry(make_ref())
    },
    sys:replace_state(beamtalk_node_monitor, fun(S) ->
        setelement(4, setelement(2, S, undefined), Connected)
    end).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    reset_state().

teardown(_) ->
    reset_state().

%%====================================================================
%% skew_count/1 — {skew_count, Node} call path
%%====================================================================

skew_count_unknown_node_returns_zero_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        ?_assertEqual(0, beamtalk_node_monitor:skew_count('unknown@host'))
    ]}.

%%====================================================================
%% handle_call — unknown-request fallback
%%====================================================================

unknown_call_returns_error_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        ?_assertEqual(
            {error, unknown_request},
            gen_server:call(beamtalk_node_monitor, unrecognised_request)
        )
    ]}.

%%====================================================================
%% handle_cast — {skew_detected, Node, Gen, Stamp, ClassName, Versions, Announce}
%%====================================================================

skew_detected_increments_count_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'peer@host', 'MyClass'),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_detected_multiple_classes_counted_individually_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'peer@host', 'ClassA'),
            cast_result(detected, 'peer@host', 'ClassB'),
            ?assertEqual(2, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_detected_same_class_twice_is_idempotent_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'peer@host', 'MyClass'),
            cast_result(detected, 'peer@host', 'MyClass'),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

%%====================================================================
%% handle_cast — {skew_cleared, Node, Gen, Stamp, ClassName}
%%====================================================================

skew_cleared_after_detect_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'peer@host', 'MyClass'),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host')),
            cast_result(cleared, 'peer@host', 'MyClass'),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_cleared_only_that_class_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'peer@host', 'ClassA'),
            cast_result(detected, 'peer@host', 'ClassB'),
            cast_result(cleared, 'peer@host', 'ClassA'),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_cleared_for_unknown_peer_is_noop_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            ok = cast_result(cleared, 'unknown@host', 'MyClass'),
            %% Sync: the subsequent skew_count call proves the cast completed.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('unknown@host'))
        end
    ]}.

%%====================================================================
%% handle_cast — unknown-message fallback
%%====================================================================

unknown_cast_is_ignored_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            ok = gen_server:cast(beamtalk_node_monitor, unrecognised_cast),
            %% Sync via a subsequent call — proves the server is still alive.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    ]}.

%%====================================================================
%% handle_info — unrecognised message
%%====================================================================

handle_info_unknown_message_ignored_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            beamtalk_node_monitor ! unexpected_info,
            %% Sync via a gen_server:call — proves the server processed the
            %% info message and is still alive.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    ]}.

%%====================================================================
%% handle_info — own nodedown clears own_name
%%====================================================================

own_nodedown_clears_own_name_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            FakeSelf = 'fake_self@testhost',
            sys:replace_state(beamtalk_node_monitor, fun(S) -> setelement(2, S, FakeSelf) end),
            beamtalk_node_monitor ! {nodedown, FakeSelf, [{nodedown_reason, net_tick_timeout}]},
            %% Sync: skew_count is a gen_server:call, serialised after the info.
            beamtalk_node_monitor:skew_count('any@node'),
            State = sys:get_state(beamtalk_node_monitor),
            ?assertEqual(undefined, element(2, State))
        end
    ]}.

%%====================================================================
%% handle_info — peer nodedown clears skew entry
%%====================================================================

peer_nodedown_clears_skew_entry_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            Peer = 'peer@testhost',
            cast_result(detected, Peer, 'SomeClass'),
            ?assertEqual(1, beamtalk_node_monitor:skew_count(Peer)),
            beamtalk_node_monitor ! {nodedown, Peer, [{nodedown_reason, disconnect}]},
            ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer))
        end
    ]}.

%%====================================================================
%% BT-5232 — a skew cast for a node that is not connected leaves no entry
%%====================================================================

skew_detected_for_unconnected_node_leaves_no_entry_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            cast_result(detected, 'ghost@host', 'MyClass'),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('ghost@host')),
            ?assertNot(maps:is_key('ghost@host', skew_map()))
        end,
        fun() ->
            %% The race from the issue: the spawned check's cast lands after
            %% the peer's `nodedown` already removed its entry.
            Peer = 'peer@testhost',
            beamtalk_node_monitor ! {nodedown, Peer, [{nodedown_reason, disconnect}]},
            cast_result(detected, Peer, 'SomeClass'),
            ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer)),
            ?assertNot(maps:is_key(Peer, skew_map()))
        end
    ]}.

%% The monitor's tally map (record element 4, see reset_state/0).
skew_map() ->
    element(4, sys:get_state(beamtalk_node_monitor)).

%% Cast a check result for `Peer`'s current connection to the running
%% monitor, stamped newer than any earlier one from this helper. A peer with
%% no entry gets a fresh, unknown generation.
cast_result(Kind, Peer, ClassName) ->
    Gen =
        case maps:find(Peer, skew_map()) of
            {ok, #{gen := G}} -> G;
            error -> make_ref()
        end,
    Msg =
        case Kind of
            detected -> detected(Peer, Gen, next_stamp(), ClassName);
            cleared -> {skew_cleared, Peer, Gen, next_stamp(), ClassName}
        end,
    gen_server:cast(beamtalk_node_monitor, Msg).

%% A `skew_detected` cast (versions 1 here, 2 on the peer) that never
%% announces.
detected(Peer, Gen, Stamp, ClassName) ->
    {skew_detected, Peer, Gen, Stamp, ClassName, {1, 2}, never}.

next_stamp() ->
    N =
        case get(test_stamp) of
            undefined -> 1;
            M -> M + 1
        end,
    put(test_stamp, N),
    N.

%%====================================================================
%% BT-5232 — a reload reads only the reloaded class's shape entry
%%====================================================================

reload_reads_one_class_entry_not_full_manifest_test_() ->
    {setup,
        fun() ->
            setup(),
            ok = meck:new(beamtalk_release, [passthrough, no_link]),
            ok = meck:expect(beamtalk_release, project_class_shape_entry, fun(_) ->
                #{version => 1, fields => #{}, migrations => #{}, module_abi => null}
            end)
        end,
        fun(X) ->
            meck:unload(beamtalk_release),
            teardown(X)
        end,
        fun() ->
            %% `node()` as the "peer": the erpc runs locally, so there is no
            %% network, and the fake class has no entry there (silent).
            ok = beamtalk_node_monitor:check_shape_skew_on_reload(
                'Bt5232FakeClass', [{node(), make_ref()}], 1
            ),
            ?assertEqual(0, meck:num_calls(beamtalk_release, shape_manifest, '_')),
            ?assertEqual(
                1, meck:num_calls(beamtalk_release, project_class_shape_entry, ['Bt5232FakeClass'])
            )
        end}.

%%====================================================================
%% BT-5232 — NodeVersionSkew fields (ADR 0126 Amendment D3)
%%====================================================================

version_skew_fields_test_() ->
    Local = #{wireVersion => 1, beamtalkVersion => <<"0.6.0">>},
    Peer = 'peer@host',
    [
        ?_assertEqual(
            none, beamtalk_node_monitor:version_skew_fields(Peer, Local, {1, <<"0.6.0">>})
        ),
        ?_assertMatch(
            #{
                localWireVersion := 1,
                remoteWireVersion := 2,
                localBeamtalkVersion := {beamtalk_string, <<"0.6.0">>},
                remoteBeamtalkVersion := {beamtalk_string, <<"0.6.0">>}
            },
            beamtalk_node_monitor:version_skew_fields(Peer, Local, {2, <<"0.6.0">>})
        ),
        ?_assertMatch(
            #{remoteWireVersion := 1, remoteBeamtalkVersion := {beamtalk_string, <<"0.5.0">>}},
            beamtalk_node_monitor:version_skew_fields(Peer, Local, {1, <<"0.5.0">>})
        ),
        ?_assertMatch(
            #{remoteWireVersion := nil, remoteBeamtalkVersion := nil, localWireVersion := 1},
            beamtalk_node_monitor:version_skew_fields(Peer, Local, unknown)
        )
    ].

%% A failed version read for a peer that is no longer connected is logged,
%% not announced: it would arrive after that peer's NodeDown.
version_read_for_unconnected_peer_not_announced_test_() ->
    {setup, fun setup/0, fun teardown/1, fun() ->
        Self = self(),
        Handler = fun(Event) -> Self ! {got_version_skew, Event} end,
        {ok, Sub} = beamtalk_announcements:subscribe('NodeVersionSkew', Self, Handler, false),
        try
            ok = beamtalk_node_monitor:check_version_skew_on_connect('ghost@invalid', make_ref()),
            ?assertNoMessage({got_version_skew, _}, 500)
        after
            beamtalk_announcements:unsubscribe(Sub)
        end
    end}.

%%====================================================================
%% terminate/2 — both branches
%%====================================================================

%% Covers terminate/2 lines 213-215, 218: the normal supervised-start case
%% where beamtalk_announcements is up and class_loaded_sub holds a real ref.
terminate_with_subscription_ref_unsubscribes_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            State = sys:get_state(beamtalk_node_monitor),
            ClassLoadedSub = element(3, State),
            ?assertNotEqual(undefined, ClassLoadedSub),
            Pid = whereis(beamtalk_node_monitor),
            Ref = erlang:monitor(process, Pid),
            ok = gen_server:stop(beamtalk_node_monitor),
            ?awaitMessage({'DOWN', Ref, process, Pid, normal}, 2000),
            ok = wait_for_restart(beamtalk_node_monitor, 2000),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    ]}.

%% Covers terminate/2 line 216: class_loaded_sub = undefined (gen_server
%% stopped when beamtalk_announcements was not running at init time).
terminate_without_subscription_ref_is_noop_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            sys:replace_state(beamtalk_node_monitor, fun(S) -> setelement(3, S, undefined) end),
            Pid = whereis(beamtalk_node_monitor),
            Ref = erlang:monitor(process, Pid),
            ok = gen_server:stop(beamtalk_node_monitor),
            ?awaitMessage({'DOWN', Ref, process, Pid, normal}, 2000),
            ok = wait_for_restart(beamtalk_node_monitor, 2000),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    ]}.

%%====================================================================
%% normalize_reason/1 — pure function, no gen_server needed
%%====================================================================

normalize_reason_atom_is_passed_through_test() ->
    ?assertEqual(disconnect, beamtalk_node_monitor:normalize_reason(disconnect)),
    ?assertEqual(net_tick_timeout, beamtalk_node_monitor:normalize_reason(net_tick_timeout)).

normalize_reason_undefined_returns_unknown_test() ->
    ?assertEqual(unknown, beamtalk_node_monitor:normalize_reason(undefined)).

normalize_reason_non_atom_returns_unknown_test() ->
    ?assertEqual(unknown, beamtalk_node_monitor:normalize_reason(<<"binary_reason">>)),
    ?assertEqual(unknown, beamtalk_node_monitor:normalize_reason(42)).

%%====================================================================
%% handle_info — own nodeup records self-identity
%%====================================================================

%% Covers handle_info lines 176-182: when this VM becomes distributed, the
%% matching nodeup (Node =:= node()) stores the current identity so the
%% later own nodedown can be recognised even after node() reverts.
own_nodeup_sets_own_name_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            OwnNode = node(),
            beamtalk_node_monitor ! {nodeup, OwnNode, []},
            %% Sync: a gen_server:call serialises after the handle_info.
            beamtalk_node_monitor:skew_count('any@node'),
            State = sys:get_state(beamtalk_node_monitor),
            ?assertEqual(OwnNode, element(2, State))
        end
    ]}.

%%====================================================================
%% handle_info — peer nodeup announces and spawns skew check
%%====================================================================

%% Covers handle_info lines 183-188: a peer-nodeup fires announce/2 and
%% spawns check_shape_skew_on_connect/1. The erpc call in that spawn fails
%% for a non-existent node, is caught and logged, and must not crash the
%% gen_server.
peer_nodeup_server_survives_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            beamtalk_node_monitor ! {nodeup, 'fake_peer@testhost', []},
            %% Sync: proves the gen_server is alive after announce + spawn.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('fake_peer@testhost'))
        end
    ]}.

%%====================================================================
%% handle_info — class_loaded spawns reload check (no-op with no peers)
%%====================================================================

%% Covers handle_info lines 201-208: a ClassLoaded event spawns
%% check_shape_skew_on_reload/2 which hits the [] base-case immediately
%% when no distributed peers are connected (the common unit-test scenario).
class_loaded_info_server_survives_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            beamtalk_node_monitor ! {'$beamtalk_class_loaded', 'SomeClass'},
            %% Sync: call proves the server processed the message and is alive.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    ]}.

%%====================================================================
%% BT-5240 — per-connection generation and per-check stamps, driven
%% directly through the callbacks on a state value
%%====================================================================

-define(GHOST, 'bt5240_ghost@nowhere').

%% One `handle_info/2` / `handle_cast/2` step on a state value.
info(Msg, S) ->
    {noreply, S1} = beamtalk_node_monitor:handle_info(Msg, S),
    S1.

cast(Msg, S) ->
    {noreply, S1} = beamtalk_node_monitor:handle_cast(Msg, S),
    S1.

count(Peer, S) ->
    {reply, N, S} = beamtalk_node_monitor:handle_call({skew_count, Peer}, from, S),
    N.

gen(Peer, S) ->
    #{gen := Gen} = maps:get(Peer, element(4, S)),
    Gen.

%% A state with `Peer` connected, built without `nodeup` so no real checks
%% are spawned and no `NodeUp` is announced.
connected(Peer) ->
    setelement(
        4,
        beamtalk_node_monitor:initial_state(),
        #{Peer => beamtalk_node_monitor:peer_entry(make_ref())}
    ).

%% The fast-reconnect race: connection #1's check finishes after the peer
%% has dropped and reconnected. Its cast must not touch connection #2's
%% tally, whichever order the two connections' casts arrive in.
previous_connection_cast_never_changes_current_tally_test() ->
    S1 = info({nodeup, ?GHOST, []}, beamtalk_node_monitor:initial_state()),
    OldGen = gen(?GHOST, S1),
    S2 = info({nodedown, ?GHOST, [{nodedown_reason, disconnect}]}, S1),
    S3 = info({nodeup, ?GHOST, []}, S2),
    NewGen = gen(?GHOST, S3),
    ?assertNotEqual(OldGen, NewGen),
    %% Connection #2's check clears the class, then #1's late detection
    %% lands, even with a larger stamp: dropped.
    S4 = cast({skew_cleared, ?GHOST, NewGen, 10, 'Bt5240Class'}, S3),
    S5 = cast(detected(?GHOST, OldGen, 20, 'Bt5240Class'), S4),
    ?assertEqual(S4, S5),
    ?assertEqual(0, count(?GHOST, S5)),
    %% Also for a class connection #2 has not checked yet.
    S6 = cast(detected(?GHOST, OldGen, 30, 'Bt5240Other'), S5),
    ?assertEqual(S5, S6),
    %% And a stale clear cannot erase connection #2's detection.
    S7 = cast(detected(?GHOST, NewGen, 40, 'Bt5240Class'), S6),
    S8 = cast({skew_cleared, ?GHOST, OldGen, 50, 'Bt5240Class'}, S7),
    ?assertEqual(1, count(?GHOST, S8)).

%% A check that outlives its peer's `nodedown` cannot bring the entry back.
cast_after_nodedown_leaves_no_entry_test() ->
    S1 = connected(?GHOST),
    Gen = gen(?GHOST, S1),
    S2 = info({nodedown, ?GHOST, [{nodedown_reason, disconnect}]}, S1),
    S3 = cast(detected(?GHOST, Gen, 1, 'Bt5240Class'), S2),
    ?assertEqual(S2, S3),
    ?assertNot(maps:is_key(?GHOST, element(4, S3))).

%% Within one connection, two checks of one class finish out of order: the
%% result of the check dispatched later is kept.
older_stamp_never_overwrites_newer_result_test() ->
    S1 = connected(?GHOST),
    Gen = gen(?GHOST, S1),
    S2 = cast(detected(?GHOST, Gen, 2, 'Bt5240Class'), S1),
    S3 = cast({skew_cleared, ?GHOST, Gen, 1, 'Bt5240Class'}, S2),
    ?assertEqual(1, count(?GHOST, S3)),
    S4 = cast({skew_cleared, ?GHOST, Gen, 3, 'Bt5240Class'}, S3),
    S5 = cast(detected(?GHOST, Gen, 2, 'Bt5240Class'), S4),
    ?assertEqual(0, count(?GHOST, S5)).

%% Two reloads of one class on the peer reach this node out of order: the
%% older one is not checked at all.
older_peer_reload_is_dropped_test() ->
    Entry = #{version => 1, fields => #{}, migrations => #{}, class_var_abi => null},
    S1 = connected(?GHOST),
    S2 = cast({peer_class_checked, ?GHOST, 'Bt5240Class', Entry, 2}, S1),
    ?assertMatch(#{reload_seq := #{'Bt5240Class' := 2}}, maps:get(?GHOST, element(4, S2))),
    S3 = cast({peer_class_checked, ?GHOST, 'Bt5240Class', Entry, 1}, S2),
    ?assertEqual(S2, S3),
    %% A reload from a peer that is not connected is dropped too.
    S4 = cast({peer_class_checked, 'other@nowhere', 'Bt5240Class', Entry, 3}, S3),
    ?assertEqual(S3, S4),
    %% The unstamped BT-5232 form is still checked, and leaves the stamp alone.
    S5 = cast({peer_class_checked, ?GHOST, 'Bt5240Class', Entry}, S4),
    ?assertMatch(#{reload_seq := #{'Bt5240Class' := 2}}, maps:get(?GHOST, element(4, S5))).

%% The full receiving side, on the running monitor: two reloads of one class
%% arrive in order, but their checks finish in reverse. The newer reload
%% (matching versions) must stay in the tally.
out_of_order_peer_reload_results_keep_newer_test_() ->
    {setup,
        fun() ->
            setup(),
            ok = meck:new(beamtalk_release, [passthrough, no_link])
        end,
        fun(X) ->
            meck:unload(beamtalk_release),
            teardown(X)
        end,
        fun() ->
            Test = self(),
            Peer = 'peer@host',
            %% Each check of the class reports to the test and waits for the
            %% version this node has at that moment.
            ok = meck:expect(beamtalk_release, project_class_shape_entry, fun
                ('Bt5240Class') ->
                    Test ! {reading, self()},
                    receive
                        {go, V} -> shape_entry(V)
                    end;
                (Other) ->
                    meck:passthrough([Other])
            end),
            gen_server:cast(
                beamtalk_node_monitor,
                {peer_class_checked, Peer, 'Bt5240Class', shape_entry(2), 1}
            ),
            Older = receive_reading(),
            gen_server:cast(
                beamtalk_node_monitor,
                {peer_class_checked, Peer, 'Bt5240Class', shape_entry(1), 2}
            ),
            Newer = receive_reading(),
            Self = self(),
            Handler = fun(Event) -> Self ! {got_shape_skew, Event} end,
            {ok, Sub} = beamtalk_announcements:subscribe('NodeShapeSkew', Self, Handler, false),
            try
                %% The newer check (same version: no skew) finishes first...
                finish_check(Newer, 1),
                ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer)),
                %% ...then the older one (skewed) finishes: dropped, and its
                %% skew is not announced either.
                finish_check(Older, 1),
                ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer)),
                ?assertNoMessage({got_shape_skew, #{className := 'Bt5240Class'}}, 500)
            after
                beamtalk_announcements:unsubscribe(Sub)
            end
        end}.

shape_entry(Version) ->
    #{version => Version, fields => #{}, migrations => #{}, class_var_abi => null}.

receive_reading() ->
    receive
        {reading, Pid} -> Pid
    after 5000 -> error(check_not_started)
    end.

%% Let a blocked check run to completion, then flush the cast it sent.
finish_check(Pid, OwnVersion) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {go, OwnVersion},
    ?awaitMessage({'DOWN', Ref, process, Pid, _}, 5000),
    _ = sys:get_state(beamtalk_node_monitor),
    ok.

%%====================================================================
%% BT-5265 — `hello`: reload notifications are accepted only from the
%% peer monitor generation that said `hello` on this connection
%%====================================================================

-define(CLASS, 'Bt5265Class').

%% A `peer_class_checked` from the peer monitor generation `PeerGen`.
peer_checked(PeerGen, Seq) ->
    {peer_class_checked, ?GHOST, PeerGen, ?CLASS, shape_entry(1), Seq}.

reload_seq(Peer, S) ->
    #{reload_seq := Seqs} = maps:get(Peer, element(4, S)),
    Seqs.

%% The sender reconnect window: a notification the peer's monitor sent for
%% its earlier connection goes out on the new one, before that monitor's
%% new `hello`. It never reaches this connection's entry, whatever order
%% the messages around it take.
earlier_connection_notification_is_dropped_test() ->
    S1 = connected(?GHOST),
    %% Before any `hello` on this connection: dropped.
    PeerGen1 = make_ref(),
    ?assertEqual(S1, cast(peer_checked(PeerGen1, 1), S1)),
    S2 = cast({hello, ?GHOST, PeerGen1, false}, S1),
    ?assertMatch(#{peer_gen := PeerGen1}, maps:get(?GHOST, element(4, S2))),
    S3 = cast(peer_checked(PeerGen1, 2), S2),
    ?assertEqual(#{?CLASS => 2}, reload_seq(?GHOST, S3)),
    %% The link drops and comes back.
    S4 = info({nodedown, ?GHOST, [{nodedown_reason, disconnect}]}, S3),
    S5 = info({nodeup, ?GHOST, []}, S4),
    %% The peer's earlier generation, before its new `hello`: dropped.
    ?assertEqual(S5, cast(peer_checked(PeerGen1, 3), S5)),
    PeerGen2 = make_ref(),
    S6 = cast({hello, ?GHOST, PeerGen2, false}, S5),
    %% ...and after it: still dropped.
    ?assertEqual(S6, cast(peer_checked(PeerGen1, 4), S6)),
    S7 = cast(peer_checked(PeerGen2, 5), S6),
    ?assertEqual(#{?CLASS => 5}, reload_seq(?GHOST, S7)).

%% A `hello` from a node this monitor has no entry for (not connected, or
%% hidden) changes nothing.
hello_from_unknown_peer_is_ignored_test() ->
    S = beamtalk_node_monitor:initial_state(),
    ?assertEqual(S, cast({hello, ?GHOST, make_ref(), true}, S)).

should_announce_test_() ->
    Skewed = {skewed, 1, 2},
    [
        ?_assert(beamtalk_node_monitor:should_announce(changed, none, Skewed)),
        ?_assert(beamtalk_node_monitor:should_announce(changed, clear, Skewed)),
        ?_assert(beamtalk_node_monitor:should_announce(changed, {skewed, 1, 3}, Skewed)),
        ?_assertNot(beamtalk_node_monitor:should_announce(changed, Skewed, Skewed)),
        ?_assertNot(beamtalk_node_monitor:should_announce(never, none, Skewed))
    ].

%%--------------------------------------------------------------------
%% The connect-time checks on the running monitor: one at `nodeup`, one
%% at the peer's `hello`. Each check's read of the peer's manifest blocks
%% until the test releases it, so the order the checks finish in is set
%% by the test.
%%--------------------------------------------------------------------

-define(PEER5265, 'bt5265_ghost@nowhere').

connect_checks_test_() ->
    {foreach,
        fun() ->
            setup(),
            ok = meck:new(beamtalk_node, [passthrough, no_link]),
            ok = meck:new(beamtalk_release, [passthrough, no_link]),
            ok = meck:expect(beamtalk_release, shape_manifest, fun() ->
                #{?CLASS => shape_entry(1)}
            end),
            ok = meck:new(beamtalk_announcements, [passthrough, no_link])
        end,
        fun(X) ->
            beamtalk_node_monitor ! {nodedown, ?PEER5265, [{nodedown_reason, disconnect}]},
            _ = sys:get_state(beamtalk_node_monitor),
            meck:unload([beamtalk_announcements, beamtalk_release, beamtalk_node]),
            teardown(X)
        end,
        [
            {"a peer without hello (0.6.0): the nodeup check detects and announces once", fun() ->
                Nodeup = connect_new(),
                finish_check(Nodeup, 2),
                ?assertEqual(1, beamtalk_node_monitor:skew_count(?PEER5265)),
                ?assertEqual([{1, 2}], shape_skews_announced())
            end},
            {"nodeup check first, then the hello check: one announcement", fun() ->
                Nodeup = connect_new(),
                Hello = say_hello(false),
                finish_check(Nodeup, 2),
                finish_check(Hello, 2),
                ?assertEqual(1, beamtalk_node_monitor:skew_count(?PEER5265)),
                ?assertEqual([{1, 2}], shape_skews_announced())
            end},
            {"hello check first, then the nodeup check: one announcement", fun() ->
                Nodeup = connect_new(),
                Hello = say_hello(false),
                finish_check(Hello, 2),
                finish_check(Nodeup, 2),
                ?assertEqual(1, beamtalk_node_monitor:skew_count(?PEER5265)),
                ?assertEqual([{1, 2}], shape_skews_announced())
            end},
            {"a peer reload made before the peer handled our nodeup is counted", fun() ->
                %% The nodeup check read the peer's entry before the reload;
                %% the hello check, after it.
                Nodeup = connect_new(),
                finish_check(Nodeup, 1),
                ?assertEqual(0, beamtalk_node_monitor:skew_count(?PEER5265)),
                Hello = say_hello(false),
                finish_check(Hello, 2),
                ?assertEqual(1, beamtalk_node_monitor:skew_count(?PEER5265)),
                ?assertEqual([{1, 2}], shape_skews_announced())
            end},
            {"a hello on a connection this monitor did not see open announces nothing", fun() ->
                sys:replace_state(beamtalk_node_monitor, fun(S) ->
                    Entry = beamtalk_node_monitor:peer_entry(make_ref(), existing),
                    setelement(4, S, maps:put(?PEER5265, Entry, element(4, S)))
                end),
                Hello = say_hello(true),
                finish_check(Hello, 2),
                ?assertEqual(1, beamtalk_node_monitor:skew_count(?PEER5265)),
                ?assertEqual([], shape_skews_announced())
            end}
        ]}.

%% Deliver `nodeup` for the fake peer and answer the pid of the connect
%% check it starts, blocked on its read.
connect_new() ->
    block_reads(),
    beamtalk_node_monitor ! {nodeup, ?PEER5265, []},
    receive_reading().

%% Make each read of the fake peer's manifest report to this process and
%% wait for the peer's version of the class. Set from the test body, since
%% `foreach` runs setup in another process.
block_reads() ->
    Test = self(),
    ok = meck:expect(beamtalk_node, remote_shape_manifest, fun
        (?PEER5265) ->
            Test ! {reading, self()},
            receive
                {go, Version} -> {ok, #{?CLASS => shape_entry(Version)}}
            end;
        (Other) ->
            meck:passthrough([Other])
    end).

%% Deliver the fake peer's `hello` and answer the pid of the check it starts.
say_hello(WantReply) ->
    block_reads(),
    gen_server:cast(beamtalk_node_monitor, {hello, ?PEER5265, make_ref(), WantReply}),
    receive_reading().

%% The `{localVersion, remoteVersion}` of each `NodeShapeSkew` the monitor
%% has announced about the fake peer, in order.
shape_skews_announced() ->
    PeerValue = beamtalk_node:from_atom(?PEER5265),
    [
        {L, R}
     || {_Pid, {beamtalk_announcements, system_announce, ['NodeShapeSkew', Fields]}, _} <-
            meck:history(beamtalk_announcements),
        #{node := Node, localVersion := L, remoteVersion := R} <- [Fields],
        Node =:= PeerValue
    ].

%%====================================================================
%% BT-5240 — NodeVersionSkew: a timeout is logged, not announced; a stale
%% connection's read is not announced
%%====================================================================

version_read_outcome_test_() ->
    [
        ?_assertEqual(
            {remote, {1, <<"0.6.0">>}},
            beamtalk_node_monitor:version_read_outcome(
                {ok, #{wireVersion => 1, beamtalkVersion => <<"0.6.0">>}}
            )
        ),
        ?_assertEqual(skip, beamtalk_node_monitor:version_read_outcome({error, timeout})),
        ?_assertEqual(
            skip, beamtalk_node_monitor:version_read_outcome({error, {node_down, 'p@h'}})
        ),
        ?_assertEqual(
            unknown,
            beamtalk_node_monitor:version_read_outcome(
                {error, {other, {exception, undef, []}}}
            )
        ),
        ?_assertEqual(unknown, beamtalk_node_monitor:version_read_outcome({ok, not_a_map}))
    ].

version_check_announcement_test_() ->
    {foreach,
        fun() ->
            setup(),
            ok = meck:new(beamtalk_call_failure, [passthrough, no_link])
        end,
        fun(X) ->
            meck:unload(beamtalk_call_failure),
            teardown(X)
        end,
        [
            {"a timeout is not announced", fun() ->
                ?assertEqual(none, version_check_announces({error, timeout}, current))
            end},
            {"an error answer is announced with nil remote fields", fun() ->
                ?assertMatch(
                    #{remoteWireVersion := nil, remoteBeamtalkVersion := nil},
                    version_check_announces({error, {other, {exception, undef, []}}}, current)
                )
            end},
            {"a mismatch is announced", fun() ->
                ?assertMatch(
                    #{remoteWireVersion := 999},
                    version_check_announces(mismatch(), current)
                )
            end},
            {"a previous connection's mismatch is not announced", fun() ->
                ?assertEqual(none, version_check_announces(mismatch(), stale))
            end}
        ]}.

mismatch() ->
    {ok, #{wireVersion => 999, beamtalkVersion => <<"bt5240">>}}.

%% Run the connect-time version check against `peer@host` with the version
%% read answering `Read`, for its current or a stale generation. Returns the
%% announced event's fields, or `none`.
version_check_announces(Read, Which) ->
    Peer = 'peer@host',
    ok = meck:expect(beamtalk_call_failure, erpc, fun
        (P, beamtalk_version, dist_versions, [], _Timeout) when P =:= Peer -> Read;
        (P, M, F, A, T) -> meck:passthrough([P, M, F, A, T])
    end),
    Gen =
        case Which of
            current -> maps:get(gen, maps:get(Peer, skew_map()));
            stale -> make_ref()
        end,
    Self = self(),
    Handler = fun(Event) -> Self ! {got_version_skew, Event} end,
    {ok, Sub} = beamtalk_announcements:subscribe('NodeVersionSkew', Self, Handler, false),
    try
        ok = beamtalk_node_monitor:check_version_skew_on_connect(Peer, Gen),
        receive
            {got_version_skew, Event} -> Event
        after 500 -> none
        end
    after
        beamtalk_announcements:unsubscribe(Sub)
    end.

wait_for_restart(Name, TimeoutMs) ->
    try
        beamtalk_test_wait:until(fun() -> is_pid(whereis(Name)) end, TimeoutMs)
    catch
        error:{wait_timeout, _} -> {error, restart_timeout}
    end.
