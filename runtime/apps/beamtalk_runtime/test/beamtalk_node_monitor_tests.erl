%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_node_monitor_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_node_monitor` gen_server callbacks (ADR 0126).

Covers: `skew_count/1` (the `{skew_count, _}` call path), the unknown-call
fallback, `{skew_detected, _, _}` and `{skew_cleared, _, _}` casts and
their interactions, the unknown-cast fallback, and `handle_info/2` for own
`nodedown`, peer `nodedown`, and unrecognised messages.

Shape-skew connect/reload paths (`check_shape_skew_on_connect/1`,
`check_one_peer_class/3`, `announce_if_skewed/4`) require a two-node setup
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
%% real `nodeup` leaves behind.
reset_state() ->
    Connected = #{
        'peer@host' => sets:new([{version, 2}]),
        'peer@testhost' => sets:new([{version, 2}])
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
%% handle_cast — {skew_detected, Node, ClassName}
%%====================================================================

skew_detected_increments_count_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_detected_multiple_classes_counted_individually_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassA'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassB'}),
            ?assertEqual(2, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_detected_same_class_twice_is_idempotent_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

%%====================================================================
%% handle_cast — {skew_cleared, Node, ClassName}
%%====================================================================

skew_cleared_after_detect_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host')),
            gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'peer@host', 'MyClass'}),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_cleared_only_that_class_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassA'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassB'}),
            gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'peer@host', 'ClassA'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    ]}.

skew_cleared_for_unknown_peer_is_noop_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun() ->
            ok = gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'unknown@host', 'MyClass'}),
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
            gen_server:cast(beamtalk_node_monitor, {skew_detected, Peer, 'SomeClass'}),
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
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'ghost@host', 'MyClass'}),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('ghost@host')),
            ?assertNot(maps:is_key('ghost@host', skew_map()))
        end,
        fun() ->
            %% The race from the issue: the spawned check's cast lands after
            %% the peer's `nodedown` already removed its entry.
            Peer = 'peer@testhost',
            beamtalk_node_monitor ! {nodedown, Peer, [{nodedown_reason, disconnect}]},
            gen_server:cast(beamtalk_node_monitor, {skew_detected, Peer, 'SomeClass'}),
            ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer)),
            ?assertNot(maps:is_key(Peer, skew_map()))
        end
    ]}.

%% The monitor's tally map (record element 4, see reset_state/0).
skew_map() ->
    element(4, sys:get_state(beamtalk_node_monitor)).

%%====================================================================
%% BT-5232 — a reload reads only the reloaded class's shape entry
%%====================================================================

reload_reads_one_class_entry_not_full_manifest_test_() ->
    {setup,
        fun() ->
            setup(),
            ok = meck:new(beamtalk_release, [passthrough, no_link]),
            ok = meck:expect(beamtalk_release, project_class_shape_entry, fun(_) ->
                #{version => 1, fields => #{}, migrations => #{}, class_var_abi => null}
            end)
        end,
        fun(X) ->
            meck:unload(beamtalk_release),
            teardown(X)
        end,
        fun() ->
            %% `node()` as the "peer": the erpc runs locally, so there is no
            %% network, and the fake class has no entry there (silent).
            ok = beamtalk_node_monitor:check_shape_skew_on_reload('Bt5232FakeClass', [node()]),
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
            ok = beamtalk_node_monitor:check_version_skew_on_connect('ghost@invalid'),
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

wait_for_restart(Name, TimeoutMs) ->
    try
        beamtalk_test_wait:until(fun() -> is_pid(whereis(Name)) end, TimeoutMs)
    catch
        error:{wait_timeout, _} -> {error, restart_timeout}
    end.
