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

%%====================================================================
%% Setup / teardown
%%====================================================================

%% Reset own_name (element 2) and skew (element 4) in the gen_server state
%% so every test starts from a clean baseline.  The record is not exported
%% from the source module, so we address the elements positionally.
reset_state() ->
    sys:replace_state(beamtalk_node_monitor, fun(S) ->
        setelement(4, setelement(2, S, undefined), #{})
    end).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    reset_state().

teardown(_) ->
    reset_state().

%%====================================================================
%% skew_count/1 — {skew_count, Node} call path
%%====================================================================

skew_count_unknown_node_returns_zero_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertEqual(0, beamtalk_node_monitor:skew_count('unknown@host'))
        ]
    end}.

%%====================================================================
%% handle_call — unknown-request fallback
%%====================================================================

unknown_call_returns_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_assertEqual(
                {error, unknown_request},
                gen_server:call(beamtalk_node_monitor, unrecognised_request)
            )
        ]
    end}.

%%====================================================================
%% handle_cast — {skew_detected, Node, ClassName}
%%====================================================================

skew_detected_increments_count_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    end}.

skew_detected_multiple_classes_counted_individually_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassA'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassB'}),
            ?assertEqual(2, beamtalk_node_monitor:skew_count('peer@host'))
        end
    end}.

skew_detected_same_class_twice_is_idempotent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    end}.

%%====================================================================
%% handle_cast — {skew_cleared, Node, ClassName}
%%====================================================================

skew_cleared_after_detect_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'MyClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host')),
            gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'peer@host', 'MyClass'}),
            ?assertEqual(0, beamtalk_node_monitor:skew_count('peer@host'))
        end
    end}.

skew_cleared_only_that_class_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassA'}),
            gen_server:cast(beamtalk_node_monitor, {skew_detected, 'peer@host', 'ClassB'}),
            gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'peer@host', 'ClassA'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count('peer@host'))
        end
    end}.

skew_cleared_for_unknown_peer_is_noop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            ok = gen_server:cast(beamtalk_node_monitor, {skew_cleared, 'unknown@host', 'MyClass'}),
            %% Sync: the subsequent skew_count call proves the cast completed.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('unknown@host'))
        end
    end}.

%%====================================================================
%% handle_cast — unknown-message fallback
%%====================================================================

unknown_cast_is_ignored_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            ok = gen_server:cast(beamtalk_node_monitor, unrecognised_cast),
            %% Sync via a subsequent call — proves the server is still alive.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    end}.

%%====================================================================
%% handle_info — unrecognised message
%%====================================================================

handle_info_unknown_message_ignored_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            beamtalk_node_monitor ! unexpected_info,
            %% Sync via a gen_server:call — proves the server processed the
            %% info message and is still alive.
            ?assertEqual(0, beamtalk_node_monitor:skew_count('any@node'))
        end
    end}.

%%====================================================================
%% handle_info — own nodedown clears own_name
%%====================================================================

own_nodedown_clears_own_name_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            FakeSelf = 'fake_self@testhost',
            sys:replace_state(beamtalk_node_monitor, fun(S) -> setelement(2, S, FakeSelf) end),
            beamtalk_node_monitor ! {nodedown, FakeSelf, [{nodedown_reason, net_tick_timeout}]},
            %% Sync: skew_count is a gen_server:call, serialised after the info.
            beamtalk_node_monitor:skew_count('any@node'),
            State = sys:get_state(beamtalk_node_monitor),
            ?assertEqual(undefined, element(2, State))
        end
    end}.

%%====================================================================
%% handle_info — peer nodedown clears skew entry
%%====================================================================

peer_nodedown_clears_skew_entry_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        fun() ->
            Peer = 'peer@testhost',
            gen_server:cast(beamtalk_node_monitor, {skew_detected, Peer, 'SomeClass'}),
            ?assertEqual(1, beamtalk_node_monitor:skew_count(Peer)),
            beamtalk_node_monitor ! {nodedown, Peer, [{nodedown_reason, disconnect}]},
            ?assertEqual(0, beamtalk_node_monitor:skew_count(Peer))
        end
    end}.
