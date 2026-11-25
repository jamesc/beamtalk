%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_test_helper_tests).

-moduledoc """
`beamtalk_dist_test_helper` itself (BT-5185): a peer is started with a
throwaway cookie, never this node's own, which `ps` would otherwise show.
""".

-include_lib("eunit/include/eunit.hrl").

peer_never_gets_this_nodes_cookie_test_() ->
    {timeout, 90, fun() ->
        beamtalk_dist_test_helper:ensure_distribution(),
        Own = atom_to_list(erlang:get_cookie()),
        {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5185_cookie"),
        try
            {ok, Args} = rpc:call(PeerNode, init, get_argument, [setcookie], 10000),
            ?assertNotEqual([], Args),
            ?assertNot(lists:member([Own], Args)),
            %% The connection works with the peer's cookie on both sides.
            ?assertEqual(pong, net_adm:ping(PeerNode))
        after
            beamtalk_dist_test_helper:stop_peer(Peer)
        end
    end}.
