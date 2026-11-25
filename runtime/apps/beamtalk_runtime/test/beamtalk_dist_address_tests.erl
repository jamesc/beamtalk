%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_address_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
`beamtalk_dist_address` against the shared loopback rule
(`tests/conformance/loopback_dist_conformance.json`), which the Rust CLI and
the LiveView front read too (BT-5127, BT-5185, BT-5233).
""".

-include_lib("eunit/include/eunit.hrl").

loopback_dist_matches_shared_conformance_fixture_test() ->
    #{<<"cases">> := Cases} = beamtalk_test_corpus:load_json_fixture([
        "tests", "conformance", "loopback_dist_conformance.json"
    ]),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(#{<<"name">> := Name, <<"value">> := Value, <<"pin">> := Pin}) ->
            Input =
                case Value of
                    null -> undefined;
                    _ -> binary_to_list(Value)
                end,
            ?assertEqual({Name, Pin}, {Name, beamtalk_dist_address:loopback_dist(Input)})
        end,
        Cases
    ).

parse_trims_and_drops_zone_test() ->
    ?assertEqual({ok, {10, 0, 0, 5}}, beamtalk_dist_address:parse(" 10.0.0.5\t")),
    ?assertEqual({ok, {16#fe80, 0, 0, 0, 0, 0, 0, 1}}, beamtalk_dist_address:parse("fe80::1%eth0")),
    ?assertEqual({ok, {0, 0, 0, 0}}, beamtalk_dist_address:parse(<<"0.0.0.0">>)).

parse_refuses_non_literals_test_() ->
    [
        ?_assertEqual(error, beamtalk_dist_address:parse(V))
     || V <- ["", "localhost", "10.1", "010.0.0.1", "10.0.0.5,10.0.0.6", "host.example.com"]
    ].

is_loopback_test_() ->
    [
        ?_assertEqual(Want, beamtalk_dist_address:is_loopback(Ip))
     || {Ip, Want} <- [
            {{127, 0, 0, 1}, true},
            {{127, 9, 9, 9}, true},
            {{0, 0, 0, 0, 0, 0, 0, 1}, true},
            {{0, 0, 0, 0, 0, 16#FFFF, 16#7F00, 1}, true},
            {{0, 0, 0, 0, 0, 16#FFFF, 16#0A00, 1}, false},
            {{10, 0, 0, 5}, false},
            {{0, 0, 0, 0}, false}
        ]
    ].
