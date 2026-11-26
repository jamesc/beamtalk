%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_call_failure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([kill_self/0]).

classify_test() ->
    MFA = {gen_server, call, [self(), x, 5000]},
    lists:foreach(
        fun({Class, Reason, Expected}) ->
            ?assertEqual(Expected, beamtalk_call_failure:classify(Class, Reason), Reason)
        end,
        [
            {exit, noproc, dead},
            {exit, {noproc, MFA}, dead},
            {exit, {{nodedown, n@h}, MFA}, {unreachable, nodedown}},
            {exit, {nodedown, n@h}, {unreachable, nodedown}},
            {exit, noconnection, {unreachable, noconnection}},
            {exit, {noconnection, MFA}, {unreachable, noconnection}},
            {exit, timeout, timeout},
            {exit, {timeout, MFA}, timeout},
            {error, {erpc, noconnection}, {unreachable, noconnection}},
            {error, {erpc, timeout}, timeout},
            %% shutdown/killed/normal mid-call are `other` by explicit policy
            {exit, {shutdown, MFA}, other},
            {exit, {killed, MFA}, other},
            {exit, {normal, MFA}, other},
            {exit, killed, other},
            {error, badarg, other},
            {throw, noproc, other}
        ]
    ).

unreachable_node_test() ->
    MFA = {gen_server, call, []},
    ?assertEqual(n@h, beamtalk_call_failure:unreachable_node({{nodedown, n@h}, MFA}, d@h)),
    ?assertEqual(n@h, beamtalk_call_failure:unreachable_node({nodedown, n@h}, d@h)),
    ?assertEqual(d@h, beamtalk_call_failure:unreachable_node(noconnection, d@h)).

system_error_constructors_test() ->
    NodeDown = beamtalk_error:node_down('Node', foo, n@h),
    ?assertEqual(node_down, NodeDown#beamtalk_error.kind),
    ?assertEqual(#{node => n@h}, NodeDown#beamtalk_error.details),
    ?assertEqual(
        {beamtalk_string,
            <<"The node may come back; retry, or use monitors to detect when it does">>},
        NodeDown#beamtalk_error.hint
    ),
    ?assertEqual(timeout, (beamtalk_error:actor_timeout(foo))#beamtalk_error.kind),
    ?assertEqual(actor_dead, (beamtalk_error:actor_dead(unknown, foo))#beamtalk_error.kind),
    ?assertEqual(
        {beamtalk_string, <<"Check spelling or use 'respondsTo:' to verify method exists">>},
        (beamtalk_error:does_not_understand('X', foo))#beamtalk_error.hint
    ).

%%% erpc/5 -----------------------------------------------------------------

erpc_ok_test() ->
    ?assertEqual({ok, 3}, beamtalk_call_failure:erpc(node(), erlang, '+', [1, 2], 1000)).

erpc_node_down_test() ->
    ?assertEqual(
        {error, {node_down, 'nope@nowhere.invalid'}},
        beamtalk_call_failure:erpc('nope@nowhere.invalid', erlang, node, [], 1000)
    ).

erpc_timeout_test() ->
    ?assertEqual(
        {error, timeout},
        beamtalk_call_failure:erpc(node(), timer, sleep, [2000], 20)
    ).

erpc_remote_error_test() ->
    ?assertEqual(
        {error, {other, boom}},
        beamtalk_call_failure:erpc(node(), erlang, error, [boom], 1000)
    ),
    ?assertEqual(
        {error, {other, undef}},
        beamtalk_call_failure:erpc(node(), beamtalk_no_such_module, f, [], 1000)
    ).

erpc_remote_exit_test() ->
    ?assertEqual(
        {error, {other, bye}},
        beamtalk_call_failure:erpc(node(), erlang, exit, [bye], 1000)
    ).

%% BT-5192: the process running the call is killed by an exit signal before it
%% answers. `erpc:call/5` raises `exit:{signal, killed}`, which escaped before.
erpc_remote_signal_test() ->
    ?assertEqual(
        {error, {other, {signal, killed}}},
        beamtalk_call_failure:erpc(node(), ?MODULE, kill_self, [], 1000)
    ).

-doc "erpc target for `erpc_remote_signal_test/0`.".
kill_self() ->
    exit(self(), kill).

erpc_other_erpc_failure_test() ->
    %% A non-function MFA is an `{erpc, badarg}` failure, not a remote exception.
    ?assertEqual(
        {error, {other, {erpc, badarg}}},
        beamtalk_call_failure:erpc(node(), erlang, "bad", [], 1000)
    ).

erpc_throw_propagates_test() ->
    ?assertThrow(oops, beamtalk_call_failure:erpc(node(), erlang, throw, [oops], 1000)).

%%% tolerate_gone ----------------------------------------------------------

tolerate_gone_test() ->
    MFA = {gen_server, call, []},
    Tolerated = [noproc, {noproc, MFA}, {{nodedown, n@h}, MFA}, {nodedown, n@h}, noconnection],
    lists:foreach(
        fun(R) ->
            ?assertEqual(dflt, beamtalk_call_failure:tolerate_gone(thrower(exit, R), dflt), R)
        end,
        Tolerated
    ),
    ?assertEqual(7, beamtalk_call_failure:tolerate_gone(fun() -> 7 end, dflt)).

tolerate_gone_propagates_by_default_test() ->
    MFA = {gen_server, call, []},
    lists:foreach(
        fun(R) ->
            ?assertExit(R, beamtalk_call_failure:tolerate_gone(thrower(exit, R), dflt))
        end,
        [{normal, MFA}, {shutdown, MFA}, {timeout, MFA}, killed, {boom, MFA}]
    ),
    ?assertError(badarg, beamtalk_call_failure:tolerate_gone(thrower(error, badarg), dflt)),
    ?assertThrow(noproc, beamtalk_call_failure:tolerate_gone(thrower(throw, noproc), dflt)).

tolerate_gone_stopping_option_test() ->
    MFA = {gen_server, call, []},
    Opts = #{stopping => true},
    lists:foreach(
        fun(R) ->
            ?assertEqual(dflt, beamtalk_call_failure:tolerate_gone(thrower(exit, R), dflt, Opts), R)
        end,
        [normal, shutdown, {normal, MFA}, {shutdown, MFA}, {{normal, x}, MFA}, {{shutdown, x}, MFA}]
    ),
    ?assertExit(
        {timeout, MFA},
        beamtalk_call_failure:tolerate_gone(thrower(exit, {timeout, MFA}), dflt, Opts)
    ),
    ?assertExit(killed, beamtalk_call_failure:tolerate_gone(thrower(exit, killed), dflt, Opts)).

tolerate_gone_timeout_option_test() ->
    MFA = {gen_server, call, []},
    Opts = #{timeout => true},
    ?assertEqual(
        dflt, beamtalk_call_failure:tolerate_gone(thrower(exit, {timeout, MFA}), dflt, Opts)
    ),
    ?assertExit(
        {normal, MFA}, beamtalk_call_failure:tolerate_gone(thrower(exit, {normal, MFA}), dflt, Opts)
    ).

thrower(Class, Reason) -> fun() -> erlang:raise(Class, Reason, []) end.
