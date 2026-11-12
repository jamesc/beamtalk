%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_call_failure_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
