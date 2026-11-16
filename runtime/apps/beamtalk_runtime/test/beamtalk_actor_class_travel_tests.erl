%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_class_travel_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4949 (ADR 0150): the class travels with a send. A bare-pid send reads the
`'$beamtalk_actor'` marker (`lookup_class/1`); a caller that holds the class
passes it to a `*_known` function. Nothing on the send path reads
`beamtalk_object_instances:class_of/1` any more.
""".
-include_lib("eunit/include/eunit.hrl").

-define(CLASS_STATS_KEY(Class), {beamtalk_string, <<"class:", Class/binary>>}).

setup() ->
    meck:new(beamtalk_object_instances, [passthrough]),
    meck:expect(beamtalk_object_instances, class_of, fun(_Pid) -> erlang:error(class_of_called) end),
    beamtalk_trace_store:clear(),
    ok.

cleanup(_) ->
    meck:unload(beamtalk_object_instances).

class_travel_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"bare-pid sends never read object_instances:class_of/1", fun no_class_of_reads/0},
        {"bare-pid send reports the marker class", fun bare_pid_reports_marker_class/0},
        {"async_send_known reports the caller's class", fun async_known_reports_hint/0},
        {"remote and non-actor pids resolve to unknown", fun unknown_pids/0},
        {"TimeoutProxy bare pid keeps an infinite call timeout", fun timeout_proxy_infinity/0},
        {"other bare pids keep the default call timeout", fun other_class_default_timeout/0}
    ]}.

no_class_of_reads() ->
    {ok, Pid} = test_counter:start(0),
    try
        0 = beamtalk_actor:sync_send(Pid, getValue, []),
        ok = beamtalk_actor:cast_send(Pid, increment, []),
        F = beamtalk_future:new(),
        ok = beamtalk_actor:async_send(Pid, increment, [], F),
        _ = beamtalk_future:await(F),
        %% `kill` goes through kill_and_wait/1, which also read the class.
        ok = beamtalk_actor:sync_send(Pid, kill, []),
        ?assertEqual(0, meck:num_calls(beamtalk_object_instances, class_of, '_'))
    after
        catch gen_server:stop(Pid)
    end.

bare_pid_reports_marker_class() ->
    {ok, Pid} = test_counter:start_link(0),
    try
        0 = beamtalk_actor:sync_send(Pid, getValue, []),
        ok = beamtalk_test_wait:sync(Pid),
        Stats = beamtalk_trace_store:get_stats(),
        ?assert(maps:is_key(?CLASS_STATS_KEY(<<"Counter">>), Stats)),
        ?assertNot(maps:is_key(?CLASS_STATS_KEY(<<"unknown">>), Stats))
    after
        gen_server:stop(Pid)
    end.

async_known_reports_hint() ->
    {ok, Pid} = test_counter:start_link(0),
    try
        F = beamtalk_future:new(),
        ok = beamtalk_actor:async_send_known(Pid, 'Hinted', increment, [], F),
        _ = beamtalk_future:await(F),
        ok = beamtalk_test_wait:sync(Pid),
        Stats = beamtalk_trace_store:get_stats(),
        ?assert(maps:is_key(?CLASS_STATS_KEY(<<"Hinted">>), Stats)),
        ?assertEqual(0, meck:num_calls(beamtalk_object_instances, class_of, '_'))
    after
        gen_server:stop(Pid)
    end.

unknown_pids() ->
    Remote = binary_to_term(<<131, 88, 119, 10, "nohost@xxx", 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1>>),
    ?assertNotEqual(node(), node(Remote)),
    ?assertEqual(unknown, beamtalk_actor:lookup_class(Remote)),
    ?assertEqual(unknown, beamtalk_actor:lookup_class(self())).

timeout_proxy_infinity() ->
    {ok, Pid} = test_marked_server:start_link('TimeoutProxy'),
    try
        ?assertEqual(infinity, traced_call_timeout(Pid))
    after
        gen_server:stop(Pid)
    end.

other_class_default_timeout() ->
    {ok, Pid} = test_marked_server:start_link('Counter'),
    try
        ?assertNotEqual(infinity, traced_call_timeout(Pid))
    after
        gen_server:stop(Pid)
    end.

%% The Timeout argument `sync_send/3` hands `gen:call/4` for a bare pid.
traced_call_timeout(Pid) ->
    Self = self(),
    erlang:trace_pattern({gen, call, 4}, true, [local]),
    erlang:trace(Self, true, [call]),
    try
        pong = beamtalk_actor:sync_send(Pid, ping, []),
        receive
            {trace, Self, call, {gen, call, [Pid, _Label, _Req, Timeout]}} -> Timeout
        after 1000 -> no_call_traced
        end
    after
        erlang:trace(Self, false, [call]),
        erlang:trace_pattern({gen, call, 4}, false, [local])
    end.
