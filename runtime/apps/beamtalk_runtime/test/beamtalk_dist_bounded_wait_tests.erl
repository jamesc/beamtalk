%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_bounded_wait_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
`Node connect`'s host lookup and `spawnAs:scope: #global`'s name registration
answer within a bound (BT-5192).

Both waited without one before: the connect policy's `inet:getaddrs/2` for as
long as the resolver kept retrying, and `global:register_name/3` for as long
as another holder kept `global`'s lock. Each test makes the wait real and
deterministic, with no sleeps:

1. `inet:getaddrs/2` is made to stall for 30 seconds (`meck`, passthrough
   for every other `inet` function), as a resolver that never answers does.
2. A helper process holds `global`'s own lock, so an unbounded registration
   never returns.

Each runs the call in a separate process and fails if it has not answered
within a few seconds; the bound under test is lowered through its
application env so the test stays fast.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%% How long a call under test may take before it counts as unbounded.
-define(ANSWER_WITHIN_MS, 4000).

%% Run `Fun` in its own process; its result, or `still_waiting` after `Ms`.
call_within(Fun, Ms) ->
    Caller = self(),
    Tag = make_ref(),
    {Pid, Ref} = spawn_monitor(fun() -> Caller ! {Tag, Fun()} end),
    receive
        {Tag, Result} ->
            erlang:demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            {crashed, Reason}
    after Ms ->
        exit(Pid, kill),
        erlang:demonitor(Ref, [flush]),
        still_waiting
    end.

with_app_env(Key, Value, Fun) ->
    Old = application:get_env(beamtalk_runtime, Key),
    ok = application:set_env(beamtalk_runtime, Key, Value),
    try
        Fun()
    after
        case Old of
            undefined -> application:unset_env(beamtalk_runtime, Key);
            {ok, V} -> application:set_env(beamtalk_runtime, Key, V)
        end
    end.

%%====================================================================
%% 1. Node connect's host lookup
%%====================================================================

host_lookup_is_bounded_test_() ->
    {timeout, 60, ?_test(host_lookup_is_bounded())}.

host_lookup_is_bounded() ->
    %% A resolver that never answers in time: `inet:getaddrs/2` (the lookup the
    %% connect policy makes) stalls far longer than the bound. Every other
    %% `inet` function passes through. A real silent DNS server is not usable
    %% here: depending on the platform's resolver configuration the lookup
    %% fails fast instead of waiting on it.
    ok = meck:new(inet, [unstick, passthrough, no_link]),
    try
        ok = meck:expect(inet, getaddrs, fun(_Host, _Family) ->
            receive
                never -> {error, nxdomain}
            after 30000 -> {error, timeout}
            end
        end),
        Node = 'w@bt5192-stalled-resolver.example',
        Result = with_app_env(node_resolve_timeout_ms, 300, fun() ->
            call_within(fun() -> beamtalk_node:connect_policy(Node, false) end, ?ANSWER_WITHIN_MS)
        end),
        %% Not proven local in time, so refused without TLS, like a host that
        %% does not resolve.
        ?assertMatch({error, #beamtalk_error{kind = insecure_distribution}}, Result)
    after
        meck:unload(inet)
    end.

%%====================================================================
%% 2. spawnAs:scope: #global's name registration
%%====================================================================

%% Hold `global`'s own lock (the one `register_name/3` takes) in a helper
%% process; answers the helper, which releases the lock on `release`.
hold_global_lock() ->
    Caller = self(),
    Holder = spawn(fun() ->
        true = global:set_lock({global, self()}, [node()], infinity),
        Caller ! {locked, self()},
        receive
            release -> global:del_lock({global, self()}, [node()])
        end
    end),
    ?awaitMessage({locked, Holder}),
    Holder.

global_registration_is_bounded_test_() ->
    {timeout, 60, ?_test(global_registration_is_bounded())}.

global_registration_is_bounded() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    Name = beamtalk_test_unique:atom("bt5192_bounded_global"),
    Holder = hold_global_lock(),
    try
        Result = with_app_env(global_register_timeout_ms, 300, fun() ->
            call_within(
                fun() -> beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0) end,
                ?ANSWER_WITHIN_MS
            )
        end),
        ?assertMatch(
            {error, #beamtalk_error{kind = timeout, details = #{name := Name}}}, Result
        )
    after
        Holder ! release
    end,
    %% Straight away, with the refused start's request possibly still queued
    %% in `global`'s registrar (it registers the stopped actor, which `global`
    %% then drops): the same name starts cleanly.
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0),
    ?assertEqual(Pid, global:whereis_name(Name)),
    gen_server:stop(Pid).

%% A supervised `withName:scope: #global` child whose registration times out is
%% not started (`ignore`), so it spends no restart intensity.
supervised_global_child_registration_timeout_is_ignored_test_() ->
    {timeout, 60, ?_test(supervised_global_child_registration_timeout_is_ignored())}.

supervised_global_child_registration_timeout_is_ignored() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    Name = beamtalk_test_unique:atom("bt5192_sup_global"),
    Holder = hold_global_lock(),
    try
        Result = with_app_env(global_register_timeout_ms, 300, fun() ->
            call_within(
                fun() -> beamtalk_supervisor:start_actor_child(test_counter, 0, {global, Name}) end,
                ?ANSWER_WITHIN_MS
            )
        end),
        ?assertEqual(ignore, Result)
    after
        Holder ! release
    end.

%% Each address family is bounded on its own: a stalled IPv6 lookup does not
%% throw away a host's local IPv4 answer.
slow_ipv6_lookup_keeps_ipv4_answer_test_() ->
    {timeout, 60, ?_test(slow_ipv6_lookup_keeps_ipv4_answer())}.

slow_ipv6_lookup_keeps_ipv4_answer() ->
    ok = meck:new(inet, [unstick, passthrough, no_link]),
    try
        ok = meck:expect(inet, getaddrs, fun
            (_Host, inet) ->
                {ok, [{127, 0, 0, 1}]};
            (_Host, inet6) ->
                receive
                    never -> {error, nxdomain}
                after 30000 -> {error, timeout}
                end
        end),
        Node = 'w@bt5192-v4-only-alias.example',
        Result = with_app_env(node_resolve_timeout_ms, 300, fun() ->
            call_within(fun() -> beamtalk_node:connect_policy(Node, false) end, ?ANSWER_WITHIN_MS)
        end),
        ?assertEqual(ok, Result)
    after
        meck:unload(inet)
    end.
