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

1. The resolver is pointed at a local UDP socket that never answers, with a
   long resolver timeout, so an unbounded lookup takes well over ten seconds.
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
    %% A DNS server that receives every query and answers none.
    {ok, Silent} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}, {active, false}]),
    {ok, Port} = inet:port(Silent),
    Saved = [{Opt, inet_db:res_option(Opt)} || Opt <- [lookup, nameservers, timeout, retry]],
    try
        ok = inet_db:set_lookup([dns]),
        ok = inet_db:res_option(nameservers, [{{127, 0, 0, 1}, Port}]),
        %% Unbounded, one lookup per address family would wait 3 x 5 s.
        ok = inet_db:res_option(timeout, 5000),
        ok = inet_db:res_option(retry, 3),
        Node = 'w@bt5192-silent-resolver.example',
        Result = with_app_env(node_resolve_timeout_ms, 300, fun() ->
            call_within(fun() -> beamtalk_node:connect_policy(Node, false) end, ?ANSWER_WITHIN_MS)
        end),
        %% Not proven local in time, so refused without TLS, like a host that
        %% does not resolve.
        ?assertMatch({error, #beamtalk_error{kind = insecure_distribution}}, Result)
    after
        [ok = inet_db:res_option(Opt, Value) || {Opt, Value} <- Saved],
        gen_udp:close(Silent)
    end.

%%====================================================================
%% 2. spawnAs:scope: #global's name registration
%%====================================================================

global_registration_is_bounded_test_() ->
    {timeout, 60, ?_test(global_registration_is_bounded())}.

global_registration_is_bounded() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    Name = list_to_atom("bt5192_bounded_global_" ++ beamtalk_test_unique:id()),
    Caller = self(),
    %% Holds `global`'s own lock (the one `register_name/3` takes) until told.
    Holder = spawn(fun() ->
        true = global:set_lock({global, self()}, [node()], infinity),
        Caller ! {locked, self()},
        receive
            release -> global:del_lock({global, self()}, [node()])
        end
    end),
    ?awaitMessage({locked, Holder}),
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
    %% `global` runs registrations one at a time, so this one returns only after
    %% the refused start's late registration (if any) has run.
    Flush = list_to_atom("bt5192_flush_" ++ beamtalk_test_unique:id()),
    yes = global:register_name(Flush, self()),
    _ = global:unregister_name(Flush),
    %% The refused start leaves nothing behind: its actor was stopped, so a
    %% late registration of it is dropped.
    ok = beamtalk_test_wait:until(fun() -> global:whereis_name(Name) =:= undefined end),
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, test_counter, 0),
    ?assertEqual(Pid, global:whereis_name(Name)),
    gen_server:stop(Pid).
