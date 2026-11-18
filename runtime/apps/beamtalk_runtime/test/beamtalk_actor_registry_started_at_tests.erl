%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_actor_registry_started_at_tests).

-moduledoc """
BT-4937 / ADR 0126 §4: `resolve_global_conflict/3` decides by each
candidate's `'$beamtalk_actor_started_at'` key. Covers the mixed case, where
one side has no start time (started by an older runtime, or a `native:`
backing process): the side that has one is kept, in either argument order.
""".

-include_lib("eunit/include/eunit.hrl").

%% A process that carries `StartedAt` (or no key at all when `undefined`).
start_proc(StartedAt) ->
    Parent = self(),
    Pid = spawn(fun() ->
        StartedAt =:= undefined orelse erlang:put('$beamtalk_actor_started_at', StartedAt),
        Parent ! {ready, self()},
        receive
            stop -> ok
        end
    end),
    %% Bounded: an unbounded receive turns a process that never started into
    %% an anonymous eunit timeout. Name what did not start instead.
    receive
        {ready, Pid} -> Pid
    after 10000 ->
        erlang:error(
            {test_proc_did_not_start, #{
                pid => Pid,
                alive => is_process_alive(Pid),
                process_info => erlang:process_info(Pid, [status, current_stacktrace]),
                run_queue => erlang:statistics(run_queue)
            }}
        )
    end.

stop_proc(Pid) ->
    Pid ! stop.

mixed_case_keeps_the_side_with_a_start_time_test_() ->
    [
        {"start time is the first argument", fun() -> mixed(first) end},
        {"start time is the second argument", fun() -> mixed(second) end}
    ].

mixed(Where) ->
    WithTime = start_proc(erlang:system_time(microsecond)),
    NoTime = start_proc(undefined),
    {P1, P2} =
        case Where of
            first -> {WithTime, NoTime};
            second -> {NoTime, WithTime}
        end,
    ?assertEqual(WithTime, beamtalk_actor_registry:resolve_global_conflict(name, P1, P2)),
    stop_proc(WithTime),
    stop_proc(NoTime).

both_have_times_keeps_the_older_test() ->
    Now = erlang:system_time(microsecond),
    Older = start_proc(Now - 1000),
    Younger = start_proc(Now),
    ?assertEqual(Older, beamtalk_actor_registry:resolve_global_conflict(name, Younger, Older)),
    ?assertEqual(Older, beamtalk_actor_registry:resolve_global_conflict(name, Older, Younger)),
    stop_proc(Older),
    stop_proc(Younger).

%% A pid on a node this node cannot reach: an external pid term built by hand
%% (NEW_PID_EXT, small UTF-8 atom node name).
unreachable_pid() ->
    Node = <<"bt4937_gone@nohost.invalid">>,
    binary_to_term(
        <<131, 88, 119, (byte_size(Node)), Node/binary, 1:32, 0:32, 1:32>>
    ).

failed_remote_read_takes_the_otp_fallback_not_the_local_side_test_() ->
    %% Each order makes one `erpc:call/5` to an unreachable node, bounded by
    %% BT_REMOTE_CALL_TIMEOUT (5000 ms), so the default 5 s eunit timeout can
    %% be spent by a single slow connect attempt.
    {timeout, 60, fun failed_remote_read_takes_the_otp_fallback_not_the_local_side/0}.

failed_remote_read_takes_the_otp_fallback_not_the_local_side() ->
    %% The remote read fails. Treating that as "no start time" would keep the
    %% local pid every time. It must take OTP's fallback instead, which keeps
    %% the pid on the lower-named node: the same answer on every node.
    Remote = unreachable_pid(),
    %% A fresh Local per argument order: the fallback may kill Local, and a
    %% dead Local would read `undefined`, not a time, hiding the {unreachable, T}
    %% order.
    lists:foreach(
        fun(Order) ->
            Local = start_proc(erlang:system_time(microsecond)),
            Expected =
                case node(Local) < node(Remote) of
                    true -> Local;
                    false -> Remote
                end,
            Args =
                case Order of
                    local_first -> [name, Local, Remote];
                    remote_first -> [name, Remote, Local]
                end,
            ?assert(is_process_alive(Local)),
            ?assertEqual(Expected, apply(beamtalk_actor_registry, resolve_global_conflict, Args)),
            stop_proc(Local)
        end,
        [local_first, remote_first]
    ).
