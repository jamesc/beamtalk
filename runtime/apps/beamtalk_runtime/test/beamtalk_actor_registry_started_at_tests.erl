%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_actor_registry_started_at_tests).

-moduledoc """
BT-4937, BT-5231 / ADR 0126 §4: `resolve_global_conflict/3` decides by each
candidate's `'$beamtalk_actor_started_at'` key. Covers the mixed case, where
one side has no start time (started by an older runtime, or a `native:`
backing process): the side that has one is kept, in either argument order.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% A process that carries `StartedAt` (or no key at all when `undefined`).
start_proc(StartedAt) ->
    Parent = self(),
    Pid = spawn(fun() ->
        StartedAt =:= undefined orelse erlang:put('$beamtalk_actor_started_at', StartedAt),
        Parent ! {ready, self()},
        receive
            stop -> ok;
            %% `gen_server:stop/3`'s request (the resolver's graceful stop),
            %% answered as a gen_server would, so the stop does not wait out
            %% its timeout.
            {system, _From, {terminate, Reason}} -> exit(Reason)
        end
    end),
    %% Bounded: an unbounded receive turns a process that never started into
    %% an anonymous eunit timeout. Name what did not start instead.
    ?awaitMessage({ready, Pid}, 10000),
    Pid.

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

%% BT-5231: a pid on a node this node cannot reach, an external pid term built
%% by hand (NEW_PID_EXT, small UTF-8 atom node name). Its node name sorts below
%% any real node name (`'!'` is below every letter), so the tie-break keeps it
%% and the local side is the loser whose stop the tests observe.
unreachable_pid() ->
    Node = <<"!bt5231_gone@nohost.invalid">>,
    binary_to_term(
        <<131, 88, 119, (byte_size(Node)), Node/binary, 1:32, 0:32, 1:32>>
    ).

%% `Loser` was stopped gracefully, not killed (`random_exit_name/3` kills).
assert_stopped_gracefully(Loser, Mon) ->
    ?awaitMessage({'DOWN', Mon, process, Loser, {shutdown, global_name_conflict}}).

-doc """
BT-5231 (ADR 0126 BT-5193 amendment): every case that cannot order the two
start times keeps the pid on the lower-named node (on one node, the lower pid)
and stops the other gracefully. Before BT-5231 the `unreachable` and
both-untimed cases took `global:random_exit_name/3` (which kills its loser and,
on one node, keeps its second argument), and equal times kept the first
argument.
""".
tie_break_keeps_lower_node_pid_test_() ->
    [
        {"both untimed", fun() -> same_node_tie(undefined) end},
        {"equal start times", fun() -> same_node_tie(erlang:system_time(microsecond)) end},
        %% One erpc to an unreachable node per argument order, bounded by
        %% BT_GLOBAL_CONFLICT_READ_TIMEOUT.
        {"unreachable start-time read", {timeout, 30, fun unreachable_read/0}}
    ].

same_node_tie(StartedAt) ->
    %% Both argument orders, each with fresh processes: the loser is stopped.
    lists:foreach(
        fun(Order) ->
            A = start_proc(StartedAt),
            B = start_proc(StartedAt),
            {Low, High} = {min(A, B), max(A, B)},
            Mon = erlang:monitor(process, High),
            Args =
                case Order of
                    low_first -> [name, Low, High];
                    high_first -> [name, High, Low]
                end,
            ?assertEqual(Low, apply(beamtalk_actor_registry, resolve_global_conflict, Args)),
            assert_stopped_gracefully(High, Mon),
            ?assert(is_process_alive(Low)),
            stop_proc(Low)
        end,
        [low_first, high_first]
    ).

unreachable_read() ->
    %% The remote read fails, which says nothing about age: the remote pid is
    %% kept because its node name is lower, never the local side by default,
    %% and the local loser is stopped, not killed.
    Remote = unreachable_pid(),
    lists:foreach(
        fun(Order) ->
            Local = start_proc(erlang:system_time(microsecond)),
            ?assert(node(Remote) < node(Local)),
            Mon = erlang:monitor(process, Local),
            Args =
                case Order of
                    local_first -> [name, Local, Remote];
                    remote_first -> [name, Remote, Local]
                end,
            ?assertEqual(Remote, apply(beamtalk_actor_registry, resolve_global_conflict, Args)),
            assert_stopped_gracefully(Local, Mon)
        end,
        [local_first, remote_first]
    ).
