%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_parallel_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for beamtalk_parallel module (BT-2974).

Focus: the two `'DOWN'` message branches in `gather_all/5` (line 343) and
`gather_any/5` (line 426) that fire when a worker process is killed externally
— bypassing `run_worker/4`'s try/catch — before it sends its result. These
branches are structurally unreachable from BUnit because blocks run inside the
Parallel infrastructure itself; only a direct EUnit test can externally kill
a worker with `exit(Pid, kill)`.

Also covers class methods, type-error paths, timeout, and FFI shims at the
Erlang level, supplementing the BUnit parallel_test.bt coverage.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% Run Fun in a helper process that traps exits (so a link-propagated 'killed'
%% signal from an externally-killed worker arrives as {'EXIT',...} rather than
%% killing the helper) and forward the return value to CallerPid.
-spec run_trapping(fun(() -> term()), pid()) -> pid().
run_trapping(Fun, CallerPid) ->
    spawn(fun() ->
        process_flag(trap_exit, true),
        Result = Fun(),
        CallerPid ! {helper_result, Result}
    end).

%%% ============================================================================
%%% 'DOWN' path — gather_all/5
%%%
%%% When a worker process is killed externally with exit(Pid, kill), it dies
%%% before run_worker/4 can send {CallRef, Idx, Result} to the caller. The
%%% caller receives a {'DOWN', Ref, process, Pid, killed} monitor message
%%% instead, and gather_all/5 converts that slot into Result error:.
%%% ============================================================================

gather_all_down_path_marks_killed_slot_as_error_test() ->
    %% Block 1 registers its Pid, then sleeps — it will be killed externally
    %% before it wakes up. Block 2 completes normally.
    %%
    %% The helper process traps exits so the link-propagated 'killed' signal
    %% (which bypasses the caller's trap_exit=false default) arrives as a
    %% message rather than crashing the helper.
    Caller = self(),
    _Helper = run_trapping(
        fun() ->
            beamtalk_parallel:'all:'([
                fun() ->
                    Caller ! {worker_pid, self()},
                    timer:sleep(5000)
                end,
                fun() -> 42 end
            ])
        end,
        Caller
    ),
    SlowPid =
        receive
            {worker_pid, P} -> P
        after 1000 -> error(no_worker_pid_registered)
        end,
    %% Kill the worker externally — exit/2 with `kill` is untrappable and
    %% bypasses run_worker/4's try/catch, so the 'DOWN' path fires.
    exit(SlowPid, kill),
    Results =
        receive
            {helper_result, R} -> R
        after 2000 -> error(gather_all_down_path_timeout)
        end,
    %% Slot 2 completed normally → Result ok: 42
    ?assertMatch(#{'isOk' := true, 'okValue' := 42}, lists:nth(2, Results)),
    %% Slot 1 was killed externally → 'DOWN' path → Result error:
    ?assertMatch(#{'isOk' := false}, lists:nth(1, Results)).

gather_all_down_path_other_slots_unaffected_test() ->
    %% Three blocks: slot 2 killed externally, slots 1 and 3 complete normally.
    %% Verifies gather_all/5 continues collecting remaining workers after a 'DOWN'.
    Caller = self(),
    _Helper = run_trapping(
        fun() ->
            beamtalk_parallel:'all:'([
                fun() -> 100 end,
                fun() ->
                    Caller ! {worker_pid, self()},
                    timer:sleep(5000)
                end,
                fun() -> 300 end
            ])
        end,
        Caller
    ),
    SlowPid =
        receive
            {worker_pid, P} -> P
        after 1000 -> error(no_worker_pid_registered)
        end,
    exit(SlowPid, kill),
    Results =
        receive
            {helper_result, R} -> R
        after 2000 -> error(gather_all_down_path_other_slots_timeout)
        end,
    ?assertMatch(#{'isOk' := true, 'okValue' := 100}, lists:nth(1, Results)),
    ?assertMatch(#{'isOk' := false}, lists:nth(2, Results)),
    ?assertMatch(#{'isOk' := true, 'okValue' := 300}, lists:nth(3, Results)).

%%% ============================================================================
%%% 'DOWN' path — gather_any/5
%%%
%%% Two branches: (a) all workers killed externally → any: returns a
%%% Result error: wrapping the list of individual failure reasons in input
%%% order; (b) one worker killed, another succeeds → winner's result returned.
%%% ============================================================================

gather_any_down_path_all_killed_returns_error_test() ->
    Caller = self(),
    _Helper = run_trapping(
        fun() ->
            beamtalk_parallel:'any:'([
                fun() ->
                    Caller ! {worker_pid, 1, self()},
                    timer:sleep(5000)
                end,
                fun() ->
                    Caller ! {worker_pid, 2, self()},
                    timer:sleep(5000)
                end
            ])
        end,
        Caller
    ),
    Pid1 =
        receive
            {worker_pid, 1, P1} -> P1
        after 1000 -> error(no_pid_1)
        end,
    Pid2 =
        receive
            {worker_pid, 2, P2} -> P2
        after 1000 -> error(no_pid_2)
        end,
    exit(Pid1, kill),
    exit(Pid2, kill),
    Result =
        receive
            {helper_result, R} -> R
        after 2000 -> error(gather_any_all_killed_timeout)
        end,
    %% When all workers fail (via 'DOWN'), any: returns Result error: with a
    %% List of the individual wrapped reasons, in input order.
    ?assertMatch(#{'isOk' := false, 'errReason' := [_, _]}, Result).

gather_any_down_path_one_killed_winner_succeeds_test() ->
    %% Block 1 registers its Pid, then sleeps. Test kills it externally.
    %% gather_any/5 handles the 'DOWN', removes slot 1 from Pending, and
    %% continues waiting. Block 2 succeeds → its result is returned.
    Caller = self(),
    _Helper = run_trapping(
        fun() ->
            beamtalk_parallel:'any:'([
                fun() ->
                    Caller ! {worker_pid, self()},
                    timer:sleep(5000)
                end,
                fun() ->
                    %% Brief sleep to ensure block 1 registers before winning,
                    %% so the test exercises the 'DOWN' path rather than the
                    %% normal kill_pending path that fires when a winner is found
                    %% before all losers have even registered.
                    timer:sleep(50),
                    winner
                end
            ])
        end,
        Caller
    ),
    SlowPid =
        receive
            {worker_pid, P} -> P
        after 1000 -> error(no_worker_pid_registered)
        end,
    exit(SlowPid, kill),
    Result =
        receive
            {helper_result, R} -> R
        after 2000 -> error(gather_any_one_killed_timeout)
        end,
    ?assertMatch(#{'isOk' := true, 'okValue' := winner}, Result).

%%% ============================================================================
%%% 'all:' — normal paths
%%% ============================================================================

all_empty_blocks_returns_empty_list_test() ->
    ?assertEqual([], beamtalk_parallel:'all:'([])).

all_single_block_success_test() ->
    Results = beamtalk_parallel:'all:'([fun() -> 7 end]),
    ?assertMatch([#{'isOk' := true, 'okValue' := 7}], Results).

all_multiple_blocks_succeed_in_order_test() ->
    %% Results must be in the same order as the input blocks, regardless of
    %% which block finishes first.
    Results = beamtalk_parallel:'all:'([fun() -> a end, fun() -> b end, fun() -> c end]),
    Values = [maps:get('okValue', R) || R <- Results],
    ?assertEqual([a, b, c], Values).

all_block_error_becomes_result_error_test() ->
    Results = beamtalk_parallel:'all:'([
        fun() -> 1 end,
        fun() -> erlang:error(deliberate_error) end
    ]),
    ?assertMatch(#{'isOk' := true, 'okValue' := 1}, lists:nth(1, Results)),
    ?assertMatch(#{'isOk' := false}, lists:nth(2, Results)).

%%% ============================================================================
%%% 'all:' — type-error paths
%%% ============================================================================

all_non_list_argument_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'all:'(not_a_list)
    ).

all_non_block_element_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'all:'([fun() -> ok end, 42])
    ).

%%% ============================================================================
%%% 'all:timeout:' — timeout paths
%%% ============================================================================

all_timeout_completes_within_deadline_test() ->
    Results = beamtalk_parallel:'all:timeout:'([fun() -> 99 end], 5000),
    ?assertMatch([#{'isOk' := true, 'okValue' := 99}], Results).

all_timeout_kills_slow_worker_and_marks_slot_as_error_test() ->
    %% Block 2 finishes quickly; block 1 sleeps past the deadline. After the
    %% deadline, gather_all/5's `after` fires, kill_pending kills block 1, and
    %% its slot becomes Result error: (timeout kind). Block 2's slot keeps its
    %% real result.
    Results = beamtalk_parallel:'all:timeout:'(
        [
            fun() ->
                timer:sleep(5000),
                slow
            end,
            fun() -> fast end
        ],
        100
    ),
    ?assertMatch(
        #beamtalk_error{kind = timeout},
        maps:get(error, maps:get('errReason', lists:nth(1, Results)))
    ),
    ?assertMatch(#{'isOk' := true, 'okValue' := fast}, lists:nth(2, Results)).

all_timeout_with_duration_argument_test() ->
    D = beamtalk_duration:'milliseconds:'(500),
    Results = beamtalk_parallel:'all:timeout:'([fun() -> 42 end], D),
    ?assertMatch([#{'isOk' := true, 'okValue' := 42}], Results).

all_timeout_negative_ms_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'all:timeout:'([fun() -> 1 end], -1)
    ).

all_timeout_non_list_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'all:timeout:'(not_a_list, 100)
    ).

%%% ============================================================================
%%% 'any:' — normal paths
%%% ============================================================================

any_single_block_success_test() ->
    Result = beamtalk_parallel:'any:'([fun() -> 7 end]),
    ?assertMatch(#{'isOk' := true, 'okValue' := 7}, Result).

any_first_succeeding_block_wins_test() ->
    %% Block 1 sleeps; block 2 returns immediately — block 2 is the winner.
    Result = beamtalk_parallel:'any:'([
        fun() ->
            timer:sleep(5000),
            slow
        end,
        fun() -> fast end
    ]),
    ?assertMatch(#{'isOk' := true, 'okValue' := fast}, Result).

any_all_fail_returns_error_with_reason_list_test() ->
    Result = beamtalk_parallel:'any:'([
        fun() -> erlang:error(fail1) end,
        fun() -> erlang:error(fail2) end
    ]),
    %% errReason is a List of the individual wrapped reasons, in input order.
    ?assertMatch(#{'isOk' := false, 'errReason' := [_, _]}, Result).

%%% ============================================================================
%%% 'any:' — type-error paths
%%% ============================================================================

any_empty_list_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'any:'([])
    ).

any_non_list_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'any:'(not_a_list)
    ).

any_non_block_element_raises_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_parallel:'any:'([fun() -> ok end, 99])
    ).

%%% ============================================================================
%%% FFI shims — all/1, all/2, any/1 delegate to canonical colon forms
%%% ============================================================================

ffi_all_shim_test() ->
    Results = beamtalk_parallel:all([fun() -> 5 end]),
    ?assertMatch([#{'isOk' := true, 'okValue' := 5}], Results).

ffi_all_timeout_shim_test() ->
    Results = beamtalk_parallel:all([fun() -> 5 end], 5000),
    ?assertMatch([#{'isOk' := true, 'okValue' := 5}], Results).

ffi_any_shim_test() ->
    Result = beamtalk_parallel:any([fun() -> 5 end]),
    ?assertMatch(#{'isOk' := true, 'okValue' := 5}, Result).
