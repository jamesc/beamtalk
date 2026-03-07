%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_timer module (BT-1121, BT-1165).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests class methods (after:do:, every:do:, sleep:), instance methods
%%% (cancel, isActive, printString), the ack-based cancel protocol,
%%% FFI shims (after/2, every/2, sleep/1), and error paths.

-module(beamtalk_timer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @doc Block until the given process exits or the timeout (ms) elapses.
%% Fails the test if the process does not exit within Timeout ms.
wait_for_exit(Pid, Timeout) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _Reason} -> ok
    after Timeout ->
        erlang:demonitor(MRef, [flush]),
        ?assert(false, "process did not exit within timeout")
    end.

%%% ============================================================================
%%% after:do: — construction
%%% ============================================================================

after_do_returns_timer_map_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    ?assertEqual('Timer', maps:get('$beamtalk_class', T)),
    ?assert(is_pid(maps:get(pid, T))),
    beamtalk_timer:cancel(T).

after_do_process_is_alive_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    ?assert(erlang:is_process_alive(maps:get(pid, T))),
    beamtalk_timer:cancel(T).

after_do_zero_delay_test() ->
    %% Zero-delay timers are valid (fires immediately)
    T = beamtalk_timer:'after:do:'(0, fun() -> ok end),
    ?assertEqual('Timer', maps:get('$beamtalk_class', T)).

after_do_negative_delay_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'after:do:'(-1, fun() -> ok end)
    ).

after_do_non_integer_delay_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'after:do:'(1.5, fun() -> ok end)
    ).

after_do_non_block_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'after:do:'(100, not_a_fun)
    ).

%%% ============================================================================
%%% every:do: — construction
%%% ============================================================================

every_do_returns_timer_map_test() ->
    T = beamtalk_timer:'every:do:'(10000, fun() -> ok end),
    ?assertEqual('Timer', maps:get('$beamtalk_class', T)),
    ?assert(is_pid(maps:get(pid, T))),
    beamtalk_timer:cancel(T).

every_do_zero_delay_test() ->
    %% Zero interval is invalid (must be positive)
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'every:do:'(0, fun() -> ok end)
    ).

every_do_negative_delay_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'every:do:'(-10, fun() -> ok end)
    ).

every_do_non_integer_delay_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'every:do:'(1.0, fun() -> ok end)
    ).

%%% ============================================================================
%%% sleep:
%%% ============================================================================

sleep_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_timer:'sleep:'(1)).

sleep_zero_test() ->
    ?assertEqual(nil, beamtalk_timer:'sleep:'(0)).

sleep_negative_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'sleep:'(-1)
    ).

sleep_non_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'sleep:'(<<"500">>)
    ).

%%% ============================================================================
%%% cancel/1 — ack-based protocol
%%% ============================================================================

cancel_pending_timer_returns_true_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    ?assert(beamtalk_timer:cancel(T)).

cancel_already_cancelled_returns_false_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    beamtalk_timer:cancel(T),
    ?assertNot(beamtalk_timer:cancel(T)).

cancel_fired_timer_returns_false_test() ->
    T = beamtalk_timer:'after:do:'(10, fun() -> ok end),
    wait_for_exit(maps:get(pid, T), 1000),
    ?assertNot(beamtalk_timer:cancel(T)).

cancel_repeating_timer_returns_true_test() ->
    T = beamtalk_timer:'every:do:'(10000, fun() -> ok end),
    ?assert(beamtalk_timer:cancel(T)).

cancel_is_deterministic_test() ->
    %% cancel/1 must only return true when it confirmed the process received
    %% the cancel message before the block ran (ack protocol).
    %% After cancel returns true, the process must be dead.
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    Result = beamtalk_timer:cancel(T),
    ?assert(Result),
    %% Process has confirmed cancel — must be dead now
    ?assertNot(erlang:is_process_alive(maps:get(pid, T))).

%%% ============================================================================
%%% isActive/1
%%% ============================================================================

is_active_pending_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    ?assert(beamtalk_timer:'isActive'(T)),
    beamtalk_timer:cancel(T).

is_active_after_cancel_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    beamtalk_timer:cancel(T),
    ?assertNot(beamtalk_timer:'isActive'(T)).

is_active_after_fired_test() ->
    T = beamtalk_timer:'after:do:'(10, fun() -> ok end),
    wait_for_exit(maps:get(pid, T), 1000),
    ?assertNot(beamtalk_timer:'isActive'(T)).

is_active_repeating_stays_active_test() ->
    T = beamtalk_timer:'every:do:'(20, fun() -> ok end),
    timer:sleep(80),
    ?assert(beamtalk_timer:'isActive'(T)),
    beamtalk_timer:cancel(T).

%%% ============================================================================
%%% printString/1
%%% ============================================================================

print_string_active_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    ?assertEqual(<<"a Timer(active)">>, beamtalk_timer:'printString'(T)),
    beamtalk_timer:cancel(T).

print_string_inactive_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    beamtalk_timer:cancel(T),
    ?assertEqual(<<"a Timer(inactive)">>, beamtalk_timer:'printString'(T)).

%%% ============================================================================
%%% FFI shims (BT-1165)
%%% ============================================================================

ffi_shim_after_returns_timer_test() ->
    T = beamtalk_timer:'after'(10000, fun() -> ok end),
    ?assertEqual('Timer', maps:get('$beamtalk_class', T)),
    beamtalk_timer:cancel(T).

ffi_shim_every_returns_timer_test() ->
    T = beamtalk_timer:every(10000, fun() -> ok end),
    ?assertEqual('Timer', maps:get('$beamtalk_class', T)),
    beamtalk_timer:cancel(T).

ffi_shim_sleep_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_timer:sleep(1)).

ffi_shim_after_error_propagates_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:'after'(-1, fun() -> ok end)
    ).

ffi_shim_every_error_propagates_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:every(0, fun() -> ok end)
    ).

ffi_shim_sleep_error_propagates_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_timer:sleep(-1)
    ).
