%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_timer_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_timer module.

Tests class methods (after:do:, every:do:, send:to:after:, send:to:every:,
sleep:), instance methods
(cancel, isActive, printString), the ack-based cancel protocol,
FFI shims (after/2, every/2, sleep/1), and error paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

-doc """
Block until the given process exits or the timeout (ms) elapses.
Fails the test if the process does not exit within Timeout ms.
""".
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
    ?assertEqual(<<"Timer(active)">>, beamtalk_timer:'printString'(T)),
    beamtalk_timer:cancel(T).

print_string_inactive_test() ->
    T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
    beamtalk_timer:cancel(T),
    ?assertEqual(<<"Timer(inactive)">>, beamtalk_timer:'printString'(T)).

%%% ============================================================================
%%% FFI shims
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

%%% ============================================================================
%%% spawn_link — automatic cleanup on caller death
%%% ============================================================================

after_timer_dies_when_caller_dies_test() ->
    %% Spawn a wrapper process that creates a timer, then exits.
    %% The timer should die because it's linked to the wrapper.
    Parent = self(),
    Wrapper = spawn(fun() ->
        T = beamtalk_timer:'after:do:'(10000, fun() -> ok end),
        Parent ! {timer, T},
        %% Exit abnormally — the link propagates this to the timer
        exit(shutdown)
    end),
    T =
        receive
            {timer, Timer} -> Timer
        after 1000 -> error(timeout)
        end,
    wait_for_exit(Wrapper, 1000),
    wait_for_exit(maps:get(pid, T), 1000).

every_timer_dies_when_caller_dies_test() ->
    Parent = self(),
    Wrapper = spawn(fun() ->
        T = beamtalk_timer:'every:do:'(10000, fun() -> ok end),
        Parent ! {timer, T},
        exit(shutdown)
    end),
    T =
        receive
            {timer, Timer} -> Timer
        after 1000 -> error(timeout)
        end,
    wait_for_exit(Wrapper, 1000),
    wait_for_exit(maps:get(pid, T), 1000).

%%% ============================================================================
%%% send:to:after: / send:to:every: — message timers (BT-4048)
%%% ============================================================================

-define(TYPE_ERROR, #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}}).

send_after_to_pid_delivers_message_test() ->
    T = beamtalk_timer:'send:to:after:'({tick, 1}, self(), 10),
    ?assertMatch(#{'$beamtalk_class' := 'Timer', tref := _}, T),
    receive
        {tick, 1} -> ok
    after 1000 -> ?assert(false, "message not delivered")
    end,
    ?assertNot(beamtalk_timer:'isActive'(T)),
    ?assertNot(beamtalk_timer:cancel(T)).

send_after_to_actor_object_delivers_message_test() ->
    Obj = #beamtalk_object{class = 'Foo', class_mod = foo, pid = self()},
    _ = beamtalk_timer:'sendAfter'(tick, Obj, 5),
    receive
        tick -> ok
    after 1000 -> ?assert(false, "message not delivered")
    end.

send_after_to_registered_name_test() ->
    Name = beamtalk_timer_tests_target,
    true = register(Name, self()),
    try
        Obj = #beamtalk_object{class = 'Foo', class_mod = foo, pid = {registered, Name}},
        _ = beamtalk_timer:'send:to:after:'(tick, Obj, 5),
        receive
            tick -> ok
        after 1000 -> ?assert(false, "message not delivered")
        end
    after
        unregister(Name)
    end.

send_after_cancel_before_fire_test() ->
    T = beamtalk_timer:'send:to:after:'(never, self(), 10000),
    ?assert(beamtalk_timer:'isActive'(T)),
    ?assertEqual(<<"Timer(active)">>, beamtalk_timer:'printString'(T)),
    ?assert(beamtalk_timer:cancel(T)),
    ?assertNot(beamtalk_timer:'isActive'(T)),
    ?assertNot(beamtalk_timer:cancel(T)),
    receive
        never -> ?assert(false, "cancelled timer delivered")
    after 50 -> ok
    end.

send_after_dead_target_is_silent_test() ->
    Dead = spawn(fun() -> ok end),
    wait_for_exit(Dead, 1000),
    %% send_after/3 to a dead local pid cancels the timer at once.
    T = beamtalk_timer:'send:to:after:'(tick, Dead, 10000),
    ?assertNot(beamtalk_timer:'isActive'(T)),
    ?assertNot(beamtalk_timer:cancel(T)).

%% A pid on another node (never connected): send_after/3 would raise badarg,
%% so the one-shot falls back to a cancellable process.
send_after_remote_pid_uses_process_test() ->
    Remote = fake_remote_pid(),
    T = beamtalk_timer:'send:to:after:'(tick, Remote, 10000),
    ?assertMatch(#{'$beamtalk_class' := 'Timer', pid := _}, T),
    ?assert(beamtalk_timer:'isActive'(T)),
    ?assert(beamtalk_timer:cancel(T)),
    ?assertNot(beamtalk_timer:'isActive'(T)).

send_after_remote_registered_uses_process_test() ->
    Obj = #beamtalk_object{
        class = 'Foo', class_mod = foo, pid = {registered, bt4048_far, 'bt4048@nowhere'}
    },
    T = beamtalk_timer:'send:to:after:'(tick, Obj, 10000),
    ?assert(beamtalk_timer:'isActive'(T)),
    ?assert(beamtalk_timer:cancel(T)).

send_after_remote_pid_fires_test() ->
    T = beamtalk_timer:'send:to:after:'(tick, fake_remote_pid(), 1),
    wait_for_exit(maps:get(pid, T), 1000),
    ?assertNot(beamtalk_timer:cancel(T)).

send_after_delay_too_large_raises_test() ->
    ?assertError(?TYPE_ERROR, beamtalk_timer:'send:to:after:'(tick, self(), 4294967296)),
    ?assertError(?TYPE_ERROR, beamtalk_timer:'send:to:every:'(tick, self(), 4294967296)).

%% A repeating timer to a registered name stops when the named process dies,
%% so a restarted process under the same name does not inherit its ticks.
send_every_registered_name_stops_when_holder_dies_test() ->
    Name = bt4048_every_target,
    Holder = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    true = register(Name, Holder),
    Obj = #beamtalk_object{class = 'Foo', class_mod = foo, pid = {registered, Name}},
    T = beamtalk_timer:'send:to:every:'(tick, Obj, 10),
    Holder ! stop,
    wait_for_exit(maps:get(pid, T), 1000).

send_every_unregistered_name_stops_test() ->
    Obj = #beamtalk_object{class = 'Foo', class_mod = foo, pid = {registered, bt4048_nobody}},
    T = beamtalk_timer:'send:to:every:'(tick, Obj, 10),
    wait_for_exit(maps:get(pid, T), 1000).

send_every_does_not_drift_test() ->
    T = beamtalk_timer:'send:to:every:'(tick, self(), 20),
    timer:sleep(210),
    ?assert(beamtalk_timer:cancel(T)),
    N = count_ticks(0),
    ?assert(N >= 8 andalso N =< 11).

count_ticks(N) ->
    receive
        tick -> count_ticks(N + 1)
    after 0 -> N
    end.

fake_remote_pid() ->
    Node = atom_to_binary('bt4048@nowhere'),
    binary_to_term(
        <<131, 88, 119, (byte_size(Node)), Node/binary, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1>>
    ).

send_after_unregistered_global_is_inert_test() ->
    Obj = #beamtalk_object{class = 'Foo', class_mod = foo, pid = {global, bt4048_nobody}},
    T = beamtalk_timer:'send:to:after:'(tick, Obj, 1),
    ?assertNot(beamtalk_timer:'isActive'(T)),
    ?assertNot(beamtalk_timer:cancel(T)).

send_after_bad_target_raises_test() ->
    ?assertError(?TYPE_ERROR, beamtalk_timer:'send:to:after:'(tick, 42, 10)).

send_after_negative_delay_raises_test() ->
    ?assertError(?TYPE_ERROR, beamtalk_timer:'send:to:after:'(tick, self(), -1)).

send_every_delivers_until_cancelled_test() ->
    T = beamtalk_timer:'sendEvery'(tick, self(), 10),
    ?assertMatch(#{'$beamtalk_class' := 'Timer', pid := _}, T),
    lists:foreach(
        fun(_) ->
            receive
                tick -> ok
            after 1000 -> ?assert(false, "tick not delivered")
            end
        end,
        [1, 2, 3]
    ),
    ?assert(beamtalk_timer:'isActive'(T)),
    ?assert(beamtalk_timer:cancel(T)),
    ?assertNot(beamtalk_timer:'isActive'(T)),
    flush_ticks(),
    receive
        tick -> ?assert(false, "tick after cancel")
    after 50 -> ok
    end.

send_every_stops_when_target_dies_test() ->
    Target = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    T = beamtalk_timer:'send:to:every:'(tick, Target, 10),
    Target ! stop,
    wait_for_exit(maps:get(pid, T), 1000),
    ?assertNot(beamtalk_timer:'isActive'(T)).

send_every_zero_interval_raises_test() ->
    ?assertError(?TYPE_ERROR, beamtalk_timer:'send:to:every:'(tick, self(), 0)).

flush_ticks() ->
    receive
        tick -> flush_ticks()
    after 0 -> ok
    end.
