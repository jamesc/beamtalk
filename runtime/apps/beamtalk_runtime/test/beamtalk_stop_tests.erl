%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stop_tests).

%%% **DDD Context:** Runtime Context — shared leaf

-moduledoc """
BT-4494: `beamtalk_stop:stop/3`. A stop whose reply lands at a very short
deadline is a plain `timeout` exit (`gen_server:stop/3` raises
`error:timeout_value` there) and leaves no `'DOWN'` message behind. The
Beamtalk-level cases are in `stdlib/test/supervisor_stop_within_test.bt` and
`stdlib/test/actor_stop_with_test.bt`.
""".

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% A gen_server whose terminate/2 sleeps `SleepMs`, so a short stop wait
%% expires while it is still shutting down.
init(SleepMs) ->
    process_flag(trap_exit, true),
    {ok, SleepMs}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

%% sleep: simulates slow shutdown (class 2)
terminate(_Reason, SleepMs) -> timer:sleep(SleepMs).

start(SleepMs) ->
    {ok, Pid} = gen_server:start(?MODULE, SleepMs, []),
    Pid.

outcome(Pid, Reason, Timeout) ->
    try beamtalk_stop:stop(Pid, Reason, Timeout) of
        ok -> ok
    catch
        Class:Why -> {Class, Why}
    end.

%% Every 'DOWN' message waiting in this process's mailbox.
stray_downs() ->
    receive
        {'DOWN', _, _, _, _} = Down -> [Down | stray_downs()]
    after 0 -> []
    end.

short_waits_time_out_cleanly_test_() ->
    {timeout, 60, fun() ->
        Outcomes = [
            begin
                %% Far longer than any wait here, so a descheduled test
                %% process cannot see the shutdown finish first; the kill
                %% below means the sleep costs no test time.
                Pid = start(5000),
                Outcome = outcome(Pid, normal, Wait),
                exit(Pid, kill),
                Outcome
            end
         || Wait <- [0, 1, 2],
            _ <- lists:seq(1, 200)
        ],
        ?assertEqual([], [O || O <- Outcomes, O =/= {exit, timeout}]),
        %% A leaked monitor's 'DOWN' would arrive once each process is killed.
        %% sleep: asserts nothing arrives within a window (class 4)
        timer:sleep(50),
        ?assertEqual([], stray_downs())
    end}.

stop_within_wait_is_ok_test() ->
    Pid = start(0),
    ?assertEqual(ok, outcome(Pid, normal, 1000)),
    ?assertNot(is_process_alive(Pid)),
    ?assertEqual([], stray_downs()).

stop_with_infinity_is_ok_test() ->
    Pid = start(20),
    ?assertEqual(ok, outcome(Pid, shutdown, infinity)),
    ?assertNot(is_process_alive(Pid)).

dead_process_is_noproc_test() ->
    Pid = start(0),
    gen_server:stop(Pid),
    ?assertEqual({exit, noproc}, outcome(Pid, normal, 1000)),
    ?assertEqual([], stray_downs()).

timed_out_stop_keeps_shutting_down_test() ->
    Pid = start(500),
    Ref = erlang:monitor(process, Pid),
    ?assertEqual({exit, timeout}, outcome(Pid, normal, 10)),
    receive
        {'DOWN', Ref, process, Pid, Why} -> ?assertEqual(normal, Why)
    after 5000 -> ?assert(false)
    end.
