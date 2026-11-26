%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bounded_tests).

%%% **DDD Context:** Runtime Context

-moduledoc "EUnit tests for `beamtalk_bounded:run/2` (BT-5192).".

-include_lib("eunit/include/eunit.hrl").

ok_test() ->
    ?assertEqual({ok, 3}, beamtalk_bounded:run(fun() -> 1 + 2 end, 1000)).

%% A raise keeps its class, so a caller can re-raise it unchanged.
raised_keeps_class_test() ->
    ?assertMatch(
        {raised, error, badarg, [_ | _]},
        beamtalk_bounded:run(fun() -> error(badarg) end, 1000)
    ),
    ?assertMatch({raised, throw, oops, _}, beamtalk_bounded:run(fun() -> throw(oops) end, 1000)).

%% On timeout the helper is dead and nothing it might still send reaches the
%% caller afterwards.
timeout_leaves_nothing_behind_test() ->
    Caller = self(),
    Result = beamtalk_bounded:run(
        fun() ->
            Caller ! {helper, self()},
            receive
            after infinity -> ok
            end
        end,
        50
    ),
    ?assertEqual(timeout, Result),
    Helper =
        receive
            {helper, Pid} -> Pid
        after 0 -> erlang:error(no_helper)
        end,
    ?assertNot(is_process_alive(Helper)),
    {messages, Left} = process_info(self(), messages),
    ?assertEqual([], Left).

%% A helper killed by someone else before answering is reported, not waited on.
down_test() ->
    Killer = spawn(fun() ->
        receive
            {helper, Pid} -> exit(Pid, kill)
        end
    end),
    Result = beamtalk_bounded:run(
        fun() ->
            Killer ! {helper, self()},
            receive
            after infinity -> ok
            end
        end,
        5000
    ),
    ?assertEqual({down, killed}, Result).
