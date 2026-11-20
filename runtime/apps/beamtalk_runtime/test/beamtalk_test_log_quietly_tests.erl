%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_log_quietly_tests).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
`beamtalk_test_log:quietly/1` (BT-5051) must put the primary logger level back
when its caller is killed, which is what EUnit does to a test that exceeds its
timeout. A `try ... after` cannot cover that case.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

level() -> maps:get(level, logger:get_primary_config()).

restores_level_after_normal_return_test() ->
    Before = level(),
    ?assertEqual(done, beamtalk_test_log:quietly(fun() -> done end)),
    ?assertEqual(Before, level()).

restores_level_after_error_test() ->
    Before = level(),
    ?assertError(boom, beamtalk_test_log:quietly(fun() -> error(boom) end)),
    ?assertEqual(Before, level()).

restores_level_when_caller_is_killed_test() ->
    Before = level(),
    Self = self(),
    Caller = spawn(fun() ->
        beamtalk_test_log:quietly(fun() ->
            Self ! {quiet, level()},
            receive
                never -> ok
            end
        end)
    end),
    ?awaitMessage({quiet, none}),
    MRef = erlang:monitor(process, Caller),
    exit(Caller, kill),
    receive
        {'DOWN', MRef, process, Caller, _} -> ok
    end,
    ok = beamtalk_test_wait:until(fun() -> level() =:= Before end).
