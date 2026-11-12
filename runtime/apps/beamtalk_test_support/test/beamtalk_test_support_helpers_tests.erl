%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_support_helpers_tests).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc "Self-tests for the BT-4677 shared helpers.".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

until_returns_ok_when_true_test() ->
    ?assertEqual(ok, beamtalk_test_wait:until(fun() -> true end, 100)).

until_polls_test() ->
    Counter = counters:new(1, []),
    ?assertEqual(
        ok,
        beamtalk_test_wait:until(
            fun() ->
                counters:add(Counter, 1, 1),
                counters:get(Counter, 1) >= 3
            end,
            2000,
            1
        )
    ).

until_times_out_test() ->
    ?assertError(
        {wait_timeout, {predicate, false}}, beamtalk_test_wait:until(fun() -> false end, 30)
    ).

dead_waits_on_monitor_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    Pid ! stop,
    ?assertEqual(ok, beamtalk_test_wait:dead(Pid)),
    %% Already dead: returns immediately.
    ?assertEqual(ok, beamtalk_test_wait:dead([Pid], 100)).

dead_times_out_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    ?assertError({wait_timeout, {alive, [Pid]}}, beamtalk_test_wait:dead(Pid, 20)),
    ok = beamtalk_test_proc:kill_and_wait(Pid).

kill_and_wait_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    ?assertEqual(ok, beamtalk_test_proc:kill_and_wait(Pid)),
    ?assertNot(is_process_alive(Pid)),
    ?assertEqual(ok, beamtalk_test_proc:kill_and_wait(Pid)).

stop_plain_and_otp_and_junk_test() ->
    Plain = beamtalk_test_proc:spawn_idle(),
    ?assertEqual(ok, beamtalk_test_proc:stop(Plain)),
    ?assertNot(is_process_alive(Plain)),
    {ok, Srv} = gen_server:start(beamtalk_test_support_dummy_server, [], []),
    ?assertEqual(ok, beamtalk_test_proc:stop(Srv)),
    ?assertNot(is_process_alive(Srv)),
    ?assertEqual(ok, beamtalk_test_proc:stop(not_a_pid)).

sync_test() ->
    {ok, Srv} = gen_server:start(beamtalk_test_support_dummy_server, [], []),
    ?assertEqual(ok, beamtalk_test_wait:sync(Srv)),
    ok = beamtalk_test_proc:stop(Srv).

await_down_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    MRef = monitor(process, Pid),
    Pid ! stop,
    ?assertEqual(ok, beamtalk_test_proc:await_down(MRef, Pid)).

tmp_dir_and_rm_rf_test() ->
    Dir = beamtalk_test_fs:tmp_dir("selftest"),
    ?assert(filelib:is_dir(Dir)),
    Nested = filename:join([Dir, "a", "b"]),
    ok = filelib:ensure_path(Nested),
    ok = file:write_file(filename:join(Nested, "f.txt"), <<"x">>),
    ?assertEqual(ok, beamtalk_test_fs:rm_rf(Dir)),
    ?assertNot(filelib:is_file(Dir)),
    ?assertEqual(ok, beamtalk_test_fs:rm_rf(Dir)).

with_tmp_home_restores_home_test() ->
    Old = os:getenv("HOME"),
    Seen = beamtalk_test_fs:with_tmp_home(fun(Dir) ->
        ?assertEqual(Dir, os:getenv("HOME")),
        ?assert(filelib:is_dir(Dir)),
        Dir
    end),
    ?assertEqual(Old, os:getenv("HOME")),
    ?assertNot(filelib:is_file(Seen)).

with_tmp_home_restores_on_crash_test() ->
    Old = os:getenv("HOME"),
    ?assertError(boom, beamtalk_test_fs:with_tmp_home(fun() -> error(boom) end)),
    ?assertEqual(Old, os:getenv("HOME")).

assert_bt_error_test() ->
    Err = #beamtalk_error{kind = does_not_understand, class = 'Integer', selector = foo},
    ?assertBtError(does_not_understand, 'Integer', foo, error(#{error => Err})),
    ?assertBtErrorResult(does_not_understand, 'Integer', foo, {error, Err}).

registry_and_runtime_test() ->
    Teardown = beamtalk_test_boot:registry(),
    ?assert(is_function(Teardown, 0)),
    ?assertEqual(ok, Teardown()),
    %% Idempotent.
    Teardown2 = beamtalk_test_boot:registry(),
    ?assertEqual(ok, Teardown2()).

class_start_stop_wait_test() ->
    Teardown = beamtalk_test_boot:registry(),
    try
        Name = 'BtTestSupportSelfClass',
        Pid = beamtalk_test_class:start(Name, #{
            superclass => none, module => beamtalk_test_support_dummy_server, meta => #{}
        }),
        ?assertEqual(ok, beamtalk_test_wait:class(Name)),
        ?assertEqual(ok, beamtalk_test_class:stop(Pid)),
        ?assertNot(is_process_alive(Pid)),
        ?assertError({class_not_registered, Name}, beamtalk_test_wait:class(Name, 30))
    after
        Teardown()
    end.
