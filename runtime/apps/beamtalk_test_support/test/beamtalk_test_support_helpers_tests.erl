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

monitored_by_waits_for_named_watcher_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    Name = beamtalk_test_unique:atom("bt5106_watcher"),
    Watcher = spawn(fun() ->
        erlang:monitor(process, Pid),
        receive
            stop -> ok
        end
    end),
    register(Name, Watcher),
    ?assertEqual(ok, beamtalk_test_wait:monitored_by(Pid, Name)),
    Watcher ! stop,
    ok = beamtalk_test_proc:kill_and_wait(Pid).

monitored_by_times_out_without_watcher_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    ?assertError(
        {wait_timeout, {predicate, false}},
        beamtalk_test_wait:monitored_by(Pid, no_such_watcher_bt5106, 30)
    ),
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
    ?assertBtError(does_not_understand, 'Integer', foo, beamtalk_error:raise(Err)),
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

%% BT-4899: throwaway-module loader.

module_load_source_and_unload_test() ->
    Name = beamtalk_test_unique:atom("bt4899_src"),
    Src = io_lib:format("-module(~p).\n-export([f/0]).\nf() -> ok.\n", [Name]),
    ok = beamtalk_test_module:load(Name, Src),
    ?assertEqual(ok, Name:f()),
    ok = beamtalk_test_module:unload(Name),
    ?assertNot(erlang:module_loaded(Name)),
    %% Idempotent.
    ?assertEqual(ok, beamtalk_test_module:unload(Name)).

module_with_module_unloads_after_fun_raises_test() ->
    Name = beamtalk_test_unique:atom("bt4899_with"),
    Forms = [{attribute, 1, module, Name}],
    ?assertEqual(ok, beamtalk_test_module:with_module(Name, Forms, fun() -> ok end)),
    ?assertNot(erlang:module_loaded(Name)),
    ?assertError(
        boom,
        beamtalk_test_module:with_module(Name, Forms, fun() ->
            ?assert(erlang:module_loaded(Name)),
            error(boom)
        end)
    ),
    ?assertNot(erlang:module_loaded(Name)).

module_meta_module_test() ->
    Name = beamtalk_test_unique:atom("bt4899_meta"),
    Meta = #{class => 'Foo', fields => [a, b]},
    Name = beamtalk_test_module:meta_module(Name, Meta),
    try
        ?assertEqual(Meta, Name:'__beamtalk_meta'())
    after
        beamtalk_test_module:unload(Name)
    end.

%% BT-4899: log capture.

log_capture_awaits_matching_event_test() ->
    beamtalk_test_log:with_capture(error, fun() ->
        logger:error("bt4899 first"),
        logger:error("bt4899 second"),
        {ok, Event} = beamtalk_test_log:await(
            fun
                (#{msg := {string, Msg}}) -> lists:prefix("bt4899 sec", Msg);
                (_) -> false
            end,
            1000
        ),
        ?assertMatch(#{level := error}, Event),
        ?assertEqual(
            timeout, beamtalk_test_log:await(fun(#{msg := {string, "nope"}}) -> true end, 20)
        )
    end).

log_capture_restores_level_and_handler_test() ->
    #{level := Before} = logger:get_primary_config(),
    HandlersBefore = logger:get_handler_ids(),
    ?assertError(
        boom, beamtalk_test_log:with_capture(all, fun() -> error(boom) end)
    ),
    ?assertEqual(Before, maps:get(level, logger:get_primary_config())),
    ?assertEqual(HandlersBefore, logger:get_handler_ids()).

%% BT-4899: process / repl helpers.

proc_dead_pid_and_shutdown_test() ->
    Dead = beamtalk_test_proc:dead_pid(),
    ?assertNot(is_process_alive(Dead)),
    Pid = beamtalk_test_proc:spawn_idle(),
    ?assertEqual(ok, beamtalk_test_proc:shutdown(Pid)),
    ?assertNot(is_process_alive(Pid)),
    ?assertEqual(ok, beamtalk_test_proc:shutdown(Dead)).

repl_assert_error_response_test() ->
    Json = iolist_to_binary(
        json:encode(#{<<"status">> => [<<"done">>, <<"error">>], <<"error">> => <<"x">>})
    ),
    ?assertMatch(#{<<"error">> := <<"x">>}, beamtalk_test_repl:assert_error_response(Json)).

%%% BT-5038: ?awaitMessage / ?assertNoMessage / with_extension / unlinking kill

flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 -> ok
    end.

await_message_binds_pattern_variables_test() ->
    self() ! {worker_ready, 42, ignored},
    ?awaitMessage({worker_ready, Id, _}),
    ?assertEqual(42, Id),
    self() ! {again, Id},
    ?awaitMessage({again, Id}, 100).

await_message_timeout_names_pattern_and_shows_mailbox_test() ->
    %% EUnit runs this module's tests in one process, so exit signals and
    %% messages left by earlier tests (trapped `'EXIT'`s) may be in the mailbox:
    %% start from an empty one so the expected mailbox is exactly ours.
    flush_mailbox(),
    self() ! {unrelated, mail},
    try
        ?awaitMessage({never_sent, _Pid}, 10),
        ?assert(false)
    catch
        error:{await_message_timeout, Info} ->
            ?assertEqual("{ never_sent , _Pid }", proplists:get_value(pattern, Info)),
            ?assertEqual(10, proplists:get_value(timeout_ms, Info)),
            ?assertEqual([{unrelated, mail}], proplists:get_value(mailbox, Info))
    after
        receive
            {unrelated, mail} -> ok
        after 0 -> ok
        end
    end.

assert_no_message_test() ->
    ?assertNoMessage({ghost, _}, 10),
    self() ! {ghost, 1},
    ?assertError({unexpected_message, _}, ?assertNoMessage({ghost, _}, 10)).

kill_and_wait_survives_linked_victim_test() ->
    Pid = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(ok, beamtalk_test_proc:kill_and_wait(Pid)),
    ?assertNot(is_process_alive(Pid)).
