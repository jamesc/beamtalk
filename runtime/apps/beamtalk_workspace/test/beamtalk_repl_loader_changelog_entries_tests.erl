%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_changelog_entries_tests).

-moduledoc """
A live edit whose ChangeLog append fails must not be silent (BT-5013): the
failure is logged at ERROR and surfaced as a reply warning, and the append call
cannot time out while the server still completes it.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

failing_append_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Ctx) ->
        [
            {"emit_change_entry warns and logs ERROR when append fails", fun() ->
                make_changes_dir_unwritable(Ctx),
                Events = beamtalk_test_log:with_capture(error, fun() ->
                    Result = beamtalk_repl_loader_changelog_entries:emit_change_entry(
                        method_info()
                    ),
                    ?assertMatch({warning, <<"The change is live", _/binary>>}, Result),
                    beamtalk_test_log:collect(200)
                end),
                ?assertMatch(
                    [#{level := error, meta := #{reason := _}}],
                    [E || E = #{level := error} <- Events]
                )
            end},
            {"changelog_warnings maps the result onto reply warnings", fun() ->
                ?assertEqual([], beamtalk_repl_loader_methods:changelog_warnings(ok)),
                ?assertEqual(
                    [<<"w">>], beamtalk_repl_loader_methods:changelog_warnings({warning, <<"w">>})
                )
            end}
        ]
    end}.

server_down_test() ->
    %% No ChangeLog server running: the call exits `noproc`, which must come back
    %% as a warning, never an exception.
    ?assertEqual(undefined, whereis(beamtalk_workspace_changelog)),
    ?assertMatch(
        {warning, _}, beamtalk_repl_loader_changelog_entries:emit_change_entry(method_info())
    ).

%% BT-5084: every emitter (not only emit_change_entry/1) must report a failed
%% append as `{warning, Text}` instead of discarding it. No ChangeLog server is
%% running, so each append exits `noproc`.
every_emitter_warns_when_append_fails_test_() ->
    ?assertEqual(undefined, whereis(beamtalk_workspace_changelog)),
    E = beamtalk_repl_loader_changelog_entries,
    Cases = [
        {"new-class", fun() -> E:emit_new_class_entry(<<"C5084">>, <<"src">>, <<"/p/c.bt">>) end},
        {"class-def", fun() -> E:emit_class_def_entry(<<"C5084">>, "new", "old") end},
        {"rewrite", fun() ->
            E:emit_rewrite_change_entry(
                #{
                    kind => 'rename-class',
                    class => <<"C5084">>,
                    intent => durable,
                    author => <<"a">>,
                    author_kind => human
                },
                #{definition => undefined, sites => []}
            )
        end},
        {"remove-method", fun() ->
            E:emit_remove_change_entry(<<"C5084">>, inc, instance, <<"a">>, human)
        end},
        {"extension remove", fun() ->
            E:emit_extension_remove_change_entry(
                <<"C5084">>, inc, instance, undefined, undefined, <<"a">>, human
            )
        end},
        {"remove-class", fun() ->
            E:emit_remove_class_change_entry(<<"C5084">>, #{}, <<"a">>, human)
        end}
    ],
    [
        {Name, fun() ->
            ?assertMatch({warning, <<"The change is live", _/binary>>}, Fun())
        end}
     || {Name, Fun} <- Cases
    ].

%% BT-5084: the eval-side warning queue the primitive paths (removeSelector:,
%% renameTo:, removeFromSystem, ...) use, since they have no reply to carry one.
warning_queue_test() ->
    _ = beamtalk_refactor_support:end_changelog_warnings(),
    %% Outside `with_changelog_warnings/1` a warning is not queued, even in a process
    %% that carries a session id (a class gen_server running a class method for a
    %% session caller is seeded that way, and nothing there would drain the queue).
    erlang:put(beamtalk_session_id, <<"s">>),
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"lost">>}),
    ?assertEqual([], beamtalk_refactor_support:take_changelog_warnings()),
    erlang:erase(beamtalk_session_id),
    ok = beamtalk_refactor_support:begin_changelog_warnings(),
    ok = beamtalk_refactor_support:note_changelog_result(ok),
    ok = beamtalk_refactor_support:note_changelog_result({error, no_workspace}),
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"a">>}),
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"b">>}),
    ?assertEqual([<<"a">>, <<"b">>], beamtalk_refactor_support:end_changelog_warnings()),
    %% Collecting is over: later warnings are not queued.
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"late">>}),
    ?assertEqual([], beamtalk_refactor_support:take_changelog_warnings()).

with_changelog_warnings_stops_collecting_on_raise_test() ->
    ?assertError(
        boom,
        beamtalk_repl_eval:with_changelog_warnings(fun() -> erlang:error(boom) end)
    ),
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"late">>}),
    ?assertEqual([], beamtalk_refactor_support:take_changelog_warnings()).

eval_reply_carries_primitive_changelog_warning_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    Result = beamtalk_repl_eval:with_changelog_warnings(fun() ->
        ok = beamtalk_refactor_support:note_changelog_result({warning, <<"w">>}),
        {ok, 1, <<>>, [<<"existing">>], State}
    end),
    ?assertMatch({ok, 1, <<>>, [<<"existing">>, <<"w">>], _}, Result).

%% BT-5084: a stalled server must not hang the caller forever.
append_timeout_is_an_error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(#{pid := Pid}) ->
        {timeout, 30, fun() ->
            ok = application:set_env(beamtalk_workspace, changelog_append_timeout_ms, 200),
            ok = sys:suspend(Pid),
            try
                Events = beamtalk_test_log:with_capture(error, fun() ->
                    ?assertMatch(
                        {error, #beamtalk_error{kind = changelog_write_error}},
                        beamtalk_workspace_changelog:append(input())
                    ),
                    beamtalk_test_log:collect(200)
                end),
                ?assertMatch([_ | _], [E || E = #{level := error} <- Events])
            after
                ok = sys:resume(Pid),
                application:unset_env(beamtalk_workspace, changelog_append_timeout_ms)
            end
        end}
    end}.

%% BT-5110 (item 6): retire_entries/1 waits as long as append/1 (the configured
%% bound, not gen_server's 5 s default), so the 200 ms bound set here is the one
%% that ends the wait.
retire_entries_honours_the_append_timeout_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(#{pid := Pid}) ->
        {timeout, 30, fun() ->
            ok = application:set_env(beamtalk_workspace, changelog_append_timeout_ms, 200),
            ok = sys:suspend(Pid),
            try
                T0 = erlang:monotonic_time(millisecond),
                ?assertExit({timeout, _}, beamtalk_workspace_changelog:retire_entries([1])),
                ?assert(erlang:monotonic_time(millisecond) - T0 < 3000)
            after
                ok = sys:resume(Pid),
                application:unset_env(beamtalk_workspace, changelog_append_timeout_ms)
            end
        end}
    end}.

%% BT-5136: an append that timed out is unconfirmed (the server still writes
%% it), so the warning says so and does not claim the entry is missing.
append_timeout_warning_says_unconfirmed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(#{pid := Pid}) ->
        {timeout, 30, fun() ->
            ok = application:set_env(beamtalk_workspace, changelog_append_timeout_ms, 200),
            ok = sys:suspend(Pid),
            try
                {warning, Text} = beamtalk_repl_loader_changelog_entries:emit_new_class_entry(
                    <<"C5136">>, <<"src">>, <<"/p/c.bt">>
                ),
                ?assertNotEqual(nomatch, binary:match(Text, <<"unconfirmed">>)),
                ?assertEqual(nomatch, binary:match(Text, <<"will not include it">>))
            after
                ok = sys:resume(Pid),
                application:unset_env(beamtalk_workspace, changelog_append_timeout_ms)
            end
        end}
    end}.

%% BT-5136: a retire that times out is still queued, so removing the class must
%% not also append a `remove-class` entry.
retire_timeout_does_not_append_remove_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(#{pid := Pid}) ->
        {timeout, 30, fun() ->
            E = beamtalk_repl_loader_changelog_entries,
            ok = E:emit_new_class_entry(<<"C5136r">>, <<"src">>, <<"/p/c.bt">>),
            ok = application:set_env(beamtalk_workspace, changelog_append_timeout_ms, 200),
            ok = sys:suspend(Pid),
            try
                Result = E:emit_remove_class_change_entry(<<"C5136r">>, #{}, <<"a">>, human),
                ?assertMatch({warning, <<"The class was removed", _/binary>>}, Result)
            after
                ok = sys:resume(Pid),
                application:unset_env(beamtalk_workspace, changelog_append_timeout_ms)
            end,
            %% The queued retire has run; nothing was appended after it.
            _ = beamtalk_workspace_changelog:epoch(),
            Kinds = [beamtalk_workspace_changelog:entry_kind(En) || En <- beamtalk_workspace_changelog:entries()],
            ?assertEqual(['new-class'], Kinds)
        end}
    end}.

%% BT-5084 item 9: removing a never-flushed new class retires its entries
%% without claiming they were flushed.
cancelled_new_class_is_not_reported_flushed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Ctx) ->
        fun() ->
            E = beamtalk_repl_loader_changelog_entries,
            ok = E:emit_new_class_entry(<<"C5084">>, <<"src">>, <<"/p/c.bt">>),
            ?assertMatch([_], beamtalk_workspace_changelog:active_entries()),
            ok = E:emit_remove_class_change_entry(<<"C5084">>, #{}, <<"a">>, human),
            ?assertEqual([], beamtalk_workspace_changelog:active_entries()),
            [Entry] = beamtalk_workspace_changelog:entries(),
            ?assert(beamtalk_workspace_changelog:entry_cancelled(Entry)),
            [View] = beamtalk_workspace_changelog_views:change_entries(),
            ?assertEqual(false, maps:get(flushed, View))
        end
    end}.

%% BT-5084 item 10: once every entry is active and the ring is over its bound,
%% appends stop rescanning the table until a flush/clear changes that.
rotation_does_not_rescan_when_nothing_is_archivable_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Ctx) ->
        {timeout, 120, fun() ->
            [{ok, _} = beamtalk_workspace_changelog:append(input()) || _ <- lists:seq(1, 1001)],
            ?assert(beamtalk_workspace_changelog:rotation_stuck()),
            ok = beamtalk_workspace_changelog:mark_flushed([1, 2]),
            ?assertNot(beamtalk_workspace_changelog:rotation_stuck())
        end}
    end}.

%% BT-5084 item 11: a `.bt-staging.*` directory a crash left behind is swept,
%% a fresh one (a live writer's) is not.
stale_staging_dirs_are_swept_test() ->
    Dir = beamtalk_test_fs:tmp_dir("bt5084staging"),
    ok = filelib:ensure_path(Dir),
    Old = filename:join(Dir, ".bt-staging.111"),
    Fresh = filename:join(Dir, ".bt-staging.222"),
    Other = filename:join(Dir, "keep"),
    [ok = filelib:ensure_path(P) || P <- [Old, Fresh, Other]],
    ok = file:write_file(filename:join(Old, "x.erl"), <<"x">>),
    ok = file:change_time(Old, {{2020, 1, 1}, {0, 0, 0}}),
    try
        ?assertEqual([Old], beamtalk_workspace_file:sweep_stale_staging_dirs(Dir)),
        ?assertNot(filelib:is_dir(Old)),
        ?assert(filelib:is_dir(Fresh)),
        ?assert(filelib:is_dir(Other))
    after
        beamtalk_test_fs:rm_rf(Dir)
    end.

%% BT-5084 (review): after a failed fsync/write `append_synced/2` cuts the log back
%% with `truncate_to/2`, so no whole line is left under a seq the next append reuses.
%% This pins `truncate_to/2` itself; the failure branch needs a write that fails
%% after landing bytes (EIO on fsync), which a unit test cannot provoke portably.
truncate_to_cuts_the_log_back_to_a_size_test() ->
    Dir = beamtalk_test_fs:tmp_dir("bt5084sync"),
    Path = filename:join(Dir, "changes.jsonl"),
    try
        ok = file:write_file(Path, <<"{\"a\":1}\n">>),
        Size = filelib:file_size(Path),
        %% Simulate the line having landed, then the cut-back to the recorded size.
        ok = file:write_file(Path, <<"{\"b\":2}\n">>, [append]),
        ?assert(filelib:file_size(Path) > Size),
        ok = beamtalk_workspace_changelog_log:truncate_to(Path, Size),
        ?assertEqual({ok, <<"{\"a\":1}\n">>}, file:read_file(Path))
    after
        beamtalk_test_fs:rm_rf(Dir)
    end.

successful_append_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Ctx) ->
        [?_assertEqual(ok, beamtalk_repl_loader_changelog_entries:emit_change_entry(method_info()))]
    end}.

append_waits_past_default_call_timeout_test_() ->
    {timeout, 30, fun append_waits_past_default_call_timeout/0}.

append_waits_past_default_call_timeout() ->
    %% A server busy longer than gen_server's 5s default (e.g. rotating) must not
    %% make the caller time out: append/1 waits up to its 60 s timeout.
    {WsId, TmpHome, OldHome} = beamtalk_test_fs:changelog_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
    try
        ok = sys:suspend(Pid),
        Self = self(),
        Caller = spawn_link(fun() ->
            Self ! {result, beamtalk_workspace_changelog:append(input())}
        end),
        %% sleep: hold the server suspended past the 5s default call timeout
        timer:sleep(5500),
        ok = sys:resume(Pid),
        receive
            {result, R} -> ?assertMatch({ok, _}, R)
        after 5000 -> ?assert(false)
        end,
        _ = Caller
    after
        beamtalk_test_proc:shutdown(Pid),
        beamtalk_test_fs:restore_home(OldHome),
        beamtalk_test_fs:rm_rf(TmpHome)
    end.

setup() ->
    {WsId, TmpHome, OldHome} = beamtalk_test_fs:changelog_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
    #{pid => Pid, workspace_id => WsId, tmp_home => TmpHome, old_home => OldHome}.

cleanup(#{pid := Pid, tmp_home := TmpHome, old_home := OldHome}) ->
    beamtalk_test_proc:shutdown(Pid),
    beamtalk_test_fs:restore_home(OldHome),
    beamtalk_test_fs:rm_rf(TmpHome),
    ok.

%% Make the sources dir a regular file so the append's disk write fails.
make_changes_dir_unwritable(#{workspace_id := WsId}) ->
    Dir = beamtalk_workspace_changelog_log:changes_dir(WsId),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    SourcesPath = filename:join(Dir, "sources"),
    _ = file:del_dir_r(SourcesPath),
    ok = file:write_file(SourcesPath, <<"not a dir">>).

method_info() ->
    #{
        class_name => <<"NoSuchBt5013Class">>,
        selector => <<"inc">>,
        is_class_method => false,
        method_source => <<"inc => 1">>,
        intent => durable,
        author => <<"repl">>,
        author_kind => human
    }.

input() ->
    #{
        class => <<"Counter">>,
        selector => <<"inc">>,
        kind => instance,
        source => <<"^ 1">>,
        prev_source => <<"^ 0">>,
        intent => durable,
        flushable => true,
        author => <<"s">>,
        author_kind => human,
        source_file => <<"/proj/src/counter.bt">>,
        span => #{start => 0, 'end' => 4}
    }.
