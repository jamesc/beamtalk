%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_changelog_entries_tests).

-moduledoc """
A live edit whose ChangeLog append fails must not be silent (BT-5013): the
failure is logged at ERROR and surfaced as a reply warning, and the append call
cannot time out while the server still completes it.
""".

-include_lib("eunit/include/eunit.hrl").

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

successful_append_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Ctx) ->
        [?_assertEqual(ok, beamtalk_repl_loader_changelog_entries:emit_change_entry(method_info()))]
    end}.

append_waits_past_default_call_timeout_test_() ->
    {timeout, 30, fun append_waits_past_default_call_timeout/0}.

append_waits_past_default_call_timeout() ->
    %% A server busy longer than gen_server's 5s default (e.g. rotating) must not
    %% make the caller time out: append/1 uses `infinity`.
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
