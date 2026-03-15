%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_reactive_subprocess gen_server (BT-1187).
%%%
%%% **DDD Context:** runtime
%%%
%%% Tests cover:
%%% - stdout lines are pushed to the notify actor via cast
%%% - stderr lines are pushed to the notify actor via cast
%%% - exit notification is cast to notify actor after stdout/stderr are flushed
%%% - partial line (no trailing newline) is flushed and pushed on exit
%%% - writeLine: writes data to subprocess stdin and returns nil
%%% - close: terminates the subprocess and marks the port closed
%%% - exitCode: returns nil while running, integer after exit

-module(beamtalk_reactive_subprocess_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Cross-platform command helpers
%%% ============================================================================

%% @private Return {Executable, Args} for echo that outputs Text and exits.
echo_cmd(Text) ->
    case os:type() of
        {unix, _} -> {<<"/bin/echo">>, [Text]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"echo ", Text/binary>>]}
    end.

%% @private Return {Executable, Args} for a shell command.
shell_cmd(Script) ->
    case os:type() of
        {unix, _} -> {<<"/bin/sh">>, [<<"-c">>, Script]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, Script]}
    end.

%% @private Return {Executable, Args} for a long-running process with no stdout.
sleep_cmd() ->
    case os:type() of
        {unix, _} -> {<<"/bin/sleep">>, [<<"60">>]};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"ping -n 60 127.0.0.1 >nul">>]}
    end.

%% @private Return {Executable, Args} for a process that exits immediately with 0.
true_cmd() ->
    case os:type() of
        {unix, _} -> {<<"/bin/true">>, []};
        {win32, _} -> {<<"cmd">>, [<<"/c">>, <<"exit 0">>]}
    end.

%% @private Return platform-appropriate shell script for multi-line stdout.
multiline_script() ->
    case os:type() of
        {unix, _} -> <<"printf 'alpha\\nbeta\\ngamma\\n'">>;
        {win32, _} -> <<"(echo alpha& echo beta& echo gamma)">>
    end.

%% @private Return platform-appropriate shell script for multi-line stderr.
multiline_stderr_script() ->
    case os:type() of
        {unix, _} -> <<"printf 'x\\ny\\nz\\n' >&2">>;
        {win32, _} -> <<"(echo x& echo y& echo z)>&2">>
    end.

%% @private Return platform-appropriate shell script for partial line (no newline).
partial_line_script() ->
    case os:type() of
        {unix, _} -> <<"printf hello">>;
        {win32, _} -> <<"<nul set /p =hello">>
    end.

%% @private Return platform-appropriate shell script for combined stdout+stderr.
stdout_stderr_script() ->
    case os:type() of
        {unix, _} -> <<"echo out; echo err >&2">>;
        {win32, _} -> <<"echo out& echo err>&2">>
    end.

%%% ============================================================================
%%% Helper: build a mock notify beamtalk_object backed by a collector process
%%% ============================================================================

%% Start a simple collector process that receives cast messages and stores them.
%% Returns {NotifyObject, CollectorPid}.
make_collector() ->
    Self = self(),
    Pid = spawn_link(fun() -> collector_loop(Self, []) end),
    Obj = #beamtalk_object{
        class = 'MockDelegate',
        class_mod = 'mock_delegate',
        pid = Pid
    },
    {Obj, Pid}.

collector_loop(Owner, Acc) ->
    receive
        %% gen_server:cast/2 sends {'$gen_cast', Msg} to the target process
        {'$gen_cast', {cast, Selector, Args}} ->
            collector_loop(Owner, [{Selector, Args} | Acc]);
        {get_events, Ref} ->
            Owner ! {events, Ref, lists:reverse(Acc)},
            collector_loop(Owner, Acc);
        stop ->
            ok
    end.

get_events(CollectorPid) ->
    Ref = make_ref(),
    CollectorPid ! {get_events, Ref},
    receive
        {events, Ref, Events} -> Events
    after 5000 ->
        error(timeout_waiting_for_events)
    end.

wait_for_exit(CollectorPid) ->
    wait_for_exit(CollectorPid, 50).

wait_for_exit(CollectorPid, 0) ->
    error({timeout_waiting_for_exit, get_events(CollectorPid)});
wait_for_exit(CollectorPid, Tries) ->
    Events = get_events(CollectorPid),
    HasExit = lists:any(
        fun
            ({'subprocessExit:from:', _}) -> true;
            (_) -> false
        end,
        Events
    ),
    case HasExit of
        true ->
            Events;
        false ->
            timer:sleep(100),
            wait_for_exit(CollectorPid, Tries - 1)
    end.

%%% ============================================================================
%%% stdout lines pushed to notify actor
%%% ============================================================================

stdout_lines_pushed_to_notify_test() ->
    {Notify, Collector} = make_collector(),
    {ShExe, ShArgs} = shell_cmd(multiline_script()),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => ShExe,
        args => ShArgs,
        notify => Notify
    }),
    Events = wait_for_exit(Collector),
    Collector ! stop,
    gen_server:stop(Pid),
    LineEvents = [E || {'subprocessLine:from:', _} = E <- Events],
    Lines = [hd(Args) || {'subprocessLine:from:', Args} <- LineEvents],
    ?assertEqual([<<"alpha">>, <<"beta">>, <<"gamma">>], Lines).

%%% ============================================================================
%%% stderr lines pushed to notify actor
%%% ============================================================================

stderr_lines_pushed_to_notify_test() ->
    {Notify, Collector} = make_collector(),
    {ShExe, ShArgs} = shell_cmd(multiline_stderr_script()),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => ShExe,
        args => ShArgs,
        notify => Notify
    }),
    Events = wait_for_exit(Collector),
    Collector ! stop,
    gen_server:stop(Pid),
    StderrEvents = [E || {'subprocessStderrLine:from:', _} = E <- Events],
    Lines = [hd(Args) || {'subprocessStderrLine:from:', Args} <- StderrEvents],
    ?assertEqual([<<"x">>, <<"y">>, <<"z">>], Lines).

%%% ============================================================================
%%% exit notification is cast after data
%%% ============================================================================

exit_notification_cast_after_data_test() ->
    {Notify, Collector} = make_collector(),
    {EchoExe, EchoArgs} = echo_cmd(<<"done">>),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => EchoExe,
        args => EchoArgs,
        notify => Notify
    }),
    Events = wait_for_exit(Collector),
    Collector ! stop,
    gen_server:stop(Pid),
    %% Exit event must appear after stdout line event
    LinePos = first_position('subprocessLine:from:', Events),
    ExitPos = first_position('subprocessExit:from:', Events),
    ?assert(is_integer(LinePos)),
    ?assert(is_integer(ExitPos)),
    ?assert(LinePos < ExitPos),
    %% Exit code should be 0
    [{_, ExitArgs} | _] = [E || {'subprocessExit:from:', _} = E <- Events],
    ?assertEqual(0, hd(ExitArgs)).

first_position(Selector, Events) ->
    first_position(Selector, Events, 0).

first_position(_Selector, [], _N) ->
    not_found;
first_position(Selector, [{Selector, _} | _], N) ->
    N;
first_position(Selector, [_ | Rest], N) ->
    first_position(Selector, Rest, N + 1).

%%% ============================================================================
%%% partial line flushed on exit
%%% ============================================================================

partial_line_flushed_on_exit_test() ->
    {Notify, Collector} = make_collector(),
    {ShExe, ShArgs} = shell_cmd(partial_line_script()),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => ShExe,
        args => ShArgs,
        notify => Notify
    }),
    Events = wait_for_exit(Collector),
    Collector ! stop,
    gen_server:stop(Pid),
    LineEvents = [E || {'subprocessLine:from:', _} = E <- Events],
    Lines = [hd(Args) || {'subprocessLine:from:', Args} <- LineEvents],
    ?assertEqual([<<"hello">>], Lines).

%%% ============================================================================
%%% writeLine: writes data to stdin and returns nil
%%% ============================================================================

writeLine_writes_and_returns_nil_test() ->
    case os:type() of
        {unix, _} ->
            {Notify, Collector} = make_collector(),
            {ok, Pid} = beamtalk_reactive_subprocess:start(#{
                executable => <<"/bin/cat">>,
                args => [],
                notify => Notify
            }),
            Result = gen_server:call(Pid, {'writeLine:', [<<"ping">>]}),
            ?assertEqual(nil, Result),
            %% cat echoes back; wait for the line to arrive
            timer:sleep(200),
            Events = get_events(Collector),
            Collector ! stop,
            gen_server:call(Pid, {close, []}),
            gen_server:stop(Pid),
            LineEvents = [E || {'subprocessLine:from:', _} = E <- Events],
            Lines = [hd(Args) || {'subprocessLine:from:', Args} <- LineEvents],
            ?assert(lists:member(<<"ping">>, Lines));
        _ ->
            {skip, "Unix-only: no unbuffered stdin echo on Windows"}
    end.

%%% ============================================================================
%%% writeLine: after close returns port_closed error
%%% ============================================================================

writeLine_after_close_returns_error_test() ->
    {Notify, Collector} = make_collector(),
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs,
        notify => Notify
    }),
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    %% After close, writeLine: should return {error, #beamtalk_error{}} with kind port_closed
    Reply = gen_server:call(Pid, {'writeLine:', [<<"hello">>]}),
    ?assertMatch({error, #beamtalk_error{kind = port_closed}}, Reply),
    Collector ! stop,
    gen_server:stop(Pid).

%%% ============================================================================
%%% close: terminates subprocess interaction
%%% ============================================================================

close_terminates_subprocess_test() ->
    {Notify, Collector} = make_collector(),
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs,
        notify => Notify
    }),
    ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    %% Second close is idempotent
    ?assertEqual(nil, gen_server:call(Pid, {close, []})),
    Collector ! stop,
    gen_server:stop(Pid).

%%% ============================================================================
%%% exitCode: nil while running, integer after exit
%%% ============================================================================

exitCode_nil_while_running_test() ->
    {Notify, Collector} = make_collector(),
    {SleepExe, SleepArgs} = sleep_cmd(),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => SleepExe,
        args => SleepArgs,
        notify => Notify
    }),
    ?assertEqual(nil, gen_server:call(Pid, {exitCode, []})),
    gen_server:call(Pid, {close, []}),
    Collector ! stop,
    gen_server:stop(Pid).

exitCode_integer_after_exit_test() ->
    {Notify, Collector} = make_collector(),
    {TrueExe, TrueArgs} = true_cmd(),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => TrueExe,
        args => TrueArgs,
        notify => Notify
    }),
    _Events = wait_for_exit(Collector),
    ExitCode = gen_server:call(Pid, {exitCode, []}),
    Collector ! stop,
    gen_server:stop(Pid),
    ?assertEqual(0, ExitCode).

%%% ============================================================================
%%% stdout and stderr are delivered independently
%%% ============================================================================

stdout_and_stderr_independent_test() ->
    {Notify, Collector} = make_collector(),
    {ShExe, ShArgs} = shell_cmd(stdout_stderr_script()),
    {ok, Pid} = beamtalk_reactive_subprocess:start(#{
        executable => ShExe,
        args => ShArgs,
        notify => Notify
    }),
    Events = wait_for_exit(Collector),
    Collector ! stop,
    gen_server:stop(Pid),
    StdoutLines = [hd(A) || {'subprocessLine:from:', A} <- Events],
    StderrLines = [hd(A) || {'subprocessStderrLine:from:', A} <- Events],
    ?assertEqual([<<"out">>], StdoutLines),
    ?assertEqual([<<"err">>], StderrLines).

%%% ============================================================================
%%% beamtalk_subprocess_port: split_lines
%%% ============================================================================

split_lines_no_newline_test() ->
    ?assertEqual({[], <<"hello">>}, beamtalk_subprocess_port:split_lines(<<"hello">>)).

split_lines_single_newline_test() ->
    ?assertEqual({[<<"hello">>], <<>>}, beamtalk_subprocess_port:split_lines(<<"hello\n">>)).

split_lines_multiple_newlines_test() ->
    ?assertEqual(
        {[<<"a">>, <<"b">>, <<"c">>], <<>>},
        beamtalk_subprocess_port:split_lines(<<"a\nb\nc\n">>)
    ).

split_lines_partial_remainder_test() ->
    ?assertEqual(
        {[<<"a">>, <<"b">>], <<"c">>},
        beamtalk_subprocess_port:split_lines(<<"a\nb\nc">>)
    ).

split_lines_strips_cr_test() ->
    ?assertEqual(
        {[<<"hello">>], <<>>},
        beamtalk_subprocess_port:split_lines(<<"hello\r\n">>)
    ).

split_lines_strips_cr_multiple_test() ->
    ?assertEqual(
        {[<<"a">>, <<"b">>, <<"c">>], <<>>},
        beamtalk_subprocess_port:split_lines(<<"a\r\nb\r\nc\r\n">>)
    ).

%%% ============================================================================
%%% beamtalk_subprocess_port: flush_and_collect
%%% ============================================================================

flush_and_collect_combines_pending_and_data_test() ->
    ?assertEqual(
        {[<<"helloworld">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<"hello">>, <<"world\n">>)
    ).

flush_and_collect_empty_pending_test() ->
    ?assertEqual(
        {[<<"line">>], <<>>},
        beamtalk_subprocess_port:flush_and_collect(<<>>, <<"line\n">>)
    ).

flush_and_collect_partial_result_test() ->
    ?assertEqual(
        {[<<"hello">>], <<"world">>},
        beamtalk_subprocess_port:flush_and_collect(<<"hel">>, <<"lo\nworld">>)
    ).
